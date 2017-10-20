/**
 * 
 */
package DWR.DMS.PTM;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.Calendar;
import java.nio.IntBuffer;
import java.io.BufferedWriter;
import java.io.IOException;
import java.lang.reflect.Constructor;
/**
 * @author xwang
 *
 */
public class RouteInputs {
	public RouteInputs(ArrayList<String> inText, String fishType) {
		if (fishType == null)
				PTMUtil.systemExit("No Particle Type! exit.");
		_fishType = fishType;
		if (inText != null){
			_barriers = new ArrayList<NonPhysicalBarrier>();
			_fishScreens = new ArrayList<IntBuffer>();
			//if pathname is not input, the entrainment file will not be written.
			_pathFileName = PTMUtil.getPathFromLine(inText.get(0), ':');	
			ArrayList<String> barriersInText = PTMUtil.getInputBlock(inText, "BARRIERS", "END_BARRIERS");
			ArrayList<String> screensInText = PTMUtil.getInputBlock(inText, "FISH_SCREENS", "END_FISH_SCREENS");
			ArrayList<String> dicuInText = PTMUtil.getInputBlock(inText, "DICU_FILTER", "END_DICU_FILTER");
			ArrayList<String> specialBehaviorInText = PTMUtil.getInputBlock(inText, "SPECIAL_BEHAVIORS", "END_SPECIAL_BEHAVIORS");
			_entrainmentRates = new HashMap<Integer, ArrayList<ArrayList<Object>>>();
			
			if( barriersInText == null || barriersInText.size() < 6)
				System.err.println("WARNING: no non-physical-barrier info found or the info is not properly defined in behavior inputs.");
			else
				setBarriers(barriersInText);
			
			if( screensInText == null || screensInText.size() < 2)
				System.err.println("WARNING: no fish screen info found or the info is not properly defined in behavior inputs.");
			else
				setFishScreens(screensInText);
			
			if( dicuInText == null || dicuInText.size() < 1)
				System.err.println("WARNING: no dicu info found or the info is not properly defined in behavior inputs.");
			else{
				try{
					_dicuFilterEfficiency = PTMUtil.getFloatFromLine(dicuInText.get(0).trim(), "Filter_Efficiency");
				}catch (NumberFormatException e){
					e.printStackTrace();
					PTMUtil.systemExit("expect a float for dicu efficiency coefficient but get:" + dicuInText.get(0));	
				}					
				setDicuFilterEfficiency();
			}
			if( specialBehaviorInText == null || specialBehaviorInText.size() < 2)
				System.err.println("WARNING: no special routing Behavior defined or defined improperly in behavior inputs.");
			else
				setSpecialBehaviors(specialBehaviorInText);
		}
	}
	public void setBarrierNodeInfo(Node[] allNodes){
	    //nodeArray starts from 1 PTMFixedInput.java line 287
		if (_barriers == null)
			System.err.println("WARNING: no non-pysical barriers info avaiable while setting up node info");
		else
			for (NonPhysicalBarrier barrier: _barriers)
				allNodes[barrier.getNodeId()].installBarrier();
	}
	
	public void setFishScreenNodeInfo(Node[] allNodes){
	    //nodeArray starts from 1 PTMFixedInput.java line 287
		if (_fishScreens == null)
			System.err.println("WARNING: no fish screen info avaiable while set up the model node info");
		else
			for (IntBuffer screen: _fishScreens)
				allNodes[screen.get(0)].installFishScreen();
	}
	
	public void setBarrierWbInfo(Waterbody[] allWbs){
		//wbArray start from 1 see PTMFixedInput.java line 180
		//Channels are first filled in wbArray
		if (_barriers == null)
			System.err.println("WARNING: no non-pysical barriers info avaiable while setting up water body info");
		else{
			for (NonPhysicalBarrier barrier: _barriers)
				allWbs[barrier.getWaterbodyId()].installBarrier(barrier.getNodeId());
		}
	}
	
	public void setFishScreenWbInfo(Waterbody[] allWbs){
		if (_fishScreens == null)
			System.err.println("WARNING: no fish screen info avaiable while set up the model water body info");
		else{
			for (IntBuffer screen: _fishScreens)
				allWbs[screen.get(1)].installFishScreen(screen.get(0));
		}
	}

	public void updateCurrentBarrierInfo(Waterbody[] allWbs, int currentTime){
		if (_barriers != null){
			for (NonPhysicalBarrier barrier: _barriers)
				allWbs[barrier.getWaterbodyId()].setCurrentBarrierOp(barrier.getNodeId(), 
						barrier.getBarrierOp(PTMUtil.modelTimeToCalendar(currentTime, Globals.TIME_ZONE)));
		}
	}
	
	public float getDicuFilterEfficiency(){return _dicuFilterEfficiency;}
	public RouteHelper getRouteHelper(){
		if (_routeHelper == null)
			setRouteHelper();
		return _routeHelper;
	}
	private void setRouteHelper(){
		if(_fishType.equalsIgnoreCase("SALMON")){
			if (_specialBehaviorNames == null)
				_routeHelper = new SalmonRouteHelper(new SalmonBasicRouteBehavior(this));
			else{
				Map<Integer, SalmonRouteBehavior> specialBehaviors = new ConcurrentHashMap<Integer, SalmonRouteBehavior>();
				//load special behaviors
				try{
					for (int nodeId: _specialBehaviorNames.keySet()){
						Class<?> c = Class.forName("DWR.DMS.PTM."+_specialBehaviorNames.get(nodeId));
						Constructor<?> con = c.getConstructor(RouteInputs.class, Integer.class);
						specialBehaviors.put(nodeId, (SalmonRouteBehavior)con.newInstance(this, nodeId));
					}
				}catch(Exception e){
					e.printStackTrace();
					System.err.println("Error: " + e.getMessage());
					PTMUtil.systemExit("SYSTEM EXIT: got error when trying to add special fish behaviors to the helper");
				}
				//create a route helper with special behaviors
				_routeHelper = new SalmonRouteHelper(new SalmonBasicRouteBehavior(this), specialBehaviors);
			}
			System.out.println("Created Particle Salmon Route Helper");
		}
		else
			PTMUtil.systemExit("System Exit: only implemented for SALMON not for: "+_fishType);
	}
	public ConcurrentHashMap<Integer, String> getSpecialBehaviorNames(){ return _specialBehaviorNames; }
	public Map<String, Integer> getNameChannelLookUp() { return _nameChanLookUp;}
	public int getChannelId(String channelName) {return _nameChanLookUp.get(channelName);}
	public void putEntrainmentRate(int nodeId, float sacUpFlow, int dayTime, int BarrierOp, Float flowSplit, double entrainmentRate){
		if ( _entrainmentRates.get(nodeId) == null)
			_entrainmentRates.put(nodeId, new ArrayList<ArrayList<Object>>());
		ArrayList<Object> elm = new ArrayList<Object>();
		elm.add(sacUpFlow);
		elm.add(dayTime);
		elm.add(BarrierOp);
		elm.add(flowSplit);
		elm.add(entrainmentRate);
		_entrainmentRates.get(nodeId).add(elm);
	}
	public Map<Integer, ArrayList<ArrayList<Object>>> getEntrainmentRates(){return _entrainmentRates;}
	public void writeEntrainmentRates(){
		if (_pathFileName == null)
				return;
		try{
			BufferedWriter srWriter = PTMUtil.getOutputBuffer(_pathFileName);
			srWriter.write("Node ID".concat(",").concat("Sac Up Flow (cfs)").concat(",")
					.concat("Day Time (day/night = 0/1)").concat(",")
					.concat("Barrier Operation (on/off = 1/0)").concat(",")
					.concat("% to GS").concat(",")
					.concat("Entrainment Probability"));
			srWriter.newLine();
			//_ttHolder will never be null and there is at least one element
			for (int ndId: _entrainmentRates.keySet()){						
				for(ArrayList<Object> elm: _entrainmentRates.get(ndId)){
					srWriter.write(Integer.toString(ndId).concat(",").concat(Float.toString(((Float) elm.get(0))))
							.concat(",").concat(Integer.toString(((Integer) elm.get(1))))
							.concat(",").concat(Integer.toString(((Integer) elm.get(2))))
							.concat(",").concat(Float.toString(((Float) elm.get(3))))
							.concat(",").concat(Double.toString(((Double) elm.get(4)))));
					srWriter.newLine();
				}	
			}
			PTMUtil.closeBuffer(srWriter);
		}catch(IOException e){
			System.err.println("error occured when writing out survival rates!");
			e.printStackTrace();
		}
	}
	private void setDicuFilterEfficiency(){
		if (_dicuFilterEfficiency > 0){
			//TODO particle has basic route behavior as Salmon???
			if(_fishType.equalsIgnoreCase("SALMON"))
				SalmonBasicRouteBehavior.setDicuFilterEfficiency(_dicuFilterEfficiency);
			else if(_fishType.equalsIgnoreCase("PARTICLE"))
				BasicRouteBehavior.setDicuFilterEfficiency(_dicuFilterEfficiency);
			else if (_fishType.equalsIgnoreCase("SMELT"))
				PTMUtil.systemExit("the method to set Dicu filter for smelt has been defined yet");
			else
				PTMUtil.systemExit("don't know how to deal with this fish type:" + _fishType+", system exit");
		}
	}
	
	private void setSpecialBehaviors(ArrayList<String> inText){
		//set up a channel look up table so that a channel number can be searched if a channel name is given 
		 Map<String, Integer> lookUp= PTMUtil.getStrIntPairsFromLine(inText.get(0), "Channel_Name_Look_Up");
		 _nameChanLookUp = new HashMap<String, Integer>();
		 //convert to internal nodeIds
		 for (String key: lookUp.keySet())
			 _nameChanLookUp.put(key, PTMHydroInput.getIntFromExtChan(lookUp.get(key)));
		// make sure the title line in the input file is right 
		String title = inText.get(1);
		String shouldBe[] = {"NODEID", "CLASS_NAME"};
		PTMUtil.checkTitle(title, shouldBe);
		// read in node vs. special junction class name
		_specialBehaviorNames = new ConcurrentHashMap<Integer, String>(); 
		for (String line: inText.subList(2, inText.size())){
			String [] items = line.trim().split("[,\\s\\t]+");
			if (items.length!=2)
				PTMUtil.systemExit("SYSTEM EXIT: error while reading special behavior inputs:"+line);; 
			_specialBehaviorNames.put(getEnvNodeId(items[0]), items[1]);
		} 
		if(DEBUG == true){
			for (String key:_nameChanLookUp.keySet())
				System.out.println("_nameChanLookUp Key: "+key+" chan: "+PTMHydroInput.getExtFromIntChan(_nameChanLookUp.get(key)));
			for (int key:_specialBehaviorNames.keySet())
				System.out.println("_specialBehaviorNames Key: "+PTMHydroInput.getExtFromIntNode(key)+" value: "+_specialBehaviorNames.get(key));
		}
	}
	private void setBarriers(ArrayList<String> inText){
		// first line of inText is number_of_barriers: number
		int numberOfBarriers = PTMUtil.getInt(inText.get(0));
		for (int i = 1; i<numberOfBarriers+1; i++){
			setBarrier(PTMUtil.getInputBlock(inText, "BARRIER"+i, "END_BARRIER"+i));
		}	
	}

	private void setBarrier(ArrayList<String> barrierText){
		String shouldBe[] = {"NODEID", "CHANNELID/RESERVOIRNAME/OBJ2OBJNAME", "DISTANCE"};
		if( barrierText == null || barrierText.size() < 4)
			PTMUtil.systemExit("SYSTEM EXIT: the non-physical-barrier operation info is not properly defined in behavior inputs.");
		else{
			PTMUtil.checkTitle(barrierText.get(0), shouldBe);
			String[] items = barrierText.get(1).trim().split("[,\\s\\t]+");
			// convert to Internal Env Ids
			int[] ids = getEnvIds(items);
			addBarrier(barrierText, ids);
		}
	}
	private void addBarrier(ArrayList<String> barrierBlock, int[] nodeWbIds){
		  ConcurrentHashMap<PTMPeriod, Integer> bOpTS = new ConcurrentHashMap<PTMPeriod, Integer>();
		  Calendar s_time = null, e_time = null;
		  int op = -99;
		  
		  // first 2 lines are barrier location info: nodeid wbid
		  // the third line is operation schedule title.  operation schedule data start from forth line
		  // and the data end before line: end_barrieri
		  for (String inLine: barrierBlock.subList(3, barrierBlock.size())){
			  String[] items = inLine.trim().split("[,\\s\\t]+");
			  int optemp = -99;
			  try{
				  if (items.length < 3)
					  throw new NumberFormatException();
				  e_time = PTMUtil.getDateTime(items[0], items[1], Globals.TIME_ZONE);
				  optemp = Integer.parseInt(items[2].trim());
			  }catch (NumberFormatException e){
					e.printStackTrace();
					PTMUtil.systemExit("operation schecule line has wrong format: "+inLine);	
			  }
			  /*
			   * schedule example:
			   * Date		Time		On/Off
			   * 2/1/2011	0:00		0
			   * 3/17/2011	18:45		1
			   * 6/30/2011	0:00		0
			   * 
			   * from startTime(2/1/2011 0:00) - EndTime(3/17/2011 18:45) barrier off
			   * from startTime(3/17/2011 18:45) - EndTime(6/30/2011 0:00) barrier on
			   * operation at endTime takes the next time interval value, e.g. op at 3/17/2011 18:45 is on
			   * see BarrierOpPeriod for period treatment
			   */
			  if (s_time != null){
				  bOpTS.put(new PTMPeriod(s_time, e_time), op);
				  //System.out.println(s_time.getTime()+"--"+e_time.getTime()+"  op:"+op);
			  }
			  op = optemp;
			  s_time = e_time;
		  }
		  _barriers.add(new NonPhysicalBarrier(nodeWbIds[0], nodeWbIds[1], bOpTS));
	}
	private void setFishScreens(ArrayList<String> inText){	
		// first line is the title
		String shouldBe[] = {"NODEID", "CHANNELID/RESERVOIRNAME/OBJ2OBJNAME", "DISTANCE"};
		PTMUtil.checkTitle(inText.get(0), shouldBe);
		for (int i = 1; i<inText.size(); i++){
			int[] ids = getEnvIds(inText.get(i).trim().split("[,\\s\\t]+")); 
			_fishScreens.add(IntBuffer.wrap(ids));
		}
	}
	
	private Integer getEnvNodeId(String idStr){
		Integer nodeId = null;
		if (idStr == null)
			PTMUtil.systemExit("SYSTEM EXIT: try to get internal Id for Node but the string is empty. ");
		try{
			nodeId = Integer.parseInt(idStr);
		}catch(NumberFormatException e){
				e.printStackTrace();
				PTMUtil.systemExit("SYSTEM EXIT: node id has a wrong format:" + idStr);
		}
		nodeId = PTMHydroInput.getIntFromExtNode(nodeId);
		return nodeId;
	}
	private int[] getEnvIds(String[] items){
		if (items.length<2)
			PTMUtil.systemExit("SYSTEM EXIT: Barrier or fish screen ID input line should have more than 3 items, and less items noticed. ");
		Integer wbId=null;
		Integer nodeId = getEnvNodeId(items[0]);
		// find external channel id or reservoir/obj2obj name
		try{
			wbId = Integer.parseInt(items[1]);
			wbId = PTMHydroInput.getIntFromExtChan(wbId);
		}catch(NumberFormatException e){
			if ((wbId=PTMEnv.getReservoirObj2ObjEnvId(items[1])) == null){
				e.printStackTrace();
				PTMUtil.systemExit("SYSTEM EXIT:wrong channel/reservior/obj2obj id:" + items[1]);
			}
		}
		if (nodeId == null || wbId == null)
			PTMUtil.systemExit("SYSTEM EXIT: wrong node/channel/reservior/obj2obj ids, node id: " + items[0]+" waterbody Id: "+items[1]);
		return new int[] {nodeId, wbId};
	}

	private String _pathFileName;
	private ArrayList<IntBuffer> _fishScreens = null;
	private float _dicuFilterEfficiency;
	private ArrayList<NonPhysicalBarrier> _barriers = null;
	private RouteHelper _routeHelper = null;
	private String _fishType = null;
	//<nodeId, specialBehavior class name>
	private ConcurrentHashMap<Integer, String> _specialBehaviorNames = null;
	private Map<String, Integer> _nameChanLookUp;
	private boolean DEBUG = false;
	//Map<nodeId, <pid, entrainment rate>>
	private Map<Integer, ArrayList<ArrayList<Object>>> _entrainmentRates;

}

/*
   import java.lang.reflect.Constructor;
 	private void addRouteBehavior(int[] ids, String name){
		try{
			if (ids == null || ids.length<2)
				PTMUtil.systemExit("SYSTEM EXIT: cannot add Route Behavior: node id and wb id missing");
			int nodeId = ids[0], wbId = ids[1];
			if (!name.contains(_fishType))
				PTMUtil.systemExit("SYSTEM EXIT: the type of fish species is not consistent with special behavior class names:"+_fishType);
			Class<?> c = Class.forName("DWR.DMS.PTM."+name);
			Constructor<?> con = c.getConstructor(Integer.class, Integer.class);
			//TODO will use switch when we switch to jave 1.7
			if (_fishType.equalsIgnoreCase("SALMON"))
				((SalmonRouteHelper)_routeHelper).addSpecialBehavior(nodeId, (SalmonRouteBehavior)con.newInstance(nodeId, wbId));
			else if (_fishType.equalsIgnoreCase("SMELT")){
				//TODO do something
				PTMUtil.systemExit("SYSTEM EXIT: the special behavior for Smelt has not been defined yet:"+_fishType);
			}
			else
				PTMUtil.systemExit("SYSTEM EXIT: the fish species type is not defined:"+_fishType);			
		}catch(Exception e){
			e.printStackTrace();
			System.err.println("Error: " + e.getMessage());
			PTMUtil.systemExit("SYSTEM EXIT: got error when trying to add special fish behaviors to the helper");
		}
		System.out.println("Added Route Behaviors...");
	}
*/
