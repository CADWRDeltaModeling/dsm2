/**
 * 
 */
package DWR.DMS.PTM;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Calendar;
import java.nio.IntBuffer;
import java.lang.reflect.Constructor;
/**
 * @author xwang
 *
 */
public class RouteInputs {
	private boolean DEBUG = false;
	public RouteInputs(ArrayList<String> inText, String fishType) {
		if (fishType == null)
				PTMUtil.systemExit("No Particle Type! exit.");
		_fishType = fishType;
		setHelper();
		if (inText != null){
			_barriers = new ArrayList<NonPhysicalBarrier>();
			_fishScreens = new ArrayList<IntBuffer>();
			ArrayList<String> barriersInText = PTMUtil.getInputBlock(inText, "BARRIERS", "END_BARRIERS");
			ArrayList<String> screensInText = PTMUtil.getInputBlock(inText, "FISH_SCREENS", "END_FISH_SCREENS");
			ArrayList<String> dicuInText = PTMUtil.getInputBlock(inText, "DICU_FILTER", "END_DICU_FILTER");
			ArrayList<String> specialBehaviorInText = PTMUtil.getInputBlock(inText, "SPECIAL_BEHAVIORS", "END_SPECIAL_BEHAVIORS");
			
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
				_dicuFilterEfficiency = PTMUtil.getDouble(dicuInText.get(0).trim());
				setDicuFilterEfficiency();
			}
			if( specialBehaviorInText == null || specialBehaviorInText.size() < 2)
				System.err.println("WARNING: no specialBehavior defined or defined improperly in behavior inputs.");
			else
				setBehaviors(specialBehaviorInText);
		}
		
		System.out.println("Created RouteHelper...");
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

	public boolean updateCurrentBarrierInfo(Waterbody[] allWbs, int currentTime){
		if (_barriers == null)
			return false;
		else{
			for (NonPhysicalBarrier barrier: _barriers)
				allWbs[barrier.getWaterbodyId()].setCurrentBarrierOp(barrier.getNodeId(), 
						barrier.getBarrierOp(PTMUtil.convertHecTime(currentTime)));
		}
		return true;
	}
	
	public double getDicuFilterEfficiency(){return _dicuFilterEfficiency;}
	public RouteHelper getRouteHelper(){ return _routeHelper;}
	
	private void setHelper(){
		if ( _fishType.equalsIgnoreCase("PARTICLE"))
			//_routeHelper = new ParticleRouteHelper(new BasicRouteBehavior());
			_routeHelper = new SalmonRouteHelper(new SalmonBasicRouteBehavior());
		else if(_fishType.equalsIgnoreCase("SALMON"))
			_routeHelper = new SalmonRouteHelper(new SalmonBasicRouteBehavior());
		else if (_fishType.equalsIgnoreCase("SMELT"))
			PTMUtil.systemExit("No smelt helper defined, system exit.");
		else
			PTMUtil.systemExit("No helper defined, system exit.");
	}
	
	private void setDicuFilterEfficiency(){
		if (_dicuFilterEfficiency > 0){
			//TODO particle has basic route behavior as Salmon???
			if(_fishType.equalsIgnoreCase("SALMON")|| _fishType.equalsIgnoreCase("PARTICLE"))
				SalmonBasicRouteBehavior.setDicuFilterEfficiency(_dicuFilterEfficiency);
		}
		else if (_fishType.equalsIgnoreCase("SMELT"))
			PTMUtil.systemExit("the method to set Dicu filter for smelt has been defined yet");
		else
			PTMUtil.systemExit("the method to set Dicu filter for other species has been defined yet");
	}
	
	private void setBehaviors(ArrayList<String> inText){
		String title = inText.get(0);
		String [] tItems = title.trim().split("[,\\s\\t]+");
		checkTitle(title);
		if (tItems.length<3 || !tItems[2].equalsIgnoreCase("Class_Name"))
			PTMUtil.systemExit("SYSTEM EXIT: error in input:"+inText.get(0));
		for (String line: inText.subList(1, inText.size())){
			String [] items = line.trim().split("[,\\s\\t]+");
			if (items.length<3)
				PTMUtil.systemExit("SYSTEM EXIT: error in input:"+line);
			int[] ids = getEnvIds(items); 
			addRouteBehavior(ids, items[2]);
		} 
	}
	
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
	
	private void setBarriers(ArrayList<String> inText){
		// first line of inText is number_of_barriers: number
		int numberOfBarriers = PTMUtil.getInt(inText.get(0));
		for (int i = 1; i<numberOfBarriers+1; i++){
			setBarrier(PTMUtil.getInputBlock(inText, "BARRIER"+i, "END_BARRIER"+i));
		}	
	}

	private void setBarrier(ArrayList<String> barrierText){
		if( barrierText == null || barrierText.size() < 4)
			PTMUtil.systemExit("SYSTEM EXIT: the non-physical-barrier operation info is not properly defined in behavior inputs.");
		else{
			checkTitle(barrierText.get(0));
			String[] items = barrierText.get(1).trim().split("[,\\s\\t]+");
			// convert to Internal Env Ids
			int[] ids = getEnvIds(items);
			addBarrier(barrierText, ids);
		}
	}
	private void addBarrier(ArrayList<String> barrierBlock, int[] nodeWbIds){
		  HashMap<BarrierOpPeriod, Integer> bOpTS = new HashMap<BarrierOpPeriod, Integer>();
		  Calendar s_time = null, e_time = null;
		  int op = -99;
		  
		  // first 2 lines are barrier location info: nodeid wbid
		  // the third line is operation schedule title.  operation schedule data start from forth line
		  // and the data end before line: end_barrieri
		  for (String inLine: barrierBlock.subList(3, barrierBlock.size())){
			  String[] items = inLine.trim().split("[,\\s\\t]+");
			  String[] dateStr = items[0].trim().split("[-/]+"), timeStr = items[1].trim().split("[:]+");
			  int optemp = -99, year = -99, month = -99, day = -99, hour = -99, minute = -99;
			  try{
				  if (items.length<3 || dateStr.length<3 || timeStr.length<2)
					  throw new NumberFormatException();
				  optemp = Integer.parseInt(items[2].trim());
				  year = Integer.parseInt(dateStr[2]);
				  // java month start from 0
				  month = Integer.parseInt(dateStr[0])-1;
				  day = Integer.parseInt(dateStr[1]);
				  hour = Integer.parseInt(timeStr[0]);
				  minute = Integer.parseInt(timeStr[1]);
				  if(DEBUG) System.out.println("year:"+year+" month:"+month+" day:"+day+" hour:"+hour+" minute:"+minute+" op:"+optemp);
			  }catch (NumberFormatException e){
					e.printStackTrace();
					PTMUtil.systemExit("operation schecule line has wrong format: "+inLine);	
			  }
					  
			  e_time = Calendar.getInstance();
			  e_time.clear();
			  if (year == -99||month == -99|| day== -99|| hour== -99|| minute== -99||optemp == -99 )
				  PTMUtil.systemExit("wrong format for barrier operation schedule data: "+inLine);
			  e_time.set(year, month, day, hour, minute);
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
				  bOpTS.put(new BarrierOpPeriod(s_time, e_time), op);
				  //System.out.println(s_time.getTime()+"--"+e_time.getTime()+"  op:"+op);
			  }
			  op = optemp;
			  s_time = e_time;
		  }
		  _barriers.add(new NonPhysicalBarrier(nodeWbIds[0], nodeWbIds[1], bOpTS));
	}
	private void setFishScreens(ArrayList<String> inText){	
		// first line is the title
		checkTitle(inText.get(0));
		for (int i = 1; i<inText.size(); i++){
			int[] ids = getEnvIds(inText.get(i).trim().split("[,\\s\\t]+")); 
			_fishScreens.add(IntBuffer.wrap(ids));
		}
	}
	
	private void checkTitle(String inTitle){
		String [] title = inTitle.trim().split("[,\\s\\t]+");
		if (!title[0].equalsIgnoreCase("NODEID")
				|| !title[1].equalsIgnoreCase("CHANNELID/RESERVOIRNAME/OBJ2OBJNAME"))
			PTMUtil.systemExit("SYSTEM EXIT: Title line is wrong:"+title[0] + " " +title[1]);
	}
	
	private int[] getEnvIds(String[] items){
		if (items.length<2)
			PTMUtil.systemExit("SYSTEM EXIT: Barrier or fish screen ID input line should have more than 3 items, and less items noticed. ");
		Integer nodeId=null, wbId=null;
		// find external node id
		try{
			nodeId = Integer.parseInt(items[0]);
		}catch(NumberFormatException e){
				e.printStackTrace();
				PTMUtil.systemExit("SYSTEM EXIT:wrong node id:" + items[0]);
		}
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
		// convert external ids to internal env ids
		nodeId = PTMHydroInput.getIntFromExtNode(nodeId);
		if (nodeId == null || wbId == null)
			PTMUtil.systemExit("SYSTEM EXIT: wrong node/channel/reservior/obj2obj ids, node id: " + items[0]+" waterbody Id: "+items[1]);
		return new int[] {nodeId, wbId};
	}

	private ArrayList<IntBuffer> _fishScreens = null;
	private double _dicuFilterEfficiency;
	private ArrayList<NonPhysicalBarrier> _barriers = null;
	private RouteHelper _routeHelper = null;
	private String _fishType = null;

}
