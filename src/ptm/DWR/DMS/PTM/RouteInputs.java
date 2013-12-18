/**
 * 
 */
package DWR.DMS.PTM;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Calendar;
import java.util.Set;
import java.nio.IntBuffer;
import java.lang.reflect.Constructor;
/**
 * @author xwang
 *
 */
public class RouteInputs {
	private boolean DEBUG = false;

	/**
	 * 
	 */
	public RouteInputs() {
		// TODO Auto-generated constructor stub
	}
	
	public RouteInputs(ArrayList<String> inText, String fishType) {
		//_barriers = new  HashMap<IntBuffer, NonPhysicalBarrier>();
		//TODO clean up
		//_nameNodeLookup = new HashMap<String, Integer>();
		//_nameWbLookup = new HashMap<String, Integer>();
		//_fishScreenMap = new ArrayList<IntBuffer>();
		//_specialBehaviorMap = new HashMap<IntBuffer, String>();
		if (fishType == null)
				PTMUtil.systemExit("No Particle Type! exit.");
		_fishType = fishType;
		_barriers = new ArrayList<NonPhysicalBarrier>();
		_fishScreens = new ArrayList<IntBuffer>();
		setHelper();
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
		if( dicuInText == null)
			System.err.println("WARNING: no dicu info found or the info is not properly defined in behavior inputs.");
		else
			_dicuFilterEfficiency = PTMUtil.getDouble(dicuInText.get(0).trim());
		if( specialBehaviorInText == null || specialBehaviorInText.size() < 2)
			System.err.println("WARNING: no specialBehavior defined or defined improperly in behavior inputs.");
		else
			setBehaviors(specialBehaviorInText);
	}
	

	public void setBarrierNodeInfo(Node[] allNodes){
	    //nodeArray starts from 1 PTMFixedInput.java line 287
		if (_barriers == null)
			System.err.println("WARNING: no non-pysical barriers info avaiable");
		else
			for (NonPhysicalBarrier barrier: _barriers)
				allNodes[barrier.getNodeId()].installBarrier();
	}
	public void setFishScreenNodeInfo(Node[] allNodes){
	    //nodeArray starts from 1 PTMFixedInput.java line 287
		if (_fishScreens == null)
			System.err.println("WARNING: no fish screen info avaiable");
		else
			for (IntBuffer screen: _fishScreens)
				allNodes[screen.get(0)].installFishScreen();
	}
	public void setBarrierWbInfo(Waterbody[] allWbs){
		//wbArray start from 1 see PTMFixedInput.java line 180
		//Channels are first filled in wbArray
		if (_barriers == null)
			System.err.println("WARNING: no non-pysical barriers info avaiable");
		else{
			for (NonPhysicalBarrier barrier: _barriers)
				allWbs[barrier.getWaterbodyId()].installBarrier(barrier.getNodeId());
		}
	}
	public void setFishScreenWbInfo(Waterbody[] allWbs){
		if (_fishScreens == null)
			System.err.println("WARNING: no fish screen info avaiable");
		else{
			for (IntBuffer screen: _fishScreens)
				allWbs[screen.get(1)].installFishScreen(screen.get(0));
		}
	}
		//TODO clean up
		/*
		else
			for (NonPhysicalBarrier barrier: _barriers){
				Waterbody wb = allChans[barrier.getWaterbodyId()];
				int nodeId = barrier.getNodeId();
				if (wb.getType() == Waterbody.CHANNEL){
					Channel aChan = (Channel) wb;
					if (nodeId == (aChan).getUpNodeId())
						aChan.installBarrierAtUpNode();
					else if (nodeId == aChan.getDownNodeId())
						aChan.installBarrierAtDownNode();
					else
						PTMUtil.systemExit("SYSTEM EXIT: wrong node Id in the route_input barrier section.");
				}
				else {
					wb.installBarrier();
					wb.setBarrierNodeId(nodeId);	
				}
			}
			
	
	public void setBarrierNodeInfo(Node[] allNodes, int nodeNum){
	    // like wbArray, nodeArray starts from 1 PTMFixedInput.java line 287
		for(int i=1; i < nodeNum+1; i++){
	        if ( allNodes[i] == null ){
	        	//System.err.println("Node,"+ i + "is null!");
	        	continue;
	        }
	        for(int j=0; j < allNodes[i].getNumberOfWaterbodies(); j++){
	      	  int wbId = allNodes[i].getWaterbodyEnvIndex(j);
	      	  int ids[] = {i,wbId};
	      	  if (_barriers != null && _barriers.get(IntBuffer.wrap(ids)) != null)
	      		allNodes[i].installBarrier();
	        }
		}
	}
	public void setBarrierChannelInfo(Waterbody[] allChans, int chanNum){
		//wbArray start from 1 see PTMFixedInput.java line 180
		//Channels are first filled in wbArray
		for (int i = 1; i<=chanNum; i++){
		  Channel aChan = (Channel) allChans[i];
		  if ( aChan == null ){
	        	System.err.println("Channel,"+ i + "is null!");
	        	continue;
	      }
		  int nodeNum = aChan.getNumberOfNodes();
		  for(int j=0; j< nodeNum; j++){
			  int nodeId = aChan.getNodeEnvIndex(j);
			  int[] ids = {nodeId, i};
			  if (_barriers != null && _barriers.get(IntBuffer.wrap(ids)) != null){
				  if (nodeId == aChan.getUpNodeId())
					  aChan.installBarrierAtUpNode();
				  if (nodeId == aChan.getDownNodeId())
					  aChan.installBarrierAtDownNode();  
			  }
		  }
		}
	}
	*/
	//public void updateCurrentInfo(Node[] allNodes, int nodeNum, Waterbody[] allChans, int chanNum, int currentTime){
	public void updateCurrentBarrierInfo(Waterbody[] allWbs, int currentTime){
		if (_barriers == null)
			System.err.println("WARNING: no non-pysical barriers info avaiable");
		else{
			for (NonPhysicalBarrier barrier: _barriers)
				allWbs[barrier.getWaterbodyId()].setCurrentBarrierOp(barrier.getNodeId(), 
						barrier.getBarrierOp(PTMUtil.convertHecTime(currentTime)));
		}
		//TODO clean up
		/*
		//update barrier op info
		for(int channelNumber=1; channelNumber <= chanNum; channelNumber++){
			Channel chan = ((Channel) allChans[channelNumber]);
			//TODO why need to find chanEnvId, is it = channelNumber???
			//int chanEnvId = chan.getEnvIndex();
    		if (chan.isBarrierAtUpNodeInstalled())
    			chan.setBarrierAtUpNodeOp(getBarrier(chan.getUpNodeId(), 
    					channelNumber).getBarrierOp(PTMUtil.convertHecTime(currentTime)));
    		else if (chan.isBarrierAtDownNodeInstalled())
    			chan.setBarrierAtDownNodeOp(getBarrier(chan.getDownNodeId(), 
    					channelNumber).getBarrierOp(PTMUtil.convertHecTime(currentTime)));
		}
		*/
	}
	//TODO clean up
	//public HashMap<IntBuffer, NonPhysicalBarrier> getBarriers(){ return _barriers;}
	//public HashMap<String, Integer> getNameNodeLookup() { return _nameNodeLookup; }
	//public ArrayList<IntBuffer> getFishScreens() { return _fishScreens; }
	public double getDicuFilterEfficiency(){return _dicuFilterEfficiency;}
	public RouteHelper getRouteHelper(){ return _routeHelper;}
	private void setHelper(){
		if(_fishType.equalsIgnoreCase("SALMON"))
			_routeHelper = new SalmonRouteHelper(new SalmonBasicRouteBehavior());
		else if (_fishType.equalsIgnoreCase("SMELT"))
			PTMUtil.systemExit("the special help for smelt has been defined yet");
		else
			PTMUtil.systemExit("the special help for smelt has been defined yet");
	}
	private void setBehaviors(ArrayList<String> inText){
		String title = inText.get(0);
		String [] tItems = title.trim().split("[,\\s\\t]+");
		checkTitle(title);
		if (tItems.length<3 || !tItems[2].equalsIgnoreCase("Class_Name"))
			PTMUtil.systemExit("SYSTEM EXIT: error in input:"+inText.get(0));
		_specialBehaviorMap = new HashMap<String, IntBuffer>();
		for (int i = 1; i<inText.size(); i++){
			String [] items = inText.get(i).trim().split("[,\\s\\t]+");
			if (items.length<3)
				PTMUtil.systemExit("SYSTEM EXIT: error in input:"+inText.get(i));
			int[] ids = getEnvIds(items); 
			_specialBehaviorMap.put(items[2], IntBuffer.wrap(ids));
		} 
		addRouteBehaviors();
	}
	private void addRouteBehaviors(){
		if (_specialBehaviorMap == null)
			System.err.println("WARNING: No special behavior added!");
		else{
			Set<String> classNames = _specialBehaviorMap.keySet();
			try{
				for (String name : classNames){
					int nodeId = _specialBehaviorMap.get(name).get(0), wbId = _specialBehaviorMap.get(name).get(1);
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
				}
				
			}catch(Exception e){
				e.printStackTrace();
				System.err.println("Error: " + e.getMessage());
				PTMUtil.systemExit("SYSTEM EXIT: got error when trying to add special fish behaviors to the helper");
			}
		}
		//TODO clean up
		/*
		boolean containGSJ = false;
		
		if (_nameNodeLookup == null)
			System.err.println("WARNING: No special behavior added!");
		
		else if (particleType.equalsIgnoreCase("SALMON")){
			Iterator<String> it = _nameNodeLookup.keySet().iterator();
			while (it.hasNext()){
				String key = it.next();
				// TODO need to think it sample too complicated according to key.  for now only one behavior for GSJunction involved
				if (key.equalsIgnoreCase("GSBARRIER")){
					int nId = _nameNodeLookup.get(key);
					((SalmonRouteHelper)rh).addSpecialBehavior(nId, new SalmonGSJRouteBehavior(nId,_nameWbLookup.get(key)));
					containGSJ = true; 
				}
			}
			if (!containGSJ)
					System.err.println("WARNING: Special route behavior for Georgian Slough Junction not found!!!");
		}
		// here you can add more special behaviors for different species.
		*/
	}
	
	//TODO clean up
	/*
	private NonPhysicalBarrier getBarrier(int nodeId, int wbId){
		int[] ids = {nodeId, wbId};
		NonPhysicalBarrier npb = _barriers.get(IntBuffer.wrap(ids));
		if (npb == null)
			PTMUtil.systemExit("Barrier is not setup, exit from PTMHydroInput line 232.");
		return npb;
	}
	*/
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
			//TODO clean up
			// items[0] is the barrier name
			//_nameNodeLookup.put(items[0], ids[0]);
			//_nameWbLookup.put(items[0], ids[1]);
			addBarrier(barrierText, ids);
		}
	}
	private void addBarrier(ArrayList<String> barrierBlock, int[] nodeWbIds){
		  HashMap<BarrierOpPeriod, Integer> bOpTS = new HashMap<BarrierOpPeriod, Integer>();
		  Calendar s_time = null, e_time = null;
		  int op = -99;
		  
		  // first 2 lines are barrier location info: name nodeid wbid
		  // the third line is operation schedule title.  operation schedule data start from forth line
		  // and the data end before line: end_barrieri
		  for (int i = 3; i<barrierBlock.size();i++){
			  String inLine = barrierBlock.get(i).trim();
			  String[] items = inLine.split("[,\\s\\t]+");
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
		  //TODO clean up
		  //_barriers.put(IntBuffer.wrap(nodeWbIds), new NonPhysicalBarrier(nodeWbIds[0], nodeWbIds[1], bOpTS));
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
		//if (!title[0].equalsIgnoreCase("NAME") || !title[1].equalsIgnoreCase("NODEID")
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
	//TODO clean up
	/* should get it from fortran lib
	private void setDICUInfo(ArrayList<String> inText){
		// first line will be the filter's efficiency
		_dicuFilterEfficiency = PTMUtil.getDouble(inText.get(0).trim());
		_dicuNodeSet = PTMUtil.readSet(PTMUtil.getInputBlock(inText, "NODES_LIST", "END_NODES_LIST"));
		
	}
	*/
	//TODO clean up
	// map key is name
	//private HashMap<IntBuffer, NonPhysicalBarrier> _barriers;
	//for look up the special behavior's node id and wb id.  the ids are converted to internal env IDs in 
	//private HashMap<String, Integer> _nameNodeLookup;
	//private HashMap<String, Integer> _nameWbLookup;
	private ArrayList<IntBuffer> _fishScreens = null;
	private HashMap<String, IntBuffer> _specialBehaviorMap = null;
	private double _dicuFilterEfficiency;
	private ArrayList<NonPhysicalBarrier> _barriers = null;
	private RouteHelper _routeHelper = null;
	private String _fishType = null;

}
