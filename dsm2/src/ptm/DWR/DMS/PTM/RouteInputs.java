/**
 * 
 */
package DWR.DMS.PTM;
import java.util.ArrayList;
import java.util.List;
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
			_pathFileNameEntrainment = PTMUtil.getPathFromLine(inText.get(0), ':');
			if(_pathFileNameEntrainment.equalsIgnoreCase(""))
				_pathFileNameEntrainment = null;
            //TODO disabled the flux reading. The flux calculation needs more work for fish particles because they move back and forth many times.
            /*
			_pathFileNameFlux = PTMUtil.getPathFromLine(inText.get(1), ':');
			if(_pathFileNameFlux.equalsIgnoreCase(""))
				_pathFileNameFlux = null;
			else
				Particle.ADD_TRACE=true;
			ArrayList<String> fluxInText = PTMUtil.getInputBlock(inText, "FLUX_CALCULATION", "END_FLUX_CALCULATION");
            */
			ArrayList<String> barriersInText = PTMUtil.getInputBlock(inText, "BARRIERS", "END_BARRIERS");
			ArrayList<String> screensInText = PTMUtil.getInputBlock(inText, "FISH_SCREENS", "END_FISH_SCREENS");
			ArrayList<String> dicuInText = PTMUtil.getInputBlock(inText, "DICU_FILTER", "END_DICU_FILTER");
			ArrayList<String> specialBehaviorInText = PTMUtil.getInputBlock(inText, "SPECIAL_BEHAVIORS", "END_SPECIAL_BEHAVIORS");
			_entrainmentRates = new HashMap<Integer, ArrayList<ArrayList<Object>>>();
            
			/*
			if( fluxInText == null || fluxInText.size() < 2)
				System.out.println("No flux calculation info found or the info is not properly defined in behavior inputs.");
			else
				setFluxInfo(fluxInText);
			*/
			if( barriersInText == null || barriersInText.size() < 6)
				System.out.println("No non-physical-barrier info found or the info is not properly defined in behavior inputs.");
			else
				setBarriers(barriersInText);
			
			if( screensInText == null || screensInText.size() < 2)
				System.out.println("No fish screen info found or the info is not properly defined in behavior inputs.");
			else
				setFishScreens(screensInText);
			
			if( dicuInText == null || dicuInText.size() < 1)
				System.out.println("No dicu info found or the info is not properly defined in behavior inputs.");
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
				System.out.println("No special routing Behavior defined or defined improperly in behavior inputs.");
			else
				setSpecialBehaviors(specialBehaviorInText);
		}
	}
	public void setBarrierNodeInfo(Node[] allNodes){
	    //nodeArray starts from 1 PTMFixedInput.java line 287
		if (_barriers != null){
			for (NonPhysicalBarrier barrier: _barriers)
				allNodes[barrier.getNodeId()].installBarrier();
		}
	}
	
	public void setFishScreenNodeInfo(Node[] allNodes){
	    //nodeArray starts from 1 PTMFixedInput.java line 287
		if (_fishScreens != null){
			for (IntBuffer screen: _fishScreens) 
				allNodes[screen.get(0)].installFishScreen();
		}
	}
	
	public void setBarrierWbInfo(Waterbody[] allWbs){
		//wbArray start from 1 see PTMFixedInput.java line 180
		//Channels are first filled in wbArray
		if (_barriers == null)
			System.out.println("No non-pysical barriers");
		else{
			for (NonPhysicalBarrier barrier: _barriers)
				allWbs[barrier.getWaterbodyId()].installBarrier(barrier.getNodeId());
		}
	}
	
	public void setFishScreenWbInfo(Waterbody[] allWbs){
		if (_fishScreens == null)
			System.out.println("No fish screens");
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
		if(_fishType.equalsIgnoreCase("SALMON_PARTICLE")){
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
			System.out.println("Created Salmon Particle Route Helper");
		}
		else if(_fishType.equalsIgnoreCase("NEUTRALLY_BUOYANT_PARTICLE")||_fishType.equalsIgnoreCase("POSITION_ORIENTED_PARTICLE")){
			_routeHelper = new ParticleRouteHelper(new BasicRouteBehavior(this));
		}
		else
			PTMUtil.systemExit("System Exit: don't know how to deal with this fish type: "+_fishType);
	}
	public ConcurrentHashMap<Integer, String> getSpecialBehaviorNames(){ return _specialBehaviorNames; }
	public String getSpecialBehaviorName(int nodeId){return _specialBehaviorNames.get(nodeId);}
	public Map<String, Integer> getNameChannelLookUp() { return _nameChanLookUp;}
	public int getChannelId(String channelName) {return _nameChanLookUp.get(channelName);}
	public int getPercentToModify(int nodeId){return _specialBehaviorPercent.get(nodeId);}
	public void putEntrainmentRate(int nodeId, List<Object> rates){
		if (_pathFileNameEntrainment == null)
			return;
		if ( _entrainmentRates.get(nodeId) == null)
			_entrainmentRates.put(nodeId, new ArrayList<ArrayList<Object>>());
		ArrayList<Object> elm = new ArrayList<Object>();
		for (Object rate:rates)
			elm.add(rate);
		_entrainmentRates.get(nodeId).add(elm);
	}
	public Map<Integer, ArrayList<ArrayList<Object>>> getEntrainmentRates(){return _entrainmentRates;}
	public void writeEntrainmentRates(){
		if(_pathFileNameEntrainment==null)
			return;
		try{
			BufferedWriter srWriter = PTMUtil.getOutputBuffer(_pathFileNameEntrainment);
			for (int ndId: _entrainmentRates.keySet()){
				if(ENTRAINMENTALLWRITEOUT){
					if(getSpecialBehaviorName(ndId).equalsIgnoreCase("SalmonGSJRouteBehavior")){
						srWriter.write("GS Junction");
						srWriter.newLine();
						srWriter.write("Node ID".concat(",").concat("pId").concat(",").concat("Sac Up Flow (cfs)").concat(",")
								.concat("Day Time (day/night = 0/1)").concat(",")
								.concat("Barrier Operation (on/off = 1/0)").concat(",")
								.concat("Inflow % to GS").concat(",")
								.concat("Entrainment Probability"));
						srWriter.newLine();
						srWriter.write("Node ID".concat(",").concat("pId").concat(",").concat("Sac Flow below GS (cfs)").concat(",")
								.concat("Sac Flow Change").concat(",")
								.concat("GS Flow").concat(",")
								.concat("DCC flow").concat(",")
								.concat("piDCC").concat(",")
								.concat("piGs").concat(",")
								.concat("Entrainment Probability"));
						srWriter.newLine();
					}
					else if (getSpecialBehaviorName(ndId).equalsIgnoreCase("SalmonSutterJRouteBehavior")
							||getSpecialBehaviorName(ndId).equalsIgnoreCase("SalmonSTMJRouteBehavior")){
						
						if(getSpecialBehaviorName(ndId).equalsIgnoreCase("SalmonSutterJRouteBehavior"))
							srWriter.write("Sutter Junction");
						else
							srWriter.write("Steamboat Junction");
						srWriter.newLine();
						srWriter.write("Node ID".concat(",").concat("pId").concat(",").concat("% flow into sut").concat(",")
								.concat("% flow into stm").concat(",")
								.concat("sut flow (cfs)").concat(",")
								.concat("stm flow (cfs)").concat(",")
								.concat("delta sut flow (std)").concat(",")
								.concat("Entrainment Probability"));
						srWriter.newLine();
					}
					else if (getSpecialBehaviorName(ndId).equalsIgnoreCase("SalmonDCCRouteBehavior")){
						srWriter.write("DCC Junction");
						srWriter.newLine();
						srWriter.write("Node ID".concat(",").concat("pId").concat(",").concat("Sac flow (cfs)").concat(",")
								.concat("dt Sacflow (cfs)").concat(",")
								.concat("GS flow (cfs)").concat(",")
								.concat("DCC flow (cfs)").concat(",")
								.concat("Entrainment Probability"));
						srWriter.newLine();
					}
					for(ArrayList<Object> elm: _entrainmentRates.get(ndId)){
						String s = Integer.toString(PTMHydroInput.getExtFromIntNode(ndId));
						for(Object item: elm)
							s = s.concat(",").concat(item.toString());
						srWriter.write(s);
						srWriter.newLine();
					}
					srWriter.newLine();
				}
				else{
					srWriter.write("Node Id".concat(",").concat("pId").concat(",").concat("Entrainment Probability"));
					srWriter.newLine();
					for(ArrayList<Object> elm: _entrainmentRates.get(ndId)){
						srWriter.write(Integer.toString(PTMHydroInput.getExtFromIntNode(ndId)).concat(",").concat(elm.get(0).toString())
								.concat(",").concat(elm.get(elm.size()-1).toString()));
						srWriter.newLine();
					}
					srWriter.newLine();
				}
			}
			PTMUtil.closeBuffer(srWriter);
		}catch(IOException e){
			System.err.println("error occured when writing out survival rates!");
			e.printStackTrace();
		}
	}
	public void writeFlux(Particle[] pArray){
		if (_pathFileNameFlux==null)
				return;
		if(pArray == null)
			PTMUtil.systemExit("particle array empty when trying to write out flux, system exit");
		try{
			int totalParticles = pArray.length;
			BufferedWriter srWriter = PTMUtil.getOutputBuffer(_pathFileNameFlux);
			srWriter.write("Name".concat(",").concat("# of particles passed")
					.concat(",").concat("total # of particles")
					.concat(",").concat("Flux %"));
			srWriter.newLine();
			for (Pair<ArrayList<Integer>,ArrayList<Integer>> g: _fluxChannelGroups){
				int count = 0;
				for(Particle p: pArray)
					count += p.particlePassed(g.getFirst(), g.getSecond());
				srWriter.write(g.getName().concat(",").concat(Integer.toString(count))
						.concat(",").concat(Integer.toString(totalParticles))
						.concat(",").concat(Float.toString(((float)count)/totalParticles)));
				srWriter.newLine();	
			}
			PTMUtil.closeBuffer(srWriter);
		}catch(IOException e){
			System.err.println("error occured when writing out survival rates!");
			e.printStackTrace();
		}
	}
	public void setWriteEntrainmentAll(boolean wAll){
		if(wAll)
			ENTRAINMENTALLWRITEOUT = true;
		else
			ENTRAINMENTALLWRITEOUT = false;
	}
	int getUpSacJChan(int jId, int pId){
		if(_upSacJChans.get(pId) == null)
			return 0;
		return _upSacJChans.get(pId)[jId];					
	}
	void setUpSacJChan(int jId, int pId, int chanId){
		if(_upSacJChans.get(pId) == null)
			_upSacJChans.put(new Integer(pId), new int[4]);
		_upSacJChans.get(pId)[jId] = chanId;
	}
	int visited(int jId, int pId){
		Map<Integer, Integer> jVis = _upSacJVisits.get(pId);
		if(jVis == null){
			_upSacJVisits.put(new Integer(pId), new HashMap<Integer, Integer>());
			_upSacJVisits.get(pId).put(jId, 1);
			return 0;
		}
		if(jVis.get(jId) == null){
			jVis.put(jId, 1);
			return 0;
		}
		int visits = jVis.get(jId);
		jVis.put(jId, visits+1);
		return visits;
	}
	private void setDicuFilterEfficiency(){
		if (!(_dicuFilterEfficiency < 0)){
			//TODO particle has basic route behavior as Salmon???
			if(_fishType.equalsIgnoreCase("SALMON_PARTICLE"))
				SalmonBasicRouteBehavior.setDicuFilterEfficiency(_dicuFilterEfficiency);
			else if(_fishType.equalsIgnoreCase("NEUTRALLY_BUOYANT_PARTICLE")||_fishType.equalsIgnoreCase("POSITION_ORIENTED_PARTICLE"))
				BasicRouteBehavior.setDicuFilterEfficiency(_dicuFilterEfficiency);
			else
				PTMUtil.systemExit("don't know how to deal with this particle type:" + _fishType+", system exit");
		}
		else
			PTMUtil.systemExit("DICU Fileter Efficiency cannot be less than 0, , system exit");
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
		String shouldBe[] = {"NODEID", "CLASS_NAME", "PERCENT_INCREASE_DECREASE"};
		PTMUtil.checkTitle(title, shouldBe);
		// read in node vs. special junction class name
		_specialBehaviorNames = new ConcurrentHashMap<Integer, String>(); 
		_specialBehaviorPercent = new ConcurrentHashMap<Integer, Integer>();
		for (String line: inText.subList(2, inText.size())){
			String [] items = line.trim().split("[,\\s\\t]+");
			if (items.length!=3)
				PTMUtil.systemExit("SYSTEM EXIT: error while reading special behavior inputs:"+line);
			int nId = getEnvNodeId(items[0]);
			_specialBehaviorNames.put(nId, items[1]);
			_specialBehaviorPercent.put(nId, PTMUtil.getIntFromString(items[2]));
		} 
		if(DEBUG == true){
			for (String key:_nameChanLookUp.keySet())
				System.out.println("_nameChanLookUp Key: "+key+" chan: "+PTMHydroInput.getExtFromIntChan(_nameChanLookUp.get(key)));
			for (int key:_specialBehaviorNames.keySet())
				System.out.println("_specialBehaviorNames Key: "+PTMHydroInput.getExtFromIntNode(key)+" value: "+_specialBehaviorNames.get(key));
		}
	}
	private void setFluxInfo(ArrayList<String> inText){
		String title = inText.get(0);
		String shouldBe[] = {"NAME", "CHANNELS_FROM", "CHANNELS_TO"};
		PTMUtil.checkTitle(title, shouldBe);
		_fluxChannelGroups = new ArrayList<Pair<ArrayList<Integer>, ArrayList<Integer>>>();
		for (String line: inText.subList(1, inText.size()))
			_fluxChannelGroups.add(PTMUtil.getGroupPair(line));
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
		String shouldBe[] = {"NODEID", "CHANNELID/RESERVOIRNAME/OBJ2OBJNAME"};
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
	private Integer getEnvChanId(String idStr){
		Integer chanId = null;
		if (idStr == null)
			PTMUtil.systemExit("SYSTEM EXIT: try to get internal Id for channel but the string is empty. ");
		try{
			chanId = Integer.parseInt(idStr);
		}catch(NumberFormatException e){
				e.printStackTrace();
				PTMUtil.systemExit("SYSTEM EXIT: node id has a wrong format:" + idStr);
		}
		chanId = PTMHydroInput.getIntFromExtChan(chanId);
		return chanId;
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

	private String _pathFileNameEntrainment;
	private String _pathFileNameFlux;
	private ArrayList<IntBuffer> _fishScreens = null;
	private float _dicuFilterEfficiency;
	private ArrayList<NonPhysicalBarrier> _barriers = null;
	private RouteHelper _routeHelper = null;
	private String _fishType = null;
	//<nodeId, specialBehavior class name>
	private ConcurrentHashMap<Integer, String> _specialBehaviorNames = null;
	//<nodeId, entrainment rate % increase/decrease>
	private ConcurrentHashMap<Integer, Integer> _specialBehaviorPercent = null;
	private Map<String, Integer> _nameChanLookUp;
	private boolean DEBUG = false;
	//Map<nodeId, <pid, entrainment rate>>
	private Map<Integer, ArrayList<ArrayList<Object>>> _entrainmentRates;
	private boolean ENTRAINMENTALLWRITEOUT = false;
	//Flux channel name look up: Map<Channel_Id, channel name>
	private ArrayList<Pair<ArrayList<Integer>, ArrayList<Integer>>> _fluxChannelGroups = null;
	/* The statistic route models used here only count for one event (not count for the particle coming back to the junction again and again)
	   to be consistent with the statistical model, a particle remembers the very first choice of the channel and will not make the choince again
	   when the particle comes back to the junction
	*/
	//<pId, [chanIds]> store the first selected channel for each particle at SUT[0], STM[1], DCC[2], GS[3] 
	private HashMap<Integer, int[]> _upSacJChans = new HashMap<Integer, int[]>(); 
	//<junction Id, <pId, <jId, visited>> store if a particle visited SUT, STM, DCC, GS 
	private HashMap<Integer, HashMap<Integer, Integer>> _upSacJVisits = new HashMap<Integer, HashMap<Integer, Integer>>();

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
