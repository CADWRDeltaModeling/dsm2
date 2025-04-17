/**
 *
 */
package DWR.DMS.PTM;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.IntBuffer;
import java.util.ArrayList;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.HashMap;
import java.util.List;

/**
 * @author xwang
 *
 */
public class SurvivalInputs {

	private Config config;
	
	public SurvivalInputs() {
		// TODO Auto-generated constructor stub
	}

	public SurvivalInputs(String fishType) {
		
		config = PTMFixedData.getConfig();
		
		if (fishType == null)
			PTMUtil.systemExit("No Particle Type! exit.");
		_fishType = fishType;
		if (config.survival_groups!= null){
			/*
			 * !!! a channel can only have at most one start, end, and exchange station.
			 */
			_startStas = new ArrayList<Integer>();
			_pathList = new ArrayList<String>();
			_survEqList = new ArrayList<String>();
			_reachSurvMap = new HashMap<String, Float>();
			survEqs = new HashMap<String, String>();
			_groupSplitRatio = new HashMap<Integer, Float>();
			_startStasDist = new HashMap<Integer, Integer>();
			_groupParas = new HashMap<Integer, Map<Integer, ArrayList<Double>>>();
			_endStasDist = new HashMap<Integer, Integer>();
			_exchStasDist = new HashMap<Integer, Integer>();
			_groupEndStas = new HashMap<Integer, ArrayList<Integer>>();
			_groupExchStas = new HashMap<Integer, ArrayList<Integer>>();
			_groupArrivals = new HashMap<Integer, Integer>();
			_groupLost = new HashMap<Integer, Integer>();
			_groupSurvival = new HashMap<Integer, Integer>();
			_groupName = new HashMap<Integer, String>();
			_startStaGroup = new HashMap<Integer, Integer>();
			_pReachProb = new HashMap<Integer, Map<Integer, Double>>();
			setSurvivalParameters();
			setOutput();
			setReachSurv();
			if(DEBUG){
				for(int ss: _startStas)
					System.err.println("_startStas:"+PTMHydroInput.getExtFromIntChan(ss));
				for (Map.Entry<Integer, Integer> entry: _startStasDist.entrySet())
					System.err.println("_startStasDist:"+PTMHydroInput.getExtFromIntChan(entry.getKey()) + " "+ entry.getValue());
				for (Map.Entry<Integer, Map<Integer, ArrayList<Double>>> outerEntry: _groupParas.entrySet()) {
					Map<Integer, ArrayList<Double>> innerMap = outerEntry.getValue();					
					for(Map.Entry<Integer, ArrayList<Double>> innerEntry : innerMap.entrySet()) {
						System.err.println("_groupParas:" + innerEntry.getKey() + " " + innerEntry.getValue().get(0) + " " + 
											innerEntry.getValue().get(1)+ " "+ innerEntry.getValue().get(2));	
					}	
				}
					
				for (Map.Entry<Integer, Integer> entry: _endStasDist.entrySet())
					System.err.println("_endStasDist:"+PTMHydroInput.getExtFromIntChan(entry.getKey()) + " "+ entry.getValue());
				for (Map.Entry<Integer, Integer> entry: _exchStasDist.entrySet())
					System.err.println("_exchStasDist:"+PTMHydroInput.getExtFromIntChan(entry.getKey()) + " "+ entry.getValue());
				for (Map.Entry<Integer, Integer> entry: _startStaGroup.entrySet())
					System.err.println("_startStaGroup:"+PTMHydroInput.getExtFromIntChan(entry.getKey()) + " "+ entry.getValue());
				for (Map.Entry<Integer, ArrayList<Integer>> entry: _groupEndStas.entrySet()){
					System.err.println("_groupEndStas:"+entry.getKey());
					for (int ges: entry.getValue())
						System.err.println("_groupEndStas:"+PTMHydroInput.getExtFromIntChan(ges));
				}
				for (Map.Entry<Integer, ArrayList<Integer>> entry: _groupExchStas.entrySet()){
					System.err.println("_groupExchStas:"+entry.getKey());
					for (int ges: entry.getValue())
						System.err.println("_groupExchStas:"+PTMHydroInput.getExtFromIntChan(ges));
				}
			}
		}
		else{
			_doSurvival = false;
		}
	}
	public int getGroupNumber(int startStation){return _startStaGroup.get(startStation);}
	public Map<Integer, Map<Integer, ArrayList<Double>>> getSurvivalParameterMap(){return _groupParas;}
	public ArrayList<Double> getSurvivalParameters(int groupId, int endStation){return _groupParas.get(groupId).get(endStation);}
	public ArrayList<Integer> getStartStations(){return _startStas;}
	public ArrayList<Integer> getEndStations(int groupId){return _groupEndStas.get(groupId);}
	public ArrayList<Integer> getExchangeableStations(int groupId){return _groupExchStas.get(groupId);}
	public int getStartStationDistance(int chanNum){return _startStasDist.get(chanNum);}
	public int getEndStationDistance(int chanNum){return _endStasDist.get(chanNum);}
	public int getExchangeableStationDistance(int chanNum){return _exchStasDist.get(chanNum);}
	public boolean isStartChan(int chanId){return _startStas.contains(chanId);}
	public boolean isStartWB(int wbId) {return _startStas.contains(wbId);}
	public boolean isEndChan(int groupId, int chanId){return _groupEndStas.get(groupId).contains(chanId);}
	public boolean isExchangeChan(int groupId, int chanId){return _groupExchStas.get(groupId).contains(chanId);}
	//public boolean fromUpstream(Particle p){return (p.nd.getEnvIndex() == ((Channel)p.wb).getUpNode().getEnvIndex());}
	//only is...(e.g., isStartChan) == true check... (e.g., checkStart) can be called
	public boolean checkStart(int chanId, float x, boolean fromUpstream){return staReached(chanId, x, _startStasDist, fromUpstream);}
	public boolean checkEnd(int chanId, float x, boolean fromUpstream){return staReached(chanId, x, _endStasDist, fromUpstream);}
	public boolean checkExchange(int chanId, float x, boolean fromUpstream){return staReached(chanId, x, _exchStasDist, fromUpstream);}
	public boolean isStart(int chanId, float x, boolean fromUpstream){return isStartChan(chanId) && checkStart(chanId, x, fromUpstream);}
	public boolean isEnd(int groupId, int chanId, float x, boolean fromUpstream){return isEndChan(groupId, chanId) && checkEnd(chanId, x, fromUpstream);}
	public boolean isExchange(int groupId, int chanId, float x, boolean fromUpstream){return isExchangeChan(groupId, chanId) 
																						&& checkExchange(chanId, x, fromUpstream); }

	public String getGroupName(int groupId) {return _groupName.get(groupId);}
	public int getGroupArrivals(int groupNumber){ return _groupArrivals.get(groupNumber);}
	public int getGroupLost(int groupNumber){ return _groupLost.get(groupNumber);}
	public boolean getDoSurvival(){return _doSurvival;}
	// x = 0 @ upstream node and x = length @ downstream node
	public boolean staReached(int chanId, float x, Map<Integer, Integer> staDist, boolean fromUpstream){
		Integer dist = staDist.get(chanId);
		if(dist == null)
			PTMUtil.systemExit("in calculating survival, couldn't find a channel distance for a station, check input, system exit.");
		int sign = fromUpstream? 1: -1;
		dist = sign*dist;
		x = sign*x;
		return !(x < dist);				
	}
	public void writeSurvivalRates(Map<Integer, IntBuffer> lastTraces){
		Pattern p;
		Matcher m;
		ArrayList<Integer> pList;
		
		try{
			if (lastTraces !=null && _groupBarriers != null) { 
				for (int gid: _groupBarriers.keySet()){
					IntBuffer nc = _groupBarriers.get(gid);
					int stuck = 0;
					for (Integer pid: lastTraces.keySet()) {
						if(lastTraces.get(pid)!= null && lastTraces.get(pid).equals(nc)) 
							stuck++;
					}
					//System.err.println(_groupLost.get(gid));
					if (stuck > 0) {
						if (_groupLost.get(gid)==null)
							_groupLost.put(gid, stuck);
						else
							_groupLost.put(gid, (_groupLost.get(gid)+stuck));
					}
					//System.err.println(Integer.toString(gid)+"  "+Integer.toString(PTMHydroInput.getExtFromIntNode(nc.get(0)))
					 //+"  "+Integer.toString(PTMHydroInput.getExtFromIntChan(nc.get(1)))+ "  "+stuck+ "  "+_groupLost.get(gid));
				}
			}
			BufferedWriter srWriter = null;
			BufferedWriter srPathWriter = null;
			if(_pathFileName != null){
				if(SURVIVALALLWRITEOUT){
					srWriter = PTMUtil.getOutputBuffer("survival_writes_all");
					srWriter.write("Particles survived + lost at an end node are not necessary " 
							+"equal to particle arrived at the beginning node because a particle can be lost at a boundary "
							+"or wondering without reach an end node or an exchange node.  "
							+"All particles not detected at an end node or an exchange node are assumed lost.");
					srWriter.newLine();
					srWriter.write("Group ID".concat(",").concat("Station Name").concat(",")
							.concat("# of Particle Arrived").concat(",").concat("# of Particle Lost")
							.concat(",").concat(" # of Particle Survived").concat(",")
							.concat("Survival Rate not count missing"));
							//.concat(",").concat("Survival Rate missing as lost"));
					srWriter.newLine();
					//_ttHolder will never be null and there is at least one element
					// if no particle arrives to a group, this group will not appear in the write out file
					for (int id: _groupArrivals.keySet()){						
						Integer ar = _groupArrivals.get(id);
						Integer lost = _groupLost.get(id);
						Integer sur = _groupSurvival.get(id);
						
						if (ar == null){
							PTMUtil.systemExit("error in writing survival output.  this particle has never arrived at the starting node of this channel group");;
						}
						if (lost == null)
							lost = 0;
						if (sur == null)
							sur = 0;
						//TODO 
						//survival rate = sur/ar (not (1-lost/ar) ) is to be consistent with Russ' XT model. if fish is not detected at the end node, it assumes that it is lost.
						// not every fish arrived is detected at the end node.
						int tot = sur + lost;
						if (tot != 0){
							srWriter.write(Integer.toString(id).concat(",").concat(_groupName.get(id)).concat(",").concat(Integer.toString(ar)).concat(",").concat(Integer.toString(lost))
									.concat(",").concat(Integer.toString(sur)).concat(",")
									.concat(Float.toString(1.0f*sur/tot)));
									//.concat(",").concat(Float.toString(1.0f*sur/ar)));
							//TODO it is confusing to calculate the survival rate including those missing. so comment this part out.
							/*
							 * Here is Adam's explanation why we calculate a survival rate only according to detected:
							 * When we fit the travel time model, we only use the particles that arrive at the end of a reach
							 * and have a recorded travel time.  The particles that don't arrive (either because they take too
							 * long and don't arrive by the end of the simulation or enter a different channel) are ignored in
							 * fitting the travel time distribution.  The main reason for this is that the travel time data we have
							 * are all, of course, from fish that have survived and migrated the specified reach successfully
							 * (and whose tags have survived, meaning < 90 days).  However a byproduct of fitting this way is that
							 * the behavioral parameters we use are only valid in describing the migration of particles which remain
							 * in the channel and which complete the reach within 90 days.  Additionally, the survival parameters we
							 * are using should be interpreted as being the cause of all mortality within a reach.  Thus, particles
							 * which do not arrive at the end of a reach (and so do not have a travel time) should be removed from
							 * survival calculations, rather than treated as additional mortality. The upshot of all of this is
							 * that I believe we should be calculating survival as
							 * (number of particles surviving at the end of the reach)
							 * / (number of particles surviving at the end of the reach + number of particles arriving at the end of the reach but determined to have died).
							 *
							 * */
							srWriter.newLine();
						}
						else {
							/*
							 * Russ and Adam decided that for a survival reach, if particles are detected at the start station
							 * but never detected at the end stations in case a barrier and a gate, we assume the particles do not survival
							 * and set the survival rate = 0
							 * */
							srWriter.write(Integer.toString(id).concat(",").concat(_groupName.get(id)).concat(",").concat(Integer.toString(ar)).concat(",").concat(Integer.toString(0))
									.concat(",").concat(Integer.toString(0)).concat(",")
									.concat(Float.toString(0.0f)));
							srWriter.newLine();
						}
					} // for (int id: _groupArrivals.keySet())

					srWriter.write("Particle ID".concat(",").concat("Group Id").concat(",").concat("Survival Rate"));
					srWriter.newLine();
					for (int parId: _pReachProb.keySet()){
						for (int gId: _pReachProb.get(parId).keySet()){
							srWriter.write(Integer.toString(parId).concat(",").concat(Integer.toString(gId)).concat(",")
								.concat(Double.toString(_pReachProb.get(parId).get(gId))));
							srWriter.newLine();
						}
					}
					PTMUtil.closeBuffer(srWriter);
				} // if(SURVIVALALLWRITEOUT)

				srPathWriter = PTMUtil.getOutputBuffer(_pathFileName);
				String tl = "Date,Scenario";
				String suvl = _start_date + ","+_scenario;
				checkTitle(String.join(",", config.particle_flux_header));
				checkTitle(String.join(",", config.individual_route_survival_header));
				checkTitle(String.join(",", config.route_survival_equations_header));
				if(config.particle_flux!=null) {
					for (int i=0; i<config.particle_flux.size(); i++) {
						try{
							if (config.particle_flux.get(i).size()!=3)
								throw new NumberFormatException();
							int up_group_num = ((Number) config.particle_flux.get(i).get(1)).intValue();
							int down_group_num = ((Number) config.particle_flux.get(i).get(2)).intValue();
							tl += "," + config.particle_flux.get(i).get(0).toString().toUpperCase();
							if (_groupArrivals.get(down_group_num)!=null && _groupSurvival.get(up_group_num)!=null) {
								float rio = 1.0f*_groupArrivals.get(down_group_num)/_groupSurvival.get(up_group_num);
								suvl += "," + Float.toString(rio);
								_groupSplitRatio.put(down_group_num, rio);
							}
							else
								suvl += "," + " ";
						}catch(NumberFormatException e){
							PTMUtil.systemExit("Expect to read name and 2 Integers in the particle_flux line, but read: " + 
												config.particle_flux.get(i) + ", System exit.");
						}
					}
				}

				float tSuv = 0;
				// write survival rate for each route
				if(config.individual_route_survival!=null) {
					for(int i=0; i<config.individual_route_survival.size(); i++) {

						try{
							if (config.individual_route_survival.get(i).size() != 2)
								throw new NumberFormatException();
							p = Pattern.compile("(?<=\\().*(?=\\))");
							m = p.matcher(config.individual_route_survival.get(i).get(1).toString());
							if(m.find()) {
								pList = PTMUtil.getInts(m.group(0));
							} 
							else {
								throw new NumberFormatException();
							}

							float pSuv=1.0f;
							float pSuvT=1.0f;
							// combine all survival reaches in the route
							for (int l: pList) {
								if ( _groupSurvival.get(l) != null && _groupLost.get(l) != null) {
									pSuv = pSuv * (1.0f*_groupSurvival.get(l)/(_groupSurvival.get(l)+_groupLost.get(l)));
									if (_groupSplitRatio.get(l) != null) 									
										pSuvT = pSuvT * (1.0f*_groupSurvival.get(l)/(_groupSurvival.get(l)+_groupLost.get(l)))*_groupSplitRatio.get(l);
									else 
										pSuvT = pSuvT * (1.0f*_groupSurvival.get(l)/(_groupSurvival.get(l)+_groupLost.get(l)));								
								}
								else if (_groupSurvival.get(l) != null) {
									pSuv = pSuv * 1.0f;
									if (_groupSplitRatio.get(l) != null)
										pSuvT = pSuvT * 1.0f*_groupSplitRatio.get(l);
									else
										pSuvT = pSuvT * 1.0f;
								}

								// in case _groupSurvival.get(l) == null, or both _groupSurvival.get(l) and _groupLost.get(l) == null
								else {
									pSuvT = 0.0f;
									if (_groupSplitRatio.get(l) != null)								
										pSuv = 0.0f;
									else
										pSuv = -99.0f;
								}
							}
							tl += "," + config.individual_route_survival.get(i).get(0).toString().toUpperCase();
							if (pSuv > -0.1f) {
								suvl += "," + Float.toString(pSuv);
								tSuv += pSuvT;
							}
							else 
								suvl += "," + " ";
						}catch(NumberFormatException e){
							PTMUtil.systemExit("Expect to read name and list of groups in the individual_route_survival line, but read: " + 
												config.individual_route_survival.get(i).toString() + ", System exit.");
						}
						
					}
				}

				tl += "," + "Combined_suv";
				//tl.concat(",").concat("Totol_suv");
				suvl +="," + Float.toString(tSuv);
				//suvl.concat(",").concat(Float.toString(tSuv));
				srPathWriter.write(tl);
				srPathWriter.newLine();
				srPathWriter.write(suvl);
				srPathWriter.newLine();
				
				// Store equations for calculating route-specific survival
				if(config.route_survival_equations!=null) {
					for (int i=0; i<config.route_survival_equations.size(); i++) {
						try {
							if (config.route_survival_equations.get(i).size()!=2) {
								throw new IllegalArgumentException();
							}
							survEqs.put(config.route_survival_equations.get(i).get(0).toString(),
										config.route_survival_equations.get(i).get(1).toString());
						} catch (IllegalArgumentException e) {
							PTMUtil.systemExit("Expected to read a route-specific survival equation in the Route_Survival_Equation group, but read: " +
									config.route_survival_equations.get(i) + ". System exit.");
						}
					}
				}

				PTMUtil.closeBuffer(srPathWriter);
				
			} // if(_pathFileName != null) 
		}catch(IOException e){
			System.err.println("error occured when writing out survival rates!");
			e.printStackTrace();
		}
	}
	
	public Map<String, String> getSurvEqs() {return survEqs;}
	public void addArrivalToGroup(int groupNumber){
		addToGroup(groupNumber, _groupArrivals);
	}
	public void addSurvivalToGroup(int groupNumber){
		addToGroup(groupNumber, _groupSurvival);
	}
	public void addLostToGroup(int groupNumber){
		addToGroup(groupNumber, _groupLost);
	}
	public void minusArrivalToGroup(int groupNumber){
		minusToGroup(groupNumber, _groupArrivals);
	}
	public void minusSurvivalToGroup(int groupNumber){
		minusToGroup(groupNumber, _groupSurvival);
	}
	public void minusLostToGroup(int groupNumber){
		minusToGroup(groupNumber, _groupLost);
	}
	public SurvivalHelper getSurvivalHelper(){
		if(_survivalHelper == null)
			setSurvivalHelper();
		return _survivalHelper;
	}
	public Map<Integer, Map<Integer, Double>> getParticleSurvivalRates(){return _pReachProb;}
	public void addSurvivalRate(int pId, int groupId, double rate){
		if(_pathFileName == null)
			return;
		Map<Integer, Double> reachSurvivals = _pReachProb.get(pId);
		if(reachSurvivals == null){
			reachSurvivals = new HashMap<Integer, Double>();
			_pReachProb.put(pId, reachSurvivals);
		}
		reachSurvivals.put(groupId, rate);
	}
	public void setSurvivalAllWriteout(boolean allWrite){
		if (allWrite)
			SURVIVALALLWRITEOUT = true;
		else
			SURVIVALALLWRITEOUT = false;
	}
	private void addToGroup(int groupNumber, Map<Integer, Integer> grp_arr_sur_lost){
		Integer i = grp_arr_sur_lost.get(groupNumber);
		if (i == null)
			grp_arr_sur_lost.put(groupNumber, 1);
		else
			grp_arr_sur_lost.put(groupNumber, ++i);
	}
	private void minusToGroup(int groupNumber, Map<Integer, Integer> grp_arr_sur_lost){
		Integer i = grp_arr_sur_lost.get(groupNumber);
		if (i == null)
			System.err.println("WARNING: cannot subtract 1 from the particle group, the list is empty!");
		else
			grp_arr_sur_lost.put(groupNumber, --i);
	}
	private void setOutput() {
		_start_date = config.simulation_start_date;
		_scenario = config.simulation_scenario;
		if (config.individual_route_survival== null) {
			PTMUtil.systemExit("Please enter individual_route_survival in the behavior input file.");
		}
		if (config.route_survival_equations==null) {
			PTMUtil.systemExit("Please enter route_survival_equations in the behavior input file.");	
		}
		if (_start_date == null)
			PTMUtil.systemExit("Please provide a simulation_start_date in the behavior input file.");
		if (config.particle_flux_header == null) {
			PTMUtil.systemExit("Please enter particle_flux_header in the behavior input file.");
		}
		
	}
	
	private void setSurvivalParameters(){
		int thisChanID, thisDist, groupNum;
		
		_pathFileName = config.survival_output_path;
		if(_pathFileName.equalsIgnoreCase(""))
			_pathFileName = null;
		int num_groups = config.survival_groups.size();
		for (int i=0; i<num_groups; i++){
			groupNum = i+1;
			
			String name = config.survival_groups.get(i).name;
			
			//stSta only has one pair, but enStas and exStas can have many pairs.
			ArrayList<Integer> enStasInt = new ArrayList<Integer>();
			ArrayList<Integer> exStasInt = new ArrayList<Integer>();
			//External channel numbers are converted to internal channel numbers, unless this is a non-channel station
			for(int j=0; j<config.survival_groups.get(i).start_stations.size(); j++) {
				int stStaInternal;
				thisChanID = getStationFromString(config.survival_groups.get(i).start_stations.get(j).get(0).toString());
				thisDist = Integer.parseInt(config.survival_groups.get(i).start_stations.get(j).get(1).toString());
				if(thisDist==MISSING) {
					stStaInternal = thisChanID;
				}
				else {
					stStaInternal = PTMHydroInput.getIntFromExtChan(thisChanID);
				}			
				_startStas.add(stStaInternal);
				_startStasDist.put(stStaInternal, thisDist);
				_startStaGroup.put(stStaInternal, i+1);
			}
			for(int j=0; j<config.survival_groups.get(i).end_stations.size(); j++) {
				int enSta;
				thisChanID = getStationFromString(config.survival_groups.get(i).end_stations.get(j).get(0).toString());
				thisDist = Integer.parseInt(config.survival_groups.get(i).end_stations.get(j).get(1).toString());
				if(thisDist==MISSING) {
					enSta = thisChanID;
				}
				else {
					enSta = PTMHydroInput.getIntFromExtChan(thisChanID);
				}
				
				_endStasDist.put(enSta, thisDist);
				enStasInt.add(enSta);
			}
			if (config.survival_groups.get(i).exchangeable_start_stations!=null) {
				for(int j=0; j<config.survival_groups.get(i).exchangeable_start_stations.size(); j++) {
					int exSta;
					thisChanID = getStationFromString(config.survival_groups.get(i).exchangeable_start_stations.get(j).get(0).toString());
					thisDist = Integer.parseInt(config.survival_groups.get(i).exchangeable_start_stations.get(j).get(1).toString());
					if(thisDist==MISSING) {
						exSta = thisChanID;
					}
					else {
						exSta = PTMHydroInput.getIntFromExtChan(thisChanID);
					}
					_exchStasDist.put(exSta, thisDist);
					exStasInt.add(exSta);
				}
			}
			_groupName.put(groupNum, name);
			//Java pass by reference. changing enStasInt and exStasInt will change the maps.
			//so don't change them once they are put into the maps unless you rewrite code here to make the map immutable 
			_groupEndStas.put(groupNum, enStasInt);
			_groupExchStas.put(groupNum, exStasInt);
			String [] paraTitle = {"END_STATION", "LAMBDA","OMEGA","X"};
			if(!PTMUtil.check(config.survival_groups.get(i).survival_params_header, paraTitle))
				PTMUtil.systemExit("wrong survival input line:" + config.survival_groups.get(i).survival_params_header);
			
			ArrayList<Double> endStationAndParas;
			Integer endStationInt;
			ArrayList<Double> paras = null;
			Map<Integer, ArrayList<Double>> thisParasMap = new HashMap<Integer, ArrayList<Double>>();
			for(int j=0; j<_groupEndStas.get(groupNum).size(); j++) {
				endStationAndParas = getXTparameters(config.survival_groups.get(i).survival_params.get(j));
				// Use _groupEndStas.get(i).get(j) instead of PTMHydroInput.getIntFromExtChan(endStationExt) so non-channel waterbodies are handled properly.
				endStationInt = _groupEndStas.get(groupNum).get(j);
				paras = new ArrayList<Double>(endStationAndParas.subList(1, 4));				
				thisParasMap.put(endStationInt, paras);
			}
			
			//ArrayList<Double> paras = PTMUtil.getDoubles(survivalStrs.get(5));
			_groupParas.put(groupNum, thisParasMap);
			
			if(config.survival_groups.get(i).barriers_header!=null && config.survival_groups.get(i).barriers!=null && config.survival_groups.get(i).barriers.size()==2) {
				String shouldBe[] = {"NODEID", "CHANNELID/RESERVOIRNAME/OBJ2OBJNAME"};
				PTMUtil.checkTitle(config.survival_groups.get(i).barriers_header, shouldBe);
				int[] ids =  PTMUtil.getEnvNodeChanIds(new String[] {config.survival_groups.get(i).barriers.get(0), config.survival_groups.get(i).barriers.get(1)});
				if (_groupBarriers == null)
					_groupBarriers = new HashMap<Integer, IntBuffer>();
				_groupBarriers.put(groupNum, IntBuffer.wrap(ids));
			}
		}
	}
	private void setSurvivalHelper(){
		if (_fishType.equalsIgnoreCase("SALMON_PARTICLE")){
			_survivalHelper = new SalmonSurvivalHelper(new SalmonBasicSurvivalBehavior(this));
			System.out.println("Created Salmon Particle Survival Helper");
		}
		else if (_fishType.equalsIgnoreCase("NEUTRALLY_BUOYANT_PARTICLE")||_fishType.equalsIgnoreCase("POSITION_ORIENTED_PARTICLE")){
			_survivalHelper = null;
			System.out.println("No Survival Helper created");
		}
		else
			PTMUtil.systemExit("don't know how to deal the fish species: "+_fishType+", system exit.");
	}
	private void setReachSurv() {
		if(config.individual_reach_survival!=null) {
			for(int i=0; i<config.individual_reach_survival.size(); i++) {
				_reachSurvMap.put(config.individual_reach_survival.get(i).get(0).toString().replaceAll("#", SurvivalCalculation.WILDCARD),
						((Number) config.individual_reach_survival.get(i).get(1)).floatValue());
			}	
		}
	}
	
	public Map<String, Float> getReachSurvMap() {return _reachSurvMap;}
	
	private void checkTitle(String inTitle){
		String [] title = inTitle.trim().split("[,\\s\\t]+");
		
		if (((title.length == 2)&&title[0].equalsIgnoreCase("Name")&&title[1].equalsIgnoreCase("Route_Survival_Calculation_Group_List")) ||
				((title.length == 3)&&title[0].equalsIgnoreCase("Name")&&title[1].equalsIgnoreCase("From_Survival_Calculation_Group")
						&&title[2].equalsIgnoreCase("To_Survival_Calculation_Group")) ||
				(title.length==2 && title[0].equalsIgnoreCase("Name") && title[1].equalsIgnoreCase("Route_Survival_Equation")) ||
				(title.length==2 && title[0].equalsIgnoreCase("fromStation_toStations") && title[1].equalsIgnoreCase("survival")))
			return;
		else
			PTMUtil.systemExit("SYSTEM EXIT: expecting survival output definition ... but get:"+ inTitle);
	}
	
	private ArrayList<Double> getXTparameters(List<Object> paramList) {
		ArrayList<Double> doubleList;
		double thisDouble;
		
		doubleList = new ArrayList<>();
		
		for(Object obj : paramList) {
			thisDouble = MISSING;
			try {
				thisDouble = Double.parseDouble(obj.toString());
			} catch (NumberFormatException e) {
				if (PTMEnv.getReservoirObj2ObjEnvId(obj.toString().toUpperCase()) == null){
					PTMUtil.systemExit(obj.toString() + " in the Survival_Inputs output line is wrong, please check");
				}
				else {
					thisDouble = Double.valueOf(PTMEnv.getReservoirObj2ObjEnvId(obj.toString().toUpperCase()));
				}
			}
			doubleList.add(thisDouble);
		}
		
		return doubleList;
		
	}
	
	private int getStationFromString(String stationStr) {
		int stationInt;
		
		stationStr = stationStr.trim().toUpperCase();
		
		stationInt = MISSING;
		try {
			stationInt = Integer.parseInt(stationStr);
		} catch(NumberFormatException e) {
			if (PTMEnv.getReservoirObj2ObjEnvId(stationStr) == null){
				PTMUtil.systemExit("station:" + stationStr + " in the Survival_Inputs output line is wrong, please check");
			}
			else {
				stationInt = PTMEnv.getReservoirObj2ObjEnvId(stationStr);
			}
		}
	
		return stationInt;
	}
	
	private String _pathFileName = null;
	// <start station chan#> start chan# is unique for each Calculation Group
	private ArrayList<Integer> _startStas=null;
	// <path name, path group list> for calculating survival rate for each path
	private ArrayList<String> _pathList=null;
	private ArrayList<String> _survEqList = null;
	private Map<String, Float> _reachSurvMap;
	private Map<String, String> survEqs = null;
	// <group#, particle split ratio> group# is unique for each Calculation Group
	private Map<Integer, Float> _groupSplitRatio=null;
	// <start station chan#, start station distance> start chan# is unique for each Calculation Group
	private Map<Integer, Integer> _startStasDist=null;
	// <end station chan#, end station distance> 
	private Map<Integer, Integer> _endStasDist=null;
	// <exchangeable station chan#, exchangeable station distance> 
	private Map<Integer, Integer> _exchStasDist=null;
	// <group#, end station chan# list> group# is unique for each Calculation Group
	private Map<Integer, ArrayList<Integer>> _groupEndStas=null;
	// <group#, exchangeable station chan# list> group# is unique for each Calculation Group
	private Map<Integer, ArrayList<Integer>> _groupExchStas=null;
	// <group#, survival parameters lambda, omega, x list> group# is unique for each Calculation Group
	private Map<Integer, Map<Integer, ArrayList<Double>>> _groupParas=null;
	// <group#, # of particles arrived in the reach group> group# is unique for each Calculation Group
	private Map<Integer, Integer> _groupArrivals=null;
	// <group#, # of particles survived in the reach group> group# is unique for each Calculation Group
	private Map<Integer, Integer> _groupSurvival=null;
	// <group#, group name> group# is unique for each Calculation Group
	private Map<Integer, String> _groupName=null;	
	// <group#, # of particles dead in the reach group> group# is unique for each Calculation Group
	private Map<Integer, Integer> _groupLost=null;
	// <start station chan#, survival calculation group number> 
	private Map<Integer,Integer> _startStaGroup = null;
	//for output survival rate for each particle for each group
	//Map<pid, Map<group#, survival probability>>
	private Map<Integer, Map<Integer, Double>> _pReachProb = null;
	private boolean _doSurvival = true;
	private SurvivalHelper _survivalHelper = null;
	private String _fishType = null;
	private boolean DEBUG = false;
	private boolean SURVIVALALLWRITEOUT = false;
	private String _start_date = null;
	private String _scenario = null;
	private Map<Integer, IntBuffer> _groupBarriers = null;
	
	static final int MISSING=-999;
}
