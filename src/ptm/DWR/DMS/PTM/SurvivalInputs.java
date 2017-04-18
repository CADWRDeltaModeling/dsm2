/**
 * 
 */
package DWR.DMS.PTM;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;

/**
 * @author xwang
 *
 */
public class SurvivalInputs {
	/**
	 * 
	 */
	public SurvivalInputs() {
		// TODO Auto-generated constructor stub
	}
	
	public SurvivalInputs(ArrayList<String> inList) {
		if (inList != null){
			/*
			 * !!! a channel can only have at most one start, end, and exchange station.
			 */
			_startStas = new ArrayList<Integer>();
			_startStasDist = new HashMap<Integer, Integer>();
			_groupParas = new HashMap<Integer, ArrayList<Double>>();
			_endStasDist = new HashMap<Integer, Integer>();
			_exchStasDist = new HashMap<Integer, Integer>();
			_groupEndStas = new HashMap<Integer, ArrayList<Integer>>();
			_groupExchStas = new HashMap<Integer, ArrayList<Integer>>();
			_startStaGroup = new HashMap<Integer, Integer>();
			setSurvivalParameters(PTMUtil.getInputBlock(inList, "SURVIVAL_PARAMETERS", "END_SURVIVAL_PARAMETERS"));
			if(DEBUG){
				for(int ss: _startStas)
					System.err.println("_startStas:"+PTMHydroInput.getExtFromIntChan(ss));
				for (Map.Entry<Integer, Integer> entry: _startStasDist.entrySet())
					System.err.println("_startStasDist:"+PTMHydroInput.getExtFromIntChan(entry.getKey()) + " "+ entry.getValue());
				for (Map.Entry<Integer, ArrayList<Double>> entry: _groupParas.entrySet())
					System.err.println("_groupParas:"+entry.getKey() + " "+ entry.getValue().get(0)+ " "+ entry.getValue().get(1)+ " "+ entry.getValue().get(2));
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
			System.err.println("No survival parameters are input, and survival rates will not be calculated.");
		}
	}
	public int getGroupNumber(int startStation){return _startStaGroup.get(startStation);}
	public Map<Integer, ArrayList<Double>> getSurvivalParameterMap(){return _groupParas;}
	public ArrayList<Double> getSurvivalParameters(int groupId){return _groupParas.get(groupId);}
	public ArrayList<Integer> getStartStations(){return _startStas;}
	public ArrayList<Integer> getEndStations(int groupId){return _groupEndStas.get(groupId);}
	public ArrayList<Integer> getExchangeableStations(int groupId){return _groupExchStas.get(groupId);}
	public int getStartStationDistance(int chanNum){return _startStasDist.get(chanNum);}
	public int getEndStationDistance(int chanNum){return _endStasDist.get(chanNum);}
	public int getExchangeableStationDistance(int chanNum){return _exchStasDist.get(chanNum);}
	public boolean isStartChan(int chanId){return _startStas.contains(chanId);}
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

	private void setSurvivalParameters(ArrayList<String> survivalParasIn){
		if (survivalParasIn == null){
			System.err.println("WARNING: No channel groups for suvival rates are defined in behavior input file!");
			return;
		}
		int num_groups = PTMUtil.getIntFromLine(survivalParasIn.get(0), "NUMBER_OF_SURVIVAL_CALCULATION_GROUP");
		for (int i=1; i<num_groups+1; i++){
			ArrayList<String> survivalStrs = PTMUtil.getInputBlock(survivalParasIn, "GROUP_"+i, "END_GROUP_"+i);
			if(survivalStrs.size() != 5)
				PTMUtil.systemExit("errors in the survival parameter input line group:"+i);
			//stSta only has one pair, but enStas and exStas can have many pairs.
			ArrayList<int[]> stStas = PTMUtil.getIntPairsFromLine(survivalStrs.get(0), "START_STATION");
			ArrayList<int[]> enStas = PTMUtil.getIntPairsFromLine(survivalStrs.get(1), "END_STATION");
			ArrayList<int[]> exStas = null;
			if (survivalStrs.get(2).split(":").length < 2)
				System.err.println("Warning: EXCHANGEABLE_START_STATION line is empty!");
			else
				exStas = PTMUtil.getIntPairsFromLine(survivalStrs.get(2), "EXCHANGEABLE_START_STATION");
			ArrayList<Integer> enStasInt = new ArrayList<Integer>();
			ArrayList<Integer> exStasInt = new ArrayList<Integer>();
			//External channel numbers are converted to internal channel numbers
			for(int[] stSt: stStas){
				int stStaInternal = PTMHydroInput.getIntFromExtChan(stSt[0]);
				_startStas.add(stStaInternal);
				_startStasDist.put(stStaInternal,stSt[1]);
				_startStaGroup.put(stStaInternal,i);
			}
			for(int[] enst: enStas){
				int enSta = PTMHydroInput.getIntFromExtChan(enst[0]);
				_endStasDist.put(enSta,enst[1]);
				enStasInt.add(enSta);
			}
			if (exStas != null){
				for(int[] exst: exStas){
					int exSta = PTMHydroInput.getIntFromExtChan(exst[0]);
					_exchStasDist.put(exSta,exst[1]);
					exStasInt.add(exSta);
				}
			}
			//Java pass by reference. changing enStasInt and exStasInt will change the maps.
			//so don't change them once they are put into the maps unless you rewrite code here to make the map immutable 
			_groupEndStas.put(i,enStasInt);
			_groupExchStas.put(i,exStasInt);
			String [] paraTitle = {"LAMBDA","OMEGA","X"};
			if(!PTMUtil.check(survivalStrs.get(3).split("[,:\\s\\t]+"), paraTitle))
				PTMUtil.systemExit("wrong survival input line:"+survivalStrs.get(3));
			ArrayList<Double> paras = PTMUtil.getDoubles(survivalStrs.get(4));
			_groupParas.put(i, paras);						
		}
	}
	// <start station chan#> start chan# is unique for each Calculation Group
	private ArrayList<Integer> _startStas=null;
	// <start station chan#, start station distance> start chan# is unique for each Calculation Group
	private Map<Integer, Integer> _startStasDist=null;
	// <end station chan#, end station distance> 
	private Map<Integer, Integer> _endStasDist=null;
	// <exchangeable station chan#, exchangeable station distance> 
	private Map<Integer, Integer> _exchStasDist=null;
	// <group#, end station chan# list> start chan# is unique for each Calculation Group
	private Map<Integer, ArrayList<Integer>> _groupEndStas=null;
	// <group#, exchangeable station chan# list> start chan# is unique for each Calculation Group
	private Map<Integer, ArrayList<Integer>> _groupExchStas=null;
	// <group#, survival parameters lambda, omega, x list> start chan# is unique for each Calculation Group
	private Map<Integer, ArrayList<Double>> _groupParas=null;
	// <start station chan#, survival calculation group number> start chan# is unique for each Calculation Group
	private Map<Integer,Integer> _startStaGroup = null;
	private boolean _doSurvival = true;
	private boolean DEBUG = false;	
}

/*
 * 
 * 
 * 
 	private void checkTitle(String inTitle){
		String [] title = inTitle.trim().split("[,\\s\\t]+");
		if (!title[0].equalsIgnoreCase("Group_Name")
				|| !title[1].equalsIgnoreCase("Lambda")|| !title[2].equalsIgnoreCase("Omega"))		
			PTMUtil.systemExit("SYSTEM EXIT: Expecting Group_Name Lambda Omega but get:"+title[0] + " " +title[1]+ " " +title[1]);
	}
	//private boolean staReached(Particle p, ArrayList<Integer> chans, Map<Integer, Integer> chanDist){}		
	//TODO no longer needed clean up
	//public boolean startStaReached(int chanNum, float x){return staReached(chanNum, x, getStartStations(), _startStasDist);}
	//public boolean endStaReached(int startChan, int chanNum, float x){return staReached(chanNum, x, getEndStations(startChan), _endStasDist);}
	//public boolean exchangeStaReached(int startChan, int chanNum, float x){return staReached(chanNum, x, getExchangeableStations(startChan), _exchStasDist);}
	//public boolean isSurvivalStaChannel(int stChanId, int chanId){
	//	return (getEndStations(stChanId).contains(chanId)||getExchangeableStations(stChanId).contains(chanId)||_startStas.contains(chanId));
	//}
	
	private boolean staReached(int chanNum, float x, ArrayList<Integer> list, Map<Integer, Integer> map){
		if (!list.contains(chanNum))
			return false;
		else{
			int dist = map.get(chanNum);
			if (x < 0) dist = -dist;
			return x > dist;				
		}			
	}
	
		
		ArrayList<String> survivalRateStrs = PTMUtil.getInputBlock(survivalParasIn, "SURVIVAL_PARAMETERS", "END_SURVIVAL_PARAMETERS");
		if (survivalRateStrs == null)
			PTMUtil.systemExit("No survival parameters found in the Channel_Groups block, system exit");
		checkTitle(survivalRateStrs.get(0));
		_survivalParas = new HashMap<String, Pair<Double, Double>>();
		_groupNames = new ArrayList<String>();
		for (String line: survivalRateStrs.subList(1, survivalRateStrs.size())){
			String [] items = line.trim().split("[,\\s\\t]+");
			if(items.length < 3)
				PTMUtil.systemExit("errors in the survival parameter input line:"+items);
			// put into the map: group name, survival parameters
			try{
				_survivalParas.put(items[0].toUpperCase(), 
						new Pair<Double, Double>(Double.parseDouble(items[1]), Double.parseDouble(items[2])));
				_groupNames.add(items[0].toUpperCase());
			}catch(NumberFormatException e){
				PTMUtil.systemExit("expect to read two doubles in the survival parameter line, but read: "+items+", System exit.");
			}
		}
		//get Channel list
		ArrayList<String> channelListStrs = PTMUtil.getInputBlock(chanGroups, "CHANNEL_LIST", "END_CHANNEL_LIST");
		if (channelListStrs == null)
			PTMUtil.systemExit("No channel list found, system exit");
		_channelGroups = new HashMap<Integer, String>();
		for (String name: _groupNames){
			if(!name.equalsIgnoreCase("ALL")){
				ArrayList<String> chanList = PTMUtil.getInputBlock(channelListStrs, name, "End_".concat(name));
				if (chanList == null)
					PTMUtil.systemExit("expect to get a channel list for group:"+name+", but got none, system exit.");
				for (String line: chanList){
					ArrayList<Integer> chanIds = PTMUtil.getInts(line);
					for (int chanId: chanIds){
						Integer envId = PTMHydroInput.getIntFromExtChan(chanId);
						if (envId <= 0)
							PTMUtil.systemExit("got a wrong channel ID:"+chanId+", system exit.");
						else
							_channelGroups.put(envId, name);
					}
				}
			}
		}


private class CalcGroup{
		private int _chanNum;
		private IntBuffer _startStation;
		private ArrayList<IntBuffer> _endStations, _exchStations;
		private double _lambda, _omega, _x;
		public CalcGroup(){}
		public CalcGroup(int chanNum, IntBuffer startSta, ArrayList<IntBuffer> endStas,ArrayList<IntBuffer> exchStas,double lambda, double omega, double x){
			_chanNum = chanNum;
			_startStation = startSta;
			_endStations = endStas;
			_exchStations = exchStas;
			_lambda = lambda;
			_omega = omega;
			_x = x;
		}
		public IntBuffer getStartStation(){return _startStation;}
		public ArrayList<IntBuffer> getEndStations(){return _endStations;}
		public ArrayList<IntBuffer> getExchStations(){return _exchStations;}
		public double getLambda(){ return _lambda;}
		public double getOmega(){ return _omega;}
		public double getX(){ return _x;}
		public void setStartStations(IntBuffer startSta){_startStation = startSta;}
		public void setEndStations(ArrayList<IntBuffer> endStas){_endStations = endStas;}
		public void setexchStations(ArrayList<IntBuffer> exchStas){_exchStations = exchStas;}
		public void setLambda(double lambda){_lambda = lambda;}
		public void setOmega(double omega){_omega = omega;}
		public void setX(double x){_x = x;}
	}
public void setChannelInfo(Waterbody[] waterbodies){
	if (_channelGroups != null){
		for (Waterbody wb: waterbodies){
			if (wb != null && wb.getType() == Waterbody.CHANNEL){
				Channel chan = (Channel) wb;
				String chanGroup = _channelGroups.get(chan.getEnvIndex());
				//if (chanGroup != null)
					//chan.setChanGroup(chanGroup);
				//else
					//chan.setChanGroup(null);
			}
		}
	}
}
	private SurvivalHelper _survivalHelper = null;
			//setHelper();
*/
//TODO never been used, may be needed later?
//public void setNodeInfo(Node[] allNodes){}
//public void updateCurrentInfo(Node[] allNodes, Waterbody[] allChans, int currentTime){}
//public void addSpecialBehaviors(SurvivalHelper sh, String particleType){}
//public SurvivalHelper getSurvivalHelper(){ return _survivalHelper;}
/*
private void setHelper(){
	//TODO particle should have its own basic behavior???
	if(_fishType.equalsIgnoreCase("PARTICLE"))
		_survivalHelper = new ParticleSurvivalHelper(new ParticleBasicSurvivalBehavior());
	else if(_fishType.equalsIgnoreCase("SALMON"))
		_survivalHelper = new SalmonSurvivalHelper(new SalmonBasicSurvivalBehavior(_survivalRates));
	else if (_fishType.equalsIgnoreCase("SMELT"))
		PTMUtil.systemExit("the special help for smelt has not been defined yet");
	else
		PTMUtil.systemExit("the special help for smelt has not been defined yet");
}
*/