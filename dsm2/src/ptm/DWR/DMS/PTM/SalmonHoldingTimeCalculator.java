package DWR.DMS.PTM;

import java.util.Calendar;
import java.util.Map;

public class SalmonHoldingTimeCalculator {
	public boolean DEBUG = false;

	public SalmonHoldingTimeCalculator(SwimInputs si) {
		_swimmingTimes = si.getPartcleMeanRearingHoldingMap();
		_daytimeHoldings = si.getPartcleDaytimeHoldingMap();
		_swimParameters = si.getSwimParameters();
		if (_swimParameters == null)
			PTMUtil.systemExit("The parameter for rear holding is not properly set! Please check the behavior input file, system exit.");
		else{
			for (String key: _swimParameters.keySet()){
				float[] paras = _swimParameters.get(key);
				if (paras == null || paras.length < 5)
					PTMUtil.systemExit("The swimming parameters are not properly set, expect 5 but less than 5, system exit.");
			}
		}
		//_daytimeNotSwimPercent = si.getDaytimeNotSwimPercent(); //used to be only one value, now one per channel group.  the values is in _swimParameters
		_sunrise = si.getSunrise();
		_sunset = si.getSunset();
		if (_sunrise == null || _sunset == null)
			PTMUtil.systemExit("Sunrise and Sunset are not properly set, check the input file, system exit.");
        _sunrise_hour = _sunrise.getFirst();
        _sunrise_min = _sunrise.getSecond();
        _sunset_hour = _sunset.getFirst();
        _sunset_min = _sunset.getSecond();
		_channelGroups = si.getChannelGroups();
	}

	public boolean getDaytimeHolding(int pId, int chanId){
		Map<Integer, Boolean> pidDaytimeHoldings =  _daytimeHoldings.get(getGroupName(chanId));
		float daytimeNotSwimPercent = _swimParameters.get(getGroupName(chanId))[4];
		if (DEBUG && pId == 1)
			System.err.println(getGroupName(chanId) + "  channel:"+PTMHydroInput.getExtFromIntChan(chanId)
					+"  daytime holding:" + pidDaytimeHoldings.get(pId) + "  not swim pct:" + daytimeNotSwimPercent);
		if (pidDaytimeHoldings.get(pId) == null){
			boolean pDaytimeHolding = PTMUtil.getRandomNumber() < daytimeNotSwimPercent;
			pidDaytimeHoldings.put(pId, pDaytimeHolding); 
		}
		if (DEBUG && pId == 1)
			System.err.println("channel:"+PTMHydroInput.getExtFromIntChan(chanId)
					+"  daytime holding:" + pidDaytimeHoldings.get(pId));
		return pidDaytimeHoldings.get(pId);
	}
	boolean daytimeHolding(int pId, int chanId){
    	Calendar curr = PTMUtil.modelTimeToCalendar(Globals.currentModelTime, Globals.TIME_ZONE);
    	//TODO will use LocaTime when switch to Java 1.8
    	boolean isDaytime = (curr.get(Calendar.HOUR_OF_DAY) > _sunrise_hour && curr.get(Calendar.HOUR_OF_DAY) < _sunset_hour) 
    			|| (curr.get(Calendar.HOUR_OF_DAY) == _sunrise_hour && curr.get(Calendar.MINUTE)>_sunrise_min)
    			|| (curr.get(Calendar.HOUR_OF_DAY) == _sunset_hour && curr.get(Calendar.MINUTE)<_sunset_min);
    	//change to a new daytime holding algorithm, please see notes
    	//return (isDaytime && (PTMUtil.getRandomNumber() < _daytimeNotSwimPercent));
    	return (isDaytime && getDaytimeHolding(pId, chanId));
	}
	// Rearing holding time is the time when a particle to swimming again, i.e., current model time + holding time
	//TODO cast float to int e.g., 900.6 = 900, good enough?   
	void setSwimTime(Particle p, int chanId){
		String groupName = getGroupName(chanId);
		// pidSwimTimes is set in SwimInputs, impossible to be null
		Map<Integer, Long> pidSwimTimes = _swimmingTimes.get(groupName);
		// if swim time never be set for this particle, set, otherwise do nothing
		// swimming time is one per channel group per particle
		if (pidSwimTimes.get(p.Id) == null){
			int holdingTime = (int) (-_swimParameters.get(groupName)[3]*(float)Math.log(PTMUtil.getRandomNumber()));
			if (holdingTime < 0)
				PTMUtil.systemExit("got a negative rearing holding time, which is imposible, system exit.");
			//pidSwimTimes.put(pId, (holdingTime+Globals.currentModelTime));
			// this is more accurate
			///*
			if (DEBUG && p.Id == 1)
				System.err.println("node: "+PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex()) +"  channel:"+PTMHydroInput.getExtFromIntChan(((Channel) p.wb).getEnvIndex())
						+"  holdingTime:" + holdingTime*60 + "  p.getCurrentParticleTimeExact():" + p.getCurrentParticleTimeExact() + "  p.age:" + p.age);
			pidSwimTimes.put(p.Id, (holdingTime+p.getCurrentParticleTimeExact()));
			//*/
		}
	}
	public Long getSwimTime(int pId, int chanId){
		String groupName = _channelGroups.get(chanId);
		Map<Integer, Long> pidSwimTimes = ((groupName == null)? _swimmingTimes.get("ALL"): _swimmingTimes.get(groupName));
		// pidSwimTimes is set in SwimInputs, impossible to be null
		if (pidSwimTimes.get(pId) == null)
			PTMUtil.systemExit("swimming time is not properly set, check the code! system exit.");
		return pidSwimTimes.get(pId);
	}
	
	private String getGroupName(int chanId){
		String groupName = _channelGroups.get(chanId);
		return ((groupName == null)? "ALL":groupName);
	}	
	
	//TODO be very careful, the class variables are visible to all particles!!!
	//private float _daytimeNotSwimPercent = 0.0f; //used to be only one value, now one per channel group.  the values is in _swimParameters
	private Pair<Integer, Integer> _sunrise = null;
	private Pair<Integer, Integer> _sunset = null;
	// Map<ChanGroupName, Map<particleId, particleMeanRearingHoldingTime>>
	private Map<String, Map<Integer, Long>> _swimmingTimes = null;
	// group name, swimming velocity parameters[] [0] constSwimmingVelocity; [1]; STD for particles; [2] STD for time steps for an individual particle; [3] rearing holding; [4] day time not swim percent
	private Map<String, float[]> _swimParameters=null;
	// Channel number (internal), chan group name
	private Map<Integer, String> _channelGroups=null;
    private int _sunrise_hour;
    private int _sunrise_min;
    private int _sunset_hour;
    private int _sunset_min;
	// Map<ChanGroupName, Map<particleId, daytimeHolding>>
	private Map<String, Map<Integer, Boolean>> _daytimeHoldings = null;
}
