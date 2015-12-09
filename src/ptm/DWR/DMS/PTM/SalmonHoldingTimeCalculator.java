package DWR.DMS.PTM;

import java.util.Calendar;
import java.util.Map;

public class SalmonHoldingTimeCalculator {

	public SalmonHoldingTimeCalculator(SwimInputs si) {
		_swimmingTimes = si.getPartcleMeanRearingHoldingMap();
		_swimParameters = si.getSwimParameters();
		if (_swimParameters == null)
			PTMUtil.systemExit("The parameter for rear holding is not properly set! Please check the behavior input file, system exit.");
		else{
			for (String key: _swimParameters.keySet()){
				float[] paras = _swimParameters.get(key);
				if (paras == null || paras.length < 4)
					PTMUtil.systemExit("The parameter for rear holding is not properly set! Please check the behavior input file, system exit.");
			}
		}
		_daytimeNotSwimPercent = si.getDaytimeNotSwimPercent();
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
	boolean daytimeHolding(){
    	Calendar curr = PTMUtil.modelTimeToCalendar(Globals.currentModelTime);
    	//TODO will use LocaTime when switch to Java 1.8
    	boolean isDaytime = (curr.get(Calendar.HOUR_OF_DAY) > _sunrise_hour && curr.get(Calendar.HOUR_OF_DAY) < _sunset_hour) 
    			|| (curr.get(Calendar.HOUR_OF_DAY) == _sunrise_hour && curr.get(Calendar.MINUTE)>_sunrise_min)
    			|| (curr.get(Calendar.HOUR_OF_DAY) == _sunset_hour && curr.get(Calendar.MINUTE)<_sunset_min);
    	return (isDaytime && (PTMUtil.getRandomNumber() < _daytimeNotSwimPercent));
	}
	// Rearing holding time is the time when a particle to swimming again, i.e., current model time + holding time
	//TODO cast float to int e.g., 900.6 = 900, good enough?   
	void setSwimTime(Particle p, int chanId){
		String groupName = _channelGroups.get(chanId);
		if (groupName == null)
				groupName = "ALL";
		// pidSwimTimes is set in SwimInputs, impossible to be null
		Map<Integer, Long> pidSwimTimes = ((groupName == null)? _swimmingTimes.get("ALL"): _swimmingTimes.get(groupName));
		// if swim time never be set for this particle, set, otherwise do nothing
		// swimming time is one per channel group per particle
		if (pidSwimTimes.get(p.Id) == null){
			//TODO clean up later
			//System.err.println(groupName + " "+ PTMHydroInput.getExtFromIntChan(p.wb.getEnvIndex()));
			int holdingTime = (int) (-_swimParameters.get(groupName)[3]*(float)Math.log(PTMUtil.getRandomNumber()));
			if (holdingTime < 0)
				PTMUtil.systemExit("got a negative rearing holding time, which is imposible, system exit.");
			//pidSwimTimes.put(pId, (holdingTime+Globals.currentModelTime));
			// this is more accurate
			pidSwimTimes.put(p.Id, (holdingTime+p.getCurrentParticleTimeExact()));
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
	
	//TODO be very careful, the class variables are visible to all particles!!!
	private float _daytimeNotSwimPercent = 0.0f;
	private Pair<Integer, Integer> _sunrise = null;
	private Pair<Integer, Integer> _sunset = null;
	// Map<ChanGroupName, Map<particleId, particleMeanRearingHoldingTime>>
	private Map<String, Map<Integer, Long>> _swimmingTimes = null;
	// group name, swimming velocity parameters[] [0] constSwimmingVelocity; [1]; STD for particles; [2] STD for time steps for an individual particle
	private Map<String, float[]> _swimParameters=null;
	// Channel number (internal), chan group name
	private Map<Integer, String> _channelGroups=null;
    private int _sunrise_hour;
    private int _sunrise_min;
    private int _sunset_hour;
    private int _sunset_min;
}
