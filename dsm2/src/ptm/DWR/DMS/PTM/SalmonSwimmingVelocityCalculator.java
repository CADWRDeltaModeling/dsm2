package DWR.DMS.PTM;

import java.util.Map;
import java.util.HashMap;

public class SalmonSwimmingVelocityCalculator {

	public SalmonSwimmingVelocityCalculator(SwimInputs si) {
		_meanSwimVels = si.getPartcleMeanSwimmingVelocityMap();
		_swimParameters = si.getSwimParameters();
		if (_swimParameters == null)
			PTMUtil.systemExit("The parameter for rear holding is not properly set! Please check the behavior input file, system exit.");
		else{
			for (String key: _swimParameters.keySet()){
				float[] paras = _swimParameters.get(key);
				if (paras == null || paras.length < 5)
					PTMUtil.systemExit("The parameter for swimming parameters are not correctly set, expect 5, but get less than 5!");
			}
		}
		_channelGroups = si.getChannelGroups();
	}
	public float CalcXSwim(float swimSpeed, float timeStep){return swimSpeed*timeStep;}
	//Careful, confusion factor is not included in this swimming velocity returned
	//TODO change to calling random numbers from Particle
	//public float getSwimmingVelocity(int pId, int chanId){
	public float getSwimmingVelocity(Particle p, int chanId){
		float [] paras = getSwimParameters(chanId);
		//return getMeanSwimmingVelocity(pId, chanId) + paras[2]*((float)PTMUtil.getNextGaussian());
		return getMeanSwimmingVelocity(p.Id, chanId) + paras[2]*((float) p.getGaussian());
	}
	void setMeanSwimmingVelocity(int pId, int chanId){
		float [] paras = getSwimParameters(chanId);
		Map<Integer, Float> pidMeanSwimVel =  _meanSwimVels.get(getGroupName(chanId));
		//one mean swimming velocity per channel group per particle
		if (pidMeanSwimVel.get(pId) == null){

			Float z = _pZValues.get(pId);
			if (z == null)
				_pZValues.put(pId, ((float)PTMUtil.getNextGaussian()));
			pidMeanSwimVel.put(pId, paras[0] + paras[1]*_pZValues.get(pId));

			//pidMeanSwimVel.put(pId, paras[0] + paras[1]*((float)PTMUtil.getNextGaussian()));
		}
	}
	public float getMeanSwimmingVelocity(int pId, int chanId){
		Map<Integer, Float> pidMeanSwimVel = _meanSwimVels.get(getGroupName(chanId));
		if (pidMeanSwimVel == null || pidMeanSwimVel.get(pId) == null)
			PTMUtil.systemExit("Mean swimming velocity never been set for channel: "
					+PTMHydroInput.getExtFromIntChan(chanId)
					+" in the group: "+getGroupName(chanId)+", check the code! system exit.");
		return pidMeanSwimVel.get(pId);
	}
	private float[] getSwimParameters(int chanId){
		float[] paras = _swimParameters.get(getGroupName(chanId));
		if (paras == null)
			PTMUtil.systemExit("The the parameters for swimming velocity are not properly set! Please check the behavior input file, system exit.");
		return paras;
	}
	private String getGroupName(int chanId){
		String groupName = _channelGroups.get(chanId);
		return ((groupName == null)? "ALL":groupName);
	}
	// these maps are set in SwimInputs and are not null.  if they are null, something is wrong.  No need to check null
	// group name, swimming velocity parameters[] [0] constSwimmingVelocity; [1]; STD for particles; [2] STD for time steps for an individual particle
	private Map<String, float[]> _swimParameters=null;
	// Map<ChanGroupName, Map<particleId, meanSwimmingVelocity>>
	private Map<String, Map<Integer, Float>> _meanSwimVels = null;
	// Channel number (internal), chan group name
	private Map<Integer, String> _channelGroups=null;
	// <pid, an normal distribution draw>
	private Map<Integer, Float> _pZValues = new HashMap<Integer, Float>();

}
