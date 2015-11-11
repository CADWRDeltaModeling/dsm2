/**
 * 
 */
package DWR.DMS.PTM;

import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;;

/**
 * @author xwang
 *
 */
public class SalmonConfusionFactorCalculator {

	/**
	 * 
	 */
	public SalmonConfusionFactorCalculator(SwimInputs si) {
		_confusionConsts = new ConcurrentHashMap<Integer, Double> ();
		_chanDirs = new ConcurrentHashMap<Integer, Integer> ();
		_randomAccess = si.getRandomAccess();;
		_accessProb = si.getAccessProbability();
		_maxConfProb = si.getMaxProbConfusion();
		_confProbSlope = si.getSlopeProbConfusion();
		_tCycleForDir = si.getNumberTidalCycles();
		_constConfProb = si.getConstProbConfusion();
		
	}
	public void updateConfusionConstsChanDirs(Waterbody[] allWaterbody){
		for (Waterbody wb: allWaterbody){
			if ((wb != null)&& (wb.getType() == Waterbody.CHANNEL)){
				Channel ch = (Channel) wb;
				calcConfusionParameters(ch.getEnvIndex(), ch.flowAt[Channel.UPNODE], ch.flowAt[Channel.DOWNNODE]);
			}
		}
	}
	public void calcConfusionParameters(int channelNumber, float upFlow, float downFlow){
		// calculate swimming related info: channel direction and confusion factor
		float curFlow = (upFlow + downFlow)/2;    		
		ArrayList<Float> flows = _flowsInTidalCycles.get(channelNumber);
		if (flows == null){
			flows = new ArrayList<Float>();
			_flowsInTidalCycles.put(channelNumber, flows);
		}
		// For Java, Lists are passed by reference.  changing flows will also change _flowsInTidalCycles.get(channelNumber). 
		flows.add(curFlow);
		Float preAdjFlow = _preAdjFlow.get(channelNumber);
		// Careful! Java passes a Float by value.  changing preAdjFlow will not change _preAdjFlow.get(channelNumber).
		if (preAdjFlow == null)
			preAdjFlow = curFlow;
		float curAdjFlow = 0.1f*curFlow + 0.9f*preAdjFlow;
		float change = curAdjFlow - preAdjFlow;
		Float preChange =  _preChange.get(channelNumber);
		if (preChange == null)
			preChange = 0.0f;
		if ((change>0.0f) && (preChange<0.0f))
			_tideCount += 1;
		if(_tideCount >= _tCycleForDir){
			int numFlows = flows.size();
			if (numFlows == 0)
				PTMUtil.systemExit("error: no flow found in the tidal cycles, system exit.");
			float sumFlows = 0.0f;
			for (float flow: flows)
				sumFlows += flow;
			_chanDirs.put(channelNumber, ((sumFlows>0)? 1: -1));
			float meanFlow = sumFlows/numFlows;
			double sumSqD = 0;
			for (float flow: flows){
				float d = (flow - meanFlow);
				sumSqD += d*d;
			}
			double sd = Math.sqrt(sumSqD/numFlows);
			if (sd < Double.MIN_VALUE){
				System.err.println("warn: flows are constant in the tidal cycle! set Stard Deviation to Double.MIN_VALUE.");
				sd = Double.MIN_VALUE;
			}
			double s_n = Math.log(Math.max(1E-10, Math.abs(meanFlow/sd)));
			double term = Math.exp(_constConfProb + _confProbSlope*s_n);
			_confusionConsts.put(channelNumber, _maxConfProb*term/(1+term));  			
			_tideCount = 0;
			flows.clear();
		}
		else{
			if(_confusionConsts.get(channelNumber) == null)
				_confusionConsts.put(channelNumber, 0.0);
			if(_chanDirs.get(channelNumber) == null)
				_chanDirs.put(channelNumber, 1);
		}
		_preAdjFlow.put(channelNumber, curAdjFlow);
		_preChange.put(channelNumber, change);    		
	}
	int getConfusionFactor(int chanId){
		// getChanDir(...) is from fish's prospective.   
		// therefore, channel direction is only considered when all of the conditions below are met.
		// This is the same as what NMFS implemented.
		return ((_randomAccess && (PTMUtil.getRandomNumber() < _accessProb)
		&& (PTMUtil.getRandomNumber() < getConfusionConst(chanId)))? (-1*getChanDir(chanId)):1);		
	}
	private double getConfusionConst(int chanId){
		Double cc = _confusionConsts.get(chanId);
		if ( cc == null){
			PTMUtil.systemExit("The fish confusion constant is not set, please check the code, system exit. ");
		}
		return cc;
	}
	private int getChanDir(int chanId){
		Integer cc = _chanDirs.get(chanId);
		if ( cc == null)
			PTMUtil.systemExit("The fish confusion constant is not set, please check the code, system exit. ");
		return cc;
	}
	// used to calc confusion factors and channel directions
	private float _constConfProb = 0.0f;
	private float _maxConfProb = 1.0f;
	private float _confProbSlope = -0.25f;
	private int _tCycleForDir = 2;
	private ConcurrentHashMap<Integer, ArrayList<Float>> _flowsInTidalCycles = new ConcurrentHashMap<Integer,ArrayList<Float>>();
	private ConcurrentHashMap<Integer, Float> _preAdjFlow = new ConcurrentHashMap<Integer, Float>();
	private ConcurrentHashMap<Integer, Float> _preChange = new ConcurrentHashMap<Integer, Float>();
	private int _tideCount = 0;
	//Map<channelId, confusion constant>
	private ConcurrentHashMap<Integer, Double> _confusionConsts = null;
	//Map<channelId, channel direction>
	private ConcurrentHashMap<Integer, Integer> _chanDirs = null;
	private boolean _randomAccess = false;
	private float _accessProb;
}
