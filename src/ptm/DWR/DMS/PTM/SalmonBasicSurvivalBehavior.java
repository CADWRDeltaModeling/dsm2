/**
 * 
 */
package DWR.DMS.PTM;
import java.util.Map;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.HashSet;

/**
 * @author xwang
 *
 */
public class SalmonBasicSurvivalBehavior implements SalmonSurvivalBehavior {
	public boolean DEBUG = false;
	private SurvivalInputs _survivalIn;
	// Map<pid, start station>
	private Map<Integer, Integer> _pStartSta;
	// Map<pid, start age>
	private Map<Integer, Float> _pStartAge;
	// Map<pid, list of start stations already used>
	private Map<Integer, HashSet<Integer>> _pGroupUsed;
	//TODO may not be needed because survival rates are not calculated as accumulative
	// Map<pid, survival probability>
	private Map<Integer, Double> _pProb;

	/**
	 * 
	 */
	public SalmonBasicSurvivalBehavior() {
		// TODO Auto-generated constructor stub
	}
	/**
	 * 
	 */
	public SalmonBasicSurvivalBehavior(SurvivalInputs survivalIn) {
		_survivalIn = survivalIn;
		_pGroupUsed = new HashMap<Integer, HashSet<Integer>>();
		_pStartSta = new HashMap<Integer, Integer>();
		_pStartAge = new HashMap<Integer, Float>();
		_pProb = new HashMap<Integer, Double>();
	}
	
	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SurvivalBehavior#isSurvived(DWR.DMS.PTM.Particle)
	 */
	public void isSurvived(Particle p) {
		if(!_survivalIn.getDoSurvival())
			return;
		int pId = p.Id;
		Channel ch = (Channel)p.wb;
		int chanId = ch.getEnvIndex();
		boolean isStart = _survivalIn.isStart(chanId, p.x, p.getFromUpstream());
		Integer sSta = _pStartSta.get(pId);
		//if this particle don't have a start station
		//it is only possible at an initial time for a particle doesn't have a start station
		if (sSta == null){
			if (isStart){
				_pStartSta.put(pId, chanId);
				_pStartAge.put(pId, p.age);
				_survivalIn.addArrivalToGroup(_survivalIn.getGroupNumber(chanId));
				if(DEBUG){
					System.err.println("pId:"+pId+" chanId:"+PTMHydroInput.getExtFromIntChan(chanId)+" pAge:"+p.age+" p.x:"+p.x+" start null ");
				}
			}
			return;
		}
		/*
		 * if exist a start station, the program will not check it until getting an end station
		 * because this could cause a premature exit
		 */
		Integer groupId = _survivalIn.getGroupNumber(sSta);
		boolean isEnd = _survivalIn.isEnd(groupId, chanId, p.x, p.getFromUpstream());
		boolean isExchange = _survivalIn.isExchange(groupId, chanId, p.x, p.getFromUpstream()); 
		if (_pGroupUsed.get(pId) == null)
			_pGroupUsed.put(pId, new HashSet<Integer>());
		_pGroupUsed.get(pId).add(groupId);
		if (sSta == chanId)
			return;
		if (isExchange){
			//to subtract 1 from the arrival number in the group that this particle is from,
			//and add one to the arrival number in the current group
			_survivalIn.minusArrivalToGroup(_survivalIn.getGroupNumber(_pStartSta.get(pId)));
			_survivalIn.addArrivalToGroup(_survivalIn.getGroupNumber(chanId));
			
			_pStartSta.put(pId, chanId);
			if(DEBUG)
				System.err.println("pId:"+pId+" chanId:"+PTMHydroInput.getExtFromIntChan(chanId)+" isExchange ");
			return;				
		}
		if (isEnd){
			ArrayList<Double> paras = _survivalIn.getSurvivalParameters(groupId);
			double lam = paras.get(0), om = paras.get(1), X = paras.get(2);
			if(DEBUG){
				int chanExt = PTMHydroInput.getExtFromIntChan(chanId);
				double start = _pStartAge.get(pId);
				System.err.println("pId:"+pId+" chan:"+chanExt+" p.x:"+p.x+" p.age:"+p.age+" p.StartAge:"+start+ "  Group:"+groupId+" Lamda:"
						+lam+" omaga:"+om+" X:"+X+" isEnd");
			}
			float t = p.age - _pStartAge.get(pId);
			/*
			 * From Anderson, J. J., Gurarie, E., & Zabel, R. W. (2005). Mean free-path length theory of 
			 * predator–prey interactions: Application to juvenile salmon migration. Ecological Modelling, 
			 * 186(2), 196–211. doi:10.1016/j.ecolmodel.2005.01.014
			 * Units of lambda are feet; units of omega are feet/sec. t in seconds and x in feet
			 */
			if (_pProb.get(pId) == null)
				_pProb.put(pId, 1.0);
			//TODO according to Russ, the survival rate should not be accumulative (i.e., calculated according to previous survival rate) so comment out
			//TODO should only be calculated according to the current rate
			//TODO _pProb may not be needed
			//double survival = _pProb.get(pId)*(Math.exp((-1.0/lam)*Math.sqrt(X*X + om*om*t*t)));
			double survival = (Math.exp((-1.0/lam)*Math.sqrt(X*X + om*om*t*t)));
			double po = PTMUtil.getRandomNumber();
			//TODO may not need the following line because the survival is not accumulative
			_pProb.put(pId, survival);		
			if (survival < po){
				p.setParticleDead();
				_survivalIn.addLostToGroup(groupId);
				if(DEBUG) 
					System.err.println("pId:" + pId +" channel:"+PTMHydroInput.getExtFromIntChan(chanId)
					+"  timeInterval:"+t+"  survival probability:"+ _pProb.get(pId)+"  p.isDead:"+p.isDead + "  rand:" + po+" isDead");	
				return;
			}
			else
				_survivalIn.addSurvivalToGroup(groupId);
			if(DEBUG){
				System.err.println("pId:" + pId +" node: "+PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex()) 
							+"  channel:"+PTMHydroInput.getExtFromIntChan(chanId)
						+ "  timeInterval:"+t+"  survival probability:"+ _pProb.get(pId)+"  p.isDead:"+p.isDead 
						+ " rand:" + po +" X:" + X+"  isEnd");
			}
			_pStartSta.put(pId, null);
			_pStartAge.put(pId, null);
			
			/* End node usually is also a start node for next group so that there is no travel time gap
			 * if it is not, it should be a mistake in the behavior input file because some of the travel time will not be used for calculating survival  
			 * it is up to user to check the mistake in the input behavior file
			 * 
			 * isStart has to be checked after all other checks (i.e., isExchange, isEnd) done to avoid exit prematurely.
			 * */
			if (isStart && !_pGroupUsed.get(pId).contains(_survivalIn.getGroupNumber(chanId))){
				_pStartSta.put(pId, chanId);
				_pStartAge.put(pId, p.age);
				_survivalIn.addArrivalToGroup(_survivalIn.getGroupNumber(chanId));
			}
			//TODO really need a warning?
			//else
				//System.err.println("Warnning: the end channel:" +PTMHydroInput.getExtFromIntChan(chanId)+" is not a next start channel, could miss travel time.");
			return;
		} //isEnd
		/*
		if (isStart && !_pGroupUsed.get(pId).contains(_survivalIn.getGroupNumber(chanId))){
			//TODO should be a warning or exit???
			System.err.println(pId+"  "+ p.x + "  "+p.getFromUpstream()+"  " + PTMHydroInput.getExtFromIntChan(chanId)+ "  "+PTMHydroInput.getExtFromIntChan(sSta));
			PTMUtil.systemExit("the particle gets to a start channel without passing through an end or exchangeable channel, impossible, system exit!");
		}
		*/
	}
}
		//the start station has been used, ignore
		//check to see if this channel is one of these
		//1. start station a. if the one has been used
		//2. exchangeable station
		//3. end station
		//4. none of above, don't do anything
		

			/*
			 * 	// Map<pid, in an end channel>
			 * 	private Map<Integer, ArrayList<Double>> _survivalParasMap;
			 * 		_survivalParasMap = _survivalIn.getSurvivalParameterMap();
	private Map<Integer, Boolean> _pInChannel;
			_pInChannel = new HashMap<Integer, Boolean>();
			 * if (stasUsed == null)
					stasUsed = new HashSet<Integer>();
				stasUsed.add(p.Id);
				_pStartStationsUsed.put(p.Id, stasUsed);
							if(DEBUG){
				System.out.println("id:"+pId+ " Lambda: "+ paras.get(0)+"  Omega: "+paras.get(1)+"  X: "+paras.get(2));
				System.out.println("channel:"+ PTMHydroInput.getExtFromIntChan(chanId)+ " timeInterval:"+t+"  survival probability:"
				+_pProb.get(pId));
			}
			 */
			//ignore if this is already used or in a start channel
		/*
			if(_survivalIn.isStartChan(chanId) start channel can also be a end channel ||?? (stasUsed != null && stasUsed.contains(chanId)))
				return;
			if(_survivalIn.isExchangeChan(sSta, chanId)){
				

		}
		int startChanId = p.getStartChannel();
		HashSet<Integer> sList = _pStartStationsUsed.get(p.Id);
		if (survivalParas.size()!=3)
			PTMUtil.systemExit("survival parameter lamda, omega, x are not input correctly, check the input file, system exit.");
		if(sList == null){
			sList = new HashSet<Integer>();
			_pStartStationsUsed.put(p.Id, sList);
		}
		/*
		 * From Anderson, J. J., Gurarie, E., & Zabel, R. W. (2005). Mean free-path length theory of 
		 * predator–prey interactions: Application to juvenile salmon migration. Ecological Modelling, 
		 * 186(2), 196–211. doi:10.1016/j.ecolmodel.2005.01.014
		 * Units of lambda are feet; units of omega are feet/sec. t in seconds and x in feet
		 */
		/*
		if (survivalParas.size()!=3)
			PTMUtil.systemExit("survival parameter lamda, omega, x are not input correctly, check the input file, system exit.");
		Double lam = survivalParas.get(0), om = survivalParas.get(1), X = survivalParas.get(2);
		
		p.setSurvivalProbability(p.getSurvivalProbability()*(Math.exp((-1.0/lam)*Math.sqrt(x*x + om*om*t*t))));
		//survivalProb = Math.exp((-1.0/lam)*Math.sqrt(x*x + om*om*t*t));
		if(DEBUG){
			System.out.println("id:"+p.Id+ " Lambda: "+ paras.getFirst()+"  Omega: "+paras.getSecond());
			System.out.println("channel:"+wb.getEnvIndex()+ " timeInterval:"+t+"  survival probability:"
			+p.getSurvivalProbability());
		}	
	}
	double po = PTMUtil.getRandomNumber();	
	if (p.getSurvivalProbability()<po){
		p.setParticleDead();
		if(DEBUG) 
			System.out.println("channel:"+PTMHydroInput.getExtFromIntChan(((Channel) wb).getEnvIndex())
			+"  id:" + p.Id + "  timeInterval:"+t+"  survival probability:"+ p.getSurvivalProbability()+"  p.isDead:"+p.isDead + "  " + po);	
	}
	if(DEBUG){
		if (p.Id == 1)
			System.err.println("node: "+PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex()) 
					+"  channel:"+PTMHydroInput.getExtFromIntChan(((Channel) wb).getEnvIndex())
				+ "  timeInterval:"+t+"  survival probability:"+ p.getSurvivalProbability()+"  p.isDead:"+p.isDead 
				+ " rand:" + po +" x:" + x);
	}
	
}
*/	
/*	
	@Override
	
	// read doug's code and Russ' paper
	public void isSurvived(Particle p) {
		Channel ch = (Channel)p.wb;
		int chanId = ch.getEnvIndex();
		int startChanId = p.getStartChannel();
		if(_survivalIn.isEndChan(startChanId, chanId))
			return;
		HashSet<Integer> sList = _pStartStationsUsed.get(p.Id);
		if(sList == null){
			sList = new HashSet<Integer>();
			_pStartStationsUsed.put(p.Id, sList);
		}
		boolean isDownNode = (ch.getDownNodeId() == p.nd.getEnvIndex());
		//p.x will not be negative because in the SalmonBasicSwimBehavior isNodeReached block 
		//p.x is corrected when reach a node 
		if (p.x < 0)
			PTMUtil.systemExit("p.x is negative, it is impossible, check the code, exit from line 65 SalmonBasicSurvivalBehavior.");
		float x = (isDownNode? -p.x: p.x); 
		//a start channel has not been set yet.  Don't need to check end or exchangeable station
		if(startChanId < 0){
			//!sList.contains(chanId), start channel is not used
			if(!_survivalIn.isStartChan(chanId)||sList.contains(chanId))
				p.isSurvivalStaChannel = false;
			else{
				if(_survivalIn.checkStart(chanId, x)){
					_pStartSta.put(p.Id, chanId);
					_pStartAge.put(p.Id, p.age);
					sList.add(chanId);
					_pInChannel.put(p.Id, false);
				}
				else
					p.isSurvivalStaChannel = true;
					
			}
		}
		//start station set.  check if a end or exchangeable station
		else{
			//end channel
			if(_survivalIn.isEndChan(startChanId, chanId)){
				//end channel and end station.  
				if(_survivalIn.checkEnd(chanId, x)){
					// calc survival probability, if not survival, set the isDead = true
					calculateSurvival(p, _survivalIn.getSurvivalParameters(startChanId), (p.age - p.getStartAge()));
					// set start station, check if the start station has been used, yes, skip
					// start channel. !sList.contains(chanId), start channel is not used
					if (_survivalIn.isStartChan(chanId)&&(!sList.contains(chanId))){
						//start channel and station, set start station
						if(_survivalIn.checkStart(chanId, x)){
							_pStartSta.put(p.Id, chanId);
							_pStartAge.put(p.Id, p.age);
							p.isSurvivalStaChannel = false;
						}
						//start channel but not station, set start channel to -999999
						else{
							_pStartSta.put(p.Id, null);
							_pStartAge.put(p.Id, null);
							p.isSurvivalStaChannel = true;
						}
					}
					//not a start channel
					else{
						_pStartSta.put(p.Id, null);
						_pStartAge.put(p.Id, null);
						p.isSurvivalStaChannel = false;
					}
				}//if(_survivalIn.checkEnd(chanId, x))
				// end channel but not station
				else
					p.isSurvivalStaChannel = true;
			}//if(_survivalIn.isEndChan(startChanId, chanId))
			// not an end channel
			else{
				//exchangeable channel
				if(_survivalIn.isExchangeChan(startChanId, chanId)){
					if(_survivalIn.checkExchange(chanId, x)){
						//change channel number but clock still ticking
						_pStartSta.put(p.Id, chanId);;
						p.isSurvivalStaChannel = false;
					}
					else
						p.isSurvivalStaChannel = true;
				}//if(_survivalIn.isExchangeChan(startChanId, chanId))
				//else not exchangeable or end or start channel do nothing
			}
		} //!(startChanId < 0)	
	}
	*/
	/*
	private void calculateSurvival(Particle p, ArrayList<Double> survivalParas, double travelTime){
		/*
		 * From Anderson, J. J., Gurarie, E., & Zabel, R. W. (2005). Mean free-path length theory of 
		 * predator–prey interactions: Application to juvenile salmon migration. Ecological Modelling, 
		 * 186(2), 196–211. doi:10.1016/j.ecolmodel.2005.01.014
		 * Units of lambda are feet; units of omega are feet/sec. t in seconds and x in feet
		 */
	/*
		if (survivalParas.size()!=3)
			PTMUtil.systemExit("survival parameter lamda, omega, x are not input correctly, check the input file, system exit.");
		Double lam = survivalParas.get(0), om = survivalParas.get(1), X = survivalParas.get(2);
		
		p.setSurvivalProbability(p.getSurvivalProbability()*(Math.exp((-1.0/lam)*Math.sqrt(x*x + om*om*t*t))));
		//survivalProb = Math.exp((-1.0/lam)*Math.sqrt(x*x + om*om*t*t));
		if(DEBUG){
			System.out.println("id:"+p.Id+ " Lambda: "+ paras.getFirst()+"  Omega: "+paras.getSecond());
			System.out.println("channel:"+wb.getEnvIndex()+ " timeInterval:"+t+"  survival probability:"
			+p.getSurvivalProbability());
		}	
	}
	double po = PTMUtil.getRandomNumber();	
	if (p.getSurvivalProbability()<po){
		p.setParticleDead();
		if(DEBUG) 
			System.out.println("channel:"+PTMHydroInput.getExtFromIntChan(((Channel) wb).getEnvIndex())
			+"  id:" + p.Id + "  timeInterval:"+t+"  survival probability:"+ p.getSurvivalProbability()+"  p.isDead:"+p.isDead + "  " + po);	
	}
	if(DEBUG){
		if (p.Id == 1)
			System.err.println("node: "+PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex()) 
					+"  channel:"+PTMHydroInput.getExtFromIntChan(((Channel) wb).getEnvIndex())
				+ "  timeInterval:"+t+"  survival probability:"+ p.getSurvivalProbability()+"  p.isDead:"+p.isDead 
				+ " rand:" + po +" x:" + x);
	}
	
}
	}
/*					
					
					
					???
					p.recordStartChannel(chanId);
					p.recordStartTime(p.age);
					p.isSurvivalStaChannel = false;
				}
				else
					p.isSurvivalStaChannel = true;
				return;
			}
			
			p.isSurvivalStaChannel = true;
			return;
		}
		if (startChanId < 0){
			if (!_survivalIn.startStaReached(chanId, p.x need to set positive or negative))
				return;
			else{
				p.recordStartChannel(chanId);
				p.recordStartTime(p.age);
				p.isSurvivalStaChannel = false;
				return;
			}
		}  
		else{
			if (_survivalIn.endStaReached(startChanId, chanId, p.x)){
				checkSurvival();
				if (_survivalIn.startStaReached(chanId, p.x)){
					p.recordStartChannel(chanId);
					p.recordStartTime(p.age);
					p.isSurvivalStaChannel = false;
				}
				return;
			}
			else if (_survivalIn.exchangeStaReached(startChanId, chanId, p.x)){
				p.recordStartChannel(chanId);
				return;
			}			
		}
		return;
	}
	*/
/*		
		
		if (_survivalIn.getAllSurvivalParameters() == null){
			System.err.println("Warning: survival rates are not set in the behavior input file and survival is not calculated.");
			p.isDead = false;
			return;
		}
		if (t<0 || x<0)
			PTMUtil.systemExit("one or both inputs of X, T in survival model are negative! System exit");

		Waterbody wb = p.wb;
		Pair<Double, Double> paras = null;
		if (wb.getType() == Waterbody.CHANNEL){
			if ((paras = _survivalIn.getSurvivalParameters(p.wb.getEnvIndex()))== null)
				PTMUtil.systemExit("couldn't find survival rate calculation parameters, system exit");
			/*
			 * From Anderson, J. J., Gurarie, E., & Zabel, R. W. (2005). Mean free-path length theory of 
			 * predator–prey interactions: Application to juvenile salmon migration. Ecological Modelling, 
			 * 186(2), 196–211. doi:10.1016/j.ecolmodel.2005.01.014
			 * Units of lambda are feet; units of omega are feet/sec. t in seconds and x in feet
			 */
/*
	Double lam = paras.getFirst();
			Double om = paras.getSecond();
			
			p.setSurvivalProbability(p.getSurvivalProbability()*(Math.exp((-1.0/lam)*Math.sqrt(x*x + om*om*t*t))));
			//survivalProb = Math.exp((-1.0/lam)*Math.sqrt(x*x + om*om*t*t));
			if(DEBUG){
				System.out.println("id:"+p.Id+ " Lambda: "+ paras.getFirst()+"  Omega: "+paras.getSecond());
				System.out.println("channel:"+wb.getEnvIndex()+ " timeInterval:"+t+"  survival probability:"
				+p.getSurvivalProbability());
			}	
		}
		double po = PTMUtil.getRandomNumber();	
		if (p.getSurvivalProbability()<po){
			p.setParticleDead();
			if(DEBUG) 
				System.out.println("channel:"+PTMHydroInput.getExtFromIntChan(((Channel) wb).getEnvIndex())
				+"  id:" + p.Id + "  timeInterval:"+t+"  survival probability:"+ p.getSurvivalProbability()+"  p.isDead:"+p.isDead + "  " + po);	
		}
		if(DEBUG){
			if (p.Id == 1)
				System.err.println("node: "+PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex()) 
						+"  channel:"+PTMHydroInput.getExtFromIntChan(((Channel) wb).getEnvIndex())
					+ "  timeInterval:"+t+"  survival probability:"+ p.getSurvivalProbability()+"  p.isDead:"+p.isDead 
					+ " rand:" + po +" x:" + x);
		}
		
	}
	*/
	/*
	public void isSurvived(Particle p, float x, float t) {
		if (_survivalIn.getSurvivalRates() == null){
			p.isDead = false;
			return;
		}
		// timeInterval in days
		// timeToAdvance in seconds
		double timeInterval = t/(60d*60d*24d);
		if (timeInterval<0)
			PTMUtil.systemExit("in particle survial behavior, expect positive time interval but get:" + timeInterval);
		double survivalProbability = 0;
		Waterbody wb = p.wb;
		Double rate = null;
		if ((wb.getType() == Waterbody.CHANNEL)
				&& ((rate = _survivalIn.getSurvivalRate(p.wb.getEnvIndex()))!= null)){
			survivalProbability = Math.exp(rate*timeInterval*(-1.0));
			if(DEBUG){
				System.out.println("id:"+p.Id+ " rate:"+ rate);
				System.out.println("channel:"+wb.getEnvIndex()+ " timeInterval:"+timeInterval*60d*60d*24d+"  survival probability:"+survivalProbability);
			}	
		}
		else
			survivalProbability = 1;
		if (survivalProbability<PTMUtil.getNextGaussian()){
			p.isDead = true;	
			if(DEBUG) 
				System.out.println("channel:"+PTMHydroInput.getExtFromIntChan(((Channel) wb).getEnvIndex())
				+"  id:" + p.Id + "  timeInterval:"+timeInterval*24*60*60+"  survival probability:"+ survivalProbability+"  p.isDead:"+p.isDead);	
		}
		
	}
	*/
	/*
	 * get the maximum distance a particle traveled from the entrance node
	 */
	
	/*
	public float getXofXTSurvival(Channel ch, Node nd, float x, float maxDist){
		int sign = (isDownNode(ch, nd)? -1: 1);
		float overHead = (isDownNode(ch, nd)? ch.getLength(): 0.0f);
		float currDist = x*sign + overHead;
		return ((maxDist < currDist)? currDist: maxDist);			 
	}
	*/
	/*
	boolean isDownNode(Channel ch, Node nd){return (ch.getDownNodeId() == nd.getEnvIndex());}
	
	
	/*
	private double exp(double val) {
        final long tmp = (long) (1512775 * val + 1072632447);
        return Double.longBitsToDouble(tmp << 32);
    }
    */

//}
