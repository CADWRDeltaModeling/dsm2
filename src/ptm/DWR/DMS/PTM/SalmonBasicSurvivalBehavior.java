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
		if (_pGroupUsed.get(pId) == null)
			_pGroupUsed.put(pId, new HashSet<Integer>());
		boolean isStart = _survivalIn.isStart(chanId, p.x, p.getFromUpstream());
		Integer sSta = _pStartSta.get(pId);		
		// sSta == null means the particle doesn't know its survival start station. 
		// need only to search for a start station.  no need to search for an end or exchange station
		if (sSta == null){
			if (isStart){
				_pStartSta.put(pId, chanId);
				_pStartAge.put(pId, p.age);
				_survivalIn.addArrivalToGroup(_survivalIn.getGroupNumber(chanId));
				_pGroupUsed.get(pId).add(_survivalIn.getGroupNumber(chanId));
				if(DEBUG){
					System.err.println("pId:"+pId+" chanId:"+PTMHydroInput.getExtFromIntChan(chanId)+" pAge:"+p.age+" p.x:"+p.x+" start null ");
				}
			}
			return;
		}
		// if the particle is in a start channel, impossible to be also in an end or exchange channel.
		// so do nothing.
		if (sSta == chanId)
			return;
		/*
		 * no need to check isStart here because sSta != null.
		 * only need to check if isExchange or isEnd 
		 */
		Integer groupId = _survivalIn.getGroupNumber(sSta);
		boolean isEnd = _survivalIn.isEnd(groupId, chanId, p.x, p.getFromUpstream());
		boolean isExchange = _survivalIn.isExchange(groupId, chanId, p.x, p.getFromUpstream()); 
		if (isExchange){
			// subtract 1 from the group that this particle leaves,
			// and add one to the group currently entered
			int newGroupId = _survivalIn.getGroupNumber(chanId);
			_survivalIn.minusArrivalToGroup(groupId);
			_survivalIn.addArrivalToGroup(newGroupId);			
			_pStartSta.put(pId, chanId);
			_pGroupUsed.get(pId).add(newGroupId);
			_pGroupUsed.get(pId).remove(sSta);
			if(DEBUG)
				System.err.println(PTMHydroInput.getExtFromIntChan(chanId)+ ","+"pId:"+pId+" chanId:"+PTMHydroInput.getExtFromIntChan(chanId)+" isExchange ");
			return;				
		}
		if (isEnd){
			ArrayList<Double> paras = _survivalIn.getSurvivalParameters(groupId);
			double lam = paras.get(0), om = paras.get(1), X = paras.get(2);
			if(DEBUG){
				int chanExt = PTMHydroInput.getExtFromIntChan(chanId);
				double start = _pStartAge.get(pId);
				System.err.println(PTMHydroInput.getExtFromIntChan(chanId)+ ","+"pId:"+pId+" chan:"+chanExt+" p.x:"+p.x+" p.age:"+p.age+" p.StartAge:"+start+ "  Group:"+groupId+" Lamda:"
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
			//TODO 
			/*
			 * according to Russ Perry, the survival rate should not be accumulative (i.e., calculated according to 
			 * previous survival rate) so comment out should only be calculated according to the current rate
			 * _pProb may not be needed
			 */
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
			 * it is up to users to check the mistake in the input behavior file
			 * 
			 * isStart has to be checked after all other checks (i.e., isExchange, isEnd) done to avoid exit prematurely.
			 * */
			if (isStart && !_pGroupUsed.get(pId).contains(_survivalIn.getGroupNumber(chanId))){
				_pStartSta.put(pId, chanId);
				_pStartAge.put(pId, p.age);
				_survivalIn.addArrivalToGroup(_survivalIn.getGroupNumber(chanId));
				_pGroupUsed.get(pId).add(_survivalIn.getGroupNumber(chanId));
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
	}//isSurvived
}
