/**
 * 
 */
package DWR.DMS.PTM;

/**
 * @author xwang
 *
 */
public class SalmonBasicSurvivalBehavior implements SalmonSurvivalBehavior {
	public boolean DEBUG = false;
	//private Map<String, Double> _survivalRates = null;
	private SurvivalInputs _survivalIn;

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
	}
	
	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SurvivalBehavior#isSurvived(DWR.DMS.PTM.Particle)
	 */
	@Override
	//TODO this function need to be worked on
	// read doug's code and Russ' paper
	public void isSurvived(Particle p, float x, float t) {
		System.err.println("should not be called now!");
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
			Double lam = paras.getFirst();
			Double om = paras.getSecond();
			//TODO is survival rate accumulative?
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
	//TODO clean up new algorithm don't need it anymore 
	/*
	public float getXofXTSurvival(Channel ch, Node nd, float x, float maxDist){
		int sign = (isDownNode(ch, nd)? -1: 1);
		float overHead = (isDownNode(ch, nd)? ch.getLength(): 0.0f);
		float currDist = x*sign + overHead;
		return ((maxDist < currDist)? currDist: maxDist);			 
	}
	*/
	boolean isDownNode(Channel ch, Node nd){return (ch.getDownNodeId() == nd.getEnvIndex());}
	
	//TODO this is fast exp calculation.  should it be used for other PTM calculations?
	/*
	private double exp(double val) {
        final long tmp = (long) (1512775 * val + 1072632447);
        return Double.longBitsToDouble(tmp << 32);
    }
    */

}
