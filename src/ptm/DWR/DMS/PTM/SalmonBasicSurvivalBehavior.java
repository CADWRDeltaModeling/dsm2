/**
 * 
 */
package DWR.DMS.PTM;
import java.security.SecureRandom;
import java.util.Map;

/**
 * @author xwang
 *
 */
public class SalmonBasicSurvivalBehavior implements SalmonSurvivalBehavior {
	private boolean DEBUG = false;
	private Map<String, Double> _survivalRates = null;

	/**
	 * 
	 */
	public SalmonBasicSurvivalBehavior() {
		// TODO Auto-generated constructor stub
	}
	/**
	 * 
	 */
	public SalmonBasicSurvivalBehavior(Map<String, Double> survivalRates) {
		_survivalRates = survivalRates;
	}
	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SurvivalBehavior#isSurvived(DWR.DMS.PTM.Particle)
	 */
	@Override
	public void isSurvived(Particle p, float timeToAdvance) {
		// timeInterval in days
		// timeToAdvance in seconds
		double timeInterval = timeToAdvance/(60d*60d*24d);
		if (timeInterval<0)
			PTMUtil.systemExit("in particle survial behavior, expect positive time interval but get:" + timeInterval);
		double survivalProbability = 0;
		Waterbody wb = p.wb;
		Double rate = null;
		if ((wb.getType() == Waterbody.CHANNEL) && (_survivalRates != null)
				&& ((rate = _survivalRates.get(((Channel) wb).getChanGroup())) != null)){
			survivalProbability = Math.exp(rate*timeInterval*(-1.0));
			if(DEBUG){
				System.out.println("id:"+p.Id+" group:"+((Channel) wb).getChanGroup()+ " rate:"+ rate);
				System.out.println("channel:"+wb.getEnvIndex()+ " timeInterval:"+timeInterval*60d*60d*24d+"  survival probability:"+survivalProbability);
			}	
		}
		else
			survivalProbability = 1;
		if (survivalProbability<(new SecureRandom()).nextDouble()){
			p.isDead = true;	
			if(DEBUG) 
				System.out.println("channel:"+PTMHydroInput.getExtFromIntChan(((Channel) wb).getEnvIndex())
				+"  id:" + p.Id + "  timeInterval:"+timeInterval*24*60*60+"  survival probability:"+ survivalProbability+"  p.isDead:"+p.isDead);	
		}
		
	}
	
	//TODO this is fast exp calculation.  should it be used for other PTM calculations?
	/*
	private double exp(double val) {
        final long tmp = (long) (1512775 * val + 1072632447);
        return Double.longBitsToDouble(tmp << 32);
    }
    */

}
