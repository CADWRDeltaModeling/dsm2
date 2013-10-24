/**
 * 
 */
package DWR.DMS.PTM;
import java.security.SecureRandom;

/**
 * @author xwang
 *
 */
public class SalmonBasicSurvivalBehavior implements SalmonSurvivalBehavior {
	//TODO Hard-wired temporarily, need to be read from input file 
	public static double _instSacMortalityRate = 0.048262; 
	public static double _instInteriorMortalityRate = 0.084402;
	public static double _instSJRMortalityRate = 0.0;
	public static double _instOtherMortalityRate = 0.0;
	private boolean DEBUG = false;

	/**
	 * 
	 */
	public SalmonBasicSurvivalBehavior() {
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SurvivalBehavior#isSurvived(DWR.DMS.PTM.Particle)
	 */
	@Override
	public void isSurvived(Particle p) {
		// age in days
		// p.age in seconds
		double age = p.age/(60d*60d*24d);
		if (age<0)
			PTMUtil.systemExit("Particle age is negative. Please fix it.  Exit from Particle line 30");
		double survivalProbability = 0;
		Waterbody aWB = p.wb;
		if (aWB.getType() == Waterbody.CHANNEL){
			switch (((Channel) aWB).getChanGroup()){
				case 1: survivalProbability = Math.exp(_instSacMortalityRate*age*(-1.0));
				if(DEBUG){
					System.out.println("in case Sac river");
					System.out.println("age:"+age+"  survival probability:"+survivalProbability);
				}			
						break;
				case 2: survivalProbability = Math.exp(_instInteriorMortalityRate*age*(-1.0));
				if(DEBUG){
					System.out.println("in case Interior");
					System.out.println("age:"+age+"  survival probability:"+survivalProbability);
				}
						break;					
				case 3: survivalProbability = Math.exp(_instSJRMortalityRate*age*(-1.0));
				if(DEBUG){
					System.out.println("in case SJR");
					System.out.println("age:"+age+"  survival probability:"+survivalProbability);
				}
						break;
				//TODO hardwired need to be changed
				case 8: survivalProbability = Math.exp(_instOtherMortalityRate*age*(-1.0));
				if(DEBUG){
					System.out.println("in case other");
					System.out.println("age:"+age+"  survival probability:"+survivalProbability);
				}
			}
		}
		else
			survivalProbability = 1;
		if (survivalProbability<(new SecureRandom()).nextDouble()){
			p.isDead = true;	
			if(DEBUG) 
				System.out.println("channel:"+PTMHydroInput.getExtFromIntChan(((Channel) aWB).getEnvIndex())
				+"  id:" + p.Id + "  age:"+age*24*60*60+"  survival probability:"+ survivalProbability+"  p.isDead:"+p.isDead);	
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
