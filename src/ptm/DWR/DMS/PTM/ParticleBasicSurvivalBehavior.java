/**
 * 
 */
package DWR.DMS.PTM;

/**
 * @author xwang
 *
 */
public class ParticleBasicSurvivalBehavior implements SurvivalBehavior {

	/**
	 * 
	 */
	public ParticleBasicSurvivalBehavior() {
		// TODO Auto-generated constructor stub
		// currently doing nothing
	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SurvivalBehavior#isSurvived(DWR.DMS.PTM.Particle, float)
	 */
	@Override
	public void isSurvived(Particle p, float x, float t) {
		// TODO Auto-generated method stub
		// currently doing nothing

	}
	public float getXofXTSurvival(Channel ch, Node nd, float x, float maxDist){
		PTMUtil.systemExit("Why am I here? in ParticleBasicSurvivalBehavior.java");
		return 0.0f;
				 
	}

}
