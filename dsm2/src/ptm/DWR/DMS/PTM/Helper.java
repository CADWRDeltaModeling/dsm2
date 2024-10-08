/**
 *
 */
package DWR.DMS.PTM;
import java.util.Map;
import java.util.HashMap;

/**
 * @author xwang
 *
 */
public abstract class Helper<K,B> {
	// Map key could be String for particle type, e.g., "SALMON", "SMELT" or integer, e.g., for Node EnvID, or anything else
	private Map<K,B> _specialBehaviors;
	private B _basic;

	/**
	 *
	 */
	public Helper(B basic, Map<K,B> specialBehaviors) {
		_basic = basic;
		_specialBehaviors = specialBehaviors;
	}
	public Helper(B basic) {
		_basic = basic;
		_specialBehaviors = new HashMap<K, B>();
	}
	public Helper() {
		_basic = null;
		_specialBehaviors = new HashMap<K, B>();
	}
	public B lookUp(K key){
		return _specialBehaviors.get(key);
	}
	public void setSpeicalBehaviors(Map<K, B> specialBehaviors){
		_specialBehaviors = specialBehaviors;
	}
	public Map<K,B> getSpeicalBehaviors(){
		return _specialBehaviors;
	}
	public void setBasicBehavior(B basic){
		_basic = basic;
	}
	public B getBasicBehavior(){
		return _basic;
	}
	public void addSpecialBehavior(K key, B b){
		if (_specialBehaviors == null)
				_specialBehaviors = new HashMap<K,B>();
		_specialBehaviors.put(key, b);
	}
	public void removeSpecialBehavior(K key){
		_specialBehaviors.remove(key);
	}
	/*
	public void setParticleType(String particleType){
		_particleType = particleType;
	}
	public String getParticleType(){
		return _particleType;
	}
	*/
	public B getBehavior(Particle p){
		K key = getKey(p);

		if (key == null)
			PTMUtil.systemExit("cannot find search key for a behavior, the method is mis-used, check the code!");

		// get a special route selection method otherwise use the basic
		B specialB = lookUp(key);
		if (specialB != null)
			return specialB;
		else if (_basic != null)
			return _basic;
		else{
			PTMUtil.systemExit("need to initilize Helper!");
			return null;
		}
	}

	/*
	 * method call to get a key (or a junction indicator)
	 * the indicator will be used to locate a route selection method
	 */
	public abstract K getKey(Particle p);
	//public abstract void help(Particle p);

}
