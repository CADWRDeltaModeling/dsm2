/**
 * 
 */
package DWR.DMS.PTM;

/**
 * @author xwang
 *
 */
public class Pair <F, S> {

	/**
	 * 
	 */
	public Pair() {
		// TODO Auto-generated constructor stub
	}
	public Pair(F first, S second){
		_first = first;
		_second = second;
	}
	public void setFirst(F first){_first = first;}
	public void setSecond(S second){_second = second;}
	public F getFirst(){ return _first;}
	public S getSecond(){ return _second;}
	private F _first = null;
	private S _second = null;

}
