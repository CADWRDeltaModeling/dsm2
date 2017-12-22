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
	public Pair(String name, F first, S second){
		_name = name;
		_first = first;
		_second = second;
	}
	public void setFirst(F first){_first = first;}
	public void setSecond(S second){_second = second;}
	public void setName(String name){_name = name; }
	public F getFirst(){ return _first;}
	public S getSecond(){ return _second;}
	public String getName(){return _name;}
	private F _first = null;
	private S _second = null;
	private String _name = null;

}
