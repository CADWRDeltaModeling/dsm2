/**
 * 
 */
package DWR.DMS.PTM;

/**
 * @author xwang
 *
 */
public class Location {
	int _nodeID = -99;
	int _wbID = -99;
	int _distance = 0;

	/**
	 * 
	 */
	public Location() {
		// TODO Auto-generated constructor stub
	}
	public Location(int nodeID, int wbID, int distance) {
		_nodeID = nodeID;
		_wbID = wbID;
		_distance = distance;
	}
	public int getNodeID(){
		return _nodeID;
	}
	public void setNodeID(int nodeID){
		_nodeID = nodeID;
	}
	public int getWbID(){
		return _wbID;
	}
	public void setWbID(int wbID){
		_wbID = wbID;
	}
	public int getDistance(){
		return _distance;
	}
	public void set(int distance){
		_distance = distance;
	}
}
