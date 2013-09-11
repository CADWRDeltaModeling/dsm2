package DWR.DMS.PTM;
import java.util.Map;
import java.util.Iterator;
import java.util.Calendar;

public class NonPhysicalBarrier {
	// ids are internal env indices
	public NonPhysicalBarrier(int nodeId, int waterbodyId, Map<BarrierOpPeriod, Integer> barrierOpTs){
		_nodeId = nodeId;
		_waterbodyId = waterbodyId;
		_barrierOpTs = barrierOpTs;	
	} 
	public NonPhysicalBarrier(){
		this(-1, -1, null);
	}
	public NonPhysicalBarrier(int nodeId, int waterbodyId){
		_nodeId = nodeId;
		_waterbodyId = waterbodyId;
		_barrierOpTs = null;	
	}
	public void setBarrier(int nodeId, int waterbodyId, Map<BarrierOpPeriod, Integer> barrierOpTs){
		_nodeId = nodeId;
		_waterbodyId = waterbodyId;
		_barrierOpTs = barrierOpTs;	
	}
	public void setLocations(int nodeId, int waterbodyId){
		_nodeId = nodeId;
		_waterbodyId = waterbodyId;
	}
	public void setBarrierOpTs(Map<BarrierOpPeriod, Integer> barrierOpTs){
		_barrierOpTs = barrierOpTs;	
	}
	public int getNodeId(){
		return _nodeId;
	}
	public int getWaterbodyId(){
		return _waterbodyId;
	}
	public int getBarrierOp(Calendar currentTime){
		BarrierOpPeriod key;
        Iterator<BarrierOpPeriod> it = _barrierOpTs.keySet().iterator();
        boolean found = false;
        do{
            key = (BarrierOpPeriod) it.next();
        }while (!(found = key.contains(currentTime))&&it.hasNext());
        if (found){
        	return _barrierOpTs.get(key);
        }
        else{
        	System.err.println("model time:"+currentTime.getTime()
        			+", cannot find a match in the barrier operation timeseries.");
        	return -1;
        }
	}
	protected Map<BarrierOpPeriod, Integer> getBarrierOpTs(){
		return _barrierOpTs;
	}
	//TODO clean up
	/* no need to have these method
	protected boolean setCurrentOperation(Calendar currentTime){
		_currentOp = getBarrierOp(currentTime);
		//System.out.println("currentOp:"+_currentOp);
		if (_currentOp == -1)
				return false;
		else
			return true;
	}
	protected int getCurrentOperation(){
		return _currentOp;
	}
	*/
	protected void InstallBarrier(){
		_installed = true;
	}
	protected boolean isInstalled(){
		return _installed;
	}
	private int _nodeId;
	private int _waterbodyId;
	private Map<BarrierOpPeriod, Integer> _barrierOpTs;
	private boolean _installed = false;
	//private int _currentOp = 0;
	//private int _barrierOn = 0;
}
