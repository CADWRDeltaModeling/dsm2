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
        if (it == null || !it.hasNext())
        	PTMUtil.systemExit("the barrier node Id:"+_nodeId+", water body Id:"+_waterbodyId+" has no operation schedule found, exit.");
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
}
