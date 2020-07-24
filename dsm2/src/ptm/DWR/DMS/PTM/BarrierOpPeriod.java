package DWR.DMS.PTM;
import java.util.Calendar;

public class BarrierOpPeriod {
	public BarrierOpPeriod(Calendar startTime, Calendar endTime){
		_startTime = startTime;
		_endTime = endTime;
	}
	public BarrierOpPeriod(Calendar currentTime){
		_startTime = currentTime;
		_endTime = currentTime;
	}
	
	public boolean contains(Calendar time){
		if ((time.equals(_startTime)||time.after(_startTime)) && (time.before(_endTime))) 
				return true;
		return false;
	}
	
	public Calendar getStartTime(){
		return _startTime;
	}
	public Calendar getEndTime(){
		return _endTime;
	}
	
	private Calendar _startTime, _endTime;
}
