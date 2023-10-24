package DWR.DMS.PTM;
import java.util.Calendar;

public class PTMPeriod {
	public PTMPeriod(Calendar startTime, Calendar endTime){
		_startTime = startTime;
		_endTime = endTime;
	}
	public PTMPeriod(Calendar currentTime){
		_startTime = currentTime;
		_endTime = currentTime;
	}

	public boolean contains(Calendar time){
		return ((time.equals(_startTime)||time.after(_startTime)) && (time.before(_endTime)));
	}

	public Calendar getStartTime(){
		return _startTime;
	}
	public Calendar getEndTime(){
		return _endTime;
	}

	private Calendar _startTime, _endTime;
}
