/**
 * 
 */
package DWR.DMS.PTM;
import java.util.Calendar;

/**
 * @author xwang
 *
 */
public class FishRelease {
	public static final int CENTER = 1;
	public static final int RANDOM = 0;
	/**
	 * 
	 */
	public FishRelease(Calendar releaseTime, int fishNumber, int releaseStyle) {
		_releaseTime = releaseTime;
		_fishNumber = fishNumber;
		_releaseStyle = releaseStyle;
	}
	
	private Calendar _releaseTime;
	private int _fishNumber;
	private int _releaseStyle;
	public Calendar getReleaseTime(){return _releaseTime;}
	public int getFishNumber(){return _fishNumber;}
	public int getReleaseStyle(){return _releaseStyle;}
}
