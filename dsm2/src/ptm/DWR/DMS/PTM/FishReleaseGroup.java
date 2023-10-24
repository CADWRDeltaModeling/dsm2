/**
 *
 */
package DWR.DMS.PTM;
import java.util.ArrayList;
import java.nio.IntBuffer;

/**
 * @author xwang
 *
 */
public class FishReleaseGroup {

	/**
	 *
	 */
	public FishReleaseGroup() {
		// TODO Auto-generated constructor stub
	}
	public FishReleaseGroup(IntBuffer station, String stationName, ArrayList<FishRelease> releases) {
		_station = station;
		_releases = releases;
		_stationName = stationName;
	}
	public void addFishRelease(FishRelease fr){
		_releases.add(fr);
	}
	private IntBuffer _station;
	private String _stationName;
	private ArrayList<FishRelease> _releases;
	public IntBuffer getStation(){return _station;}
	public String getStationName(){return _stationName;}
	public ArrayList<FishRelease> getFishReleases(){return _releases;}
}
