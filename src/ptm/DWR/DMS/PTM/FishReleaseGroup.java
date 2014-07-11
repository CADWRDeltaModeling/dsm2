/**
 * 
 */
package DWR.DMS.PTM;
import java.util.ArrayList;

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
	public FishReleaseGroup(int nodeId, ArrayList<FishRelease> releases) {
		_ndId = nodeId;
		_releases = releases;
	}
	public void addFishRelease(FishRelease fr){
		_releases.add(fr);
	}
	private int _ndId;
	private ArrayList<FishRelease> _releases;
	public int getNodeId(){return _ndId;}
	public ArrayList<FishRelease> getFishReleases(){return _releases;}
}
