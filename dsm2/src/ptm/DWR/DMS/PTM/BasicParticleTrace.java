/**
 * 
 */
package DWR.DMS.PTM;
import java.util.ArrayList;
/**
 * this class stores tracks for a particle
 * it also calculates if a particle passes a particular channel at the end. 
 * @author xwang
 *
 */
public class BasicParticleTrace {

	/**
	 * 
	 */
	public BasicParticleTrace() {
		_tracks = new ArrayList<Track>();
	}
	public void addTrack(long time, int wbId, int nodeId){_tracks.add(new Track(time, wbId, nodeId));}
	/*
	 * check if a particle passes a certain channel
	 * return 1 if passes
	 *        0 if never passes or eventually leaves
	 */
	public int particlePassed(ArrayList<Integer> fromWbIds, ArrayList<Integer> toWbIds){
		Track tr0 = _tracks.get(0);
		int preWbId = tr0.getWbId();
		long preTime = tr0.getTime();
		int count = 0;
		//Java foreach operator will reserve the list order, so no worries
		for(Track track: _tracks){
			int currWbId = track.getWbId();
			long currTime = track.getTime();
			boolean preInFromGroup = fromWbIds.contains(preWbId), preInToGroup = toWbIds.contains(preWbId);
			boolean currInFromGroup = fromWbIds.contains(currWbId), currInToGroup =	toWbIds.contains(currWbId);
			//Only the last entering or leaving matters
			if((currTime > preTime)){
				if(preInFromGroup&&currInToGroup)
					count = 1;
				else if(preInToGroup&&currInFromGroup)
					count = 0;
			
			}
			/*
			if(preInFromGroup&&currInToGroup)
				count++;
			else if(preInToGroup&&currInFromGroup)
				count--;
			*/			
			preWbId = currWbId;
			preTime = currTime;
		}
		/*
		 * a particle could circle around many times, therefore the count could 
		 * less than -1 (in case of leaving multiple times) or greater than +1 (in case of entering multiple times ).  
		 * only count once when the particle eventually passes the channel 
		 * or 0 when the particle eventually reentering to the from-channels.	 		
		 */
		//return (count>0?1:0);
		return count;
	}
	public int getSizeOfTracks(){return _tracks.size();}
	private ArrayList<Track> _tracks;
	private class Track{
		private long _time;
		private int _wbId, _nodeId;
		public Track(long time, int wbId, int nodeId){
			_time = time;
			_wbId = wbId;
			_nodeId = nodeId;
		}
		public void addTrack(long time, int wbId, int nodeId){
			_time = time;
			_wbId = wbId;
			_nodeId = nodeId;
		}
		public long getTime(){return _time;}
		public int getWbId(){return _wbId;}
		public int getNodeId(){return _nodeId;}
	}
}
