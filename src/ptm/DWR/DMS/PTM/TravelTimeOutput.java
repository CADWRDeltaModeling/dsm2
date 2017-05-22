/**
 * 
 */
package DWR.DMS.PTM;
import java.io.BufferedWriter;
import java.io.IOException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.Map;
import java.util.ArrayList;
import java.nio.IntBuffer;
import java.util.List;

/**
 * @author xwang
 *
 */
public class TravelTimeOutput {

	/**
	 * Output travel time to a .csv file 
	 */
	public TravelTimeOutput(ArrayList<String> outText) { // outText:  the line specify travel time output in the behavior input file
		if (outText == null)
			System.err.println("Warning: travel time output info is not defined in behavior input file!");
		else{
			_pathName = PTMUtil.getPathFromLine(outText.get(0), ':');
			String shouldBe[] = {"NODEID", "CHANNELID/RESERVOIRNAME/OBJ2OBJNAME", "DISTANCE", "STATION_NAME"};
			PTMUtil.checkTitle(outText.get(1), shouldBe);
			if (outText.size()<3 )
				PTMUtil.systemExit("travel time output info is not entered correctly, please revise them in behavior input file");
			setIdsDistance(outText.subList(2, outText.size()));
		}
		_recorderTest = new ConcurrentHashMap<Integer, Boolean>();
	}

	private float getLength(int chanId){
		try{
			return (((Channel)Globals.Environment.getWaterbody(chanId)).getLength());
		}catch(Exception notWb){
			notWb.printStackTrace();
			PTMUtil.systemExit("SYSTEM EXIT:wrong channel/reservior/obj2obj id:" + chanId);
		}
		return -999999f;
	}
	private void setIdsDistance(List<String> stationText){
		_outputStations = new ArrayList<IntBuffer>();
		_stationNames = new ConcurrentHashMap<IntBuffer, String>();
		_ttHolder = new ConcurrentHashMap<String, Map<Integer, TTEntry>>();
		_staDist = new ConcurrentHashMap<IntBuffer, Float>();
		//TODO clean up later, sublisted in the calling function
		//for (String stationLine: stationText.subList(1, stationText.size())){
		for (String stationLine: stationText){
			int[] station = new int[2];
			float d = 0.0f;
			String[] items = stationLine.trim().split("[,\\s\\t]+");
			if (items.length<4)
				PTMUtil.systemExit("SYSTEM EXIT: expect at least 4 items in travel time output line in behavior input file. ");
			if (items[3] == null)
				PTMUtil.systemExit("expect a travel time output station name, but found none, system exit. ");
			try{
				// nodeId
				station[0] = PTMHydroInput.getIntFromExtNode(Integer.parseInt(items[0]));
			}catch(NumberFormatException e){
					e.printStackTrace();
					PTMUtil.systemExit("node id:" + items[0]+ " in the travel time output line is wrong, please check");
			}
			try{
				// wbId
				station[1] = PTMHydroInput.getIntFromExtChan(Integer.parseInt(items[1]));
			}catch(NumberFormatException e){
				if (PTMEnv.getReservoirObj2ObjEnvId(items[1]) == null){
					PTMUtil.systemExit("channel/reservior/obj2obj id:" + items[1] + " in the travel time output line is wrong, please check");
				}
				else
					station[1] = PTMEnv.getReservoirObj2ObjEnvId(items[1]);
			}
			try{
				//distance
				d = Float.parseFloat(items[2]);
			}catch(NumberFormatException e){
				if (items[2].equalsIgnoreCase("LENGTH"))
					d = getLength(station[1]);
				else{
					PTMUtil.systemExit("distance input:" + items[2] +" for channel:" + items[0] + " and node:"+items[1] + " in travel time output line is wrong, please check." );
				}
			}
			IntBuffer ndWb = IntBuffer.wrap(station);
			_outputStations.add(ndWb);
			_stationNames.put(ndWb, items[3]);
			_staDist.put(ndWb, new Float(d));
			_ttHolder.put(items[3], new ConcurrentHashMap<Integer, TTEntry>());
		}
	}
	
	public void travelTimeOutput(){
		//TODO clean up _ttHolder will never be zero see line above line 91
		/*
		if (_ttHolder == null){
			System.err.println("warning: entire travel time map is empty. no travel time output!");
			return;
		}
		*/
		try{
			//TODO clean up later
			//BufferedWriter ttWriter = PTMUtil.getOutputBuffer("output/travel_time_in_min.csv");
			BufferedWriter ttWriter = PTMUtil.getOutputBuffer(_pathName);
			ttWriter.write("PID".concat(",").concat("Release_Sta").concat(",").concat("Release_Time").concat(",").concat("Detect_Sta").concat(",").concat("Travel_Time(Min)"));
			ttWriter.newLine();
			//_ttHolder will never be null and there is at least one element
			for (String stationName: _ttHolder.keySet()){
				Map<Integer, TTEntry> travelTimePerStation = _ttHolder.get(stationName);
				for (int pId: travelTimePerStation.keySet()){
					TTEntry tt4P = travelTimePerStation.get(pId);
					//travelTimePerStation will never be null
					if (tt4P != null){ 
						ttWriter.write(Integer.toString(pId).concat(",").
									concat(tt4P.getInsertStationName()).concat(",").concat(PTMUtil.modelTimeToCalendar(tt4P.getInsertTime()).getTime().toString()).
									concat(",").concat(stationName).concat(",").concat(Integer.toString((int)tt4P.getTravelTime())));
						ttWriter.newLine();	
					}
				}
				
			}
			PTMUtil.closeBuffer(ttWriter);
		}catch(IOException e){
			System.err.println("error occured when writing out travel times!");
			e.printStackTrace();
		}
	}
	public void recordTravelTime(int id, String inStation, long inTime, float ageInSec, IntBuffer ndWb, float velocity, float x, float deltaX){
		// only record particles from upstream
		if (deltaX >0){
			String staName = _stationNames.get(ndWb);
			/*
			 if staName == null, the particle is not at the recording location
			 _recorderTest != null, the particle has been recorded, don't record again
			 a particle can hit multiple receiving stations.  
			 the station with the shortest travel time is the one that the particle first hits.
			 do not record travel time after the first hit
			 * */
			if ((staName != null) && (_recorderTest.get(id) == null)){
				float dist = _staDist.get(ndWb);
				float distDiff = Math.abs(x-dist);
				// p.x is always > = 0
				// if p.x >= dist record the travel time
				if ((!(x<dist)) || (distDiff <_threshold)){
					float tt = ageInSec/60.0f;
					if (distDiff>_threshold)
						// p.x always > 0 and velocity > 0 because !(p.x<dist) and deltaX >0
						tt -= (x-dist)/velocity;
					_ttHolder.get(staName).put(id, new TTEntry(inStation, inTime, tt));
					_recorderTest.put(id, true);
				}
			}
		}
	}
	public void setThreshold(float t){_threshold = t;}
	public float getThreshold(){ return _threshold;}
	// list of detection stations
	private ArrayList<IntBuffer> _outputStations;
	// map of IntBuffer<nodeId, wbId> and detection station name
	private Map<IntBuffer, String> _stationNames = null;
	// detection station, {particleId, TTEntry} 
	private Map<String, Map<Integer, TTEntry>> _ttHolder;
	// map of IntBuffer<nodeId, wbId> and distance
	private Map<IntBuffer, Float> _staDist;
	// test if travel time has been recorded Map<pId, hasRecorded>
	private Map<Integer, Boolean> _recorderTest;
	private String _pathName;
	private float _threshold = 0.000001f;
	
	//TODO assume that there is only one travel time per particle
	private class TTEntry{
		private String _inSta;
		private long _inTime;
		private float _travelTime;
		public TTEntry(String inSta, long inTime, float travelTime){
			_inSta = inSta;
			_inTime = inTime;
			_travelTime = travelTime;
		}
		public String getInsertStationName(){return _inSta;}
		public long getInsertTime(){
			return _inTime;
		}
		public float getTravelTime() {return _travelTime;}
	}
}
