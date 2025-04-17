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
import java.util.TimeZone;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;

/**
 * @author xwang
 *
 */
public class TravelTimeOutput {

	private Config config;
	
	/**
	 * Output travel time to a .csv file
	 */
	public TravelTimeOutput() {
		config = PTMFixedData.getConfig();
		
		if(config.travel_time_output_path==null || config.travel_time_header==null || config.travel_time==null) {
			System.out.println("No travel time output info defined in behavior input file");
		}
		else{
			_pathName = config.travel_time_output_path;
			if(_pathName==null || _pathName.equalsIgnoreCase("")){
				_pathName = null;
				System.err.println("not output travel times!");
				return;
			}
			String shouldBe[] = {"NODEID", "CHANNELID/RESERVOIRNAME/OBJ2OBJNAME", "DISTANCE", "STATION_NAME"};
			PTMUtil.checkTitle(config.travel_time_header, shouldBe);
			setIdsDistance();
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
	private void setIdsDistance(){
		//_outputStations = new ArrayList<IntBuffer>();
		_stationNames = new ConcurrentHashMap<Integer, String>();
		_ttHolder = new ConcurrentHashMap<String, Map<Integer, TTEntry>>();
		_staDist = new ConcurrentHashMap<Integer, Float>();
		List<Object> thisStationDef;
		
		//TODO clean up later, sublisted in the calling function
		for(int i=0; i<config.travel_time.size(); i++) {
			thisStationDef = config.travel_time.get(i);
			
			int[] station = new int[2];
			float d = 0.0f;
			if (thisStationDef.size()<4)
				PTMUtil.systemExit("SYSTEM EXIT: expect at least 4 items in travel time output line in behavior input file. ");
			if (thisStationDef.get(3)== null)
				PTMUtil.systemExit("expect a travel time output station name, but found none, system exit. ");
			try{
				// nodeId
				station[0] = PTMHydroInput.getIntFromExtNode((int) thisStationDef.get(0));
			}catch(NumberFormatException e){
					e.printStackTrace();
					PTMUtil.systemExit("node id:" + thisStationDef.get(0) + " in the travel time output line is wrong, please check");
			}
			try{
				// wbId
				station[1] = PTMHydroInput.getIntFromExtChan((int) thisStationDef.get(1));
			}catch(NumberFormatException e){
				if (PTMEnv.getReservoirObj2ObjEnvId((String) thisStationDef.get(1)) == null){
					PTMUtil.systemExit("channel/reservior/obj2obj id:" + thisStationDef.get(1) + " in the travel time output line is wrong, please check");
				}
				else
					station[1] = PTMEnv.getReservoirObj2ObjEnvId((String) thisStationDef.get(1));
			}
			try{
				//distance				
				d = ((Number) thisStationDef.get(2)).floatValue();
			}catch(NumberFormatException e){
				if (((String) thisStationDef.get(2)).equalsIgnoreCase("LENGTH"))
					d = getLength(station[1]);
				else{
					PTMUtil.systemExit("distance input:" + thisStationDef.get(2) +" for channel:" + thisStationDef.get(0) 
					+ " and node:" + thisStationDef.get(1) + " in travel time output line is wrong, please check." );
				}
			}
			IntBuffer ndWb = IntBuffer.wrap(station);
			//_outputStations.add(ndWb);
			_stationNames.put(station[1], (String) thisStationDef.get(3));
			_staDist.put(station[1], Float.valueOf(d));
			_ttHolder.put((String) thisStationDef.get(3), new ConcurrentHashMap<Integer, TTEntry>());
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
		if (_pathName == null)
			return;
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
						//creating DateFormat for converting time from local machine time zone to specified time zone
						DateFormat converter = new SimpleDateFormat("MM/dd/yyyy  HH:mm:ss");
						converter.setTimeZone(Globals.TIME_ZONE);
						String dateStr = converter.format(PTMUtil.modelTimeToCalendar(tt4P.getInsertTime(),Globals.TIME_ZONE).getTime());
						ttWriter.write(Integer.toString(pId).concat(",").
									concat(tt4P.getInsertStationName()).concat(",").concat(dateStr).concat(",").
									concat(stationName).concat(",").concat(Integer.toString((int)tt4P.getTravelTime())));
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
	public void recordTravelTime(int id, String inStation, long inTime, double ageInSec, IntBuffer ndWb, float velocity, float x, boolean fromUpstream){
		//only record when need to output TravelTime
		if (_pathName == null)
			return;


		/* after discussed with Adam Pope and Russell Perry of USGS, we decided that we lift the restrict that we only record travel times for particles from upstream
		// only record particles from upstream
		if (fromUpstream){
			String staName = _stationNames.get(ndWb);

			 //if staName == null, the particle is not at the recording location
			 //_recorderTest != null, the particle has been recorded, don't record again
			 //a particle can hit multiple receiving stations.
			 //the station with the shortest travel time is the one that the particle first hits.
			 //do not record travel time after the first hit

			if ((staName != null) && (_recorderTest.get(id) == null)){
				float dist = _staDist.get(ndWb);
				float distDiff = Math.abs(x-dist);
				// p.x is always > = 0
				// if p.x >= dist record the travel time
				if ((!(x<dist)) || (distDiff <_threshold)){
					double tt = ageInSec;
					if (distDiff>_threshold){
						// p.x always > 0 and velocity > 0 because !(p.x<dist) and deltaX >0
						if (velocity < 0.0001f)
							System.err.println("warning: particle# "+id+" has very low advection and swimming velocities:"
							+ velocity+", could cause an error at travel time calculation");
						tt -= (x-dist)/velocity;
					}
					_ttHolder.get(staName).put(id, new TTEntry(inStation, inTime, tt/60.0d));
					_recorderTest.put(id, true);
				}
			}
		}
	*/

		String staName = _stationNames.get(ndWb.get(1));
		/*
		if staName == null, the particle is not at the recording location
		 _recorderTest != null, the particle has been recorded, don't record again
		 a particle can hit multiple receiving stations.
		 the station with the shortest travel time is the one that the particle first hits.
		 do not record travel time after the first hit
		  * */
		if ((staName != null) && (_recorderTest.get(id) == null)){
			//if (velocity == Float.MAX_VALUE)
				//System.err.println(id + " " + staName+ " "+ ndWb.get(1)+"  "+x+"  "+velocity+"  " + fromUpstream + "  " + _staDist);
			float dist = _staDist.get(ndWb.get(1));
			int sign = fromUpstream? 1: -1;
			dist = sign*dist;
			x = sign*x;
			if (!(x < dist)) {
				double tt = ageInSec;
				if (Math.abs(velocity) < 0.0001f)
					System.err.println("warning: particle# "+id+" has very low advection and swimming velocities:"
						+ velocity+", could cause an error at travel time calculation");
				// when x<0 and dist<0, velocity has to be negative.  However, it could have a round up error so take abs to be safe
				tt -= Math.abs(x-dist)/Math.abs(velocity);
				if (tt < 0)
				//if (tt < 0 || (x<0 && dist<0 && velocity > 0) || (x>0 && dist>0 && velocity < 0))
					//System.err.println("The travel time is negative. pId:"+id+"  velocity:"+velocity+" x:"+x+"  dist:"+dist+"  node/chan:"+ Arrays.toString(ndWb.array())+"  tt:"+tt+"  System exit.");
					PTMUtil.systemExit("The travel time is negative. velocity:"+velocity+" x:"+x+"  dist:"+dist+"  node/chan:"+ Arrays.toString(ndWb.array())+"  tt:"+tt+"  System exit.");
				_ttHolder.get(staName).put(id, new TTEntry(inStation, inTime, tt/60d));
				_recorderTest.put(id, true);
			}
		}
	}
	public void setThreshold(float t){_threshold = t;}
	public float getThreshold(){ return _threshold;}
	// list of detection stations
	//private ArrayList<IntBuffer> _outputStations;
	// map of channel number and detection station name
	private Map<Integer, String> _stationNames = null;
	// detection station, {particleId, TTEntry}
	private Map<String, Map<Integer, TTEntry>> _ttHolder;
	// map of channel number and distance
	private Map<Integer, Float> _staDist;
	// test if travel time has been recorded Map<pId, hasRecorded>
	private Map<Integer, Boolean> _recorderTest;
	private String _pathName;
	private float _threshold = 0.000001f;

	//TODO assume that there is only one travel time per particle
	private class TTEntry{
		private String _inSta;
		private long _inTime;
		private double _travelTime;
		public TTEntry(String inSta, long inTime, double travelTime){
			_inSta = inSta;
			_inTime = inTime;
			_travelTime = travelTime;
		}
		public String getInsertStationName(){return _inSta;}
		public long getInsertTime(){
			return _inTime;
		}
		public double getTravelTime() {return _travelTime;}
	}
}
