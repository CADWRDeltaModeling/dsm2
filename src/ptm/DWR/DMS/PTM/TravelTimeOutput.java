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
			String shouldBe[] = {"NODEID", "CHANNELID/RESERVOIRNAME/OBJ2OBJNAME", "DISTANCE", "STATION_NAME"};
			PTMUtil.checkTitle(outText.get(0), shouldBe);
			if (outText.size()<2 )
				PTMUtil.systemExit("travel time output stations are not entered correctly, please revise them in behavior input file");
			setIdsDistance(outText);
		}
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
	private void setIdsDistance(ArrayList<String> stationText){
		_outputStations = new ArrayList<IntBuffer>();
		_stationNames = new ConcurrentHashMap<IntBuffer, String>();
		_ttHolder = new ConcurrentHashMap<String, Map<Integer, TTEntry>>();
		_staDist = new ConcurrentHashMap<IntBuffer, Float>();
		for (String stationLine: stationText.subList(1, stationText.size())){
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
		if (_ttHolder == null){
			System.err.println("warning: entire travel time map is empty. no travel time output!");
			return;
		}
		try{
			BufferedWriter ttWriter = PTMUtil.getOutputBuffer("output/travel_time_in_min.csv");
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
	/*
	public boolean needToRecord(Particle p, float x, float deltaX){
		// only record particles from upstream
		if (deltaX >0){
			IntBuffer wbNd = IntBuffer.wrap(new int[] {p.nd.getEnvIndex(), p.wb.getEnvIndex()});
			String staName = _stationNames.get(wbNd);
			// if staName != null, the particle is not at the recording location
			// _ttHolder.get(staName).get(new Integer(p.Id)) != null, the particle has been recorded, don't record again
			if ((staName != null) && (_ttHolder.get(staName).get(new Integer(p.Id)) == null)){
				float dist = _staDist.get(wbNd);
				// it covers x<0 because dist is always > = 0
				// if x >= dist record the travel time
				if (!(x<dist))
					return true;
			}
		}
		return false;
	}
	public void recordTravelTime(Particle p, float x, float deltaX){
		IntBuffer wbNd = IntBuffer.wrap(new int[] {p.nd.getEnvIndex(), p.wb.getEnvIndex()});
		String staName = _stationNames.get(wbNd);
		_ttHolder.get(staName).put(p.Id, new TTEntry(p.getInsertionStation(), p.getInsertionTime(), p.age));
	}
	*/
	public void recordTravelTime(Particle p, IntBuffer ndWb, float x, float deltaX){
		// only record particles from upstream
		if (deltaX >0){
			String staName = _stationNames.get(ndWb);
			// if staName == null, the particle is not at the recording location
			// _ttHolder.get(staName).get(new Integer(p.Id)) != null, the particle has been recorded, don't record again
			if ((staName != null) && (_ttHolder.get(staName).get(p.Id) == null)){
				float dist = _staDist.get(ndWb);
				// it covers x<0 because dist is always > = 0
				// if x >= dist record the travel time
				if (!(x<dist))
					_ttHolder.get(staName).put(p.Id, new TTEntry(p.getInsertionStation(), p.getInsertionTime(), p.age/60.0f));
			}
		}
	}
	
	//TODO clean up no need for this anymore
	//IntBuffer:0:nodeId, 1:wbId, 2:distance; 
	//Map<detection station, Map<release station name, Map<release time, array[pid, travel time]>>>
	//private Map<IntBuffer, Map<String, Map<Calendar, ArrayList<Pair<Integer, Float>>>>> _travelTimeOutput;
	
	// list of detection stations
	private ArrayList<IntBuffer> _outputStations;
	// map of IntBuffer<nodeId, wbId> and detection station name
	private Map<IntBuffer, String> _stationNames = null;
	// detection station, {particleId, TTEntry} 
	private Map<String, Map<Integer, TTEntry>> _ttHolder;
	// map of IntBuffer<nodeId, wbId> and distance
	private Map<IntBuffer, Float> _staDist;

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

	// test if travel time has been recorded Map<pId, hasRecorded>
	//private Map<Integer, Boolean> _recorderTest;
	//TODO clean up no one uses it.
	//public Map<IntBuffer, String> getStationNames(){return _stationNames;}

	//TODO clean up not used anymore
	//public void setOutputNodeId(int nodeId){_outputNodeId = nodeId;}
	//public void setOutputWbId(int wbId){_outputWbId = wbId;}
	//public void setOutputChannelDistance(float distance){_distanceFromUpNode = distance;}
	/*
	public void setTravelTime(int nodeId, int wbId, int distance, String releaseStationName, Calendar releaseTime, Integer pid, Float travelTimeInMin){ 
		//station: detect station where travel time needs to be output
		IntBuffer station = IntBuffer.wrap(new int[]{nodeId, wbId, distance});
		if (_travelTimeOutput == null)
				_travelTimeOutput = new ConcurrentHashMap<IntBuffer, Map<String, Map<Calendar, ArrayList<Pair<Integer, Float>>>>>();
		Map<String, Map<Calendar, ArrayList <Pair<Integer, Float>>>> travelTimesPerStation = _travelTimeOutput.get(station);
		if (travelTimesPerStation == null){
			travelTimesPerStation = new ConcurrentHashMap<String, Map<Calendar, ArrayList<Pair<Integer, Float>>>>();
			_travelTimeOutput.put(station, travelTimesPerStation);
		}
		Map<Calendar, ArrayList <Pair<Integer, Float>>> travelTimesPerNode = travelTimesPerStation.get(releaseStationName);
		if (travelTimesPerNode == null){
			travelTimesPerNode = new ConcurrentHashMap<Calendar, ArrayList<Pair<Integer, Float>>>();
			travelTimesPerStation.put(releaseStationName, travelTimesPerNode);
		}
		ArrayList <Pair<Integer, Float>> travelTimesPerRelease = travelTimesPerNode.get(releaseTime);
		if (travelTimesPerRelease == null){
			travelTimesPerRelease = new ArrayList <Pair<Integer, Float>>();
			travelTimesPerNode.put(releaseTime, travelTimesPerRelease);
		}
		Pair<Integer, Float> travelTime = new Pair<Integer, Float>(pid, travelTimeInMin); 
		travelTimesPerRelease.add(travelTime);
	}
	*/
	//TODO Clean up not used anymore
	/*
	public void setOutputNodeInfo(Node[] allNodes){
	    //nodeArray starts from 1 PTMFixedInput.java line 287
		if (_outputStations == null)
			System.err.println("WARNING: no output station info avaiable while setting up node info");
		else
			for (IntBuffer station: _outputStations)
				allNodes[station.get(0)].setOutputNode();
	}
	*/
	//TODO Clean up not used anymore
	/*
	public void setOutputWbInfo(Waterbody[] allWbs){
		//wbArray start from 1 see PTMFixedInput.java line 180
		//Channels are first filled in wbArray
		if (_outputStations == null)
			System.err.println("WARNING: no water body output station info avaiable while setting up water body info");
		else{
			for (IntBuffer station: _outputStations){
				Waterbody w = allWbs[station.get(1)];
				w.setOutputWb();
				// only channels need to set distance
				if (w.getPTMType() == Waterbody.CHANNEL){
					Channel c = (Channel) w;
					if (!c.getUpNode().isOutputNode())
						PTMUtil.systemExit("the node specified in the input file for the output channel:"+
									PTMHydroInput.getExtFromIntChan(station.get(1))+" is not an upstream node. Please check.");
					else{
						int d = station.get(2);
						if (d == -999999)
							c.setOutputDistance((int) c.getLength());
						else
							c.setOutputDistance(d);
					}
				}
			}
		}
	}
	
	
	public void travelTimeOutput(){
		if (_travelTimeOutput == null){
			System.err.println("warning: entire travel time map is empty. no travel time output!");
			return;
		}
		try{
			BufferedWriter ttWriter = PTMUtil.getOutputBuffer("output/travel_time_in_min.csv");
			ttWriter.write("PID".concat(",").concat("Release_Sta").concat(",").concat("Release_Time").concat(",").concat("Detect_Sta").concat(",").concat("Travel_Time(Min)"));
			ttWriter.newLine();
			for (IntBuffer station: _travelTimeOutput.keySet()){
				int[] wbNd = {station.get(0), station.get(1)};
				String stationName = _stationNames.get(IntBuffer.wrap(wbNd));
				if (stationName == null)
					PTMUtil.systemExit("try to output travel time, but no station name found, node Id:"
							+ PTMHydroInput.getExtFromIntNode(station.get(0)));
				Map<String, Map<Calendar, ArrayList<Pair<Integer, Float>>>> travelTimePerStation = _travelTimeOutput.get(station);
				if (travelTimePerStation == null)
					System.err.println("warning: no travel time recorded for station:" + stationName);
				else{
					for (String s: travelTimePerStation.keySet()){
						Map<Calendar, ArrayList<Pair<Integer, Float>>> travelTimePerNode = travelTimePerStation.get(s);
						if (travelTimePerNode == null){
							PTMUtil.systemExit("try to write travel time to a file but no record found with the release station:" 
									+ s +" , system exit.");
						}
						for (Calendar c: travelTimePerNode.keySet()){
							ArrayList<Pair<Integer, Float>> tts = travelTimePerNode.get(c);
							if (tts == null)
								PTMUtil.systemExit("try to write travel time to a file but no record found with the release station:" 
										+ s +" and time:" +c.getTime()+", system exit.");
							for (Pair<Integer, Float> tt: tts){
								ttWriter.write(tt.getFirst().toString().concat(",").
										concat(s).concat(",").concat(c.getTime().toString()).concat(",").
									    concat(stationName).concat(",").concat(tt.getSecond().toString()));
							ttWriter.newLine();
							}
						}
					}
				}
			}
			PTMUtil.closeBuffer(ttWriter);
		}catch(IOException e){
			System.err.println("error occured when writing out travel times!");
			e.printStackTrace();
		}
	}
	public void recordTravelTime(Particle p, float x, float deltaX){
		// only record particles from upstream
		//if (_recorderTest.get(p.Id) && (deltaX >0)){
		if (deltaX >0){
			IntBuffer wbNd = IntBuffer.wrap(new int[] {p.nd.getEnvIndex(), p.wb.getEnvIndex()});
			String staName = _stationNames.get(wbNd);
			// if staName != null, the particle is not at the recording location
			// _ttHolder.get(staName).get(new Integer(p.Id)) != null, the particle has been recorded once
			if ((staName != null) && (_ttHolder.get(staName).get(new Integer(p.Id)) == null)){
				//IntBuffer wbNd = IntBuffer.wrap(new int[] {p.nd.getEnvIndex(), p.wb.getEnvIndex()});
				float dist = _staDist.get(wbNd);
				//if (dist != null){
				// if x >= dist record the travel time
				// it covers x<0 because dist is always > = 0
				if (!(x<dist)){
					//String staName = _stationNames.get(wbNd);
					// intialized in constructor _ttHolder.get(staName) will not return null
					_ttHolder.get(staName).put(p.Id, new TTEntry(p.getInsertionStation(), p.getInsertionTime(), p.age));
					//_recorderTest.put(p.Id, new Boolean(true));
				}
			}
		}
	}
	*/	
}
