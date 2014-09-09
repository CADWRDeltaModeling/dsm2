/**
 * 
 */
package DWR.DMS.PTM;
import java.io.BufferedWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Calendar;
import java.util.ArrayList;
import java.nio.IntBuffer;

/**
 * @author xwang
 *
 */
public class TravelTimeOutput {

	/**
	 * 
	 */
	public TravelTimeOutput(ArrayList<String> outText) {
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
	//public void setOutputNodeId(int nodeId){_outputNodeId = nodeId;}
	//public void setOutputWbId(int wbId){_outputWbId = wbId;}
	//public void setOutputChannelDistance(float distance){_distanceFromUpNode = distance;}
	public void setTravelTime(int nodeId, int wbId, int distance, String releaseStationName, Calendar releaseTime, Integer pid, Float travelTimeInMin){ 
		IntBuffer station = IntBuffer.wrap(new int[]{nodeId, wbId, distance});
		if (_travelTimeOutput == null)
				_travelTimeOutput = new HashMap<IntBuffer, Map<String, Map<Calendar, ArrayList<Pair<Integer, Float>>>>>();
		Map<String, Map<Calendar, ArrayList <Pair<Integer, Float>>>> travelTimesPerStation = _travelTimeOutput.get(station);
		if (travelTimesPerStation == null){
			travelTimesPerStation = new HashMap<String, Map<Calendar, ArrayList<Pair<Integer, Float>>>>();
			_travelTimeOutput.put(station, travelTimesPerStation);
		}
		Map<Calendar, ArrayList <Pair<Integer, Float>>> travelTimesPerNode = travelTimesPerStation.get(releaseStationName);
		if (travelTimesPerNode == null){
			travelTimesPerNode = new HashMap<Calendar, ArrayList<Pair<Integer, Float>>>();
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
	
	public void setOutputNodeInfo(Node[] allNodes){
	    //nodeArray starts from 1 PTMFixedInput.java line 287
		if (_outputStations == null)
			System.err.println("WARNING: no output station info avaiable while setting up node info");
		else
			for (IntBuffer station: _outputStations)
				allNodes[station.get(0)].setOutputNode();
	}
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
	public Map<IntBuffer, String> getStationNames(){return _stationNames;}
	//TODO clean up
	/*
	public float getOutputChannelDistance(){
		if (_isLength){
			try{
				return (((Channel)Globals.Environment.getWaterbody(_outputWbId)).getLength());
			}catch(Exception notWb){
				notWb.printStackTrace();
				PTMUtil.systemExit("SYSTEM EXIT:wrong channel/reservior/obj2obj id:" + _wbInput);
			}
		}
		return _distanceFromUpNode;
	}
	*/
	private void setIdsDistance(ArrayList<String> stationText){
		_outputStations = new ArrayList<IntBuffer>();
		_stationNames = new HashMap<IntBuffer, String>();
		for (String stationLine: stationText.subList(1, stationText.size())){
			int[] station = new int[3];
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
				station[2] = Integer.parseInt(items[2]);
			}catch(NumberFormatException e){
				if (items[2].equalsIgnoreCase("LENGTH"))
					station[2] = -999999;
				else{
					PTMUtil.systemExit("distance input:" + items[2] +" for channel:" + items[0] + " and node:"+items[1] + " in travel time output line is wrong, please check." );
				}
			}
			_outputStations.add(IntBuffer.wrap(station));
			int[] wbNd = {station[0], station[1]};
			_stationNames.put(IntBuffer.wrap(wbNd), items[3]);
		}
	}
	//IntBuffer:station nodeId, wbId, distance; Map<release node, Map<release time, array[pid, detection time]>>
	private Map<IntBuffer, Map<String, Map<Calendar, ArrayList<Pair<Integer, Float>>>>> _travelTimeOutput;
	//IntBuffer: 0: nodeId, 1: wbId, 2: distance,
	private ArrayList<IntBuffer> _outputStations;
	//IntBuffer: 0: nodeId, 1: wbId; String: station name.  
	//a node id and wb id are engough to identify a station name because it is impossible for a channel has two station names
	private Map<IntBuffer, String> _stationNames = null;
	
	
}
