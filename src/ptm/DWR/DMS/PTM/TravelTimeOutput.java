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
			String shouldBe[] = {"NODEID", "CHANNELID/RESERVOIRNAME/OBJ2OBJNAME", "DISTANCE"};
			PTMUtil.checkTitle(outText.get(0), shouldBe);
			//TODO only allow one output for now, may change later
			if (outText.size()<2 || outText.size() > 2)
				PTMUtil.systemExit("only allow to output travel times at one station, please revise travel time output in behavior input file");
			setIdsDistance(outText.get(1).trim().split("[,\\s\\t]+"));
		}
	}
	//public void setOutputNodeId(int nodeId){_outputNodeId = nodeId;}
	//public void setOutputWbId(int wbId){_outputWbId = wbId;}
	//public void setOutputChannelDistance(float distance){_distanceFromUpNode = distance;}
	public void setTravelTime(Integer releaseNodeId, Calendar releaseTime, Integer pid, Float travelTimeInMin){
		if (_travelTimeOutput == null)
				_travelTimeOutput = new HashMap<Integer, Map<Calendar, ArrayList<Pair<Integer, Float>>>>();
		Map<Calendar, ArrayList <Pair<Integer, Float>>> travelTimesPerNode = _travelTimeOutput.get(releaseNodeId);
		if (travelTimesPerNode == null){
			travelTimesPerNode = new HashMap<Calendar, ArrayList<Pair<Integer, Float>>>();
			_travelTimeOutput.put(releaseNodeId, travelTimesPerNode);
		}
		ArrayList <Pair<Integer, Float>> travelTimesPerRelease = travelTimesPerNode.get(releaseTime);
		if (travelTimesPerRelease == null){
			travelTimesPerRelease = new ArrayList <Pair<Integer, Float>>();
			travelTimesPerNode.put(releaseTime, travelTimesPerRelease);
		}
		Pair<Integer, Float> travelTime = new Pair<Integer, Float>(pid, travelTimeInMin); 
		travelTimesPerRelease.add(travelTime);
	}
	public void travelTimeOutput(){
		if (_travelTimeOutput == null){
			System.err.println("warning: entire travel time map is empty. no travel time output!");
			return;
		}
		for (Integer n: _travelTimeOutput.keySet()){
			BufferedWriter ttWriter = PTMUtil.getOutputBuffer("output/Node"+PTMHydroInput.getExtFromIntNode(n)+"-Chan"
																+ PTMHydroInput.getExtFromIntChan(getOutputWbId()) 
																+ "Dist" + (int)getOutputChannelDistance()+"-TravelTimeInMin.csv");
			Map<Calendar, ArrayList<Pair<Integer, Float>>> m = _travelTimeOutput.get(n);
			if (m == null){
				PTMUtil.systemExit("try to write travel time to a file but the map with the release node:" + n
						+" is empty, system exit.");
			}
			try{
				ttWriter.write("PID".concat(",").concat("Release_Node").concat(",").concat("Release_Time").concat(",").concat("Travel_Time(Min)"));
				ttWriter.newLine();
				for (Calendar c: m.keySet()){
					ArrayList<Pair<Integer, Float>> tts = m.get(c);
					for (Pair<Integer, Float> tt: tts){
						ttWriter.write(tt.getFist().toString().concat(",").
								concat((new Integer(PTMHydroInput.getExtFromIntNode(n))).toString()).concat(",").concat(c.getTime().toString()).concat(",").
							    concat(tt.getSecond().toString()));
					ttWriter.newLine();
					}
			}
			PTMUtil.closeBuffer(ttWriter);
			}catch(IOException e){
				System.err.println("error occured when writing out travel times!");
				e.printStackTrace();
			}
		}
	}
	public int getOutputNodeId(){ return _outputNodeId;}
	public int getOutputWbId(){return _outputWbId;}
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
	private void setIdsDistance(String[] items){
		if (items.length<3)
			PTMUtil.systemExit("SYSTEM EXIT: expect 3 items in travel time output line in behavior input file. ");
		try{
			_outputNodeId = PTMHydroInput.getIntFromExtNode(Integer.parseInt(items[0]));
		}catch(NumberFormatException e){
				e.printStackTrace();
				PTMUtil.systemExit("SYSTEM EXIT:wrong node id:" + items[0]);
		}
		try{
			_wbInput = items[1];
			_outputWbId = PTMHydroInput.getIntFromExtChan(Integer.parseInt(items[1]));
		}catch(NumberFormatException e){
			if ((_outputWbId=PTMEnv.getReservoirObj2ObjEnvId(items[1])) == null){
				e.printStackTrace();
				PTMUtil.systemExit("SYSTEM EXIT:wrong channel/reservior/obj2obj id:" + items[1]);
			}
		}
		try{
			_distanceFromUpNode = Float.parseFloat(items[2]);
		}catch(NumberFormatException e){
			if (items[2].equalsIgnoreCase("LENGTH")){
				_distanceFromUpNode = -1.0f;
				_isLength = true;
			}
			else{
				e.printStackTrace();
				PTMUtil.systemExit("SYSTEM EXIT:wrong distance number:" + items[2]);
			}
		}
	}
	private int _outputNodeId = -99999;
	private Integer _outputWbId = -999999;
	private float _distanceFromUpNode = 0.0f; 
	private boolean _isLength = false;
	private String _wbInput = null;
	// release node, Map<release time, array[pid, detection time]>
	private Map<Integer, Map<Calendar, ArrayList<Pair<Integer, Float>>>> _travelTimeOutput;
	
	
}
