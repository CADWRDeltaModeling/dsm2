//    Copyright (C) 1996, 2009 State of California, Department of Water
//    Resources.
//
//    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
//    numerical model.  No protection claimed in original FOURPT and
//    Branched Lagrangian Transport Model (BLTM) code written by the
//    United States Geological Survey.  Protection claimed in the
//    routines and files listed in the accompanying file "Protect.txt".
//    If you did not receive a copy of this file contact
//    Tara Smith, below.
//
//    This program is licensed to you under the terms of the GNU General
//    Public License, version 2, as published by the Free Software
//    Foundation.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, contact Tara Smith, below,
//    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
//    02139, USA.
//
//    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
//    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
//    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
//    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
//    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
//    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
//    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
//    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
//    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
//    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
//    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
//    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
//    DAMAGE.
//
//    For more information about DSM2, contact:
//
//    Tara Smith
//    California Dept. of Water Resources
//    Division of Planning, Delta Modeling Section
//    1416 Ninth Street
//    Sacramento, CA  95814
//    916-653-9885
//    tara@water.ca.gov
//
//    or see our home page: http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/
package DWR.DMS.PTM;

import java.io.IOException;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import ucar.ma2.Array;
import ucar.ma2.ArrayChar;
import ucar.ma2.ArrayDouble;
import ucar.ma2.ArrayStructureBB;
import ucar.ma2.Index;
import ucar.ma2.InvalidRangeException;
import ucar.ma2.Section;
import ucar.ma2.ArrayChar.D1;
import ucar.ma2.ArrayDouble.D0;
import ucar.ma2.StructureMembers.Member;
import ucar.nc2.Attribute;
import ucar.nc2.AttributeContainer;
import ucar.nc2.Dimension;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Structure;
import ucar.nc2.Variable;

/**
 * This class handles reading of the DSM2 HYDRO tidefile.
 * <p>
 * This information is then used to update the Waterbody object array.
 * <p>
 *
 * @author Nicky Sandhu (DWR)
 * @author Doug Jackson (QEDA Consulting, LLC)
 */
// this class is mainly used to read hydro info for each time step and set up wbarray
public class PTMHydroInput{

	private final static boolean DEBUG = false;
	private static final double acreToSqft = 43560;

	private NetcdfFile ncd;
	private int currentModelTime, prevIndex, nextIndex, chunkSize,
	flowStartIndex, flowEndIndex, stageStartIndex, areaStartIndex,
	resHeightStartIndex, resFlowStartIndex, qextFlowStartIndex, transferFlowStartIndex;
	private boolean indexChanged;
	private float theta;
	private String tidefile;
	private List<Integer> julMin, chanFlowJulMin, chanStageJulMin, chanAreaJulMin, 
	resHeightJulMin, resFlowJulMin, qextFlowJulMin, transferFlowJulMin;
	private Array chanBottomUp, chanBottomDown,
	flowChunk, stageChunk, areaChunk, resHeightChunk, resFlowChunk, qextFlowChunk, transferFlowChunk;
	private Map<String, Map<String, List<Double>>> resVol;
	private Map<String, Map<String, List<Integer>>> resNodeConnect;
	private List<String> reservoirs;
	private Map<String, Double> reservoirsTopArea, reservoirsBotElev;

	public PTMHydroInput(PTMFixedInput fixedInput) {
		ncd = Grid.getNCD();

		this.tidefile = fixedInput.getTidefile();
		System.out.println("Tidefile: " + tidefile);

		chanFlowJulMin = createJulMin("/hydro/data/channel_flow");
		chanStageJulMin = createJulMin("/hydro/data/channel_stage");
		chanAreaJulMin = createJulMin("/hydro/data/channel_area");
		resHeightJulMin = createJulMin("/hydro/data/reservoir_height");
		resFlowJulMin = createJulMin("/hydro/data/reservoir_flow");
		qextFlowJulMin = createJulMin("/hydro/data/qext_flow");
		transferFlowJulMin = createJulMin("/hydro/data/transfer_flow");

		// Verify that all of the julMin vectors are identical and therefore can be represented
		// by the single julMin variable
		if(!(chanFlowJulMin.equals(chanStageJulMin) && chanFlowJulMin.equals(chanAreaJulMin) && 
				chanFlowJulMin.equals(resHeightJulMin) &&
				chanFlowJulMin.equals(resFlowJulMin) && chanFlowJulMin.equals(qextFlowJulMin) &&
				chanFlowJulMin.equals(transferFlowJulMin))) {
			PTMUtil.systemExit("Channel flow, channel stage, channel area, reservoirHeight, reservoir flow, " +
					"qext flow, and transfer flow must all have the same time stamps. Check tidefile. System exit");

		}
		julMin = chanFlowJulMin;

		readChanBottom();
		readResVol();
		readRes();

		resNodeConnect = Grid.getResNodeConnect();

		flowChunk = null;
		stageChunk = null;
		areaChunk = null;
		resHeightChunk = null;
		resFlowChunk = null;
		qextFlowChunk = null;
		transferFlowChunk = null;

		indexChanged = false;
		nextIndex = Grid.MISSING;

		// Above 50, chunkSize appears to have a negligible effect on runtime => fix to 100
		chunkSize = 100;
	}

	/**
	 * Create a List<Integer> of Julian minutes using the time dimension of the specified array
	 * @param path					String specifying the complete HDF5 path to the variable
	 * @return						List<Integer> containing Julian minutes
	 */
	public List<Integer> createJulMin(String path) {
		Variable var;
		Pattern patternMin, patternHour, patternInt;
		Matcher matchMin, matchHour, matchInt;
		List<LocalDateTime> datetime;
		List<Integer> julMin;
		DateTimeFormatter formatter, currentFormatter;
		LocalDateTime startDatetime, current;
		Dimension timeDim;
		AttributeContainer attrib;
		Attribute startTime, interval;
		String intervalStr;
		int timeStep, currentHour, currentMin;
		int julian;
		Duration duration;

		datetime = null;
		julMin = null;

		formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
		currentFormatter = DateTimeFormatter.ofPattern("ddLLLyyyy");

		// Read attributes	
		var = ncd.findVariable(path);

		timeDim = var.getDimension(0);
		attrib = var.attributes();

		startTime = attrib.findAttribute("start_time");
		interval = attrib.findAttribute("interval");
		intervalStr = (String) interval.getValue(0);

		// Create datetime sequence       
		startDatetime = LocalDateTime.parse((CharSequence) startTime.getValue(0), formatter);

		patternMin = Pattern.compile("min", Pattern.CASE_INSENSITIVE);
		matchMin = patternMin.matcher(intervalStr);
		patternHour = Pattern.compile("hour", Pattern.CASE_INSENSITIVE);
		matchHour = patternHour.matcher(intervalStr);

		patternInt = Pattern.compile("\\d+");
		matchInt = patternInt.matcher(intervalStr);

		duration = null;
		if(matchInt.find()) {
			timeStep = Integer.parseInt(matchInt.group());

			if(matchMin.find()) {
				duration = Duration.ofMinutes(timeStep);
			}
			else if(matchHour.find()) {
				duration = Duration.ofHours(timeStep);
			}
		}

		datetime = new ArrayList<>();
		julMin = new ArrayList<>();
		current = startDatetime;
		for(int i=0; i<timeDim.getLength(); i++) {

			// Convert current datetime into Julian minutes
			currentHour = current.getHour();
			currentMin = current.getMinute();
			julian = PTMUtil.datjul(current.format(currentFormatter));

			datetime.add(current);
			julMin.add(julian*24*60+currentHour*60+currentMin);
			current = current.plus(duration);
		}

		// julMin should already be sorted, but just in case
		Collections.sort(julMin);

		return julMin;
	}

	/** 
	 * Read the tidefile channel_bottom array into chanBottomUp and chanBottomDown variables
	 */
	public void readChanBottom() {
		Variable chanBottomVar, chanBottomUpSlice, chanBottomDownSlice;

		try {
			chanBottomVar = ncd.findVariable("/hydro/geometry/channel_bottom");
			chanBottomUpSlice = chanBottomVar.slice(0, 0);
			chanBottomDownSlice = chanBottomVar.slice(0, 1);

			chanBottomUp = chanBottomUpSlice.read();
			chanBottomDown = chanBottomDownSlice.read();
		} catch (InvalidRangeException | IOException e) {
			PTMUtil.systemExit("Exception: " + e);
		}
	}

	/**
	 * Read tidefile reservoir array into reservoirs variable
	 */
	public void readRes() {
		String resName;
		double area, elev;
		Variable resVar;
		Structure structure;
		ArrayStructureBB arrayStructureBB;
		List<Member> members;
		Member nameMember, areaMember, elevMember;
		ArrayChar.D1 nameData;
		ArrayDouble.D0 areaData, elevData;

		reservoirs = new ArrayList<>();
		reservoirsTopArea = new HashMap<>();
		reservoirsBotElev = new HashMap<>();

		try {
			resVar = ncd.findVariable("/hydro/input/reservoir");
			structure = (Structure) resVar;
			arrayStructureBB = (ArrayStructureBB) structure.read();

			members = arrayStructureBB.getMembers();

			nameMember = members.get(0);
			areaMember = members.get(1);
			elevMember = members.get(2);

			for (int i=0; i<arrayStructureBB.getSize(); i++) {
				nameData = (ArrayChar.D1) arrayStructureBB.getArray(i, nameMember);
				areaData = (ArrayDouble.D0) arrayStructureBB.getArray(i, areaMember);
				elevData = (ArrayDouble.D0) arrayStructureBB.getArray(i, elevMember); 

				resName = nameData.getString().trim().toUpperCase();
				area = areaData.get();
				elev = elevData.get();

				reservoirs.add(resName);
				reservoirsTopArea.put(resName, area);
				reservoirsBotElev.put(resName, elev);
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * Read tidefile reservoir_vol array into resVol variable
	 */
	public void readResVol() {
		String resName;
		double resElev, resArea;
		Variable resVolVar;
		Structure structure;
		ArrayStructureBB arrayStructureBB;
		List<Member> members;
		Member nameMember, elevMember, areaMember;
		ArrayChar.D1 nameData;
		ArrayDouble.D0 elevData, areaData;
		List<Double> thisDz, thisVol, thisElev, thisArea;
		Map<String, List<Double>> thisRes;

		resVol = new HashMap<>();

		try {
			resVolVar = ncd.findVariable("/hydro/input/reservoir_vol");
			structure = (Structure) resVolVar;

			if(structure==null) {
				System.out.println("reservoir_vol table of tidefile is null: no reservoir geometries defined.");
			}
			else {
				arrayStructureBB = (ArrayStructureBB) structure.read();
				members = arrayStructureBB.getMembers();

				nameMember = members.get(0);
				elevMember = members.get(1);
				areaMember = members.get(2);			

				for (int i=0; i<arrayStructureBB.getSize(); i++) {
					nameData = (D1) arrayStructureBB.getArray(i, nameMember);
					elevData = (D0) arrayStructureBB.getArray(i, elevMember);
					areaData = (D0) arrayStructureBB.getArray(i, areaMember);

					resName = nameData.getString().toUpperCase();
					resElev = elevData.get();
					resArea = areaData.get();

					if(!resVol.containsKey(resName)) {
						thisRes = new HashMap<>();
						thisRes.put("elev", new ArrayList<Double>());
						thisRes.put("area_acre", new ArrayList<Double>());
						thisRes.put("area_sqft", new ArrayList<Double>());
						resVol.put(resName, thisRes);
					}

					resVol.get(resName).get("elev").add(resElev);
					resVol.get(resName).get("area_acre").add(resArea);
					resVol.get(resName).get("area_sqft").add(resArea*acreToSqft);
				}
			}

		} catch (IOException e) {
			PTMUtil.systemExit("Exception: " + e);
		}

		// Calculate volume
		for(String key: resVol.keySet()) {
			resVol.get(key).put("dz", new ArrayList<Double>());
			resVol.get(key).put("vol", new ArrayList<Double>());

			thisDz = resVol.get(key).get("dz");
			thisVol = resVol.get(key).get("vol");
			thisElev = resVol.get(key).get("elev");
			thisArea = resVol.get(key).get("area_sqft");

			thisDz.add(0.0);
			thisVol.add(0.0);

			for(int i=1; i<thisElev.size(); i++) {
				thisDz.add(thisElev.get(i) - thisElev.get(i-1));
				thisVol.add(thisVol.get(i-1) + (thisArea.get(i-1) + thisArea.get(i))*0.5*thisDz.get(i));
			}
		}
	}

	/**
	 * Return the index of the smallest julMin that is greater than or equal to currentModelTime
	 * @param julMin				List containing Julian minutes
	 * @return						index of the smallest julMin that is greater than or equal to currentModelTime
	 */
	public int getNextIndex(List<Integer> julMin) {
		int insertionPoint, nextIndex;

		insertionPoint = Collections.binarySearch(julMin, currentModelTime);

		// binarySearch returns (-(insertion point) - 1) if the value isn't found in the list
		nextIndex = 0;
		if(insertionPoint>=0) {
			// If currentModelTime is in julMin, return insertionPoint
			nextIndex = insertionPoint;
		} else {
			insertionPoint = -insertionPoint-1;

			if(insertionPoint==0 || insertionPoint==julMin.size()) {
				PTMUtil.systemExit("currentModelTime is outside the range of julMin.");
			}
			else {
				nextIndex = insertionPoint;
			}
		}

		return nextIndex;
	}

	/**
	 * Read next chunks of data if data for the current time step isn't contained in the current chunks
	 */
	public void updateChunks() {
		if(flowChunk==null) {
			readFlowChunk();
			readStageChunk();
			readAreaChunk();
			readResHeightChunk();
			readResFlowChunk();
			readQextFlowChunk();
			readTransferFlowChunk();
		}

		if(nextIndex>flowEndIndex) {
			readFlowChunk();
			readStageChunk();
			readAreaChunk();
			readResHeightChunk();
			readResFlowChunk();
			readQextFlowChunk();
			readTransferFlowChunk();
		}		
	}

	/**
	 * Read the next chunk of channel flow data from the tidefile
	 */
	public void readFlowChunk() {
		Variable var;
		String path;
		int[] origin, size;
		int thisChunkSize;

		path = "/hydro/data/channel_flow";

		// Read data for currentModelTime
		try {
			var = ncd.findVariable(path);

			thisChunkSize = Math.min(var.getDimension(0).getLength()-prevIndex, chunkSize);

			flowStartIndex = prevIndex;
			flowEndIndex = flowStartIndex + thisChunkSize - 1;
			origin = new int[] {flowStartIndex, 0, 0};
			size = new int[] {thisChunkSize, var.getDimension(1).getLength(), var.getDimension(2).getLength()};
			flowChunk = var.read(new Section(origin, size));
		} catch (IOException | InvalidRangeException ioe) {
			PTMUtil.systemExit("Exception: " + ioe);
		}
	}

	/**
	 * Read the next chunk of channel stage data from the tidefile
	 */
	public void readStageChunk() {
		Variable var;
		String path;
		int[] origin, size;
		int thisChunkSize;

		path = "/hydro/data/channel_stage";

		// Read data for currentModelTime
		try {
			var = ncd.findVariable(path);

			thisChunkSize = Math.min(var.getDimension(0).getLength()-prevIndex, chunkSize);

			stageStartIndex = prevIndex;
			origin = new int[] {stageStartIndex, 0, 0};
			size = new int[] {thisChunkSize, var.getDimension(1).getLength(), var.getDimension(2).getLength()};
			stageChunk = var.read(new Section(origin, size));
		} catch (IOException | InvalidRangeException ioe) {
			PTMUtil.systemExit("Exception: " + ioe);
		}
	}

	/**
	 * Read the next chunk of channel area data from the tidefile
	 */
	public void readAreaChunk() {
		Variable var;
		String path;
		int[] origin, size;
		int thisChunkSize;

		path = "/hydro/data/channel_area";

		// Read data for currentModelTime
		try {
			var = ncd.findVariable(path);

			thisChunkSize = Math.min(var.getDimension(0).getLength()-prevIndex, chunkSize);

			areaStartIndex = prevIndex;
			origin = new int[] {areaStartIndex, 0, 0};
			size = new int[] {thisChunkSize, var.getDimension(1).getLength(), var.getDimension(2).getLength()};
			areaChunk = var.read(new Section(origin, size));
		} catch (IOException | InvalidRangeException ioe) {
			PTMUtil.systemExit("Exception: " + ioe);
		}
	}

	/**
	 * Read the next chunk of reservoir height data from the tidefile
	 */
	public void readResHeightChunk() {
		Variable var;
		String path;
		int[] origin, size;
		int thisChunkSize;

		path = "/hydro/data/reservoir_height";

		// Read data for currentModelTime
		try {
			var = ncd.findVariable(path);

			thisChunkSize = Math.min(var.getDimension(0).getLength()-prevIndex, chunkSize);

			resHeightStartIndex = prevIndex;
			origin = new int[] {resHeightStartIndex, 0};
			size = new int[] {thisChunkSize, var.getDimension(1).getLength()};
			resHeightChunk = var.read(new Section(origin, size));
		} catch (IOException | InvalidRangeException ioe) {
			PTMUtil.systemExit("Exception: " + ioe);
		}
	}

	/**
	 * Read the next chunk of reservoir flow data from the tidefile
	 */
	public void readResFlowChunk() {
		Variable var;
		String path;
		int[] origin, size;
		int thisChunkSize;

		path = "/hydro/data/reservoir_flow";

		// Read data for currentModelTime
		try {
			var = ncd.findVariable(path);

			thisChunkSize = Math.min(var.getDimension(0).getLength()-prevIndex, chunkSize);

			resFlowStartIndex = prevIndex;
			origin = new int[] {resFlowStartIndex, 0};
			size = new int[] {thisChunkSize, var.getDimension(1).getLength()};
			resFlowChunk = var.read(new Section(origin, size));
		} catch (IOException | InvalidRangeException ioe) {
			PTMUtil.systemExit("Exception: " + ioe);
		}
	}

	/**
	 * Read the next chunk of qext flow data from the tidefile
	 */
	public void readQextFlowChunk() {
		Variable var;
		String path;
		int[] origin, size;
		int thisChunkSize;

		path = "/hydro/data/qext_flow";

		// Read data for currentModelTime
		try {
			var = ncd.findVariable(path);

			thisChunkSize = Math.min(var.getDimension(0).getLength()-prevIndex, chunkSize);

			qextFlowStartIndex = prevIndex;
			origin = new int[] {qextFlowStartIndex, 0};
			size = new int[] {thisChunkSize, var.getDimension(1).getLength()};
			qextFlowChunk = var.read(new Section(origin, size));
		} catch (IOException | InvalidRangeException ioe) {
			PTMUtil.systemExit("Exception: " + ioe);
		}
	}

	/**
	 * Read the next chunk of transfer flow data from the tidefile
	 */
	public void readTransferFlowChunk() {
		Variable var;
		String path;
		int[] origin, size;
		int thisChunkSize;

		path = "/hydro/data/transfer_flow";

		// Read data for currentModelTime
		try {
			var = ncd.findVariable(path);

			thisChunkSize = Math.min(var.getDimension(0).getLength()-prevIndex, chunkSize);

			transferFlowStartIndex = prevIndex;
			origin = new int[] {transferFlowStartIndex, 0};
			size = new int[] {thisChunkSize, var.getDimension(1).getLength()};
			transferFlowChunk = var.read(new Section(origin, size));
		} catch (IOException | InvalidRangeException ioe) {
			PTMUtil.systemExit("Exception: " + ioe);
		}
	}

	/**
	 * Calculate the current volume for the specified reservoir
	 * @param reservoirNumber		one-based reservoir number
	 * @return						current reservoir volume
	 */
	public Double calcResVol(int reservoirNumber) {
		String resName;
		List<Double> elevs, areas, vols;
		float resHeight;
		int insertionPoint, thisNextIndex;
		Index index;
		double z1, z2, A1, A2, V1, F, topArea, botElev, vol;

		resName = reservoirs.get(reservoirNumber-1);

		index = resHeightChunk.getIndex();
		resHeight = resHeightChunk.getFloat(index.set(nextIndex-resHeightStartIndex, reservoirNumber-1));

		// Constant volume
		if(!resVol.containsKey(resName)) {

			topArea = reservoirsTopArea.get(resName);
			botElev = reservoirsBotElev.get(resName);

			// Top area is in units of square feet/1E6
			vol = topArea*1E6*(resHeight-botElev);

			return vol;
		}

		elevs = resVol.get(resName).get("elev");
		areas = resVol.get(resName).get("area_sqft");
		vols = resVol.get(resName).get("vol");

		insertionPoint = Collections.binarySearch(elevs, (double) resHeight);

		// binarySearch returns (-(insertion point) - 1) if the value isn't found in the list
		thisNextIndex = 0;
		if(insertionPoint>=0) {
			// If the exact elevation is found, return its corresponding volume
			return resVol.get(resName).get("vol").get(insertionPoint);
		} else {
			insertionPoint = -insertionPoint-1;

			if(insertionPoint==0) {
				PTMUtil.systemExit("Reservoir height is below the range of the reservoir elevations. System exit.");
			} 
			else if (insertionPoint==elevs.size()) {
				// Higher than the highest layer => assume constant area and extrapolate
				z1 = elevs.get(elevs.size());
				A1 = areas.get(areas.size());
				V1 = vols.get(vols.size());
				vol = V1 + A1*(resHeight - z1);
				return vol;
			}
			else {
				thisNextIndex = insertionPoint;
			}
		}

		z1 = elevs.get(thisNextIndex-1);
		z2 = elevs.get(thisNextIndex);
		A1 = areas.get(thisNextIndex-1);
		A2 = areas.get(thisNextIndex);
		V1 = vols.get(thisNextIndex-1);			
		F = (A2 - A1)/(z2 - z1);

		vol = 0.5*F*Math.pow(resHeight - z1, 2) + A1*(resHeight - z1) + V1;

		return vol;
	}

	/**
	 * Calculate the current depth for the specified reservoir
	 * @param reservoirNumber		one-based reservoir number
	 * @return						current reservoir depth
	 */
	public float getReservoirDepth(int reservoirNumber) {
		Index index;
		float resHeight;

		index = resHeightChunk.getIndex();
		resHeight = resHeightChunk.getFloat(index.set(nextIndex-resHeightStartIndex, reservoirNumber-1));
		return resHeight;
	}

	/**
	 * Update currentModelTime and nextIndex and then update data chunks
	 * @param currentModelTime		current model time
	 */
	public final void getNextChunk(int currentModelTime) {
		int prevNextIndex;
		this.currentModelTime = currentModelTime;

		// Record the current nextIndex so we can detect a change
		prevNextIndex = nextIndex;
		nextIndex = getNextIndex(julMin);
		indexChanged = nextIndex!=prevNextIndex;
		if(prevNextIndex==Grid.MISSING) {prevNextIndex=nextIndex;}

		// Update prevIndex if the nextIndex has changed
		if(indexChanged) {
			prevIndex = prevNextIndex;
		}

		// Update chunks now that prevIndex has potentially been updated
		updateChunks();
	}

	/**
	 * Update information in the Waterbody array in PTMEnv
	 * @param wbArray				array of waterbodies
	 * @param lFD					maximum numbers of channels, nodes, etc.
	 */
	public final void updateWaterbodiesHydroInfo(Waterbody [] wbArray, LimitsFixedData lFD){
		//update Channel depths, flows and area of flow
		float[] depthArray = new float[2];
		float[] flowArray = new float[2];
		float[] stageArray = new float[2];
		float[] areaArray = new float[2];
		int numConst = PTMFixedData.getQualConstituentNames().length;
		float[][] qualityArray = new float[2][numConst];
		Index index;

		float upNodeDepth, downNodeDepth, upNodeStage, downNodeStage, upNodeArea, downNodeArea;

		theta = Globals.Environment.getPTMFixedInput().getTheta();

		// channelNumber is waterbody's envIndex because waterbodies start from channels
		for(int channelNumber=1;
				channelNumber <= PTMFixedData.getMaxNumberOfChannels();
				channelNumber++){

			if (wbArray[channelNumber] !=null){
				// Calculate depth
				index = stageChunk.getIndex();
				upNodeDepth = stageChunk.getFloat(index.set(prevIndex-stageStartIndex, channelNumber-1, 0))*(1-theta) + stageChunk.getFloat(index.set(nextIndex-stageStartIndex, channelNumber-1, 0))*theta;
				downNodeDepth = stageChunk.getFloat(index.set(prevIndex-stageStartIndex, channelNumber-1, 1))*(1-theta) + stageChunk.getFloat(index.set(nextIndex-stageStartIndex, channelNumber-1, 1))*theta;

				depthArray[Channel.UPNODE]   = upNodeDepth;
				depthArray[Channel.DOWNNODE] = downNodeDepth;

				// Calculate stage	
				upNodeStage = (stageChunk.getFloat(index.set(prevIndex-stageStartIndex, channelNumber-1, 0)) + chanBottomUp.getFloat(channelNumber-1))*(1-theta) + 
						(stageChunk.getFloat(index.set(nextIndex-stageStartIndex, channelNumber-1, 0)) + chanBottomUp.getFloat(channelNumber-1))*theta;
				downNodeStage = (stageChunk.getFloat(index.set(prevIndex-stageStartIndex, channelNumber-1, 1)) + chanBottomDown.getFloat(channelNumber-1))*(1-theta) +
						(stageChunk.getFloat(index.set(nextIndex-stageStartIndex, channelNumber-1, 1)) + chanBottomDown.getFloat(channelNumber-1))*theta;			

				stageArray[Channel.UPNODE]   = upNodeStage;
				stageArray[Channel.DOWNNODE] = downNodeStage;

				index = flowChunk.getIndex();
				flowArray[Channel.UPNODE]= flowChunk.getFloat(index.set(nextIndex-flowStartIndex, channelNumber-1, 0));
				flowArray[Channel.DOWNNODE] = flowChunk.getFloat(index.set(nextIndex-flowStartIndex, channelNumber-1, 1));

				// Calculate area
				index = areaChunk.getIndex();
				upNodeArea = areaChunk.getFloat(index.set(prevIndex-areaStartIndex, channelNumber-1, 0))*(1-theta) + 
						areaChunk.getFloat(index.set(nextIndex-areaStartIndex, channelNumber-1, 0))*theta;
				downNodeArea = areaChunk.getFloat(index.set(prevIndex-areaStartIndex, channelNumber-1, 1))*(1-theta) + 
						areaChunk.getFloat(index.set(nextIndex-areaStartIndex, channelNumber-1, 1))*theta;

				areaArray[Channel.UPNODE]   = upNodeArea;
				areaArray[Channel.DOWNNODE] = downNodeArea;

				// no use of quality currently
				for (int indx = 0; indx < qualityArray[0].length; indx++){
					//TODO fixme: got rid of quality temporarily
					qualityArray[Channel.UPNODE][0] = 0.f;
					qualityArray[Channel.DOWNNODE][0] = 0.f;
				}

				Channel chan = ((Channel) wbArray[channelNumber]);
				chan.setDepth(depthArray);
				chan.setStage(stageArray);
				chan.setFlow(flowArray);
				chan.setArea(areaArray);
			}//end if (wbArray)
		}//end for (channelNumber)

		// update Reservoir dynamic information
		flowArray = new float[PTMFixedData.getMaxNumberOfReservoirNodes()+1];
		depthArray = new float[1];
		for(int reservoirNumber=1;
				reservoirNumber <= PTMFixedData.getMaxNumberOfReservoirs();
				reservoirNumber++){
			int envIndex = PTMFixedData.getUniqueIdForReservoir(reservoirNumber);

			if(wbArray[envIndex] != null){

				float volume = calcResVol(reservoirNumber).floatValue();

				((Reservoir )wbArray[envIndex]).setVolume(volume);
				if (DEBUG) System.out.println(wbArray[envIndex]);

				//update Reservoir flows except for pumping flow which is set later
				for(int connection=1;
						connection <= wbArray[envIndex].getNumberOfNodes();
						connection++){
					int nodeNumber = wbArray[envIndex].getNode(connection-1).getEnvIndex();

					if (DEBUG) System.out.println("Node Number is " + nodeNumber
							+ " for reservoir number " + reservoirNumber
							+ " for connecton number " + connection);
					int nodeLocalIndex = 0;
					nodeLocalIndex = wbArray[envIndex].getNodeLocalIndex(nodeNumber);
					if (nodeLocalIndex == -1){
						System.out.println("PTMHydroInput.java: Node " + nodeNumber
								+ " not found in waterbody " + envIndex);
					}
					flowArray[nodeLocalIndex] = getResFlow(reservoirNumber, nodeNumber);

					if (DEBUG){
						System.out.println("Resrvoir # " + reservoirNumber
								+ " Connection #: " + connection + " flow= "
								+ getResFlow(reservoirNumber, nodeNumber));
					}
				}//end for (connection)

				if (DEBUG){
					System.out.print("Wb EnvIndex: " + envIndex
							+ "Reservoir local index: " + reservoirNumber);
					for(int j=0; j < wbArray[envIndex].getNumberOfNodes(); j++)
						System.out.println(", flow = " + flowArray[j]);
				}
				wbArray[envIndex].setFlow(flowArray);
				depthArray[0] = getReservoirDepth(reservoirNumber);

				((Reservoir )wbArray[envIndex]).setDepth(depthArray);
			}
		}

		// update stage boundary flows
		flowArray = new float[1];
		for (int stgId = 1;
				stgId <= PTMFixedData.getMaxNumberOfStageBoundaries();
				stgId++){
			int envIndex = PTMFixedData.getUniqueIdForStageBoundary(stgId);
			if (wbArray[envIndex] != null ){
				flowArray[0] = getStageBoundaryFlow(stgId);
				wbArray[envIndex].setFlow(flowArray);
			}
		}

		// update boundary flows
		if (DEBUG) System.out.println("Updating external flows");
		flowArray = new float[1];
		for (int extId = 1;
				extId <= PTMFixedData.getMaxNumberOfBoundaryWaterbodies();
				extId ++){
			flowArray[0] = getBoundaryFlow(extId);
			int envIndex = PTMFixedData.getUniqueIdForBoundary(extId);
			if (DEBUG){
				System.out.println("Wb EnvIndex: " + envIndex
						+ "extId: " + extId + ", flow = " + flowArray[0]);
			}
			if (wbArray[envIndex] != null) wbArray[envIndex].setFlow(flowArray);
		}

		// update internal or conveyor flows
		if (DEBUG) System.out.println("Updating internal flows");
		flowArray = new float[2];
		for (int intId = 0 ; intId < PTMFixedData.getMaxNumberOfConveyors(); intId ++){
			flowArray[0] = getConveyorFlow(intId);
			flowArray[1] = -getConveyorFlow(intId);
			int envIndex = PTMFixedData.getUniqueIdForConveyor(intId);
			if (DEBUG){
				System.out.println("Wb EnvIndex: " + envIndex
						+ "Id: " + intId + ", flow = "
						+ flowArray[0] + ", " + flowArray[1]);
			}
			if (wbArray[envIndex] != null) wbArray[envIndex].setFlow(flowArray);
		}

		// update stage boundary flows ?
		if (DEBUG) System.out.println("Updated all flows");
	}

	public final void updateNodesHydroInfo(Node [] nodeArray) {
		for (Node node: nodeArray){
			if (node != null){
				node.setTotalWaterbodyInflows();
				node.setTotalAgDiversions();
			}
		}
	}

	/**
	 * Convert from internal to external channel number
	 * @param inchanneNumber		internal channel number
	 * @return						external channel number
	 */
	public static int getExtFromIntChan(int inchanneNumber) {
		return PTMFixedData.getExtChanNum(inchanneNumber);
	}

	/**
	 * Convert from internal to external node number
	 * @param innodeNumber			internal node number
	 * @return						external node number
	 */
	public static int getExtFromIntNode(int innodeNumber) {
		return PTMFixedData.getExtNodeNum(innodeNumber);
	}

	/**
	 * Convert from external to internal channel number
	 * @param exchannelNumber		external channel number
	 * @return						internal channel number
	 */
	public static int getIntFromExtChan(int exchannelNumber) {
		return PTMFixedData.getIntChanNum(exchannelNumber);
	}

	/**
	 * Convert from external to internal node number
	 * @param exnodeNumber			external node number
	 * @return						internal node number
	 */
	public static int getIntFromExtNode(int exnodeNumber) {
		return PTMFixedData.getIntNodeNum(exnodeNumber);
	}

	/**
	 * Get the flow for the specified reservoir and node
	 * @param reservoirNumber		one-based reservoir number
	 * @param nodeNumber			index into the reservoir's node array
	 * @return						flow
	 */
	public float getResFlow(int reservoirNumber, int nodeNumber) {
		Map<String, List<Integer>> thisResNodeConnect;
		int nodeIndex, flowType, flowIndex;
		Index index;
		float flow = -999;

		thisResNodeConnect = resNodeConnect.get(reservoirs.get(reservoirNumber-1));

		nodeIndex = thisResNodeConnect.get("nodeNum").indexOf(nodeNumber);
		flowType = thisResNodeConnect.get("flowType").get(nodeIndex);
		flowIndex = thisResNodeConnect.get("flowIndex").get(nodeIndex);

		if(flowType==Grid.RES_FLOW) {
			index = resFlowChunk.getIndex();
			flow = resFlowChunk.getFloat(index.set(nextIndex-resFlowStartIndex, flowIndex-1));	
		}
		else if(flowType==Grid.QEXT_FLOW) {
			index = qextFlowChunk.getIndex();
			flow = -qextFlowChunk.getFloat(index.set(nextIndex-qextFlowStartIndex, flowIndex-1));
		}
		else if(flowType==Grid.CONVEYOR_FLOW) {
			index = transferFlowChunk.getIndex();
			flow = -transferFlowChunk.getFloat(index.set(nextIndex-transferFlowStartIndex, flowIndex-1));
		}
		else {
			PTMUtil.systemExit("getResFlow: Unrecognized flowType " + flowType + ". System exit.");
		}

		return flow;
	}

	/**
	 * Get boundary flow for the specified qext index
	 * @param qextIndex				one-based qext index
	 * @return						flow
	 */
	public float getBoundaryFlow(int qextIndex) {
		Index index;
		float flow;

		if((qextIndex-1)<qextFlowChunk.getShape()[1]) {
			index = qextFlowChunk.getIndex();
			flow = qextFlowChunk.getFloat(index.set(nextIndex-qextFlowStartIndex, qextIndex-1));
			return flow;
		}
		else {
			return 0.0f;
		}
	}

	/**
	 * Get stage boundary flow for the specified boundary flow index
	 * @param bId					stage boundary flow index
	 * @return						flow-flow balance at node
	 */
	public float getStageBoundaryFlow(int bId) {
		float thisFlow, flow;
		Map<String, ArrayList<Integer>> stageBoundaryFlowIndices;
		ArrayList<Integer> flowIndex, objType, upDownIndex;
		Index index;

		// Return zero if bId is out of the range of stage boundaries
		if(bId>PTMFixedData.getStageBoundaryNames().size()) {
			return 0.0f;
		}

		stageBoundaryFlowIndices = PTMFixedData.getStageBoundaryFlowIndices(bId);
		flowIndex = stageBoundaryFlowIndices.get("flowIndex");
		objType = stageBoundaryFlowIndices.get("objType");
		upDownIndex = stageBoundaryFlowIndices.get("upDownIndex");

		// Calculate flow balance at node
		flow = 0.0f;		
		for(int i=0; i<flowIndex.size(); i++) {
			thisFlow = 0.0f;
			if(objType.get(i)==Grid.OBJ_CHAN) {
				index = flowChunk.getIndex();
				if(upDownIndex.get(i)==0) {
					thisFlow = -flowChunk.getFloat(index.set(nextIndex-flowStartIndex, flowIndex.get(i)-1, 0));
				}
				else if(upDownIndex.get(i)==1) {
					thisFlow = flowChunk.getFloat(index.set(nextIndex-flowStartIndex, flowIndex.get(i)-1, 1));
				}
			}
			else if(objType.get(i)==Grid.OBJ_NODE) {
				index = qextFlowChunk.getIndex();
				thisFlow = -qextFlowChunk.getFloat(index.set(nextIndex-qextFlowStartIndex, flowIndex.get(i)-1));
			}
			flow+=thisFlow;
		}

		// In Fortran, stage boundary flow = flow - get_flow_balance_at_node
		return -flow;
	}

	/**
	 * Get flow for the specified conveyor -- currently not implemented in the Java I/O code => return zero
	 * @param cId					conveyor index
	 * @return						flow (zero)
	 */
	public float getConveyorFlow(int cId) {
		return 0.0f;
	}
}
