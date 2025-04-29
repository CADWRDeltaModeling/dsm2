package DWR.DMS.PTM;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ucar.ma2.ArrayChar;
import ucar.ma2.ArrayInt;
import ucar.ma2.ArrayStructureBB;
import ucar.ma2.StructureMembers.Member;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Structure;
import ucar.nc2.Variable;
import ucar.nc2.dataset.NetcdfDatasets;

/**
 * Class containing channel, node, reservoir, qext, and conveyor objects corresponding to the DSM2 HYDRO grid
 * 
 * @author Doug Jackson (QEDA Consulting, LLC)
 */
public class Grid {
	
	public static NetcdfFile ncd;
	
	public static final int OBJ_CHAN = 1;
	public static final int OBJ_NODE = 2;
	public static final int OBJ_RESERVOIR = 3;
	public static final int OBJ_QEXT = 5;
	public static final int OBJ_CONVEYOR = 6;
	
	public static Map<String, Integer> fluxTypes;
	
	private static List<GridChannel> channels;
	private static List<Integer> extChanNums;
	private static List<GridNode> nodes;
	private static List<Integer> extNodeNums;
	private static List<Integer> intNodeNums;
	private static List<GridReservoir> reservoirs;
	private static List<Integer> intResNums;
	private static Map<String, Map<String, List<Integer>>> resNodeConnect;
	private static List<GridBoundary> stageBoundaries;
	private static List<GridBoundary> boundaries;
	private static List<GridConveyor> conveyors;
	private static Map<Integer, GridWaterbody> waterbodies;
	
	public static final int RES_FLOW = 0;
	public static final int QEXT_FLOW = 1;
	public static final int CONVEYOR_FLOW = 2;
	
	static final int MISSING=-999;
	
	static {
		try {
			ncd = NetcdfDatasets.openFile(PTMFixedData.getTidefile(), null);
		} catch (IOException ioe) {
			PTMUtil.systemExit("Could not open tidefile: " + ioe);
		}
		
		fluxTypes = new HashMap<>();
		fluxTypes.put("CHAN", 100);
		fluxTypes.put("RES", 101);
		fluxTypes.put("QEXT", 105);
		fluxTypes.put("FLOW_BOUNDARY", 105);
		fluxTypes.put("SOURCE_SINK", 105);
		fluxTypes.put("CONVEYOR", 106);
		fluxTypes.put("STAGE", 105);
		
		channels = new ArrayList<>();
		extChanNums = new ArrayList<>();
		nodes = new ArrayList<>();
		extNodeNums = new ArrayList<>();
		intNodeNums = new ArrayList<>();
		reservoirs = new ArrayList<>();
		intResNums = new ArrayList<>();
		stageBoundaries = new ArrayList<>();
		boundaries = new ArrayList<>();
		conveyors = new ArrayList<>();
	}

	/**
	 * Obtain a handle to the tidefile
	 * @return						handle to the tidefile
	 */
	public static NetcdfFile getNCD() {
		return ncd;
	}
	
	/**
	 * Close the tidefile
	 */
	public static void closeTidefile() {
		try {
			ncd.close();
		} catch (IOException e) {
			PTMUtil.systemExit("Exception: " + e);
		}
	}
	
	/**
	 * Read the reservoir_node_connect table from the tidefile and store it in the resNodeConnect hashmap
	 */
	public static void readResNodeConnect() {
		String resName;
		GridReservoir thisGridRes;
		GridNode thisNode;
		int intNodeNum, extNodeNum, resNodeIndex;
		Variable resNodeConnectVar;
		Structure structure;
		ArrayStructureBB arrayStructureBB;
		List<Member> members;
		Member nameMember, intNodeNumMember, extNodeNumMember, resNodeIndexMember;
		ArrayChar.D1 nameData;
		ArrayInt.D0 intNodeNumData, extNodeNumData, resNodeIndexData;
		Map<String, List<Integer>> thisRes;
		
		resNodeConnect = new HashMap<>();
		
		try {

			resNodeConnectVar = ncd.findVariable("/hydro/geometry/reservoir_node_connect");
			structure = (Structure) resNodeConnectVar;
			arrayStructureBB = (ArrayStructureBB) structure.read();
			members = arrayStructureBB.getMembers();

			nameMember = members.get(1);
			intNodeNumMember = members.get(4);
			extNodeNumMember = members.get(5);
			resNodeIndexMember = members.get(0);			

			for (int i=0; i<arrayStructureBB.getSize(); i++) {
				nameData = (ArrayChar.D1) arrayStructureBB.getArray(i, nameMember);
				intNodeNumData = (ArrayInt.D0) arrayStructureBB.getArray(i, intNodeNumMember);
				extNodeNumData = (ArrayInt.D0) arrayStructureBB.getArray(i, extNodeNumMember); 
				resNodeIndexData = (ArrayInt.D0) arrayStructureBB.getArray(i, resNodeIndexMember);
				
				resName = nameData.getString().trim().toUpperCase();
				intNodeNum = intNodeNumData.get();
				extNodeNum = extNodeNumData.get();
				resNodeIndex = resNodeIndexData.get();
				
				thisGridRes = Grid.getReservoir(resName);
				thisGridRes.addExtNodeNum(extNodeNum);
				thisNode = Grid.createNode(extNodeNum);
				thisNode.addReservoir(thisGridRes);
								
				if(!resNodeConnect.containsKey(resName)) {
					thisRes = new HashMap<>();
					thisRes.put("nodeNum", new ArrayList<Integer>());
					thisRes.put("flowIndex", new ArrayList<Integer>());
					thisRes.put("flowType", new ArrayList<Integer>());
					resNodeConnect.put(resName, thisRes);
				}
				
				resNodeConnect.get(resName).get("nodeNum").add(intNodeNum);
				resNodeConnect.get(resName).get("flowIndex").add(resNodeIndex);
				resNodeConnect.get(resName).get("flowType").add(RES_FLOW);
			}
		} catch (IOException ioe) {
			PTMUtil.systemExit("Exception: " + ioe);
		}
	}
	
	/**
	 * Read the reservoir_flow_connections table from the tidefile and store in the resNodeConnect hashmap
	 */
	public static void readResFlowConnect() {
		String resName, flowName, flowType;
		GridReservoir thisGridRes;
		GridNode thisNode;
		GridBoundary thisBoundary;
		int extNodeNum, flowIndex;
		Variable resFlowConnectVar;
		Structure structure;
		ArrayStructureBB arrayStructureBB;
		List<Member> members;
		Member nameMember, flowIndexMember, flowNameMember, flowTypeMember;
		ArrayChar.D1 nameData, flowNameData, flowTypeData;
		ArrayInt.D0 flowIndexData;
		Map<String, List<Integer>> thisRes;
		
		try {

			resFlowConnectVar = ncd.findVariable("/hydro/geometry/reservoir_flow_connections");
			structure = (Structure) resFlowConnectVar;
			arrayStructureBB = (ArrayStructureBB) structure.read();
			members = arrayStructureBB.getMembers();

			nameMember = members.get(1);
			flowIndexMember = members.get(4);
			flowNameMember = members.get(5);
			flowTypeMember = members.get(6);

			for (int i=0; i<arrayStructureBB.getSize(); i++) {
				nameData = (ArrayChar.D1) arrayStructureBB.getArray(i, nameMember);
				flowIndexData = (ArrayInt.D0) arrayStructureBB.getArray(i, flowIndexMember);
				flowNameData = (ArrayChar.D1) arrayStructureBB.getArray(i, flowNameMember);
				flowTypeData = (ArrayChar.D1) arrayStructureBB.getArray(i, flowTypeMember); 
				
				resName = nameData.getString().trim().toUpperCase();
				flowName = flowNameData.getString().trim().toUpperCase();
				flowType = flowTypeData.getString().trim();
				flowIndex = flowIndexData.get();
				
				if(!resNodeConnect.containsKey(resName)) {
					thisRes = new HashMap<>();
					thisRes.put("nodeNum", new ArrayList<Integer>());
					thisRes.put("flowIndex", new ArrayList<Integer>());
					thisRes.put("flowType", new ArrayList<Integer>());
					resNodeConnect.put(resName, thisRes);
				}
				
				if(flowType.equalsIgnoreCase("qext")) {
					extNodeNum = PTMFixedData.getQextNodeNum(flowName);
					
					thisGridRes = Grid.getReservoir(resName);
					thisGridRes.addExtNodeNum(extNodeNum);
					thisNode = Grid.createNode(extNodeNum);
					thisNode.addReservoir(thisGridRes);
					thisBoundary = new GridBoundary(flowName, extNodeNum, PTMFixedData.getIntQextNum(flowName));
					thisNode.addBoundary(thisBoundary);
					addBoundary(thisBoundary);
					
					// Add this hidden node to intNodeNums (if it's not in intNodeNums already, that implies
					// that it was just added to extNodeNums, so the order of the two lists should be synced).
					if(!intNodeNums.contains(extNodeNum)) {intNodeNums.add(extNodeNum);}

					resNodeConnect.get(resName).get("nodeNum").add(extNodeNum);
					resNodeConnect.get(resName).get("flowIndex").add(flowIndex);
					resNodeConnect.get(resName).get("flowType").add(QEXT_FLOW);
				}
				else if(flowType.equalsIgnoreCase("transfer")) {
					// Currently only supporting a single node number associated with each conveyor => index = 0
					extNodeNum = PTMFixedData.getConveyorNodeNum(flowName, 0);
					
					resNodeConnect.get(resName).get("nodeNum").add(extNodeNum);
					resNodeConnect.get(resName).get("flowIndex").add(flowIndex);
					resNodeConnect.get(resName).get("flowType").add(CONVEYOR_FLOW);
				}
			}
		} catch (IOException ioe) {
			PTMUtil.systemExit("Exception: " + ioe);
		}
	}
	
	/**
	 * Obtain the resNodeConnect hashmap
	 * @return						resNodeConnect hashmap
	 */
	public static Map<String, Map<String, List<Integer>>> getResNodeConnect() {
		return resNodeConnect;
	}
	
	/**
	 * Add a GridChannel object to the grid
	 * @param thisChannel			channel object
	 */
	public static void addChannel(GridChannel thisChannel) {
		GridNode upNode, downNode;
		
		channels.add(thisChannel);
		extChanNums.add(thisChannel.getExtChanNum());
		
		upNode = createNode(thisChannel.getExtUpNode());
		downNode = createNode(thisChannel.getExtDownNode());
		thisChannel.addNodes(upNode, downNode);
		if(upNode!=null) {upNode.addChannel(thisChannel);}
		if(downNode!=null) {downNode.addChannel(thisChannel);}
	}
	
	/**
	 * Add a stage boundary object to the grid
	 * @param stageBoundary			stage boundary object (GridBoundary)
	 */
	public static void addStageBoundary(GridBoundary stageBoundary) {
		if(!stageBoundaries.contains(stageBoundary)) {stageBoundaries.add(stageBoundary);}
	}

	/**
	 * Add a boundary object to the grid
	 * @param boundary				boundary object (GridBoundary)
	 */
	public static void addBoundary(GridBoundary boundary) {
		if(!boundaries.contains(boundary)) {boundaries.add(boundary);}
	}
	
	/**
	 * Return a boolean indicating whether the grid contains the specified external node number
	 * @param extNodeNum			external node number
	 * @return						boolean indicating whether the grid contains the external node number
	 */
	public static boolean containsNode(int extNodeNum) {
		return extNodeNums.contains(extNodeNum);
	}
	
	/**
	 * Create a new GridNode object if it doesn't already exist and add it to the grid
	 * @param extNodeNum			external node number
	 * @return						new or existing GridNode object
	 */
	public static GridNode createNode(int extNodeNum) {
		GridNode tempNode = null;
		
		if(!containsNode(extNodeNum)) {
			tempNode = new GridNode(extNodeNum);
			nodes.add(tempNode);
			extNodeNums.add(tempNode.getExtNodeNum());
		}
		else {
			tempNode = nodes.get(extNodeNums.indexOf(extNodeNum));
		}
		
		return tempNode;
	}
	
	/**
	 * Add all waterbodies to lists of nodes; create reservoirs and boundaries
	 */
	public static void addWaterbodiesToNodes() {
		int thisExtNodeNum;
		String thisName;
		GridNode thisNode;
		GridBoundary thisBoundary;
		
		for(GridChannel thisChannel : channels) {
			thisChannel.setIntChanNum();
		}
		for(GridReservoir thisReservoir : reservoirs) {
			thisReservoir.setReservoirNum();
		}
		for(int i=0; i<extNodeNums.size(); i++) {
			intNodeNums.add(PTMFixedData.getIntNodeNum(extNodeNums.get(i)));
		}
		for(int i=0; i<reservoirs.size(); i++) {
			intResNums.add(PTMFixedData.getIntResNum(reservoirs.get(i).getName()));
		}
		for(GridConveyor thisConveyor : conveyors) {
			thisConveyor.setConveyorNum();
			thisConveyor.addNodesToReservoirs();
		}
		for(GridReservoir thisReservoir : reservoirs) {
			for(int extNodeNum : thisReservoir.getExtNodeNums()) {
				thisNode = Grid.getIntNode(PTMFixedData.getIntNodeNum(extNodeNum));
				thisNode.addReservoir(thisReservoir);
			}
		}
		for(String stageBoundaryName : PTMFixedData.getStageBoundaryNames()) {
			thisExtNodeNum = PTMFixedData.getStageBoundaryExtNode(stageBoundaryName);
			thisNode = Grid.getIntNode(PTMFixedData.getIntNodeNum(thisExtNodeNum));
			thisBoundary = new GridBoundary(stageBoundaryName, thisNode.getExtNodeNum(), 
					PTMFixedData.getIntStageBoundaryNum(stageBoundaryName));
			thisNode.addStageBoundary(thisBoundary);
			thisNode.setBoundaryType("STAGE");
			addStageBoundary(thisBoundary);
		}
		for(Map.Entry<String, Integer> entry : PTMFixedData.getQextExtNodeNums().entrySet()) {
			thisName = entry.getKey();
			thisExtNodeNum = entry.getValue();
			
			if(PTMFixedData.extNodeNumsContains(thisExtNodeNum)) {
				thisNode = Grid.getIntNode(PTMFixedData.getIntNodeNum(thisExtNodeNum));
				thisBoundary = new GridBoundary(thisName, thisNode.getExtNodeNum(), PTMFixedData.getIntQextNum(thisName));
				thisNode.addBoundary(thisBoundary);
				addBoundary(thisBoundary);
			}
		}
		for(GridConveyor thisConveyor : conveyors) {
			for(int intNodeNum : thisConveyor.getNodeArray()) {
				thisNode = Grid.getIntNode(intNodeNum);				
				thisNode.addConveyor(thisConveyor);
			} 
		}
	}
	
	/**
	 * Obtain GridNode object for the specified internal node number
	 * @param intNodeNum			internal node number
	 * @return						GridNode object
	 */
	public static GridNode getIntNode(int intNodeNum) {
		if(intNodeNums.contains(intNodeNum)) {return nodes.get(intNodeNums.indexOf(intNodeNum));}
		else {return null;}
	}

	/**
	 * Add a GridReservoir object to the grid
	 * @param thisReservoir			GridReservoir object
	 */
	public static void addReservoir(GridReservoir thisReservoir) {
		reservoirs.add(thisReservoir);
	}
	
	/**
	 * Obtain GridReservoir object for the specified name
	 * @param resName				reservoir name
	 * @return						GriReservoir object
	 */
	public static GridReservoir getReservoir(String resName) {
		int intResNum;
		
		resName = resName.toUpperCase();
		intResNum = PTMFixedData.getIntResNum(resName);
		return reservoirs.get(intResNums.indexOf(intResNum));
	}
	
	/**
	 * Add a GridConveyor object to the grid
	 * @param thisConveyor			GridConveyor object
	 */
	public static void addConveyor(GridConveyor thisConveyor) {
		conveyors.add(thisConveyor);
	}

	/**
	 * Create placeholder objects to fill empty spots in waterbody lists
	 */
	public static void createPlaceholderWaterbodies() {
		List<Integer> channelNums, reservoirNums, stageBoundaryNums, boundaryNums, conveyorNums;
		int thisWaterbodyNum;
		GridChannel tempChannel;
		GridReservoir tempReservoir;
		GridBoundary tempBoundary;
		GridConveyor tempConveyor; 
				
		// Create all possible channel objects if they weren't already created
		channelNums = new ArrayList<>();
		for(GridChannel thisChannel : channels) {
			channelNums.add(thisChannel.getIntChanNum());
		}
		
		for(int localIndex=1; localIndex<=PTMFixedData.getMaxNumberOfChannels(); localIndex++) {
			thisWaterbodyNum = PTMFixedData.getUniqueIdForChannel(localIndex);
			if(!channelNums.contains(thisWaterbodyNum)) {
				tempChannel = new GridChannel(Grid.MISSING, Grid.MISSING, Grid.MISSING);
				tempChannel.setIntChanNum(thisWaterbodyNum);
				tempChannel.setIsPlaceholder(true);
				channels.add(tempChannel);
			}
		}
		
		// Create all possible reservoir objects if they weren't already created
		reservoirNums = new ArrayList<>();
		for(GridReservoir thisReservoir : reservoirs) {
			reservoirNums.add(thisReservoir.getReservoirNum());
		}
		
		for(int localIndex=1; localIndex<=PTMFixedData.getMaxNumberOfReservoirs(); localIndex++) {
			thisWaterbodyNum = PTMFixedData.getUniqueIdForReservoir(localIndex);
			if(!reservoirNums.contains(thisWaterbodyNum)) {
				tempReservoir = new GridReservoir("RESERVOIR");
				tempReservoir.setReservoirNum(thisWaterbodyNum);
				tempReservoir.setIsPlaceholder(true);
				reservoirs.add(tempReservoir);
			}
		}
		
		// Create all possible stage boundary objects if they weren't already created
		stageBoundaryNums = new ArrayList<>();
		for(GridBoundary thisStageBoundary : stageBoundaries) {
			stageBoundaryNums.add(thisStageBoundary.getBoundaryNum());
		}
		
		for(int localIndex=1; localIndex<=PTMFixedData.getMaxNumberOfStageBoundaries(); localIndex++) {
			thisWaterbodyNum = PTMFixedData.getUniqueIdForStageBoundary(localIndex);
			if(!stageBoundaryNums.contains(thisWaterbodyNum)) {
				tempBoundary = new GridBoundary("STAGE_BOUNDARY", Grid.MISSING, thisWaterbodyNum);
				tempBoundary.setIsPlaceholder(true);
				stageBoundaries.add(tempBoundary);
			}
		}
		
		// Create all possible boundary objects if they weren't already created
		boundaryNums = new ArrayList<>();
		for(GridBoundary thisBoundary : boundaries) {
			boundaryNums.add(thisBoundary.getBoundaryNum());
		}
		
		for(int localIndex=1; localIndex<=PTMFixedData.getMaxNumberOfBoundaryWaterbodies(); localIndex++) {
			thisWaterbodyNum = PTMFixedData.getUniqueIdForBoundary(localIndex);
			if(!boundaryNums.contains(thisWaterbodyNum)) {
				tempBoundary = new GridBoundary("BOUNDARY", Grid.MISSING, thisWaterbodyNum);
				tempBoundary.setIsPlaceholder(true);
				boundaries.add(tempBoundary);
			}
		}
		
		// Create all possible conveyor objects if they weren't already created
		conveyorNums = new ArrayList<>();
		for(GridConveyor thisConveyor : conveyors) {
			conveyorNums.add(thisConveyor.getConveyorNum());
		}
		
		for(int localIndex=1; localIndex<=PTMFixedData.getMaxNumberOfConveyors(); localIndex++) {
			thisWaterbodyNum = PTMFixedData.getUniqueIdForConveyor(localIndex);
			if(!conveyorNums.contains(thisWaterbodyNum)) {
				tempConveyor = new GridConveyor("CONVEYOR");
				tempConveyor.setConveyorNum(thisWaterbodyNum);
				tempConveyor.setIsPlaceholder(true);
				conveyors.add(tempConveyor);
			}
		}
	}
	
	/**
	 * Return boolean indicating whether the waterbody corresponding to the specified internal number is not a placeholder
	 * @param intNum				internal waterbody number
	 * @return						boolean indicating whether waterbody is a not placeholder
	 */
	public static boolean waterbodyIsNotPlaceholder(int intNum) {
		return !waterbodies.get(intNum).getIsPlaceholder();
	}
	
	public static void compileWaterbodies() {
		waterbodies = new HashMap<>();
		for(GridChannel thisChannel : channels) {
			waterbodies.put(thisChannel.getIntChanNum(), thisChannel);
		}
		for(GridReservoir thisReservoir : reservoirs) {
			waterbodies.put(thisReservoir.getReservoirNum(), thisReservoir);
		}
		for(GridBoundary thisStageBoundary : stageBoundaries) {
			waterbodies.put(thisStageBoundary.getBoundaryNum(), thisStageBoundary);
		}
		for(GridBoundary thisBoundary : boundaries) {
			waterbodies.put(thisBoundary.getBoundaryNum(), thisBoundary);
		}
		for(GridConveyor thisConveyor : conveyors) {
			waterbodies.put(thisConveyor.getConveyorNum(), thisConveyor);
		}
	}
	
	/**
	 * Obtain waterbody type code for the specified internal waterbody number
	 * @param intNum				internal waterbody number
	 * @return						waterbody type code
	 */
	public static int getWaterbodyObjectTypeCode(int intNum) {
		return waterbodies.get(intNum).getTypeCode();
	}
	
	/**
	 * Obtain waterbody type for the specified internal waterbody number
	 * @param intNum				internal waterbody number
	 * @return						waterbody type
	 */
	public static int getWaterbodyObjectType(int intNum) {
		return waterbodies.get(intNum).getType();
	}
	
	/**
	 * Obtain node internal number array for the specified internal waterbody number
	 * @param intNum				internal waterbody number
	 * @return						node internal number array
	 */
	public static int[] getNodeArrayForWaterbody(int intNum) {
		return waterbodies.get(intNum).getNodeArray();
	}
	
	/**
	 * Obtain boundary type for the specified internal node number
	 * @param intNum				internal node number
	 * @return						boundary type
	 */
	public static String getBoundaryTypeForNode(int intNum) {
		return nodes.get(intNodeNums.indexOf(intNum)).getBoundaryType();
	}
}
