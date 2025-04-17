 package DWR.DMS.PTM;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Object corresponding to a node in the DSM2 HYDRO grid
 * 
 * @author Doug Jackson (QEDA Consulting, LLC)
 *
 */
public class GridNode {
	
	public final int type = Grid.OBJ_NODE;

	private int extNodeNum;
	private List<GridChannel> channels;
	private List<GridReservoir> reservoirs;
	private List<GridConveyor> conveyors;
	private List<GridBoundary> stageBoundaries;
	private List<GridBoundary> boundaries;
	private String boundaryType;
	
	public GridNode(int extNodeNum) {
		this.extNodeNum = extNodeNum;
		
		channels = new ArrayList<>();
		reservoirs = new ArrayList<>();
		conveyors = new ArrayList<>();
		stageBoundaries = new ArrayList<>();
		boundaries = new ArrayList<>();
		
		boundaryType = "";
	}
	
	/**
	 * Obtain external node number
	 * @return						external node number
	 */
	public int getExtNodeNum() {
		return extNodeNum;
	}
	
	/**
	 * Add a GridChannel object to the list of channels connected to this node
	 * @param channel				GridChannel object
	 */
	public void addChannel(GridChannel channel) {
		if(!channels.contains(channel)) {channels.add(channel);}
	}
	
	/**
	 * Add a GridReservoir object to the list of reservoirs connected to this node
	 * @param reservoir				GridReservoir object
	 */
	public void addReservoir(GridReservoir reservoir) {
		if(!reservoirs.contains(reservoir)) {reservoirs.add(reservoir);}
	}
	
	/**
	 * Add a GridConveyor object to the list of conveyors connected to this node
	 * @param conveyor				GridConveyor object
	 */
	public void addConveyor(GridConveyor conveyor) {
		if(!conveyors.contains(conveyor)) {conveyors.add(conveyor);}
	}
	
	/**
	 * Add a GridBoundary object to the list of stage boundaries connected to this node
	 * @param stageBoundary			GridBoundary object representing a stage boundary
	 */
	public void addStageBoundary(GridBoundary stageBoundary) {
		if(!stageBoundaries.contains(stageBoundary)) {stageBoundaries.add(stageBoundary);}
	}

	/**
	 * Add a GridBoundary object to the list of boundaries connected to this node
	 * @param boundary				GridBoundary object representing a boundary
	 */
	public void addBoundary(GridBoundary boundary) {
		if(!boundaries.contains(boundary)) {boundaries.add(boundary);}
	}
	
	/**
	 * Obtain the array of internal waterbody numbers connected to this node
	 * @return						array of internal waterbody numbers
	 */
	public int[] getWaterbodyIdArray() {
		List<Integer> waterbodyIdArray;
		
		waterbodyIdArray = new ArrayList<Integer>();
		for(GridChannel thisChannel : channels) {
			waterbodyIdArray.add(thisChannel.getIntChanNum());
		}
		for(GridReservoir thisReservoir : reservoirs) {
			waterbodyIdArray.add(thisReservoir.getReservoirNum());
		}
		for(GridBoundary thisStageBoundary : stageBoundaries) {
			waterbodyIdArray.add(thisStageBoundary.getBoundaryNum());
		}
		for(GridBoundary thisBoundary : boundaries) {
			waterbodyIdArray.add(thisBoundary.getBoundaryNum());
		}
		for(GridConveyor thisConveyor : conveyors) {
			waterbodyIdArray.add(thisConveyor.getConveyorNum());
		}
		
		Collections.sort(waterbodyIdArray);
		
		return waterbodyIdArray.stream().mapToInt(Integer::intValue).toArray();
	}
	
	/**
	 * Obtain internal node number
	 * @return					internal node number
	 */
	public int getIntNodeNum() {
		return PTMFixedData.getIntNodeNum(extNodeNum);
	}
	
	/**
	 * Set boundary type
	 * @param b					boundary type
	 */
	public void setBoundaryType(String b) {
		boundaryType = b;
	}
	
	/**
	 * Obtain boundary type
	 * @return					boundary type
	 */
	public String getBoundaryType() {
		return boundaryType;
	}
	
}
