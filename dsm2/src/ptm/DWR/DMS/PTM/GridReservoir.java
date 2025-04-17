package DWR.DMS.PTM;

import java.util.ArrayList;
import java.util.List;

/**
 * Object corresponding to a reservoir in the DSM2 HYDRO grid
 * 
 * @author Doug Jackson (QEDA Consulting, LLC)
 *
 */
public class GridReservoir extends GridWaterbody {

	private String name;
	private int reservoirNum;
	private List<Integer> extNodeNums;
	
	public GridReservoir(String name) {
		this.name = name;
		
		extNodeNums = new ArrayList<>();
		
		type = Grid.OBJ_RESERVOIR;
		typeCode = Grid.fluxTypes.get("RES");
		
		// Assume this is not a placeholder reservoir if not otherwise specified
		isPlaceholder = false;
	}

	/**
	 * Obtain name of reservoir
	 * @return						name
	 */
	public String getName() { 
		return name;
	}
	
	/**
	 * Add the specified external node number to the list of nodes connected to this reservoir
	 * @param extNodeNum			external node number
	 */
	public void addExtNodeNum(int extNodeNum) {
		if(!extNodeNums.contains(extNodeNum)) {extNodeNums.add(extNodeNum);}
	}
	
	/**
	 * Obtain the list of external node numbers for nodes attached to this reservoir
	 * @return						list of external node numbers
	 */
	public List<Integer> getExtNodeNums() {
		return extNodeNums;
	}
	
	/**
	 * Obtain the reservoir number
	 * @return						reservoir number
	 */
	public int getReservoirNum() {
		return reservoirNum;
	}
	
	/**
	 * Set the reservoir number based on reservoir name
	 */
	public void setReservoirNum() {
		reservoirNum = PTMFixedData.getIntResNum(name);
	}
	
	/**
	 * Set the reservoir number using the specified reservoir number
	 * @param reservoirNum			reservoir number
	 */
	public void setReservoirNum(int reservoirNum) {
		this.reservoirNum = reservoirNum;
	}

	@Override
	public int[] getNodeArray() {
		List<Integer> nodeArray;
		
		nodeArray = new ArrayList<>();
		for(int extNodeNum : extNodeNums) {
			nodeArray.add(PTMFixedData.getIntNodeNum(extNodeNum));
		}
		return nodeArray.stream().mapToInt(Integer::intValue).toArray();
	}
}
