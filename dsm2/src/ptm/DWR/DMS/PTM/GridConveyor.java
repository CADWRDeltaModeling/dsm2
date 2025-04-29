package DWR.DMS.PTM;

import java.util.ArrayList;
import java.util.List;

/**
 * Object corresponding to a conveyor in the DSM2 HYDRO grid
 * 
 * @author Doug Jackson (QEDA Consulting, LLC)
 *
 */
public class GridConveyor extends GridWaterbody {

	private String name;
	private int conveyorNum;
	private int[] nodeArray;
	private String[] connectionTypes;
	private String[] reservoirNames;
	
	public GridConveyor(String name) {
		this.name = name;
		
		type = Grid.OBJ_CONVEYOR;
		typeCode = Grid.fluxTypes.get("CONVEYOR");
		
		// Assume this is not a placeholder conveyor if not otherwise specified
		isPlaceholder = false;
		
		// Initialize nodeArray
		nodeArray = null;
	}
	
	public GridConveyor(String name, String fromType, String fromID, String toType, String toID) {
		int newNodeNum;
		
		this.name = name;
		
		type = Grid.OBJ_CONVEYOR;
		typeCode = Grid.fluxTypes.get("CONVEYOR");
		
		nodeArray = new int[] {Grid.MISSING, Grid.MISSING};
		connectionTypes = new String[] {fromType, toType};
		reservoirNames = new String[] {null, null};
		
		// Set from node number
		if(fromType.equalsIgnoreCase("node")) {
			nodeArray[0] = PTMFixedData.getIntNodeNum(Integer.parseInt(fromID));
		}
		else if(fromType.equalsIgnoreCase("reservoir")) {
			newNodeNum = PTMFixedData.createVirtualNode();
			Grid.createNode(newNodeNum);
			nodeArray[0] = newNodeNum;
			
			reservoirNames[0] = fromID;
		}
		
		// Set to node number
		if(toType.equalsIgnoreCase("node")) {
			nodeArray[1] = PTMFixedData.getIntNodeNum(Integer.parseInt(toID));
		}
		else if(toType.equalsIgnoreCase("reservoir")) {
			newNodeNum = PTMFixedData.createVirtualNode();
			Grid.createNode(newNodeNum);
			nodeArray[1] = newNodeNum;
			
			reservoirNames[1] = toID;
			
			// Add this new virtual node to PTMFixedData.conveyorNodeNums
			PTMFixedData.addConveyorNodeNum(name, newNodeNum);
		}
	}
	
	/**
	 * Obtain the conveyor number
	 * @return						conveyor number
	 */
	public int getConveyorNum() {
		return conveyorNum;
	}
	
	/**
	 * Set the conveyor number based on reservoir name
	 */
	public void setConveyorNum() {
		conveyorNum = PTMFixedData.getIntConveyorNum(name);
	}
	
	/**
	 * Set the conveyor number using the specified conveyor number
	 * @param conveyorNum			conveyor number
	 */
	public void setConveyorNum(int conveyorNum) {
		this.conveyorNum = conveyorNum;
	}

	public void addNodesToReservoirs() {
		GridReservoir tempReservoir;
		
		for(int i=0; i<2; i++) {
			if(connectionTypes[i].equalsIgnoreCase("reservoir")) {
				tempReservoir = Grid.getReservoir(reservoirNames[i]);
				tempReservoir.addExtNodeNum(nodeArray[i]);
			}
		}
	}
	
	@Override
	public int[] getNodeArray() {
		return nodeArray;
	}
}
