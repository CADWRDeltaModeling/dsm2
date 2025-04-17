package DWR.DMS.PTM;

/**
 * Object corresponding to a channel in the DSM2 HYDRO grid
 * 
 * @author Doug Jackson (QEDA Consulting, LLC)
 *
 */
public class GridChannel extends GridWaterbody {
		
	private int extChanNum, extUpNode, extDownNode, intChanNum;
	private GridNode upNode, downNode;
	
	public GridChannel(int extChanNum, int extUpNode, int extDownNode) {
		this.extChanNum = extChanNum;
		this.extUpNode = extUpNode;
		this.extDownNode = extDownNode;
		
		upNode = null;
		downNode = null;
		
		type = Grid.OBJ_CHAN;
		typeCode = Grid.fluxTypes.get("CHAN");
		
		// Assume this is not a placeholder channel if not otherwise specified
		isPlaceholder = false;
	}
	
	/**
	 * Obtain external channel number
	 * @return						external channel number
	 */
	public int getExtChanNum() {
		return extChanNum;
	}
	
	/**
	 * Set internal channel number based on external channel number
	 */
	public void setIntChanNum() {
		intChanNum = PTMFixedData.getIntChanNum(extChanNum);
	}
	
	/**
	 * Set internal channel number using specified value
	 * @param intChanNum			internal channel number
	 */
	public void setIntChanNum(int intChanNum) {
		this.intChanNum = intChanNum;
	}
	
	/**
	 * Obtain the internal channel number
	 * @return						internal channel number
	 */
	public int getIntChanNum() {
		return intChanNum;
	}
	
	/**
	 * Obtain external number for upNode
	 * @return						external number for upNode
	 */
	public int getExtUpNode() {
		return extUpNode;
	}
	
	/**
	 * Obtain external number for downNode
	 * @return						external number for downNode
	 */
	public int getExtDownNode() {
		return extDownNode;
	}
	
	/**
	 * Add GridNode objects representing upNode and downNode
	 * @param uN					GridNode representing upNode
	 * @param dN					GridNode representing downNode
	 */
	public void addNodes(GridNode uN, GridNode dN) {
		upNode = uN;
		downNode = dN;
	}

	@Override
	public int[] getNodeArray() {
		return new int[] {upNode.getIntNodeNum(), downNode.getIntNodeNum()};
	}
}
