package DWR.DMS.PTM;

/**
 * Object corresponding to a boundary in the DSM2 HYDRO grid
 * 
 * @author Doug Jackson (QEDA Consulting, LLC)
 *
 */
public class GridBoundary extends GridWaterbody {
	
	private String name;
	private int extNodeNum;
	private int boundaryNum;
	
	public GridBoundary(String name, int extNodeNum, int boundaryNum) {
		this.name = name;
		this.extNodeNum = extNodeNum;
		this.boundaryNum = boundaryNum;
		
		type = Grid.OBJ_QEXT;
		typeCode = Grid.fluxTypes.get("FLOW_BOUNDARY");
		
		// Assume this is not a placeholder boundary if not otherwise specified
		isPlaceholder = false;
	}

	/**
	 * Obtain the name of the boundary
	 * @return						name
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * Obtain the internal boundary number
	 * @return						internal boundary number
	 */
	public int getBoundaryNum() {
		return boundaryNum;
	}

	@Override
	public int[] getNodeArray() {
		if(extNodeNum==Grid.MISSING) {
			return new int[] {0};
		}
		return new int[] {PTMFixedData.getIntNodeNum(extNodeNum)};
	}
}
