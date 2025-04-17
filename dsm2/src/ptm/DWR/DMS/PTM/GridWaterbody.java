package DWR.DMS.PTM;

/**
 * Abstract class for Grid waterbody objects.
 * 
 * @author Doug Jackson (QEDA Consulting, LLC)
 *
 */
public abstract class GridWaterbody {
	
	protected int type;
	protected int typeCode;
	protected boolean isPlaceholder;
	
	/**
	 * Obtain the type of this waterbody
	 * @return						type
	 */
	public int getType() {
		return type;
	}
	
	/**
	 * Obtain the type code of this waterbody
	 * @return						type code
	 */
	public int getTypeCode() {
		return typeCode;
	}
	
	/**
	 * Obtain an array of internal node numbers
	 * @return						array of internal node numbers
	 */
	public abstract int[] getNodeArray();
	
	/**
	 * Set boolean indicating whether this waterbody is a placeholder or not
	 * @param p						boolean (true = is placeholder)
	 */
	public void setIsPlaceholder(boolean p) {
		isPlaceholder = p;
	}
	
	/**
	 * Return boolean indicating whether this waterbody is a placeholder or not
	 * @return						boolean (true = is placeholder)
	 */
	public boolean getIsPlaceholder() {
		return isPlaceholder;
	}

}
