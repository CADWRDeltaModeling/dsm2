package DWR.DMS.PTM;
/**
 * Encapsulates node creation and initialization information
 */
public class nodeFixedData{
  
  /**
   * constructor
   */
public nodeFixedData(int id, String boundaryType, 
		     int[] waterbodyIdArray){
  this.id = id;
  this.boundaryType = boundaryType;
  this.waterbodyIdArray = waterbodyIdArray;
}
  /**
   *
   */
public String toString(){
  return "NodeFixedData: Number "+id
    + " Type " + boundaryType;
}

  /**
   *
   */
public int getNumberOfWaterbodies(){
  return waterbodyIdArray.length;
}

  /**
   *
   */
public String getBoundaryType(){
  return boundaryType;
}

  /**
   *
   */
public int[] getWaterbodyIdArray(){
  return waterbodyIdArray;
}

  /**
   *
   */
protected int id;
  /**
   *
   */
protected String boundaryType;
  /**
   *
   */
protected int[] waterbodyIdArray;
}
