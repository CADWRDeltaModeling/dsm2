package DWR.DMS.PTM;
/**
 * encapsulates information for creating of waterbody
 */
public class waterbodyFixedData {
public waterbodyFixedData(int type, int id, int[] nodeIdArray){
  this.type = type;
  this.id = id;
  this.nodeIdArray = nodeIdArray;
}
public int getId(){
  return id;
}
public int[] getNodeArray(){
  return nodeIdArray;
}
public int type;
public int id;
public int[] nodeIdArray;
}
