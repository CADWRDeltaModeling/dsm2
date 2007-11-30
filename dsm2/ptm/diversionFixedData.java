package DWR.DMS.PTM;
public class diversionFixedData extends waterbodyFixedData{

public diversionFixedData(int id, int[] nodeArray){
  super(waterbody.DIVERSION, id, nodeArray);
}
public String toString(){
  return "Diversion Fixed Data: Id " + id;
}
}
