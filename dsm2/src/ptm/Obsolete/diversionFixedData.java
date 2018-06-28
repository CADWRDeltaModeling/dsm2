package DWR.DMS.PTM;
public class diversionFixedData extends WaterbodyFixedData{

public diversionFixedData(int id, int[] nodeArray){
  super(Waterbody.DIVERSION, id, nodeArray);
}
public String toString(){
  return "Diversion Fixed Data: Id " + id;
}
}
