package DWR.DMS.PTM;
public class pumpFixedData extends waterbodyFixedData{

public pumpFixedData(int id, int[] nodeArray){
  super(waterbody.PUMP, id, nodeArray);
}
public String toString(){
  return "Pump Fixed Data: Id " + id;
}

}
