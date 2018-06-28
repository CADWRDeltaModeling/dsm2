package DWR.DMS.PTM;
public class pumpFixedData extends WaterbodyFixedData{

public pumpFixedData(int id, int[] nodeArray){
  super(Waterbody.PUMP, id, nodeArray);
}
public String toString(){
  return "Pump Fixed Data: Id " + id;
}

}
