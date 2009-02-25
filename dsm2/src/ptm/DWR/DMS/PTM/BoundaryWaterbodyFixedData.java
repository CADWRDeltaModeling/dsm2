package DWR.DMS.PTM;
public class BoundaryWaterbodyFixedData extends WaterbodyFixedData{

public BoundaryWaterbodyFixedData(int id, int[] nodeArray){
  super(Waterbody.BOUNDARY_WATERBODY, id, nodeArray);
}

public String toString(){
  String rep = null;
  if(this != null){
    rep = "Boundary Waterbody Fixed Data: Id " + id;
    rep += "\n" + nodeIdArray[0];
  }
  return rep;
}

}
