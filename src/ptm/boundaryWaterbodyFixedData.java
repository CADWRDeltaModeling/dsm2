package DWR.DMS.PTM;
public class boundaryWaterbodyFixedData extends waterbodyFixedData{

public boundaryWaterbodyFixedData(int id, int[] nodeArray){
  super(waterbody.BOUNDARY_WATERBODY, id, nodeArray);
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
