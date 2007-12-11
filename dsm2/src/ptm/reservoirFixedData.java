package DWR.DMS.PTM;
public class reservoirFixedData extends waterbodyFixedData{

public reservoirFixedData(int id, String name,
			  float area, float bottomElevation,
			  int[] nodeArray){
  super(waterbody.RESERVOIR, id, nodeArray);
  this.name = name;
  this.area = area;
  this.bottomElevation = bottomElevation;
}
public String toString(){
  return "ReservoirFixedData: Number "+id
    + " Name: " + name
    + " Type " + type
    + " Area: " + area ;
}
public String getName(){
  return name;
}
public float getBottomElevation(){
  return bottomElevation;
}
public float getArea(){
  return area;
}
String name;
float area;
float bottomElevation;
}
