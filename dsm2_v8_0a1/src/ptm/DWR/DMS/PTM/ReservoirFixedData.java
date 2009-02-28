package DWR.DMS.PTM;
public class ReservoirFixedData extends WaterbodyFixedData{

public ReservoirFixedData(int id, String name,
			  float area, float bottomElevation,
			  int[] nodeArray){
  super(Waterbody.RESERVOIR, id, nodeArray);
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
