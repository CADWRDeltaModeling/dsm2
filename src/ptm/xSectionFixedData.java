package DWR.DMS.PTM;
/**
 * encapsulates information for creating of xSection
 */
public class xSectionFixedData {

public xSectionFixedData(int id, int numberOfElevations, float distance,
			 float[] width, float[] area, float[] elevations, float minElevation){
  this.id = id;
  this.numberOfElevations = numberOfElevations;
  this.distance = distance;
  this.width = width;
  this.area = area;
  this.elevations = elevations;
  this.minElevation = minElevation;
}
public String toString(){
  String rep = "X Section Id: " + id;
  return rep;
}
public int id;
public int numberOfElevations;
public float distance;
public float[] width;
public float[] area;
public float[] elevations;
public float minElevation;
}
