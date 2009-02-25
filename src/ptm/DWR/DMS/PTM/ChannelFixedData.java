package DWR.DMS.PTM;
public class ChannelFixedData extends WaterbodyFixedData{

public ChannelFixedData(int id, int length,
			int[] xSectionIds, float[] xSectionDistance,
			int[] nodeArray){
  super(Waterbody.CHANNEL, id, nodeArray);

  this.length = length;
  this.xSectionIds = xSectionIds;
  this.xSectionDistance = xSectionDistance;
}
@Override
public String toString(){
  return "ChannelFixedData: Number "+id
    + " Type " + type
    + " Length: " + length;
}
public int getLength(){
  return length;
}
public int[] getXSectionIds(){
  return xSectionIds;
}
public float[] getXSectionDistance(){
  return xSectionDistance;
}
public int length;
public int[] xSectionIds;
public float[] xSectionDistance;
}
