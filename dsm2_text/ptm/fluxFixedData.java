package DWR.DMS.PTM;
class fluxFixedData {
  
public fluxFixedData(int[] inArray, int[] inTypeArray, int[] inAccountTypeArray,
		     int[] outArray, int[] outTypeArray, int[] outAccountTypeArray){
  this.nodeId = -1;
  this.inArray = inArray;
  this.inTypeArray = inTypeArray;
  this.inAccountTypeArray = inAccountTypeArray;
  this.outArray = outArray;
  this.outTypeArray = outTypeArray;
  this.outAccountTypeArray = outAccountTypeArray;
}

public int getNumberIncoming(){
  return inArray.length;
}

public int getNumberOutgoing(){
  return outArray.length;
}
  /**
   *
   */
public String toString(){
  StringBuffer buf = new StringBuffer(200);
  String eol = "\n";
  buf.append("Flux Fixed Data: ").append(eol);
  buf.append("Node Id: " + nodeId).append(eol);
  if ( inArray != null ){
    for(int i=0; i < inArray.length; i++)
      buf.append("inArray[").append(i).append("]= ").append(inArray[i]).append(eol);
    for(int i=0; i < inArray.length; i++)
      buf.append("inTypeArray[").append(i).append("]= ").append(inArray[i]).append(eol);
    for(int i=0; i < inArray.length; i++)
      buf.append("inAccountTypeArray[").append(i).append("]= ").append(inArray[i]).append(eol);
  }
  if ( outArray != null ){
    for(int i=0; i < outArray.length; i++)
      buf.append("outArray[").append(i).append("]= ").append(outArray[i]).append(eol);
    for(int i=0; i < outArray.length; i++)
      buf.append("outTypeArray[").append(i).append("]= ").append(outArray[i]).append(eol);
    for(int i=0; i < outArray.length; i++)
      buf.append("inAccountTypeArray[").append(i).append("]= ").append(outArray[i]).append(eol);
  }
  return buf.toString();
}
public int nodeId;
public int[] inArray;
public int[] outArray;
public int[] inTypeArray;
public int[] outTypeArray;
public int[] inAccountTypeArray;
public int[] outAccountTypeArray;
}
