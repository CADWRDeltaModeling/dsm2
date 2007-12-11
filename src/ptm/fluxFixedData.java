package DWR.DMS.PTM;
class fluxFixedData {
  
public fluxFixedData(group inGroup, group outGroup){
  this.nodeId = -1;
  this.inGroup = inGroup;
  this.outGroup = outGroup;
}

public group getInGroup(){return inGroup;}
public group getOutGroup(){return outGroup;}
public int getNodeId(){return nodeId;}
/**
 *
 */
public String toString(){
  StringBuffer buf = new StringBuffer("Flux Data");
  buf.append(" In Group: ").append(inGroup.toString());
  buf.append(" Out Group: "+outGroup.toString()+" Node Id: " + nodeId);
  return buf.toString();
}
public int nodeId;
private group inGroup;
private group outGroup;

}
