package DWR.DMS.PTM;
class FluxFixedData {

    public FluxFixedData(Group inGroup, Group outGroup){
      this.nodeId = -1;
      this.inGroup = inGroup;
      this.outGroup = outGroup;
    }

    public Group getInGroup(){return inGroup;}
    public Group getOutGroup(){return outGroup;}
    public int getNodeId(){return nodeId;}
    /**
     *
     */
    @Override
    public String toString(){
      StringBuffer buf = new StringBuffer("Flux Data");
      buf.append(" In Group: ").append(inGroup.toString());
      buf.append(" Out Group: "+outGroup.toString()+" Node Id: " + nodeId);
      return buf.toString();
    }
    public int nodeId;
    private Group inGroup;
    private Group outGroup;

}
