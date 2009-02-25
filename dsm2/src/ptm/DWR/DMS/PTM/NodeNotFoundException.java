package DWR.DMS.PTM;
public class NodeNotFoundException extends PTMException{
public NodeNotFoundException(String msg){
  super(msg);
  System.out.println("Node for which information is requested is not applicable");
}
}
