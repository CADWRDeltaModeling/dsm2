package DWR.DMS.PTM;
public class nodeNotFoundException extends PTMException{
public nodeNotFoundException(String msg){
  super(msg);
  System.out.println("Node for which information is requested is not applicable");
}
}
