package DWR.DMS.PTM.behave;

public class Behavior {
  public Behavior (){
    
  }

  public void setRelevant(boolean relevance){
    _relevance = relevance;
  }

  public boolean isRelevant() {
    return _relevance;
  }
  
  public float timeConvert(float time, int units) {
    return time;
  }

  boolean _relevance;

  //  public ElementNode 
}
