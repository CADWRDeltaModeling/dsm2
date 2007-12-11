package DWR.DMS.PTM;
public class timeFixedData {
public timeFixedData(int startTime, int endTime, int PTMTimeStep, int displayInterval){
  this.startTime = startTime;
  this.endTime = endTime;
  this.PTMTimeStep = PTMTimeStep;
  this.displayInterval = displayInterval;
}
public String toString(){
  String rep = "Time Fixed Data \n";
  rep += "Start Time = " + startTime + "\n";
  rep += "End Time = " + endTime + "\n";
  rep += "PTM Time Step = " + PTMTimeStep + "\n";
  rep += "Display Interval = " + displayInterval + "\n";
  return rep;
}
public int getDisplayInterval(){
  return displayInterval;
}
public int getStartTime(){
  return startTime;
}
public int getEndTime(){
  return endTime;
}
public int getRunLength(){
  return endTime-startTime;
}
public int getPTMTimeStep(){
  return PTMTimeStep;
}
protected int startTime;
protected int endTime;
protected int PTMTimeStep;
protected int displayInterval;
}
