package DWR.DMS.PTM;
class limitsFixedData {
public limitsFixedData(int maxChannels, 
		       int maxReservoirs, 
		       int maxDiversions, 
		       int maxPumps, 
		       int maxBoundaryWaterbodies, 
		       int maxNodes, 
		       int maxXSections){
  this.maxChannels =  maxChannels; 
  this.maxReservoirs =  maxReservoirs; 
  this.maxDiversions =  maxDiversions; 
  this.maxPumps =  maxPumps; 
  this.maxBoundaryWaterbodies =  maxBoundaryWaterbodies; 
  this.maxNodes =  maxNodes; 
  this.maxXSections =  maxXSections;
}

public String toString(){
  return "limitsFixedData: \n";
}

public int getMaximumNumberOfChannels(){
  return maxChannels;
}
public int getMaximumNumberOfReservoirs(){
  return maxReservoirs;
}
public int getMaximumNumberOfDiversions(){
  return maxDiversions;
}
public int getMaximumNumberOfPumps(){
  return maxPumps;
}
public int getMaximumNumberOfNodes(){
  return maxNodes;
}
public int getMaximumNumberOfBoundaryWaterbodies(){
  return maxBoundaryWaterbodies;
}
public int getMaximumNumberOfXSections(){
  return maxXSections;
}
public int getMaximumNumberOfReservoirNodes(){
  return 20;
}
protected int maxChannels; 
protected int maxReservoirs; 
protected int maxDiversions; 
protected int maxPumps; 
protected int maxBoundaryWaterbodies; 
protected int maxNodes; 
protected int maxXSections;           
}
