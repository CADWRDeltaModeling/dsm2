/*<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public !<license as published by
C!    the Free Software Foundation, either version 3 of the !<license, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public !<license for more details.

C!    You should have received a copy of the GNU General Public !<license
C!    along with DSM2.  If not, see <http://www.gnu.org/!<licenses/>.
</license>*/
package DWR.DMS.PTM;
/**
 *  This class controls the various classes for Flux output. It is responsible
 *  for reading in the trace of particles from the trace file and creating the
 *  Flux calculators as defined in the fixed input system. Finally it is responsible
 *  in handling PTMFluxOutput object to write out the Flux output.
 */

public class FluxMonitor{

  public static final boolean DEBUG = false;
  
  /**
   *  Constructor
   */
  public FluxMonitor(String fileName, int iType, FluxInfo fInfo, GroupInfo gInfo){
	  
    inputType = iType;
    traceFileName = fileName;
    fluxInfoPtr = fInfo;
    groupInfoPtr = gInfo;
    fluxAtNode = new Flux[fluxInfoPtr.getNumberOfFluxes()];
    
    //NodeFlux, TypeFlux (? only TypeFlux is currently used)
    for(int i=0; i < fluxInfoPtr.getNumberOfFluxes(); i++){
      if(fluxInfoPtr.info[i].nodeId != -1){
        fluxAtNode[i] = new NodeFlux(fluxInfoPtr.info[i], fluxInfoPtr.pInfo.doFluxCumulative());
      }
      else{
        fluxAtNode[i] = new TypeFlux(fluxInfoPtr.info[i]);
      }
    }
    
    //GroupFlux
    int ngrp = groupInfoPtr.getNumberOfGroups();
    fluxOfGroup = new Flux[ngrp];
    
    for(int i=0; i < ngrp; i++){
      fluxOfGroup[i] = new GroupFlux(groupInfoPtr.getOutputGroups()[i]);
    }
    traceArray = null;
  }

  /**
   *  Calculate Flux
   */
  public final void calculateFlux() {
	
    int startId = 0;
    int endId = 0;
    int[] startTime = new int[1], endTime = new int[1], timeStep = new int[1];
    
    int[] totalNumberOfParticles = new int[1];
    totalNumberOfParticles[0] = getNumberOfParticlesFromTrace();
    traceArray = null;
    
    //calculate traceArray with MAX_PARTICLES as size limit
    for (startId = 1; endId < totalNumberOfParticles[0]; startId += MAX_PARTICLES) {
    	
      endId = Math.min(startId + MAX_PARTICLES - 1, totalNumberOfParticles[0]);
      int nParticles = endId - startId + 1;
      //totalNumberOfParticles will be reset in createTraceArray
      createTraceArray(startId, endId, startTime, endTime, timeStep, totalNumberOfParticles);
      
      for (int i = 0; i < fluxInfoPtr.getNumberOfFluxes(); i++) {
        fluxAtNode[i].calculateFlux(traceArray, nParticles,
      	                            startTime[0], endTime[0], timeStep[0],
      	                            totalNumberOfParticles[0]);
      }
      
      for (int i = 0; i < groupInfoPtr.getNumberOfGroups(); i++) {
        if (DEBUG) System.out.println("group " + i + "\n");
        if (DEBUG) System.out.println(fluxOfGroup[i]);
        fluxOfGroup[i].calculateFlux(traceArray, nParticles,
                                     startTime[0], endTime[0], timeStep[0],
                                     totalNumberOfParticles[0]);
        if (DEBUG) System.out.println("group done " + i);
      }
    }
  }
  
  /**
   *  Output Flux to file
   */
  public final void writeOutput(){
    fluxOut = new PTMFluxOutput(getStartTimeFromTrace());
    if (fluxInfoPtr.getNumberOfFluxes() > 0)
      fluxOut.FluxOutput(fluxAtNode, fluxInfoPtr.doFluxPercentage());
    if (groupInfoPtr.getNumberOfGroups() > 0)
      fluxOut.GroupOutput(fluxOfGroup, groupInfoPtr.doGroupPercentage());
    fluxOut.output();
    fluxOut.closeFile();
  }

  /**
   *  Get the total number of particles from trace
   */
  public final int getNumberOfParticlesFromTrace(){
    int [] startTime = new int[1],endTime = new int[1],timeStep = new int[1];
    int [] totalNumberOfParticles = new int[1];
    
    try{
      // the constructor PTMTraceInput(...)	only reads the trace file header
      PTMTraceInput traceInput = new PTMTraceInput(traceFileName,inputType,
                                                   startTime, endTime, timeStep,
                                                   totalNumberOfParticles);
    }catch(java.io.FileNotFoundException e){
      System.out.println("Error Trace file " + traceFileName + " not found!!");
    }
    return totalNumberOfParticles[0];
  }

  /**
   *  Get start time from trace
   */
  public final int getStartTimeFromTrace(){
    int [] startTime = new int[1],endTime = new int[1],timeStep = new int[1];
    int [] totalNumberOfParticles = new int[1];

    try{
      PTMTraceInput traceInput = new PTMTraceInput(traceFileName,inputType,
                                                   startTime, endTime, timeStep,
                                                   totalNumberOfParticles);
    }catch(java.io.FileNotFoundException e){
      System.out.println("Error Trace file " + traceFileName + " not found!!");
    }
    return startTime[0];
  }

  protected PTMFluxOutput fluxOut;
  /**
   *  traceArray size limit
   *  prevent crash due to potential huge memory size usage
   */
  protected final int MAX_PARTICLES = 10000;   //default
  //protected final int MAX_PARTICLES = 4;   //for test usage
  protected ParticleTrace [] traceArray;
  protected String traceFileName;
  protected int inputType;
  protected FluxInfo fluxInfoPtr;
  protected GroupInfo groupInfoPtr;
  protected Flux [] fluxAtNode;
  protected Flux [] fluxOfGroup;
  
  /**
   *  Create trace array for each particle
   *  This array contains part of the total particles
   *  i.e. nParticles out of totalNumberOfParticles
   */
  protected void createTraceArray(int sId, int eId,
                                  int [] startTime, int [] endTime,
                                  int [] timeStep, int [] totalNumberOfParticles){
  
    //TODO should check to see if start time , end time , time step and number of
    // particles match that from PTMEnv.
    try{
      PTMTraceInput traceInput;
      traceInput = new PTMTraceInput(traceFileName,inputType,
                                     startTime, endTime, timeStep,
                                     totalNumberOfParticles);
      int nParticles = eId - sId + 1; 
      // PTMTraceInput only takes arrays, so here defines one element arrays
      int[] tm=new int[1], pNum=new int[1], nd=new int[1], wb=new int[1];
      
      //renew traceArray
      if(traceArray != null) {
        for(int i=0; i< nParticles; i++)
  	      traceArray[i].resetAll();
      }
      else{
        traceArray = new ParticleTrace[nParticles];
        for(int i=0; i< nParticles; i++)
  	      traceArray[i] = new ParticleTrace();
      }
      
      //vars transfer from trace to traceArray
      //tm, pNum, nd, wb are set in the input method (input read data from the trace files)
      while(tm[0] != -1){
        traceInput.input(tm, pNum, nd, wb);
        if (tm[0] != -1 && (pNum[0] >= sId && pNum[0] <= eId)) 
          traceArray[pNum[0]-sId].insert(wb[0], nd[0], tm[0]);
      }
      
    }catch(java.io.FileNotFoundException e){
      System.out.println("Error Trace file " + traceFileName + " not found!!");
    }catch(java.io.IOException e){
      System.out.println("Error reading trace from file " + traceFileName);
    }//end try-catch
  }
  
}

