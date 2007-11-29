//    Copyright (C) 1996 State of California, Department of Water
//    Resources.
//
//    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
//    numerical model.  No protection claimed in original FOURPT and
//    Branched Lagrangian Transport Model (BLTM) code written by the
//    United States Geological Survey.  Protection claimed in the
//    routines and files listed in the accompanying file "Protect.txt".
//    If you did not receive a copy of this file contact Dr. Paul
//    Hutton, below.
//
//    This program is licensed to you under the terms of the GNU General
//    Public License, version 2, as published by the Free Software
//    Foundation.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, contact Dr. Paul Hutton, below,
//    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
//    02139, USA.
//
//    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
//    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
//    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
//    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
//    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
//    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
//    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
//    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
//    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
//    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
//    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
//    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
//    DAMAGE.
//
//    For more information about DSM2, contact:
//
//    Dr. Paul Hutton
//    California Dept. of Water Resources
//    Division of Planning, Delta Modeling Section
//    1416 Ninth Street
//    Sacramento, CA  95814
//    916-653-5601
//    hutton@water.ca.gov
//
//    or see our home page: http://wwwdelmod.water.ca.gov/

package DWR.DMS.PTM;

/**
 *  CLASS
 *  groupMonitor
 *  This class controls the various classes for flux output. It is responsible
 *  for reading in the trace of particles from the trace file and creating the
 *  flux calculators as defined in the fixed input system. Finally it is responsible
 *  in handling PTMFluxOutput object to write out the flux output.
 */

public class fluxMonitor{
public static final boolean DEBUG = true;
  /**
   *  constructor
   */
public fluxMonitor(String fileName, int iType, fluxInfo fInfo, groupInfo gInfo){
  inputType = iType;
  traceFileName = fileName;
  fluxInfoPtr = fInfo;
  groupInfoPtr = gInfo;
  fluxAtNode = new flux[fluxInfoPtr.getNumberOfFluxes()];
  for(int i=0; i < fluxInfoPtr.getNumberOfFluxes(); i++){
    if(fluxInfoPtr.info[i].nodeId != -1){
      fluxAtNode[i] = new nodeFlux(fluxInfoPtr.info[i], fluxInfoPtr.pInfo.doFluxCumulative());
    }
    else{
      fluxAtNode[i] = new typeFlux(fluxInfoPtr.info[i]);
    }
  }
  if ( groupInfoPtr.doGroupOutput() ){
    fluxOfGroup = new flux[groupInfoPtr.getNumberOfGroups()];
    for(int i=0; i < groupInfoPtr.getNumberOfGroups(); i++){
      fluxOfGroup [i] = new groupFlux(groupInfoPtr, i);
    }
  }
  traceArray = null;
}



  /**
   *  calculate flux
   */
public final void calculateFlux(){

  int startId = 0;
  int endId = 0;
  int [] startTime = new int[1],endTime = new int[1],timeStep = new int[1];

  int [] totalNumberOfParticles = new int[1];
  totalNumberOfParticles[0] = getNumberOfParticlesFromTrace();
  traceArray = null;
  for( startId=1; endId < totalNumberOfParticles[0]; startId+= MAX_PARTICLES){
    endId = Math.min(startId+MAX_PARTICLES-1,totalNumberOfParticles[0]);
    System.out.println("Starting flux calculation for particles " 
		       + startId + " to " + endId );
    int nParticles = endId-startId+1;
    createTraceArray(startId, endId, 
		     startTime, endTime, timeStep, totalNumberOfParticles);

    for(int i=0; i < fluxInfoPtr.getNumberOfFluxes(); i++){
      fluxAtNode[i].
	calculateFlux(traceArray, nParticles, 
		      startTime[0], endTime[0], timeStep[0], totalNumberOfParticles[0]);
    }
    for(int i=0; i < groupInfoPtr.getNumberOfGroups(); i++){
      fluxOfGroup[i].
	calculateFlux(traceArray, nParticles, 
		      startTime[0], endTime[0], timeStep[0], totalNumberOfParticles[0]);
    }
  }
}


  /**
   *  output flux to file
   */
public final void writeOutput(){
  fluxOut = new PTMFluxOutput(getStartTimeFromTrace());
  if (fluxInfoPtr.getNumberOfFluxes() > 0)
    fluxOut.FluxOutput(fluxAtNode, fluxInfoPtr.doFluxPercentage());
  if (groupInfoPtr.doGroupOutput())
    fluxOut.GroupOutput(fluxOfGroup, groupInfoPtr.doGroupPercentage());
  fluxOut.output();
  fluxOut.closeFile();
}



  /**
   *  get number of particles from trace
   */
public final int getNumberOfParticlesFromTrace(){
  int [] startTime = new int[1],endTime = new int[1],timeStep = new int[1];
  
  int [] totalNumberOfParticles = new int[1];
  try{
    PTMTraceInput traceInput = new PTMTraceInput(traceFileName,inputType,
						 startTime, endTime, timeStep,
						 totalNumberOfParticles);
  }catch(java.io.FileNotFoundException e){
    System.out.println("Error Trace file " + traceFileName + " not found!!");
  }
  return totalNumberOfParticles[0];
}


  /**
   *  get start time from trace
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
protected final int MAX_PARTICLES = 10000;
protected particleTrace [] traceArray;
protected String traceFileName;
protected int inputType;
protected fluxInfo fluxInfoPtr;
protected groupInfo groupInfoPtr;
protected flux [] fluxAtNode;
protected flux [] fluxOfGroup;

protected void createTraceArray(int sId, int eId,
				int [] startTime, int [] endTime,
				int [] timeStep, int [] totalNumberOfParticles){
  
  // should check to see if start time , end time , time step and number of
  // particles match that from PTMEnv.
  try{
    PTMTraceInput traceInput;
    traceInput = new PTMTraceInput(traceFileName,inputType,
				   startTime, endTime, timeStep,
				   totalNumberOfParticles);
    
    int nParticles = eId-sId+1;
    
    int[] tm=new int[1], pNum=new int[1], nd=new int[1], wb=new int[1];
    if(traceArray != null) {
      for(int i=0; i< nParticles; i++)
	traceArray[i].resetAll();
    }
    else{
      traceArray = new particleTrace[nParticles];
      for(int i=0; i< nParticles; i++)
	traceArray[i] = new particleTrace();
    }
    while( tm[0] != -1 ){
      traceInput.input(tm, pNum, nd, wb);
      if (tm[0] != -1 && (pNum[0] >= sId && pNum[0] <= eId)) 
	traceArray[pNum[0]-sId].insert(wb[0], nd[0], tm[0]);
    }
  }catch(java.io.FileNotFoundException e){
    System.out.println("Error Trace file " + traceFileName + " not found!!");
  }catch(java.io.IOException e){
    System.out.println("Error reading trace from file " + traceFileName);
  }
}
}

