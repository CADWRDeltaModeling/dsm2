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
 * Calculates the flux of particles based on the types of waterbodies
 * optionally excluding specific waterbodies of those types
 *
 * @author Nicky Sandhu
 * @version $Id: typeFlux.java,v 1.2 2000/08/07 17:00:35 miller Exp $
 */
public class typeFlux extends flux{
  /**
   *
   */
public typeFlux(fluxFixedData info){
  fluxType = TYPE_FLUX;
  this.info = info;
}
  /**
   *
   */
public void calculateFlux(particleTrace [] traceArray ,int numberOfTraceParticles, 
			  int sTime, int eTime, int tStep, int nParticles){
  if( initialized == false) {
    super.calculateFlux(traceArray, numberOfTraceParticles, 
			sTime, eTime, tStep, nParticles);
  }

  numberOfParticles =  numberOfTraceParticles;
  
  for(int pNum=0; pNum < numberOfParticles; pNum++){
    int traceNum=1;
    int particleFlux=0;
    int maxTraces = traceArray[pNum].getNumberOfTraces();
    //System.out.println("Particle #: " + pNum);
    try{
      for(int index=0; index < numberOfTimeSteps; index ++ ){
	if (traceNum <= maxTraces && 
	    traceArray[pNum].getTime(traceNum) == index*timeStep + startTime){
	  waterbody wbIn = 
	    Globals.Environment.getWaterbody(traceArray[pNum].getWaterbodyId(traceNum-1));
	  waterbody wbOut = 
	    Globals.Environment.getWaterbody(traceArray[pNum].getWaterbodyId(traceNum));
	  //	  if (wbIn != null) System.out.println(wbIn.getAccountingType());
	  //if (wbOut != null) 
	  //  System.out.println("Out number: " + wbOut.getEnvIndex() + " , type = " +wbOut.getAccountingType());
	  //System.out.println();
	  if (isCorrectType(wbIn, wbOut) && isNotException(wbIn, wbOut)) {
	    //System.out.println(wbOut);
	    particleFlux++;
	  }
	  traceNum++;
	}//if (traceArray[pNum].getTime(traceNum) == index*
	flux[index]+=particleFlux;
      }//for(index
    }catch(java.lang.ArrayIndexOutOfBoundsException e){
      e.printStackTrace();
    }
  }//for(pNum
}
  /**
   *
   */
boolean isCorrectType(waterbody wbIn, waterbody wbOut){
  boolean inType=false;
  boolean outType=false;
  int i=0;
  int nInType = info.inAccountTypeArray.length;
  int nOutType = info.outAccountTypeArray.length;
  if(nInType > 0){
    for(i=0; i<nInType; i++){
      if(info.inAccountTypeArray[i] == -100){
	if(wbIn.getObjectType() == info.inTypeArray[i]) inType = true;
	//	System.out.println("inTypeArray "+wbIn);
      }
      else {
	if(info.inAccountTypeArray[i] != 0) {
	  if(wbIn.getAccountingType() == info.inAccountTypeArray[i]) inType = true;
	}
      }
    }
  }
  else{
    inType = true;
  }
  if(nOutType > 0){
    for(i=0; i<nOutType; i++){
      if(info.outAccountTypeArray[i] == -100){
	if(wbOut.getObjectType() == info.outTypeArray[i]) outType = true;
	//	System.out.println("outTypeArray "+wbOut);
      }
      else {
	if(info.outAccountTypeArray[i] != 0){
	  if(wbOut.getAccountingType() == info.outAccountTypeArray[i]) outType = true;
	}
      }
    }
  }
  else{
    outType = true;
  }
  return(inType && outType);
}
  /**
   * returns true if this combination of in and out waterbodies is acceptable
   */
boolean isNotException(waterbody wbIn, waterbody wbOut){
  boolean inEx=false;
  boolean outEx=false;
  int i=0;
  int nIn = info.inArray.length;
  int nOut = info.outArray.length;
  if(nIn > 0){
    for(i=0; i<nIn; i++){
      if(info.inArray[i] != 0){
	if(wbIn.getEnvIndex() == -info.inArray[i]) inEx=true;
      }
    }
  }
  if(nOut > 0){
    for(i=0; i<nOut; i++){
      if(info.outArray[i] != 0){
	if(wbOut.getEnvIndex() == -info.outArray[i]) outEx=true;
      }
    }
  }
  return(!(inEx || outEx));
}

protected fluxFixedData info;
}
