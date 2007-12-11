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
//$Id: nodeFlux.java,v 1.3.6.3 2007/07/31 18:30:40 eli2 Exp $
package DWR.DMS.PTM;
/**
 *  This class calculates the flux of particles across a node defined by
 *  incoming and outgoing waterbody patterns.
 *  <p>
 * @author Nicky Sandhu
 * @version $Id: nodeFlux.java,v 1.3.6.3 2007/07/31 18:30:40 eli2 Exp $
 */

public class nodeFlux extends flux{
  /**
   * constructor
   */
public nodeFlux(fluxFixedData info, boolean cumulative){
  
  cumulativeFlux = cumulative;
  if (info.nodeId != -1){
    fluxType = NODE_FLUX;
    this.info = info;
  }
  else{
    System.out.println("Incorrect info initialization parameters");
  }
}

  /**
   *  calculates total flux and fills up array of flux over time as well
   */
public void calculateFlux(particleTrace [] traceArray, int numberOfTraceParticles
			  ,int sTime, int eTime, int tStep, int nParticles){
  if( initialized == false){
    super.calculateFlux(traceArray, numberOfTraceParticles, 
			sTime, eTime, tStep, nParticles);
    numberOfParticleCircleFlux=0;
    circleFlux = new int [numberOfTimeSteps];
    for(int i=0; i< numberOfTimeSteps; i++) circleFlux[i]=0;
  }

  numberOfParticles = numberOfTraceParticles;

  int particleFlux = 0;
  //int previousIndex;  @todo remove?
  boolean contributedToFlux = false;
  int index;
  // do for each particle...
  for(int pNum=0; pNum < numberOfParticles; pNum++){
    contributedToFlux = false;
    particleFlux = 0;
    //previousIndex = 0;
    int traceNum=1;
    int maxTraces = traceArray[pNum].getNumberOfTraces();
    //    System.out.println(pNum+" maxTraces = "+maxTraces);
    //    System.out.println("number of timesteps = "+numberOfTimeSteps+" @ "+timeStep);
    try {
      for(index=0; index < numberOfTimeSteps; index++ ){
	if(! doFluxCumulative()) particleFlux = 0; // instantaneous values
	while ( traceNum <= maxTraces && 
		traceArray[pNum].getTime(traceNum) == index*timeStep + startTime){
	    if ( traceArray[pNum].getNodeId(traceNum) == info.getNodeId() ) {
	    waterbody wbIn = 
	      Globals.Environment.getWaterbody(traceArray[pNum].getWaterbodyId(traceNum-1));
	    waterbody wbOut = 
	      Globals.Environment.getWaterbody(traceArray[pNum].getWaterbodyId(traceNum));
	    if ( isIncoming(wbIn) && isOutgoing(wbOut)){
	      particleFlux ++;
	      contributedToFlux = true;
	      //	      System.out.println("Particle #: " + pNum + " Flux = " + particleFlux);
	    }
	    else if ( isOutgoing(wbIn) && isIncoming(wbOut)){
	      particleFlux --;
	      contributedToFlux = true;
	      //	      System.out.println("Particle #: " + pNum + " Flux = " + particleFlux);
	    }
	  }
	  traceNum++;
	}//if (traceArray[pNum].getTime(traceNum) == index*
	//	System.out.println("Flux["+index+"]= "+ flux[index]);
	//	flux[index] += particleFlux;
	flux[index] += particleFlux;
	//	System.out.println("Flux["+index+"] = "+flux[index]);
	if(particleFlux > 0 ) circleFlux[index] += particleFlux - 1;
	else if (particleFlux < 0) circleFlux[index] += particleFlux + 1;
	else if (particleFlux == 0) circleFlux[index]+=0;
      }//for(index
    }catch( java.lang.ArrayIndexOutOfBoundsException e){
      // continue; //@todo Eli: why Do they really happen?
      System.out.println("Node flux out of bounds: " + traceArray[pNum]);
      e.printStackTrace();
    }
  }//for(pNum
}

  /**
   *  returns node Env Index
   */
public final int getNodeEnvIndex(){
  return info.nodeId;
}


  /**
   *  gets number of incoming waterbodies
  
public final int getNumberIncoming(){
  return info.inArray.length;
} */


  /**
   *  gets number of outgoing waterbodies
  
public final int getNumberOutgoing(){
  return info.outArray.length;
} */


  /**
   *  returns the EnvIndex of locally indexed incoming waterbody
  
public final int getIncoming(int index){
  return info.inArray[index];
} */


  /**
   *  returns the EnvIndex of locally indexed outgoing waterbody
  
public final int getOutgoing(int index){
  return info.outArray[index];
} */

  /**
   *  returns true for cumulative flux output
   */
public final boolean doFluxCumulative(){
  //  return info.doFluxCumulative();
  return cumulativeFlux;
}


protected boolean cumulativeFlux;

  /**
   *  flux information is contained in info
   */
protected fluxFixedData info;
  /**
   *  An array of flux over time indexed from starttime to endtime
   *  using timestep which counts only repeated contributions to flux
   *  from the same particle.
   */
protected int [] circleFlux;

  /**
   *  # of particles contributing to circle flux and storage
   */
protected int numberOfParticleCircleFlux;

  /**
   *  checks to see if the waterbody Id matches any of the specified
   *  incoming groups.
   */
protected final boolean isIncoming(waterbody wb){
    System.out.println("Testing outgoing: "+wb);
    return info.getInGroup().containsWaterbody(wb);
}


  /**
   *  checks to see if the waterbody Id matches any of the specified
   *  outgoing groups.
   */
protected final boolean isOutgoing(waterbody wb){
    System.out.println("Testing outgoing: "+wb);
  return info.getOutGroup().containsWaterbody(wb);
}

public String toString(){
  StringBuffer rep = new StringBuffer("");
  rep.append( "Node Flux : " ).append( " Node Number: " ).append( getNodeEnvIndex() ).append( "\n");
  rep.append( "Start Time: " ).append( getStartTime() ).append( "\n");
  rep.append( "End Time: " ).append( getEndTime() ).append( "\n");
  rep.append( "Time Step: " ).append( getPTMTimeStep() ).append( "\n");

  for(int cTime=getStartTime();
      cTime< getEndTime();
      cTime += 24*60){
    rep.append( "Time: " ).append( cTime ).append( " ");
    rep.append( "Flux: " ).append( getFlux(cTime) ).append( "\n");
  }
  return rep.toString();
}
}

