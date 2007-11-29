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
//$Id: groupFlux.java,v 1.3 2000/08/07 17:00:32 miller Exp $
package DWR.DMS.PTM;
/**
 *  This class calculates the number of particles in a group defined by
 *  incoming and outgoing waterbody patterns.
 *  <p>
 * @author Aaron Miller
 * @version $Id: groupFlux.java,v 1.3 2000/08/07 17:00:32 miller Exp $
 */

public class groupFlux extends flux{
  /**
    * constructor
    */
  public groupFlux(groupInfo info, int groupNumber){
  
    this.info = info;
    this.groupNumber = groupNumber+1;
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
    int pNum;
    int particleFlux = 0;
    int previousIndex;
    boolean contributedToFlux = false;
    int index;
    // do for each particle...
    for(pNum=0; pNum < numberOfParticles; pNum++){
      contributedToFlux = false;
      particleFlux = 0;
      previousIndex = 0;
      int traceNum=1;
      int maxTraces = traceArray[pNum].getNumberOfTraces();
      try {
	for(index=0; index < numberOfTimeSteps; index++ ){
	  while ( traceNum <= maxTraces && 
		  traceArray[pNum].getTime(traceNum) == index*timeStep + startTime){

	    waterbody wbIn = 
	      Globals.Environment.getWaterbody(traceArray[pNum].getWaterbodyId(traceNum-1));
	    waterbody wbOut = 
	      Globals.Environment.getWaterbody(traceArray[pNum].getWaterbodyId(traceNum));
	  
	    if ( traceArray[pNum].getNodeId(traceNum) == -1 ) {
	      if ( isInSameGroup(wbIn, wbOut) == true ){
		particleFlux --;
	      }
	    }

	    else if ( isInSameGroup(wbIn, wbOut) == false ){
	    
	      if ( isSame(wbOut) ){
		particleFlux ++;
		contributedToFlux = true;
	      }
	      else if ( isSame(wbIn) ){
		particleFlux --;
		contributedToFlux = true;
	      }
	    }
	    traceNum++;
	  }
	  flux[index] += particleFlux;
	  if(particleFlux > 0 ) circleFlux[index] += particleFlux - 1;
	  else if (particleFlux < 0) circleFlux[index] += particleFlux + 1;
	  else if (particleFlux == 0) circleFlux[index]+=0;
	}
      }catch( java.lang.ArrayIndexOutOfBoundsException e){
     
	System.out.println("ERROR!!! " + traceArray[pNum]);
	e.printStackTrace();
      }
    }
  }


  /**
   *  group information is contained in info
   */
  protected groupInfo info;

  protected int groupNumber;
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
   *  incoming arrays.
   */
  protected final boolean isInSameGroup(waterbody wb1, waterbody wb2){
    boolean isSame = false;
    if(wb1 != null && wb2 != null)
      if(wb1.getGroup() == groupNumber && wb2.getGroup() == groupNumber) 
	isSame = true;
    return isSame;
  }

  protected final boolean isDifferent(waterbody wb){
    boolean notMatching = true;
    if(wb != null)
      if(wb.getGroup() == groupNumber) 
	notMatching = false;
    return notMatching;
  }

  /**
   *  checks to see if the waterbody Id matches any of the specified
   *  outgoing arrays.
   */
  protected final boolean isSame(waterbody wb){
    boolean Matching = false;
    if (wb != null)
      if(wb.getGroup() == groupNumber) 
	Matching = true;
    return Matching;
  }

  public String toString(){
    StringBuffer rep = new StringBuffer("");
    //  rep.append( "Node Flux : " ).append( " Node Number: " ).append( getNodeEnvIndex() ).append( "\n");
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

