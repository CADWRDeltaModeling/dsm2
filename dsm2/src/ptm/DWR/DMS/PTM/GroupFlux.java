//    Copyright (C) 1996, 2009 State of California, Department of Water
//    Resources.
//
//    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
//    numerical model.  No protection claimed in original FOURPT and
//    Branched Lagrangian Transport Model (BLTM) code written by the
//    United States Geological Survey.  Protection claimed in the
//    routines and files listed in the accompanying file "Protect.txt".
//    If you did not receive a copy of this file contact
//    Tara Smith, below.
//
//    This program is licensed to you under the terms of the GNU General
//    Public License, version 2, as published by the Free Software
//    Foundation.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, contact Tara Smith, below,
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
//    Tara Smith
//    California Dept. of Water Resources
//    Division of Planning, Delta Modeling Section
//    1416 Ninth Street
//    Sacramento, CA  95814
//    916-653-9885
//    tara@water.ca.gov
//
//    or see our home page: http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/
//$Id: GroupFlux.java,v 1.3.6.5 2007/07/31 18:30:40 eli2 Exp $
package DWR.DMS.PTM;
/**
 *  This class calculates the number of particles in a Group defined by
 *  incoming and outgoing Waterbody patterns.
 *  <p>
 * @author Aaron Miller
 * @version $Id: GroupFlux.java,v 1.3.6.5 2007/07/31 18:30:40 eli2 Exp $
 */

public class GroupFlux extends Flux{
  /**
   * Constructor
   */
  public GroupFlux(Group wbGroup){
    this.wbGroup=wbGroup;
  }

  /**
   * Calculates total Flux and fills up array of Flux over time as well
   */
  @Override
  public void calculateFlux(ParticleTrace[] traceArray,
                            int numberOfTraceParticles,
                            int sTime, int eTime, int tStep,
                            int nParticles) {
    if (initialized == false) {
      super.calculateFlux(traceArray, numberOfTraceParticles, sTime,
    	                    eTime, tStep, nParticles);
      numberOfParticleCircleFlux = 0;
      circleFlux = new int[numberOfTimeSteps];
      for (int i = 0; i < numberOfTimeSteps; i++)
    	  circleFlux[i] = 0;
    }

    numberOfParticles = numberOfTraceParticles;
    int pNum;
    int particleFlux = 0;
    boolean contributedToFlux = false;
    int index;

    // do for each particle...
    //@todo circleFlux and contributedToFlux are confusing and seem not to do anything
    for (pNum = 0; pNum < numberOfParticles; pNum++) {
      contributedToFlux = false;
      particleFlux = 0;
      int traceNum = 1;
      int maxTraces = traceArray[pNum].getNumberOfTraces();

      try {
        for (index = 0; index < numberOfTimeSteps; index++) {
          while (traceNum <= maxTraces
                 && traceArray[pNum].getTime(traceNum) ==
                 index * timeStep + startTime) {
            Waterbody wbIn = Globals.Environment
                            .getWaterbody(traceArray[pNum]
                            .getWaterbodyId(traceNum - 1));
            Waterbody wbOut = Globals.Environment
                            .getWaterbody(traceArray[pNum]
                            .getWaterbodyId(traceNum));

            //@todo why this stuff?
            //if (traceArray[pNum].getNodeId(traceNum) == -1) {
            //if (isInSameGroup(wbIn, wbOut) == true) {
            //		particleFlux--;
            //	}
            //}else
            //System.out.println("Waterbodies being tested: \nIn:"+wbIn+"\nOut:"+wbOut+"\n");
            //System.out.println("Out type:"+wbOut.getType());
            //System.out.println("Out env:"+wbOut.getEnvIndex());
            //System.err.println("A");

            boolean inWB=wbGroup.containsWaterbody(wbIn);
            boolean outWB=wbGroup.containsWaterbody(wbOut);

            if ( inWB != outWB ) { // exactly one matches wb
              if (inWB) {
                particleFlux--;
                contributedToFlux = true;
              } else if (outWB) {
                particleFlux++;
                contributedToFlux = true;
              }
            }
            traceNum++;
          }//end while(traceNum)

          flux[index] += particleFlux;
          //System.out.println("particleFlux["+index+"]="+Flux[index]);
          if (particleFlux > 0)
            circleFlux[index] += particleFlux - 1;
          else if (particleFlux < 0)
            circleFlux[index] += particleFlux + 1;
          else if (particleFlux == 0)
            circleFlux[index] += 0;
        }// end for(index)

      } catch (java.lang.ArrayIndexOutOfBoundsException e) {
        System.out.println("ERROR!!! " + traceArray[pNum]);
        e.printStackTrace();
      }// end try-catch
    }//end for(pNum)
  }

  /**
   *  Group in which residence is being measured
   */
  private Group wbGroup;

  /**
   *  An array of Flux over time indexed from starttime to endtime
   *  using timestep which counts only repeated contributions to Flux
   *  from the same particle.
   */
  protected int [] circleFlux;

  /**
   *  # of particles contributing to circle Flux and storage
   */
  protected int numberOfParticleCircleFlux;

  /**
   * String representation
   */
  @Override
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

