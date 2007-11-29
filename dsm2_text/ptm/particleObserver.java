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

//$Id: particleObserver.java,v 1.3.8.1 2003/04/08 21:21:15 miller Exp $
package DWR.DMS.PTM;
import java.io.*;
class particleObserver{

public final static int WATERBODY_CHANGE=1, NODE_CHANGE=2, TIME_INTERVAL=3, DEATH=4;
  /**
   *  constructor
   */
public particleObserver(String traceFileName, int outputType, 
			int startTime, int endTime, int PTMTimeStep, 
			int nParticles){
  traceOn = true;
  try{
    if (traceOn) output = new PTMTraceOutput(traceFileName, outputType,
					     startTime,  endTime,  PTMTimeStep, 
					     nParticles);
  }catch(IOException ioe){
    System.out.println("Exception " + ioe + " occurred");
    System.out.println("particle Observer initialization failed");
    System.out.println("Trace Filename " + traceFileName);
  }
}

  /**
   *  destructor
public  ~particleObserver(){
  delete output;
}
   */


  /**
   *  itself upto the particle
   */
public final void setObserverForParticle(particle observed){
  observed.installObserver(this);
}


  /**
   *  changes go through this function
   */
public final void observeChange(int change, particle observed) {
  switch(change) {
  case WATERBODY_CHANGE: 
    observeWaterbodyChange(observed);
    break;
  case NODE_CHANGE: 
    observeNodeChange(observed);
    break;
  case TIME_INTERVAL:
    observeTimeInterval(observed);
    break;
  case DEATH:
    observeDeath(observed);
    break;
  default:
    return;
  }				  
};


  /**
   *  observes the fact that the particle has entered a waterbody
   *  after encountering a node
   */
public  void observeWaterbodyChange(particle observed){ 
  if (traceOn) {

    int time = observed.getCurrentParticleTime();
//      short pId = (short) observed.getId();
    int pId = observed.getId();
    short nodeId = -1;
    if (observed.getRecentNode() != null) 
      nodeId = (short) observed.getRecentNode().getEnvIndex();
    else
      nodeId = -1;
    short wbId = (short) observed.getCurrentWaterbody().getEnvIndex();
    output.output(time,pId,nodeId,wbId);
  }
}


  /**
   *  observes the fact that a node has been encountered.
   */
public  void observeNodeChange(particle observed){ 
}


  /**
   *  observes the fact that a certain time interval has gone by
   *  since the last observed time...
   */
public  void observeTimeInterval(particle observed){ 
}

  /**
   *  observes the fact that the particle has died
   */
public  void observeDeath(particle observed){ 
  if (traceOn) {

    int time = observed.getCurrentParticleTime();
//      short pId = (short) observed.getId();
    int pId = observed.getId();
    short nodeId = -1;
    short wbId = (short) observed.getCurrentWaterbody().getEnvIndex();
    //    short wbId = -1;
    output.output(time,pId,nodeId,wbId);
  }
}

  /**
   *  changes go through this function
   */
public final void showChange(int change, particle observed){
switch(change) {
  case WATERBODY_CHANGE: 
    showWaterbodyChange(observed);
    break;
  case NODE_CHANGE: 
    showNodeChange(observed);
    break;
  case TIME_INTERVAL:
    showTimeInterval(observed);
    break;
  default:
    return;
  }		
}


  /**
   *  shows the fact that the particle has entered a waterbody
   *  after encountering a node
   */
public  void showWaterbodyChange(particle observed){ 
  if (traceOn) {
    waterbody wb;
    float [] px = new float[1] , py = new float[1] , pz = new float[1];
    wb = observed.getLocation(px, py, pz);
    System.out.println("Particle " + "  " + observed.getId()
		       + " in waterbody " + "  " + wb.getEnvIndex()
		       + " which is a " + "  " + wb.getPTMType());
    System.out.flush();
  }
}


  /**
   *  shows the fact that a node has been encountered.
   */
public  void showNodeChange(particle observed){ 
  waterbody wb;
  float[] px = new float[1], py = new float[1], pz = new float[1];

  System.out.println("Particle " + "  " +observed.getId()+
		     " at Node " + "  " +observed.getRecentNode().getEnvIndex());

  wb = observed.getLocation(px, py, pz);

  System.out.println("Particle " + "  " +observed.getId()
		     + " in waterbody " + "  " +wb.getEnvIndex() 
		     + " which is a " + "  " +wb.getPTMType());

  System.out.flush();
}


  /**
   *  shows the fact that a certain time interval has gone by
   *  since the last observed time...
   */
public  void showTimeInterval(particle observed){ 
}




  /**
   *  flag to see if trace is to be registered
   */
private boolean traceOn;

  /**
   *  pointer to trace output
   */
private PTMTraceOutput output;
}


