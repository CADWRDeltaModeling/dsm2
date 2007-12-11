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

//$Id: particleTrace.java,v 1.2 2000/08/07 17:00:34 miller Exp $



  /**
  *  CLASS
  *  particleTrace
  *  This class stores the trace of a particle object as a series of timestamps,
  *  waterbody entered and node entered from.
  *  This information can be written out by PTMTraceOutput and read in by
  *  PTMTraceInput. The information is then used in determining flux of the particle
  *  across a region.
*/
package DWR.DMS.PTM;
import java.util.Vector;
public class particleTrace{

  /**
   *  constructor
   */
public particleTrace(){
  traceNumber = 0;
  channelNumber = new Vector(INITIAL_SIZE, RESIZE_STEP);
  nodeNumber = new Vector(INITIAL_SIZE, RESIZE_STEP);
  entryTime = new Vector(INITIAL_SIZE, RESIZE_STEP);
  channelNumber.insertElementAt(new Integer(0), traceNumber);
  nodeNumber.insertElementAt(new Integer(0), traceNumber);
  entryTime.insertElementAt(new Integer(0), traceNumber);
}


  /**
   *  insert a new trace entry
   */
public final void insert(int newChannelNumber, int newNodeNumber, int newEntryTime){
  traceNumber++;
  channelNumber.ensureCapacity(traceNumber+1);
  nodeNumber.ensureCapacity(traceNumber+1);
  entryTime.ensureCapacity(traceNumber+1);
  channelNumber.insertElementAt(new Integer(newChannelNumber), traceNumber);
  nodeNumber.insertElementAt(new Integer(newNodeNumber), traceNumber);
  entryTime.insertElementAt(new Integer(newEntryTime), traceNumber);
}


  /**
   *  return total number of traces
   */
public final int getNumberOfTraces(){ return traceNumber;};

public String toString(){
  String rep = " " + getNumberOfTraces();
  for(int i=1; i<getNumberOfTraces(); i++){
    rep += "@Time " + getTime(i) 
      + " @Node " + getNodeId(i)
      + " @Channel " + getWaterbodyId(i)+"\n";
  }
  return rep;
}

  /**
   *  returns node entered from for a given trace number
   */
public final int getNodeId(int traceNum){ 
  return ((Integer) nodeNumber.elementAt(traceNum)).intValue();
}


  /**
   *  returns the waterbody number for a given trace number
   */
public final int getWaterbodyId(int traceNum){ 
  return ((Integer) channelNumber.elementAt(traceNum)).intValue();
}


  /**
   *  returns the timestamp of the given trace
   */
public final int getTime(int traceNum) { 
  return ((Integer) entryTime.elementAt(traceNum)).intValue(); 
}


  /**
   *  resets the trace number while keeping the memory allocated in each of the
   *  arrays. This helps conserve time in multiple file sweeps in case of large number
   *  of particles.
   */
public final void resetAll(){
  traceNumber=0;
}




  /**
   *  intial guess at number of traces
   */
protected static int INITIAL_SIZE  = 300;

  /**
   *  guess at what resizing to be done in case initial guess is exceeded
   */
protected static int RESIZE_STEP = 10;

  /**
   *  an dynamically extensible array containg waterbody numbers
   */
protected Vector channelNumber;

  /**
   *  an array containg node numbers
   */
protected Vector nodeNumber;

  /**
   *  an array of time stamps
   */
protected Vector entryTime;

  /**
   *  an index to the current trace number
   */
protected int traceNumber;
}


