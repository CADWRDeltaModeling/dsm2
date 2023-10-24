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



  /**
  *  CLASS
  *  ParticleTrace
  *  This class stores the trace of a particle object as a series of timestamps,
  *  waterbody entered and node entered from.
  *  This information can be written out by PTMTraceOutput and read in by
  *  PTMTraceInput. The information is then used in determining flux of the particle
  *  across a region.
*/
package DWR.DMS.PTM;
import java.util.Vector;
public class ParticleTrace{

  /**
   *  constructor
   */
public ParticleTrace(){
  traceNumber = 0;
  channelNumber = new Vector<Integer>(INITIAL_SIZE, RESIZE_STEP);
  nodeNumber = new Vector<Integer>(INITIAL_SIZE, RESIZE_STEP);
  entryTime = new Vector<Integer>(INITIAL_SIZE, RESIZE_STEP);
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
protected Vector<Integer> channelNumber;

  /**
   *  an array containg node numbers
   */
protected Vector<Integer> nodeNumber;

  /**
   *  an array of time stamps
   */
protected Vector<Integer> entryTime;

  /**
   *  an index to the current trace number
   */
protected int traceNumber;
}


