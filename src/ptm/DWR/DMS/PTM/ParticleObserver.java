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
import java.io.*;
/**
 *  Records particles critical events, e.g. insertion, change of waterbody, 
 *  change of node, time interval change, death
 *  ??only the first 2 & the last 1 are implemented
 *  
 */
class ParticleObserver{

  public final static int INSERT=5, WATERBODY_CHANGE=1, NODE_CHANGE=2, TIME_INTERVAL=3, DEATH=4; 
  
  /**
   *  Constructor
   */
  public ParticleObserver(String traceFileName, int outputType,
                          int startTime, int endTime, int PTMTimeStep, 
                          int nParticles){
    traceOn = true;
    try{
      if (traceOn) output = new PTMTraceOutput(traceFileName, outputType,
                                               startTime, endTime, PTMTimeStep, 
                                               nParticles);
    }catch(IOException ioe){
      System.out.println("Exception " + ioe + " occurred");
      System.out.println("particle Observer initialization failed");
      System.out.println("Trace Filename " + traceFileName);
    }
  }

  /**
   *  destructor
  public  ~ParticleObserver(){
    delete output;
  }
   */

  /**
   *  Set observer to designated Particle
   */
  public final void setObserverForParticle(Particle observed){
    observed.installObserver(this);
  }

  /**
   *  Particle change events judgment
   */
  public final void observeChange(int change, Particle observed) {
    switch(change) {
    case INSERT:
      observeInsert(observed);
      break;	
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
  }

  /**
   *  observes the fact that the Particle has been inserted in a node
   */
  public void observeInsert(Particle observed){
    if (traceOn) {
      long time = observed.getCurrentParticleTime();
      int pId = (int) observed.getId();
      short nodeId = -1;
      short wbId = 0;
      output.output(time,pId,nodeId,wbId);
    }
  }

  /**
   *  observes the fact that the Particle has entered a Waterbody
   *  after encountering a node
   */
  public void observeWaterbodyChange(Particle observed){
    if (traceOn) {
      long time = observed.getCurrentParticleTime();
      int pId = observed.getId();
      int nodeId = -1;
      if (observed.getRecentNode() != null) 
        nodeId =  observed.getRecentNode().getEnvIndex();
      else
        nodeId = -1;
      int wbId =  observed.getCurrentWaterbody().getEnvIndex();
      output.output(time,pId,nodeId,wbId);
      long timeExact = observed.getCurrentParticleTimeExact();
      observed.addParticleTrace(timeExact, wbId, nodeId);
    }
  }

  /**
   *  observes the fact that a node has been encountered.
   */
  public void observeNodeChange(Particle observed){
  }

  /**
   *  observes the fact that a certain time interval has gone by
   *  since the last observed time...
   */
  public void observeTimeInterval(Particle observed){
  }

  /**
   *  observes the fact that the Particle has died
   */
  public void observeDeath(Particle observed){
    if (traceOn) {
      long time = observed.getCurrentParticleTime();
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
  public final void showChange(int change, Particle observed){
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
   *  shows the fact that the Particle has entered a Waterbody
   *  after encountering a node
   */
  public void showWaterbodyChange(Particle observed){
    if (traceOn) {
      Waterbody wb;
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
  public void showNodeChange(Particle observed){
    Waterbody wb;
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
  public void showTimeInterval(Particle observed){
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


