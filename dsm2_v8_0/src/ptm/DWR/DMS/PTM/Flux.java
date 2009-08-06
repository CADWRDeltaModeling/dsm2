/*<license>
    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
    Department of Water Resources.
    This file is part of DSM2.

    DSM2 is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public !<license as published by
    the Free Software Foundation, either version 3 of the !<license, or
    (at your option) any later version.

    DSM2 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public !<license for more details.

    You should have received a copy of the GNU General Public !<license
    along with DSM2.  If not, see <http://www.gnu.org/!<licenses/>.
</license>*/

package DWR.DMS.PTM;
/**
 *  This is a base class for calculation of Flux as specified from
 *  fixed input.
 * @author Nicky Sandhu
 * @version $Id: Flux.java,v 1.2 2000/08/07 17:00:31 miller Exp $
 */
public class Flux{
  /**
   *
   */
public Flux(){initialized=false;}
  /**
   * returns the type of Flux: node Flux or type Flux
   */
public int getFluxType(){ return fluxType;}
  /**
   * this is subtyped by subtypes to calculate Flux
   */
public void calculateFlux(ParticleTrace [] traceArray, int numberOfTraceParticles,
			  int sTime, int eTime, int tStep, int nParticles){
  startTime = sTime;
  endTime = eTime;
  timeStep = tStep;
  numberOfTimeSteps = (endTime-startTime)/timeStep;
  numberOfParticles =  numberOfTraceParticles;
  totalNumberOfParticles = nParticles;
  flux = new int [numberOfTimeSteps];
  for(int i=0; i< numberOfTimeSteps; i++) flux[i]=0;
  initialized = true;
}
  /**
   * gets the Flux at the given time
   */
public int getFlux(int currentTime){
  int currentIndex = (currentTime-startTime)/timeStep;
  return flux[currentIndex];
}
  /**
   * gets the start time for Flux calculations
   */
public int getStartTime(){
  return startTime;
}

  /**
   * gets the end time for Flux calculations
   */
public  int getEndTime(){
  return endTime;
}

  /**
   * gets the particle model time step 
   */
public  int getPTMTimeStep(){
  return timeStep;
}

  /**
   * gets the number of particles used in Flux calculations
   */
public  int getNumberOfParticles(){
  return totalNumberOfParticles;
}

public final int NODE_FLUX=1;
public final int TYPE_FLUX=2;

protected int fluxType;
protected int startTime, endTime, timeStep;
protected int numberOfParticles;
protected int totalNumberOfParticles;
protected int numberOfTimeSteps;
protected int [] flux;
protected boolean initialized;
}

