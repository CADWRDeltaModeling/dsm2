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
 *  This is a base class for calculation of flux as specified from
 *  fixed input.
 * @author Nicky Sandhu
 * @version $Id: flux.java,v 1.2 2000/08/07 17:00:31 miller Exp $
 */
public class flux{
  /**
   *
   */
public flux(){initialized=false;}
  /**
   * returns the type of flux: node flux or type flux
   */
public int getFluxType(){ return fluxType;}
  /**
   * this is subtyped by subtypes to calculate flux 
   */
public void calculateFlux(particleTrace [] traceArray, int numberOfTraceParticles,
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
   * gets the flux at the given time
   */
public int getFlux(int currentTime){
  int currentIndex = (currentTime-startTime)/timeStep;
  return flux[currentIndex];
}
  /**
   * gets the start time for flux calculations
   */
public int getStartTime(){
  return startTime;
}

  /**
   * gets the end time for flux calculations
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
   * gets the number of particles used in flux calculations
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

