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
//$Id: mainPTM.java,v 1.7 2000/08/07 17:00:33 miller Exp $
package DWR.DMS.PTM;
import java.io.IOException;
/**
  *
  */
public class mainPTM {

public static void main(String[] args) {
  try {
    //set version and module ids
    Globals.initialize();
    int i;
    
    
    // Initialize environment
    String fixedInputFilename = "dsm2.inp";
    if(args.length > 0) fixedInputFilename = args[0];
    PTMEnv Environment = new PTMEnv(fixedInputFilename);
    
    if (DEBUG) System.out.println("Environment initialized");
    // set global environment pointer
    Globals.Environment = Environment;
    
    // get runtime parameters
    int startTime = Environment.getStartTime();
    int runTime = Environment.getRunLength();
    int endTime = startTime + runTime;
    int PTMTimeStep = Environment.getPTMTimeStep();
    if(DEBUG) System.out.println("Initialized model run times");
    // set array of particles and initialize them

    particle [] particleArray = null;
    
    int numberOfParticles=0;
    int numberOfRestartParticles = 0;
    boolean isRestart = Environment.isRestartRun();

    if(DEBUG) System.out.println("Checking for restart run");
    // if restart then inject those particles
    if(isRestart){
      String inRestartFilename = Environment.getInputRestartFileName();
      PTMRestartInput restartInput = 
	new PTMRestartInput(inRestartFilename,
			    Environment.getFileType(inRestartFilename),
			    particleArray);
      restartInput.input();
      numberOfRestartParticles = particleArray.length;
    }
    boolean behavior = false;
    try {
      // set behavior file and return status
      behavior = Environment.setParticleBehavior();
      //      behavior = true;
    } catch(IOException ioe) {}

    // total number of particles
    numberOfParticles = numberOfRestartParticles +
      Environment.getNumberOfParticlesInjected();
    if(DEBUG) System.out.println("total number of particles injected are " + numberOfParticles);
    
    particleArray = new particle[numberOfParticles];
    if(DEBUG) System.out.println("restart aprticles " + numberOfRestartParticles);

    if(behavior) {
      for(int pNum = numberOfRestartParticles; pNum < numberOfParticles; pNum++)
	particleArray[pNum] = new BehavedParticle(Environment.getParticleFixedInfo());
      System.out.println("BehavedParticle");
    }
    else {
      for(int pNum = numberOfRestartParticles; pNum < numberOfParticles; pNum++)
	particleArray[pNum] = new particle(Environment.getParticleFixedInfo());
    }
    if(DEBUG) System.out.println("particles initialized");
    
    // insert particles from fixed input information
    Environment.setParticleInsertionInfo(particleArray, numberOfRestartParticles);
    if(DEBUG) System.out.println("Set insertion info");
    // set observer on each particle
    String traceFileName = Environment.getTraceFileName();
    if ( traceFileName == null || traceFileName.length() == 0 ) {
      System.err.println("Trace file needs to be specified");
      System.err.println("Exiting");
      System.exit(-1);
    }
    particleObserver observer = null;
    observer = new particleObserver(traceFileName,
				    Environment.getFileType(traceFileName),
				    startTime, endTime, PTMTimeStep,
				    numberOfParticles);
    if(DEBUG) System.out.println("Set observer");

    for(i=0; i<numberOfParticles; i++) {
      if ( observer != null) observer.setObserverForParticle(particleArray[i]);
    }
    
    // initialize output restart file
    String outRestartFilename = Environment.getOutputRestartFileName();
    PTMRestartOutput outRestart = null;
    try {
      outRestart = new PTMRestartOutput(outRestartFilename,
					Environment.getFileType(outRestartFilename),
					Environment.getRestartOutputInterval(),
					particleArray);
    }catch(IOException ioe){
    }
    if(DEBUG) System.out.println("Set restart output");
    
    // initialize animation output
    String animationFileName = Environment.getAnimationFileName();
    PTMAnimationOutput animationOutput = null;
    try {
      animationOutput = new PTMAnimationOutput(animationFileName,
					       Environment.getFileType(animationFileName),
					       Environment.getAnimationOutputInterval(),
					       numberOfParticles,
					       Environment.getNumberOfAnimatedParticles(),
					       particleArray);
    }catch(IOException ioe){
    }
    if(DEBUG) System.out.println("Set anim output");
    
    System.out.println("Total number of particles: " + numberOfParticles + "\n");
    
    // time step (converted to seconds) and display interval
    int timeStep = PTMTimeStep*60;
    int displayInterval = Environment.getDisplayInterval();
    
    // initialize current model time
    //   Globals.currentModelTime = startTime;
    //main loop for running particle model 
    for(Globals.currentModelTime = startTime; 
	Globals.currentModelTime <=endTime; 
	Globals.currentModelTime+=PTMTimeStep){
      
      // output runtime information to screen
      mainPTM.display(displayInterval);

      if (behavior)
	Globals.currentMilitaryTime = Integer.parseInt(Globals.getModelTime(Globals.currentModelTime));
      // get latest hydro information
      Environment.getHydroInfo(Globals.currentModelTime);
      if (DEBUG) System.out.println("Updated flows");
      // update particle positions
      
      for(i=0;i<numberOfParticles;i++){
	particleArray[i].updatePosition(timeStep);
      }
      if (DEBUG) System.out.println("Updated particle positions");
      
      // animation output
      if ( animationOutput != null ) animationOutput.output();
      
      // write out restart file information
      if ( outRestart != null ) outRestart.output();
      
    }
    if ( animationOutput != null ) animationOutput.FlushAndClose();
    System.out.println(" ");
    // write out restart file information
    if ( outRestart != null ) outRestart.output();
    
    // clean up after run is over
    observer = null;
    
    particleArray = null;
    
    // output flux calculations in dss format
    fluxInfo fluxFixedInfo = Environment.getFluxFixedInfo();
    groupInfo groupFixedInfo = Environment.getGroupFixedInfo();

    fluxMonitor fluxCalculator = new fluxMonitor(traceFileName,
						   Environment.getFileType(traceFileName),
						   fluxFixedInfo,
						   groupFixedInfo);
    
    fluxCalculator.calculateFlux();
    
    fluxCalculator.writeOutput();

    System.out.println("");
  }catch(Exception e){
    e.printStackTrace();
    System.out.println("Exception " + e + " occurred");
    System.exit(-1);
  }
}

public static boolean DEBUG = false;  
protected static int previousDisplayTime = 0;
  //public native static void display(int displayInterval);
public static void display(int displayInterval) {
  if(previousDisplayTime == 0){ 
    previousDisplayTime = Globals.currentModelTime - displayInterval;
  }

  if(Globals.currentModelTime >= previousDisplayTime + displayInterval){
    // output model date and time
    String modelTime = Globals.getModelTime(Globals.currentModelTime);
    String modelDate = Globals.getModelDate(Globals.currentModelTime);
    System.out.println("Model date: " + modelDate + " time: " + modelTime);
    previousDisplayTime = Globals.currentModelTime;
  }
}


}
