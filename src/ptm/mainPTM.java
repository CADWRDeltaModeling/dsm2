/*<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
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
import java.io.IOException;
/**
 *
 */
public class mainPTM {

    public static void main(String[] args) {
        try {
            //set version and module ids
            if (DEBUG) System.out.println("initializing globals");
            Globals.initialize();
            if (DEBUG) System.out.println("initialized globals");
    
    
    
            // Initialize environment
            if (DEBUG) System.out.println("Initializing environment");
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

            for(int i=0; i<numberOfParticles; i++) {
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
      
                for(int i=0;i<numberOfParticles;i++){
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
