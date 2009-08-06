/** <license>
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
 *  PTM is an acronym for "Particle Tracking Model". This is version 2 of PTM
 *  which utilizes information from DSM2 to track particles moving according
 *  to hydrodynamics and quality information.<p>
  *
 * This class defines the parameters and information about the environment
 * a Particle moves in. This information is either fixed or dynamic. Fixed information
 * is information that is relatively fixed during a run of the model such
 * as the output filenames, the flow network etcetra. Dynamic information
 * is information that most likely changes every time step such as the
 * flow, velocity, volume, etcetra.<p>
 *
 * The network consists of nodes and waterbodies. Waterbody is an entity
 * containing water such as a channel, reservoir, etcetra. Node is a connection
 * between waterbodies. Each waterbody is given a unique id and also contains
 * information about which nodes it is connected to. Each node also has a unique
 * id and the information about which waterbodies it is connected to.
 *
 * @author Nicky Sandhu
 * @version $Id: MainPTM.java,v 1.5.6.4 2006/04/04 18:16:23 eli2 Exp $
 */

package DWR.DMS.PTM;
import java.io.IOException;


/**
 * main function of PTM
 */
public class MainPTM {

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

            Particle [] particleArray = null;
    
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
    
            particleArray = new Particle[numberOfParticles];
            if(DEBUG) System.out.println("restart aprticles " + numberOfRestartParticles);

            if(behavior) {
                for(int pNum = numberOfRestartParticles; pNum < numberOfParticles; pNum++)
                    particleArray[pNum] = new BehavedParticle(Environment.getParticleFixedInfo());
                System.out.println("BehavedParticle");
            }
            else {
                for(int pNum = numberOfRestartParticles; pNum < numberOfParticles; pNum++)
                    particleArray[pNum] = new Particle(Environment.getParticleFixedInfo());
            }
            if(DEBUG) System.out.println("particles initialized");
    
            // insert particles from fixed input information
            Environment.setParticleInsertionInfo(particleArray, numberOfRestartParticles);
            if(DEBUG) System.out.println("Set insertion info");
            // set observer on each Particle
            String traceFileName = Environment.getTraceFileName();
            if ( traceFileName == null || traceFileName.length() == 0 ) {
                System.err.println("Trace file needs to be specified");
                System.err.println("Exiting");
                System.exit(-1);
            }
            ParticleObserver observer = null;
            observer = new ParticleObserver(traceFileName,
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
            //main loop for running Particle model
            for(Globals.currentModelTime = startTime; 
                Globals.currentModelTime <=endTime; 
                Globals.currentModelTime+=PTMTimeStep){
      
                // output runtime information to screen
                MainPTM.display(displayInterval);

                if (behavior)
                    Globals.currentMilitaryTime = Integer.parseInt(Globals.getModelTime(Globals.currentModelTime));
                // get latest hydro information
                Environment.getHydroInfo(Globals.currentModelTime);
                if (DEBUG) System.out.println("Updated flows");
                // update Particle positions
      
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
            FluxInfo fluxFixedInfo = Environment.getFluxFixedInfo();
            GroupInfo groupFixedInfo = Environment.getGroupFixedInfo();

            FluxMonitor fluxCalculator = new FluxMonitor(traceFileName,
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
