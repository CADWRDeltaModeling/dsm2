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
import java.util.Calendar;
/*
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
            String ptmInputFile = "dsm2.inp";
            if(args.length > 0)
            	ptmInputFile = args[0];
            
            PTMEnv Environment = new PTMEnv(ptmInputFile);
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

            /*
             * This part is coded very wrong, disable it now and rewrite it later
             */
            if(isRestart)
            	PTMUtil.systemExit("restart functionality is disabled! exit.");
            /*
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
            */
            boolean behavior = false;
            //add behaviors
            Environment.addBehaviors();
            try {
                // set behavior file and return status
            	//TODO This behavior module is Aaron's original one and needs to be revisited, only works for delta smelt
            	//getBehaviorFileName() somehow return the sameName as getBehaviorInfileName()
            	if (Environment.getParticleType().equalsIgnoreCase("SMELT"))
                	behavior = Environment.setParticleBehavior();
                //      behavior = true;
            } catch(IOException ioe) {}

            // total number of particles
            numberOfParticles = numberOfRestartParticles
                              + Environment.getNumberOfParticlesInjected();
            if(DEBUG) System.out.println("total number of particles injected are " + numberOfParticles);
    
            particleArray = new Particle[numberOfParticles];
            if(DEBUG) System.out.println("restart particles " + numberOfRestartParticles);
            
            //TODO need to be rewrite 
            if(behavior) {
                for(int pNum = numberOfRestartParticles; pNum < numberOfParticles; pNum++)
                    particleArray[pNum] = new BehavedParticle(Environment.getParticleFixedInfo());
                System.out.println("Behaved Particle");
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
            if ( traceFileName == null || traceFileName.length() == 0 ) 
                PTMUtil.systemExit("Trace file needs to be specified, Exit from MainPTM line 132.");
            ParticleObserver observer = null;
            observer = new ParticleObserver(traceFileName,
                                            Environment.getFileType(traceFileName),
                                            startTime, endTime, PTMTimeStep,
                                            numberOfParticles);
            RouteHelper routeHelper = Environment.getRouteHelper();
            SwimHelper swimHelper = Environment.getSwimHelper();
            SurvivalHelper survivalHelper = Environment.getSurvivalHelper();
            if(DEBUG) System.out.println("Set observer, helpers");
            for(int i=0; i<numberOfParticles; i++) {
            	if (observer != null && routeHelper != null && swimHelper != null && survivalHelper != null){
            		observer.setObserverForParticle(particleArray[i]);
            		routeHelper.setRouteHelperForParticle(particleArray[i]);
            		swimHelper.setSwimHelperForParticle(particleArray[i]);
            		survivalHelper.setSurvivalHelperForParticle(particleArray[i]);
            	}
            	else
            		PTMUtil.systemExit("observer or helpers are not properly set, system exit");
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
            	//no restart option exit, do nothing
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

            //Environment.getHydroInfo(startTime-PTMTimeStep*4);//@todo: warning if < hydro start time 
            // initialize current model time
            //   Globals.currentModelTime = startTime;
            //main loop for running Particle model
            
            //currentModelTime, startTime, endTime, PTMTimeStep are in minutes
            // timeStep is in seconds, which is used to update positions
            for(Globals.currentModelTime  = startTime; 
                Globals.currentModelTime <= endTime; 
                Globals.currentModelTime += PTMTimeStep){

                // output runtime information to screen
            	MainPTM.display(displayInterval);
                if (behavior)
                    Globals.currentMilitaryTime = Integer.parseInt(Globals.getModelTime(Globals.currentModelTime));
                
                // get latest hydro information using Global/model time in minutes!!!!!!
                Environment.getHydroInfo(Globals.currentModelTime);
                Waterbody[] allWbs = Environment.getWbArray();
                //update confusion factors, channel direction, etc.
                swimHelper.updateCurrentInfo(allWbs);
                //update barrier op infos, etc.
                routeHelper.updateCurrentInfo(allWbs, Globals.currentModelTime);
                if (DEBUG) System.out.println("Updated flows");         	
            	// update Particle positions
                for (int i=0; i<numberOfParticles; i++){
            		// timeStep in seconds (PTMTimeStep in minutes)!
                	if (DEBUG) System.out.println("Update particle " + i +" position");
            		particleArray[i].updatePosition(timeStep);
            		if (DEBUG) System.out.println("Updated particle "+i +" position");
            		particleArray[i].clear();
                }
      
                // animation output
                if ( animationOutput != null ) animationOutput.output();
                // write out restart file information
                if ( outRestart != null ) outRestart.output();
      
            }
            //print out travel times
            Environment.getBehaviorInputs().getTravelTimeOutput().travelTimeOutput();
            if ( animationOutput != null ) animationOutput.FlushAndClose();
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
    	//TODO put back later
    	/*
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
        */
    }


}
