
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
import java.util.Map;

import java.util.HashMap;
import java.nio.IntBuffer;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
/*
 * main function of PTM
 */
public class MainPTM {
	public static boolean MULTI_THREADED = false;
	public static String configFile;

	public static void main(String[] args) {
		//DEBUG = true;
		try {
			// Initialize environment
			if (DEBUG) System.out.println("Initializing environment");
			configFile = args[0];
			System.out.println("Configuration file: " + configFile);

			PTMEnv Environment = new PTMEnv();
			if (DEBUG) System.out.println("Environment initialized");

			// set global environment pointer
			Globals.Environment = Environment;
			
			// Echo the configuration to a runnable YAML
			PTMFixedData.echoConfig();

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

			Environment.addBehaviors();

			numberOfParticles = numberOfRestartParticles
					+ Environment.getNumberOfParticlesInjected();
			if(DEBUG) System.out.println("total number of particles injected are " + numberOfParticles);

			particleArray = new Particle[numberOfParticles];
			if(DEBUG) System.out.println("restart particles " + numberOfRestartParticles);

			for(int pNum = numberOfRestartParticles; pNum < numberOfParticles; pNum++)
				particleArray[pNum] = new Particle(Environment.getParticleFixedInfo());
			if(DEBUG) System.out.println("particles initialized");

			// insert particles from fixed input information
			Environment.setParticleInsertionInfo(particleArray, numberOfRestartParticles);
			if(DEBUG) System.out.println("Set insertion info");
			// set observer on each Particle
			String traceFileName = Environment.getTraceFileName();
			if ( traceFileName == null || traceFileName.length() == 0 )
				PTMUtil.systemExit("Trace file needs to be specified, Exit from MainPTM");
			ParticleObserver observer = null;
			observer = new ParticleObserver(traceFileName,
					Environment.getFileType(traceFileName),
					startTime, endTime, PTMTimeStep,
					numberOfParticles);
			RouteHelper routeHelper = Environment.getRouteHelper();
			SwimHelper swimHelper = Environment.getSwimHelper();
			SurvivalHelper survivalHelper = Environment.getSurvivalHelper();

			if(DEBUG) System.out.println("Set observer, helpers");
			//in case of particle, no need for check survival
			for(int i=0; i<numberOfParticles; i++) {
				if (observer != null && routeHelper != null && swimHelper != null) {
					observer.setObserverForParticle(particleArray[i]);
					routeHelper.setRouteHelperForParticle(particleArray[i]);
					swimHelper.setSwimHelperForParticle(particleArray[i]);
				}
				else
					PTMUtil.systemExit("observer or swim and route helpers are not properly set, system exit");
				//only salmon needs survival calc
				if (survivalHelper != null)
					survivalHelper.setSurvivalHelperForParticle(particleArray[i]);
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
				System.out.println("No restart file output");
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
				System.out.println("No animation file output");
			}
			if(DEBUG) System.out.println("Set anim output");

			System.out.println("Total number of particles: " + numberOfParticles + "\n");

			// time step (converted to seconds) and display interval
			int timeStep = PTMTimeStep*60;
			int displayInterval = Environment.getDisplayInterval();

			//currentModelTime, startTime, endTime, PTMTimeStep are in minutes
			// timeStep is in seconds, which is used to update positions
			long loopStartTime = System.currentTimeMillis();
			for(Globals.currentModelTime  = startTime;
					Globals.currentModelTime <= endTime;
					Globals.currentModelTime += PTMTimeStep){

				// output runtime information to screen
				if(Globals.DisplaySimulationTimestep)
					MainPTM.display(displayInterval);
				//if (smeltBehavior)
				Globals.currentMilitaryTime = Integer.parseInt(Globals.getModelTime(Globals.currentModelTime));
				
				// Trigger any scheduled insertions into an external routing model
				AltRouteInterface.runAllInsertions();

				// get latest hydro information using Global/model time in minutes!!!!!!
				Environment.getHydroInfo(Globals.currentModelTime);
				Waterbody[] allWbs = Environment.getWbArray();
				//update confusion factors, channel direction, etc.
				swimHelper.updateCurrentInfo(allWbs);
				//update barrier op infos, etc.
				routeHelper.updateCurrentInfo(allWbs, Globals.currentModelTime);
				if (DEBUG) System.out.println("Updated flows");
				if (MULTI_THREADED){
					ParticleLoop.doAll(particleArray, timeStep);
				} else {
					// update Particle positions
					for (int i=0; i<numberOfParticles; i++){
						// timeStep in seconds (PTMTimeStep in minutes)!
						if (DEBUG) System.out.println("Update particle " + i +" position");
						particleArray[i].updatePosition(timeStep);
						if (DEBUG) System.out.println("Updated particle "+i +" position");
						//set Particle._timeUsedInSecond = 0.0
						particleArray[i].clear();
					}
				}
				// animation output
				if ( animationOutput != null ) animationOutput.output();
				// write out restart file information
				if ( outRestart != null ) outRestart.output();
			}
			System.out.println("Execution time in milliseconds: " + (System.currentTimeMillis() - loopStartTime));
			if(Environment.getParticleType().equalsIgnoreCase("Salmon_Particle")){
				Map<Integer, IntBuffer> lastTraces = new HashMap<Integer, IntBuffer>();
				for (int i=0; i<numberOfParticles; i++){
					Particle p = particleArray[i];
					if (p.nd!=null && p.wb != null) {
						int[] ndwb = {p.nd.getEnvIndex(), p.wb.getEnvIndex()};
						lastTraces.put(p.Id, IntBuffer.wrap(ndwb));
					}
				}
				//print out travel times
				Environment.getBehaviorInputs().getTravelTimeOutput().travelTimeOutput();
				Environment.getBehaviorInputs().getSurvivalInputs().writeSurvivalRates(lastTraces);
				RouteInputs rIn = Environment.getBehaviorInputs().getRouteInputs();

				rIn.writeEntrainmentRates();
				if(Particle.ADD_TRACE)
					rIn.writeFlux(particleArray);
			}

			if ( animationOutput != null ) animationOutput.FlushAndClose();
			// write out restart file information
			if ( outRestart != null ) outRestart.output();

			// Write outputs to netCDF file
			Output output = new Output(particleArray);

			// clean up after run is over
			observer = null;
			particleArray = null;

			if (TransProbs.getFileOpen()) {
				TransProbs.closeFile();
			}

			Grid.closeTidefile();

			System.out.println("done simulation");

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
