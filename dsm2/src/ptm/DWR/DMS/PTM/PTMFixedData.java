//    Copyright (C) 1996, 2009 State of California, Department of Water
//    Resources.
//
//    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
//    numerical model.  No protection claimed in original FOURPT and
//    Branched Lagrangian Transport Model (BLTM) code written by the
//    United States Geological Survey.  Protection claimed in the
//    routines and files listed in the accompanying file "Protect.txt".
//    If you did not receive a copy of this file contact
//    Tara Smith, below.
//
//    This program is licensed to you under the terms of the GNU General
//    Public License, version 2, as published by the Free Software
//    Foundation.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, contact Tara Smith, below,
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
//    Tara Smith
//    California Dept. of Water Resources
//    Division of Planning, Delta Modeling Section
//    1416 Ninth Street
//    Sacramento, CA  95814
//    916-653-9885
//    tara@water.ca.gov
//
//    or see our home page: http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/
package DWR.DMS.PTM;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.fasterxml.jackson.dataformat.yaml.YAMLGenerator;
import com.fasterxml.jackson.dataformat.yaml.YAMLMapper;

import ucar.ma2.ArrayChar;
import ucar.ma2.ArrayDouble;
import ucar.ma2.ArrayInt;
import ucar.ma2.ArrayStructureBB;
import ucar.ma2.StructureMembers.Member;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Structure;
import ucar.nc2.Variable;

/**
 * Encapsulates the fixed information for this model
 *
 * @author Nicky Sandhu
 * @version $Id: PTMFixedData.java,v 1.6.6.5 2007/07/31 18:30:39 eli2 Exp $
 */
// this class is mainly used for read fixed data
public class PTMFixedData {

	// Constants
	private static final int MAX_CHANNELS = 800;
	private static final int MAX_RESERVOIRS = 100;
	private static final int MAX_DIVERSIONS = 0;
	private static final int MAX_PUMPS = 0;
	private static final int MAX_BOUNDARY_WATERBODIES = 1000;
	private static final int MAX_NODES = MAX_CHANNELS + 10;
	private static final int MAX_RESERVOIR_NODES = 50;
	private static final int MAX_CONVEYORS = 50;
	private static final int MAX_CROSS_SECTIONS = MAX_CHANNELS*5;
	private static final int MAX_STAGE_BOUNDARIES = 5;

	// Assume Fortran's base-one index
	public static final int BASE_INDEX = 1;

	// Missing value used in Fortran code
	public static final int MISS_VAL = -901;

	private static List<Integer> extChanNums;
	private static List<Integer> chanLength;
	private static int[][] extChanNodes;
	private static List<String> reservoirNames;
	private static List<String> conveyorNames;
	private static List<Float> resArea;
	private static List<Float> resBottomElev;
	private static List<String> qextNames;
	private static Map<String, Integer> qextExtNodeNums;
	private static Map<String, List<Integer>> conveyorNodeNums;
	private static Map<String, Integer> qextFlowIndices;
	private static Map<String, Integer> stageBoundaryExtNodes;
	private static Map<String, Map<String, ArrayList<Integer>>> stageBoundaryFlowIndices;
	private static List<String> stageBoundaryNames;
	private static List<Integer> extNodeNums;
	private static float theta;
	private static Map<String, FluxGroup> fluxGroups;

	private static int numChannels, numReservoirs, numConveyors, numBoundaryWaterbodies, numStageBoundaries,
	numVirtualNodes, modelStartTime, modelEndTime;

	private static Config config;

	private static NetcdfFile ncd;

	static {
		ObjectMapper mapper = new ObjectMapper(new YAMLFactory());
		try {
			config = mapper.readValue(new File(MainPTM.configFile), Config.class);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		conveyorNodeNums = new HashMap<>();

		ncd = Grid.getNCD();

		// Read grid data from the tidefile
		readChannel();

		readReservoir();	    
		readQext();
		readStageBoundaries();
		readNodeFlowConnections();
		readBoundaryStage();
		createStageBoundaryFlowIndices();
		readTransfer();

		Grid.addWaterbodiesToNodes();
		// Reading resFlowConnect here so all the qextExtNodeNums are defined beforehand
		Grid.readResNodeConnect();
		Grid.readResFlowConnect();	    
		Grid.createPlaceholderWaterbodies();
		Grid.compileWaterbodies();

		theta = config.theta;

		// Create flux groups
		createFluxGroups();

		modelStartTime = Grid.MISSING;
		modelEndTime = Grid.MISSING;
	}

	/**
	 * Obtain the config object
	 * @return						Config object
	 */
	public static Config getConfig() {
		return config;
	}

	/**
	 * Obtain the path to the tidefile that is specified in the config file
	 * @return						full path to the tidefile
	 */
	public static String getTidefile() {
		return config.tidefile;
	}

	/**
	 * Obtain the theta value (low-pass filter constant) that is specified in the config file
	 * @return						theta
	 */
	public static float getTheta() {
		return theta;
	}

	/**
	 * Obtain grid limits (e.g., max number of channels) and return in LimitsFixedData object
	 * @return						LimitsFixedData object
	 */
	public LimitsFixedData getLimitsFixedData(){
		int maxChannels = getMaxNumberOfChannels();
		int maxReservoirs = getMaxNumberOfReservoirs();
		int maxDiversions = getMaxNumberOfDiversions();
		int maxPumps = getMaxNumberOfPumps();
		int maxBoundaryWaterbodies = getMaxNumberOfBoundaryWaterbodies();
		int maxNodes = getMaxNumberOfNodes();
		int maxXSections = getMaxNumberOfCrossSections();

		return new LimitsFixedData(maxChannels,
				maxReservoirs,
				maxDiversions,
				maxPumps,
				maxBoundaryWaterbodies,
				maxNodes,
				maxXSections);
	}

	/**
	 * Obtain particle data and return in a ParticleFixedData object
	 * @return						ParticleFixedData object
	 */
	public ParticleFixedData getParticleFixedData(){
		ParticleFixedData pFD = new ParticleFixedData();

		int nInjections = getParticleNumberOfInjections();
		int[] nNode = getParticleInjectionNodes();
		int[] nInjected = getParticleNumberOfParticlesInjected();
		int[] startJulmin = getParticleInjectionStartJulmin();
		int[] lengthJulmin = getParticleInjectionLengthJulmin();
		boolean qBinary = qualBinaryBooleanInput();
		int ngroups = getNumberOfChannelGroups();
		String[] qNames =null;

		pFD.setVariables(config.ptm_ivert, config.ptm_itrans, config.ptm_iey, config.ptm_iez,
				config.ptm_iprof, config.ptm_igroup, config.ptm_flux_percent, config.ptm_group_percent,
				config.ptm_flux_cumulative);
		pFD.setVariables(config.ptm_random_seed, config.ptm_trans_constant, config.ptm_vert_constant,
				config.ptm_trans_a_coef, config.ptm_trans_b_coef, config.ptm_trans_c_coef, config.ptm_num_animated);
		pFD.setVariables(nInjections,
				nNode, nInjected,
				startJulmin, lengthJulmin);
		pFD.setVariables(ngroups,qBinary,qNames);

		return pFD;
	}

	/**
	 * Obtain flux data and return in a FluxFixedData object
	 * @return						FluxFixedData object
	 */
	public FluxFixedData[] getFluxFixedData(){    
		int numberOfFluxes;
		FluxFixedData[] fFD;
		int[] inArray, outArray, inTypeArray, outTypeArray;
		String name, nameIn, nameOut;

		numberOfFluxes = getNumberOfFluxes();
		fFD = new FluxFixedData[numberOfFluxes];
		for(int i=0; i<numberOfFluxes; i++) {
			inArray = getFluxArray(config.particle_flux_output.get(i).get(Config.PARTICLE_FLUX_FROM_WB_INDEX).toString(), 
					config.particle_flux_output.get(i).get(Config.PARTICLE_FLUX_FROM_WB_TYPE_INDEX).toString());
			outArray = getFluxArray(config.particle_flux_output.get(i).get(Config.PARTICLE_FLUX_TO_WB_INDEX).toString(), 
					config.particle_flux_output.get(i).get(Config.PARTICLE_FLUX_TO_WB_TYPE_INDEX).toString());

			inTypeArray = getFluxType(config.particle_flux_output.get(i).get(Config.PARTICLE_FLUX_FROM_WB_INDEX).toString(), 
					config.particle_flux_output.get(i).get(Config.PARTICLE_FLUX_FROM_WB_TYPE_INDEX).toString());
			outTypeArray = getFluxType(config.particle_flux_output.get(i).get(Config.PARTICLE_FLUX_TO_WB_INDEX).toString(), 
					config.particle_flux_output.get(i).get(Config.PARTICLE_FLUX_TO_WB_TYPE_INDEX).toString());
			name = config.particle_flux_output.get(i).get(Config.PARTICLE_FLUX_NAME_INDEX).toString().toUpperCase();
			nameIn = config.particle_flux_output.get(i).get(Config.PARTICLE_FLUX_FROM_WB_INDEX).toString().toUpperCase();
			nameOut = config.particle_flux_output.get(i).get(Config.PARTICLE_FLUX_TO_WB_INDEX).toString().toUpperCase();
			fFD[i] = new FluxFixedData(name,
					new WaterbodyGroup(nameIn, inTypeArray, inArray),
					new WaterbodyGroup(nameOut, outTypeArray, outArray));
		}

		return fFD;
	}

	/**
	 * Obtain output groups
	 * @return						output groups
	 */
	public Group[] getOutputGroups(){
		int numberOfGroups = getNumberOfGroupOutputs();
		Group [] groups = new Group[numberOfGroups];

		for(int i=0; i<groups.length; i++){
			int[] memberArray = getGroupMemberIndex(i);
			int[] typeArray = getGroupMemberType(i);
			String name = getGroupMemberName(i); //"TODO:GROUP";  //@todo: get names for printing error/info messages
			groups[i] = new WaterbodyGroup(name,typeArray,memberArray);
		}
		return groups;
	} 

	/**
	 * Return boolean indicating whether the qual binary exists (currently not implemented)
	 * @return
	 */
	public boolean qualBinaryBooleanInput(){
		return false;
	}

	/**
	 * Obtain the total number of waterbodies in the grid
	 * @return						total number of waterbodies
	 */
	public int getNumberOfWaterbodies() {
		return getNumberOfChannels() + getNumberOfReservoirs() +
				getNumberOfStageBoundaries() + getNumberOfBoundaryWaterbodies() +
				getNumberOfConveyors();
	}

	/**
	 * Obtain the number of channels in the grid
	 * @return						number of channels
	 */
	public int getNumberOfChannels() {
		return numChannels;
	}

	/**
	 * Obtain the number of channel groups specified in the config file
	 * @return						number of channel groups
	 */
	public int getNumberOfChannelGroups() {
		return config.particle_group_output.size();
	}

	/**
	 * Obtain the number of reservoirs in the grid
	 * @return						number of reservoirs
	 */
	public int getNumberOfReservoirs() {
		return numReservoirs;
	}

	/**
	 * Obtain the number of diversions in the grid
	 * @return						number of diversions
	 */
	public int getNumberOfDiversions() {
		return 0;
	}

	/**
	 * Obtain the number of pumps in the grid; currently hard-coded to zero
	 * @return						number of pumps
	 */
	public int getNumberOfPumps() {
		return 0;
	}

	/**
	 * Obtain the number of boundary waterbodies in the grid
	 * @return						number of boundary waterbodies
	 */
	public int getNumberOfBoundaryWaterbodies() {
		return numBoundaryWaterbodies;
	}

	/**
	 * Obtain the number of stage boundaries in the grid
	 * @return						number of stage boundaries
	 */
	public static int getNumberOfStageBoundaries() {
		return numStageBoundaries;
	}

	/**
	 * Obtain the number of conveyors in the grid; currently hard-coded to zero per the Fortran code
	 * @return						number of conveyors (zero)
	 */
	public int getNumberOfConveyors() {
		return 0;
	}

	/**
	 * Obtain the number of nodes in the grid
	 * @return						number of nodes
	 */
	public int getNumberOfNodes() {
		return extNodeNums.size() + numVirtualNodes;
	}

	/**
	 * Obtain the number of cross sections in the grid
	 * @return						number of cross sections
	 */
	public int getNumberOfXSections() {
		return 0;
	}

	/**
	 * Obtain the maximum number of channel specified in the Java code
	 * @return						maximum number of channels
	 */
	static int getMaxNumberOfChannels() {
		return MAX_CHANNELS;
	}

	/**
	 * Obtain the maximum number of reservoirs specified in the Java code
	 * @return						maximum number of reservoirs
	 */
	static int getMaxNumberOfReservoirs() {
		return MAX_RESERVOIRS;
	}

	/**
	 * Obtain the maximum number of diversions specified in the Java code
	 * @return						maximum number of diversions
	 */
	static int getMaxNumberOfDiversions() {
		return MAX_DIVERSIONS;
	}

	/**
	 * Obtain the maximum number of pumps specified in the Java code
	 * @return						maximum number of pumps
	 */
	static int getMaxNumberOfPumps() {
		return MAX_PUMPS;
	}

	/**
	 * Obtain the maximum number of boundary waterbodies specified in the Java code
	 * @return						maximum number of boundary waterbodies
	 */
	static int getMaxNumberOfBoundaryWaterbodies() {
		return MAX_BOUNDARY_WATERBODIES;
	}

	/**
	 * Obtain the maximum number of nodes specified in the Java code
	 * @return						maximum number of nodes
	 */
	static int getMaxNumberOfNodes() {
		return MAX_NODES + 2*MAX_CONVEYORS;
	}

	/**
	 * Obtain the maximum number of conveyors specified in the Java code
	 * @return						maximum number of conveyors
	 */
	static int getMaxNumberOfConveyors() {
		return MAX_CONVEYORS;
	}

	/**
	 * Obtain the maximum number of cross sections specified in the Java code
	 * @return						maximum number of cross sections
	 */
	static int getMaxNumberOfCrossSections() {
		return MAX_CROSS_SECTIONS;
	}

	/**
	 * Obtain the maximum number of stage boundaries specified in the Java code
	 * @return						maximum number of stage boundaries
	 */
	static int getMaxNumberOfStageBoundaries() {
		return MAX_STAGE_BOUNDARIES;
	}

	/**
	 * Obtain the maximum number of reservoir nodes specified in the Java code
	 * @return						maximum number of reservoir nodes
	 */
	static int getMaxNumberOfReservoirNodes() {
		return MAX_RESERVOIR_NODES;
	}

	/**
	 * Obtain the maximum number of waterbodies specified in the Java code
	 * @return						maximum number of waterbodies
	 */
	static int getMaxNumberOfWaterbodies() {
		return getMaxNumberOfChannels() + getMaxNumberOfReservoirs() + getMaxNumberOfDiversions() +
				getMaxNumberOfPumps() + getMaxNumberOfStageBoundaries() + getMaxNumberOfBoundaryWaterbodies() +
				getMaxNumberOfConveyors();
	}

	/**
	 * Obtain the unique ID for the specified channel number
	 * @param i						channel number (1 to number of channels)
	 * @return						unique ID
	 */
	static int getUniqueIdForChannel(int i) {
		return i;
	}

	/**
	 * Obtain the unique ID for the specified reservoir number
	 * @param i						reservoir number (1 to number of reservoirs)
	 * @return						unique ID
	 */
	static int getUniqueIdForReservoir(int i) {
		return MAX_CHANNELS + i;
	}

	/**
	 * Obtain the unique ID for the specified stage boundary number
	 * @param i						stage boundary number (1 to number of stage boundaries)
	 * @return						unique ID
	 */
	static int getUniqueIdForStageBoundary(int i) {
		return MAX_CHANNELS + MAX_RESERVOIRS + i;
	}

	/**
	 * Obtain the unique ID for the specified boundary number
	 * @param i						boundary number (1 to number of boundaries)
	 * @return						unique ID
	 */
	static int getUniqueIdForBoundary(int i) {
		return MAX_CHANNELS + MAX_RESERVOIRS + MAX_STAGE_BOUNDARIES + i;
	}

	/**
	 * Obtain the unique ID for the specified conveyor number
	 * @param i						conveyor number (1 to number of conveyors)
	 * @return						unique ID
	 */
	static int getUniqueIdForConveyor(int i) {
		return MAX_CHANNELS + MAX_RESERVOIRS + MAX_STAGE_BOUNDARIES + MAX_BOUNDARY_WATERBODIES + i;
	}

	int getLocalIdForWaterbody(int uniqueId) {
		if(uniqueId<=(MAX_CHANNELS)) {
			return uniqueId;
		}
		else if(uniqueId<=(MAX_CHANNELS + MAX_RESERVOIRS)) {
			return uniqueId - MAX_CHANNELS;
		}
		else if(uniqueId<=(MAX_CHANNELS + MAX_RESERVOIRS + MAX_STAGE_BOUNDARIES)) {
			return uniqueId - (MAX_CHANNELS + MAX_RESERVOIRS);
		}
		else if(uniqueId<=(MAX_CHANNELS + MAX_RESERVOIRS + MAX_STAGE_BOUNDARIES + MAX_BOUNDARY_WATERBODIES)) {
			return uniqueId - (MAX_CHANNELS + MAX_RESERVOIRS + MAX_STAGE_BOUNDARIES);
		}
		else if(uniqueId>(MAX_CHANNELS + MAX_RESERVOIRS + MAX_STAGE_BOUNDARIES + MAX_BOUNDARY_WATERBODIES)) {
			return uniqueId - (MAX_CHANNELS + MAX_RESERVOIRS + MAX_STAGE_BOUNDARIES + MAX_BOUNDARY_WATERBODIES);
		}

		return Grid.MISSING;
	}

	/**
	 * Obtain qual constituent names; currently hard-coded to return an empty String[]
	 * @return						qual constituent names
	 */
	static String[] getQualConstituentNames() {
		return new String[] {};
	}

	/**
	 * Obtain the name associated with the specified waterbody ID
	 * @param wbId					unique ID for the waterbody
	 * @return						waterbody name
	 */
	String getWaterBodyName(int wbId) {
		String waterbodyName;
		int index;

		waterbodyName = "";

		if(wbId>=getUniqueIdForReservoir(BASE_INDEX) && wbId<getUniqueIdForStageBoundary(BASE_INDEX)) {
			index = wbId-getUniqueIdForReservoir(BASE_INDEX);
			if(index>=0 && index<reservoirNames.size()) {waterbodyName = reservoirNames.get(index);}
		}
		else if(wbId>=getUniqueIdForStageBoundary(BASE_INDEX) && wbId<getUniqueIdForBoundary(BASE_INDEX)) {
			waterbodyName = "STAGE_BOUNDARY";
		}
		else if(wbId>=getUniqueIdForBoundary(BASE_INDEX) && wbId<getUniqueIdForConveyor(BASE_INDEX)) {
			index = wbId-getUniqueIdForBoundary(BASE_INDEX);
			if(index>=0 && index<qextNames.size()) {waterbodyName = qextNames.get(index);}
		}
		else {
			PTMUtil.systemExit("getWaterbodyName: Attempting to look up an undefined waterbody index: " + wbId + ". System exit.");
		}

		return waterbodyName;
	}

	/**
	 * Obtain the array of waterbody IDs associated with the specified node
	 * @param i						interior node number
	 * @return						array of waterbody IDs
	 */
	int[] getWaterbodyIdArrayForNode(int i) {
		GridNode tempNode;
		int[] waterbodyIdArray = {};

		tempNode = Grid.getIntNode(i);
		if(tempNode!=null) {waterbodyIdArray = tempNode.getWaterbodyIdArray();}
		return waterbodyIdArray;
	}

	/**
	 * Obtain the length of the specified channel
	 * @param i						channel number (1 to number of channels)
	 * @return						length
	 */
	int getChannelLength(int i) {
		return chanLength.get(i-1);
	}

	/**
	 * Obtain the area of the specified reservoir
	 * @param i						reservoir number (1 to number of reservoirs)
	 * @return						area
	 */
	float getReservoirArea(int i) {
		if(i<=resArea.size()) {
			return resArea.get(i-1);
		}
		return 0;
	}

	/**
	 * Obtain the bottom elevation of the specified reservoir
	 * @param i						reservoir number (1 to number of reservoirs)
	 * @return						bottom elevation
	 */
	float getReservoirBottomElevation(int i) {
		if(i<=resBottomElev.size()) {
			return resBottomElev.get(i-1);
		}
		return 0;
	}

	/**
	 * Obtain the name of the specified reservoir
	 * @param i						reservoir number (1 to number of reservoirs)
	 * @return						name
	 */
	String getReservoirName(int i) {
		if(i<=reservoirNames.size()) {
			return reservoirNames.get(i-1);
		}
		return "";
	}

	/**
	 * Obtain the name of the specified conveyors; currently hard-coded to ""
	 * @param i						conveyor number (1 to number of conveyors)
	 * @return						name
	 */
	String getConveyorName(int i) {
		return "";
	}

	/**
	 * Obtain the widths of the specified cross section; currently hard-coded to MISS_VAL
	 * @param i						cross section number
	 * @return						widths
	 */
	float[] getXSectionWidths(int i) {
		return new float[] {MISS_VAL, MISS_VAL};
	}

	/**
	 * Obtain the elevations of the specified cross section; currently hard-coded to MISS_VAL
	 * @param i						cross section number
	 * @return						elevations
	 */
	float[] getXSectionElevations(int i) {
		return new float[] {MISS_VAL, MISS_VAL};
	}

	/**
	 * Obtain the areas of the specified cross section; currently hard-coded to MISS_VAL
	 * @param i						cross section number
	 * @return						areas
	 */
	float[] getXSectionAreas(int i) {
		return new float[] {MISS_VAL, MISS_VAL};
	}

	/**
	 * Obtain the minimum elevations of the specified cross section; currently hard-coded to MISS_VAL
	 * @param i						cross section number
	 * @return						minimum elevations
	 */
	float getXSectionMinimumElevation(int i) {
		return MISS_VAL;
	}

	/**
	 * Obtain the number of particle injections for neutrally buoyant or position-oriented particles
	 * @return						number of particle injections
	 */
	int getParticleNumberOfInjections() {
		if(config.particle_insertion!=null) {
			return config.particle_insertion.size();
		}

		return 0;
	}

	/**
	 * Obtain the number of particle injection nodes; for neutrally buoyant or position-oriented particles
	 * @return						number of particle injection nodes
	 */
	int[] getParticleInjectionNodes() {
		int numInjections;
		int[] injectionNodes;
		int extInjectionNode;

		if(config.particle_insertion!=null) {
			numInjections = getParticleNumberOfInjections();
			injectionNodes = new int[numInjections];

			for(int i=0; i<numInjections; i++) {
				extInjectionNode = ((Number) config.particle_insertion.get(i).get(0)).intValue();
				injectionNodes[i] = PTMFixedData.getIntNodeNum(extInjectionNode);
			}
			return injectionNodes;
		}

		return null;
	}

	/**
	 * Obtain the number of particles injected; for neutrally buoyant or position-oriented particles
	 * @return						number of particles injected
	 */
	int[] getParticleNumberOfParticlesInjected() {
		int numInjections;
		int[] particlesInjected;

		if(config.particle_insertion!=null) {
			numInjections = getParticleNumberOfInjections();
			particlesInjected = new int[numInjections];

			for(int i=0; i<numInjections; i++) {
				particlesInjected[i] = ((Number) config.particle_insertion.get(i).get(1)).intValue();
			}
			return particlesInjected;
		}

		return null;
	}

	/**
	 * Obtain the particle injection start times; for neutrally buoyant or position-oriented particles
	 * @return						number of particle injection start times
	 */
	int[] getParticleInjectionStartJulmin() {
		int numInjections, modelStartTime, thisDelayMinutes;
		int[] delays;

		if(config.particle_insertion!=null) {
			numInjections = getParticleNumberOfInjections();
			delays = new int[numInjections];

			modelStartTime = getModelStartTime();

			for(int i=0; i<numInjections; i++) {
				thisDelayMinutes = parseIntervalToMin(config.particle_insertion.get(i).get(2).toString());
				delays[i] = modelStartTime + thisDelayMinutes;
			}
			return delays;
		}

		return null;
	}

	/**
	 * Obtain the number of particle injection lengths; for neutrally buoyant or position-oriented particles
	 * @return						number of particle injection lengths
	 */
	int[] getParticleInjectionLengthJulmin() {
		int[] particleInjectionLength;

		if(config.particle_insertion!=null) {
			particleInjectionLength = new int[config.particle_insertion.size()];

			for(int i=0; i<config.particle_insertion.size(); i++) {
				particleInjectionLength[i] = parseIntervalToMin(config.particle_insertion.get(i).get(3).toString());
			}
			return particleInjectionLength;
		} 

		return null;
	}

	/**
	 * Obtain the number of fluxes specified in the config file 
	 * @return						number of fluxes
	 */
	public static int getNumberOfFluxes() {
		return config.particle_flux_output.size();
	}

	/**
	 * Obtain the flux array for the specified waterbody
	 * @param extWb					external waterbody number
	 * @param wbType					waterbody type
	 * @return						flux array
	 */
	public static int[] getFluxArray(String extWb, String wbType) {

		if(wbType.equalsIgnoreCase("GROUP")) {

			if(fluxGroups.containsKey(extWb.toUpperCase())) {
				return fluxGroups.get(extWb.toUpperCase()).getIntIds();
			}
			else {
				return new int[] {Grid.MISSING};
			}
		}

		return new int[] {getWaterbodyUniqueId(extWb, wbType)};  
	}

	/**
	 * Obtain the flux type code
	 * @param extWb					external waterbody number
	 * @param wbType					waterbody type
	 * @return						flux type code
	 */
	public static int[] getFluxType(String extWb, String wbType) {

		if(wbType.equalsIgnoreCase("GROUP")) {
			if(fluxGroups.containsKey(extWb.toUpperCase())) {
				return fluxGroups.get(extWb.toUpperCase()).getTypes();
			}
			else {
				return new int[] {Grid.MISSING};
			}
		}

		return new int[] {getFluxTypeCode(wbType)};

	}

	/**
	 * Obtain the flux type code for the specified waterbody type
	 * @param wbType					waterbody type
	 * @return						flux type code
	 */
	public static int getFluxTypeCode(String wbType) {
		int fluxType;

		if(Grid.fluxTypes.containsKey(wbType.toUpperCase())) {
			fluxType = Grid.fluxTypes.get(wbType.toUpperCase());
		}
		else {
			fluxType = -1;
		}
		return fluxType;
	}

	/**
	 * Obtain the unique ID for the specified external waterbody number and waterbody type
	 * @param extWb					external waterbody number
	 * @param wbType					waterbody type
	 * @return						unique ID
	 */
	public static int getWaterbodyUniqueId(String extWb, String wbType) {
		int intWbId;

		if(wbType.equalsIgnoreCase("CHAN")) {
			intWbId = getIntChanNum(Integer.parseInt(extWb));
			return getUniqueIdForChannel(intWbId);
		}
		else if(wbType.equalsIgnoreCase("RES")) {
			return getIntResNum(extWb);
		}
		return Grid.MISSING;
	} 

	/**
	 * Obtain the number of output groups specified in the config file
	 * @return						number of output groups
	 */
	int getNumberOfGroupOutputs() {
		return config.particle_group_output.size();
	}

	/**
	 * Obtain the internal numbers of the members of the specified group
	 * @param i						zero-based index of the group
	 * @return						internal numbers of the group members
	 */
	int[] getGroupMemberIndex(int i) {
		String groupName = config.particle_group_output.get(i).get(Config.PARTICLE_GROUP_OUTPUT_GROUPNAME_INDEX).toString();
		return fluxGroups.get(groupName.toUpperCase()).getIntIds(); 
	}

	/**
	 * Obtain the waterbody types of the members of the specified group
	 * @param i						zero-based index of the group
	 * @return						waterbody types of the group members
	 */
	int [] getGroupMemberType(int i) {
		String groupName = config.particle_group_output.get(i).get(Config.PARTICLE_GROUP_OUTPUT_GROUPNAME_INDEX).toString();
		return fluxGroups.get(groupName.toUpperCase()).getTypes();
	}

	/**
	 * Obtain the name of the specified group
	 * @param i						zero-based index of the group
	 * @return						name of the group
	 */
	String getGroupMemberName(int i) {
		String name = config.particle_group_output.get(i).get(Config.PARTICLE_GROUP_OUTPUT_NAME_INDEX).toString().toUpperCase();
		return name;
	}

	/**
	 * Obtain the Julian minutes corresponding to the specified String date and time
	 * @param date					String date in ddLLLyyyy format
	 * @param time					String time in HHmm format
	 * @return						Julian minutes
	 */
	int getJulianTime(String date, String time) {
		DateTimeFormatter formatter, datjulFormatter;
		LocalDateTime datetime;
		int julian;
		int hour, min;

		formatter = new DateTimeFormatterBuilder().parseCaseInsensitive()
				.appendPattern("ddLLLyyyyHHmm").toFormatter(Locale.ENGLISH);
		datetime = LocalDateTime.parse(date + time, formatter);

		hour = datetime.getHour();
		min = datetime.getMinute();

		datjulFormatter = DateTimeFormatter.ofPattern("ddLLLyyyy");

		julian = PTMUtil.datjul(datetime.format(datjulFormatter));

		return julian*24*60 + hour*60 + min;  
	}

	/**
	 * Obtain model start time in Julian minutes
	 * @return						model start time in Julian minutes
	 */
	int getModelStartTime() {
		if(modelStartTime==Grid.MISSING) {
			modelStartTime = getJulianTime(config.ptm_start_date, config.ptm_start_time);
		}
		return modelStartTime;
	}

	/**
	 * Obtain model end time in Julian minutes
	 * @return						model end time in Julian minutes
	 */
	int getModelEndTime() {
		if(modelEndTime==Grid.MISSING) {
			modelEndTime = getJulianTime(config.ptm_end_date, config.ptm_end_time); 
		}
		return modelEndTime;
	}

	/**
	 * Obtain PTM time step specified in the config file
	 * @return						PTM time step in minutes
	 */
	int getPTMTimeStep() {
		return parseIntervalToMin(config.ptm_time_step);
	}

	/**
	 * Obtain display interval specified in the config file
	 * @return						display interval
	 */
	int getDisplayInterval() {
		return parseIntervalToMin(config.display_intvl);
	}

	/**
	 * Parse a String interval and convert to minutes
	 * @param interval				String representing a time interval, e.g., 1DAY
	 * @return						interval in minutes
	 */
	int parseIntervalToMin(String interval) {
		Pattern p;
		Matcher m;
		String valStr;
		int val;

		interval = interval.toUpperCase();

		p = Pattern.compile("\\d*");
		m = p.matcher(interval);

		val = 0;
		if(m.find()) {
			valStr = m.group(0);
			val = Integer.parseInt(valStr);
		}

		if(interval.contains("DAY")) {
			val = val*24*60;
		} 
		else if(interval.contains("HOUR")) {
			val = val*60;
		}  

		return val;

	}

	/**
	 * Obtain the filename of the specified type
	 * @param type					filename type, e.g., "ANIM"
	 * @return						full path of the file
	 */
	public String getFileName(String type) {
		if(config.io_file==null) {return "";}

		try {
			for(int i=0; i<config.io_file.size(); i++) {
				if(config.io_file.get(i).type.equalsIgnoreCase(type.toUpperCase())) {
					return config.io_file.get(i).file;
				}
			}
		}
		catch (Exception e) {
			PTMUtil.systemExit("Could not obtain " + type + " file. System exit.");
		}
		return "";
	}

	/**
	 * Obtain the interval in minutes for the specified file type
	 * @param type					filename type, e.g., "ANIM"
	 * @return						interval in minutes
	 */
	int getIntervalInMin(String type) {
		try {
			for(int i=0; i<config.io_file.size(); i++) {
				if(config.io_file.get(i).type.equalsIgnoreCase(type.toUpperCase())) {
					return parseIntervalToMin(config.io_file.get(i).interval);
				}
			}
		}
		catch (Exception e) {
			PTMUtil.systemExit("Could not obtain " + type + " interval. System exit.");
		}
		return Grid.MISSING;
	}

	/**
	 * Obtain the full path to the animation file
	 * @return						full path to the animation output file
	 */
	String getAnimationFileName() {
		return getFileName("ANIM");
	}

	/**
	 * Obtain the animation output interval
	 * @return						output interval in minutes
	 */
	int getAnimationOutputInterval() {
		return getIntervalInMin("ANIM");
	}

	/**
	 * Obtain the full path to the trace file
	 * @return						full path to the trace file
	 */
	String getTraceFileName() {
		return getFileName("TRACE");
	}

	/**
	 * Obtain the full path to the restart output file
	 * @return						full path to the restart output file
	 */
	String getRestartOutputFileName() {
		return getFileName("RESTART_OUTPUT");
	}

	/**
	 * Obtain the restart output interval
	 * @return						restart output interval in minutes
	 */
	int getRestartOutputInterval() {
		return getIntervalInMin("RESTART_OUTPUT");
	}

	/**
	 * Obtain the full path to the restart input file
	 * @return						full path to the restart input file
	 */
	String getRestartInputFileName () {
		return getFileName("RESTART_INPUT");
	}

	/**
	 * Read the channel table from the tidefile
	 */
	public static void readChannel() {
		Variable chanVar;
		Structure structure;
		ArrayStructureBB arrayStructureBB;
		List<Member> members;
		Member chanNumMember, chanLengthMember, upNodeMember, downNodeMember;
		ArrayInt.D0 chanNumData, chanLengthData, upNodeData, downNodeData;
		int[] nodes;
		int boundaryNodeIndex;
		GridChannel thisChannel;
		List<Integer> coreNodes, boundaryNodes;

		extChanNums = null;

		coreNodes = new ArrayList<Integer>();
		boundaryNodes = new ArrayList<Integer>();

		// Read channel information
		try {

			chanVar = ncd.findVariable("/hydro/input/channel");
			structure = (Structure) chanVar;
			arrayStructureBB = (ArrayStructureBB) structure.read();
			members = arrayStructureBB.getMembers();
			chanNumMember = members.get(0);
			chanLengthMember = members.get(1);
			upNodeMember = members.get(4);
			downNodeMember = members.get(5);

			extChanNums = new ArrayList<>();
			chanLength = new ArrayList<>();
			numChannels = (int) arrayStructureBB.getSize();
			extChanNodes = new int[numChannels][2];
			for (int i=0; i<numChannels; i++) {
				chanNumData = (ArrayInt.D0) arrayStructureBB.getArray(i, chanNumMember);
				chanLengthData = (ArrayInt.D0) arrayStructureBB.getArray(i,  chanLengthMember);
				upNodeData = (ArrayInt.D0) arrayStructureBB.getArray(i, upNodeMember);
				downNodeData = (ArrayInt.D0) arrayStructureBB.getArray(i, downNodeMember);

				// Create a new GridChannel object and add it to Grid
				thisChannel = new GridChannel(chanNumData.get(), upNodeData.get(), downNodeData.get());
				Grid.addChannel(thisChannel);

				extChanNums.add(chanNumData.get());
				chanLength.add(chanLengthData.get());
				extChanNodes[i][0] = upNodeData.get();
				extChanNodes[i][1] = downNodeData.get();

				// Update coreNodes and boundaryNodes lists
				nodes = new int[] {upNodeData.get(), downNodeData.get()};

				for(int node: nodes) {
					// First, check to see if we've already added this to coreNodes. If not, check to
					// see if it's already in boundaryNodes. If so, it's connected to multiple waterbodies,
					// so it's a core node. If not, call it a boundary node until we encounter it again.
					if(coreNodes.indexOf(node)==-1) {
						boundaryNodeIndex = boundaryNodes.indexOf(node);
						if(boundaryNodeIndex!=-1) {
							boundaryNodes.remove(boundaryNodeIndex);
							coreNodes.add(node);
						}
						else {
							boundaryNodes.add(node);
						}
					}
				}
			}

			Collections.sort(coreNodes);
			Collections.sort(boundaryNodes);

			extNodeNums = new ArrayList<Integer>();
			extNodeNums.addAll(coreNodes);
			extNodeNums.addAll(boundaryNodes);

		} catch (IOException ioe) {
			PTMUtil.systemExit("Exception: " + ioe);
		}
	}

	/**
	 * Read the reservoir table from the tidefile
	 */
	public static void readReservoir() {
		Variable resVar;
		Structure structure;
		ArrayStructureBB arrayStructureBB;
		List<Member> members;
		Member nameMember, areaMember, bottomElevMember;
		ArrayChar.D1 resNameData;
		ArrayDouble.D0 areaData, bottomElevData;
		GridReservoir thisReservoir;
		String thisResName;

		reservoirNames = null;

		// Read reservoir information
		try {

			resVar = ncd.findVariable("/hydro/input/reservoir");
			structure = (Structure) resVar;
			arrayStructureBB = (ArrayStructureBB) structure.read();
			members = arrayStructureBB.getMembers();
			nameMember = members.get(0);
			areaMember = members.get(1);
			bottomElevMember = members.get(2);

			reservoirNames = new ArrayList<>();
			resArea = new ArrayList<>();
			resBottomElev = new ArrayList<>();
			numReservoirs = (int) arrayStructureBB.getSize();
			for (int i=0; i<numReservoirs; i++) {
				resNameData = (ArrayChar.D1) arrayStructureBB.getArray(i, nameMember);
				areaData = (ArrayDouble.D0) arrayStructureBB.getArray(i, areaMember);
				bottomElevData = (ArrayDouble.D0) arrayStructureBB.getArray(i, bottomElevMember); 
				thisResName = resNameData.getString().trim().toUpperCase();

				// Create a new GridReservoir object and add it to Grid
				thisReservoir = new GridReservoir(thisResName);
				Grid.addReservoir(thisReservoir);

				reservoirNames.add(resNameData.getString().trim().toUpperCase());
				resArea.add((float) (areaData.get()*1.0E6));
				resBottomElev.add((float) bottomElevData.get());
			}
		} catch (IOException ioe) {
			PTMUtil.systemExit("Exception: " + ioe);
		}
	}

	/**
	 * Read the transfer table from the tidefile
	 */
	public static void readTransfer() {
		Variable transferVar;
		Structure structure;
		ArrayStructureBB arrayStructureBB;
		List<Member> members;
		Member nameMember, fromTypeMember, fromNumMember, toTypeMember, toNumMember;
		ArrayChar.D1 nameData, fromTypeData, fromIDdata, toTypeData, toIDdata;
		GridConveyor thisConveyor;
		String thisConveyorName, fromType, fromID, toType, toID;

		conveyorNames = null;

		// Read transfer (conveyor) information
		try {

			transferVar = ncd.findVariable("/hydro/input/transfer");
			structure = (Structure) transferVar;

			if(structure==null) {
				System.out.println("transfer table of tidefile is null: no conveyors defined.");
				return;
			}

			arrayStructureBB = (ArrayStructureBB) structure.read();
			members = arrayStructureBB.getMembers();
			nameMember = members.get(0);
			fromTypeMember = members.get(1);
			fromNumMember = members.get(2);
			toTypeMember = members.get(3);
			toNumMember = members.get(4);

			conveyorNames = new ArrayList<>();
			numConveyors = (int) arrayStructureBB.getSize();
			for (int i=0; i<numConveyors; i++) {
				nameData = (ArrayChar.D1) arrayStructureBB.getArray(i, nameMember);
				fromTypeData = (ArrayChar.D1) arrayStructureBB.getArray(i, fromTypeMember);
				fromIDdata = (ArrayChar.D1) arrayStructureBB.getArray(i, fromNumMember);
				toTypeData = (ArrayChar.D1) arrayStructureBB.getArray(i, toTypeMember);
				toIDdata = (ArrayChar.D1) arrayStructureBB.getArray(i, toNumMember); 

				thisConveyorName = nameData.getString().trim().toUpperCase();
				fromType = fromTypeData.toString().trim();
				fromID = fromIDdata.toString().trim();
				toType = toTypeData.toString().trim();
				toID = toIDdata.toString().trim();

				// Create a new GridReservoir object and add it to Grid
				thisConveyor = new GridConveyor(thisConveyorName, fromType, fromID, toType, toID);
				Grid.addConveyor(thisConveyor);

				conveyorNames.add(nameData.getString().trim().toUpperCase());
			}
		} catch (IOException ioe) {
			PTMUtil.systemExit("Exception: " + ioe);
		}
	}

	/**
	 * Read the qext table from the tidefile
	 */
	public static void readQext() {
		Variable qextVar;
		Structure structure;
		ArrayStructureBB arrayStructureBB;
		List<Member> members;
		Member nameMember, attachedObjTypeMember;
		ArrayChar.D1 qextNameData;
		String thisQextName;
		ArrayInt.D0 attachedObjTypeData;

		qextNames = null;

		try {

			qextVar = ncd.findVariable("/hydro/geometry/qext");
			structure = (Structure) qextVar;
			arrayStructureBB = (ArrayStructureBB) structure.read();
			members = arrayStructureBB.getMembers();
			nameMember = members.get(0);
			attachedObjTypeMember = members.get(2);

			qextNames = new ArrayList<>();

			qextExtNodeNums = new HashMap<>();
			numVirtualNodes = 0;
			numBoundaryWaterbodies = (int) arrayStructureBB.getSize();
			for (int i=0; i<numBoundaryWaterbodies; i++) {
				qextNameData = (ArrayChar.D1) arrayStructureBB.getArray(i, nameMember);
				thisQextName = qextNameData.getString().trim().toUpperCase();
				qextNames.add(thisQextName);

				attachedObjTypeData = (ArrayInt.D0) arrayStructureBB.getArray(i, attachedObjTypeMember);
				if(attachedObjTypeData.get()!=Grid.OBJ_NODE) {
					qextExtNodeNums.put(thisQextName, createVirtualNode());
				}
			}
		} catch (IOException ioe) {
			PTMUtil.systemExit("Exception: " + ioe);
		}
	}

	/**
	 * Increment numVirtualNodes and return the new virtual node number
	 * @return						new virtual node number
	 */
	public static int createVirtualNode() {	  
		numVirtualNodes++;
		return MAX_NODES + numVirtualNodes;
	}

	/**
	 * Read the stage_boundaries table from the tidefile
	 */
	public static void readStageBoundaries() {
		Variable stageBoundariesVar;
		Structure structure;
		ArrayStructureBB arrayStructureBB;
		List<Member> members;
		Member nameMember;
		ArrayChar.D1 stageBoundariesNameData;

		stageBoundaryNames = null;

		try {

			stageBoundariesVar = ncd.findVariable("/hydro/geometry/stage_boundaries");
			structure = (Structure) stageBoundariesVar;
			arrayStructureBB = (ArrayStructureBB) structure.read();
			members = arrayStructureBB.getMembers();
			nameMember = members.get(0);

			stageBoundaryNames = new ArrayList<>();
			numStageBoundaries = (int) arrayStructureBB.getSize();
			for (int i=0; i<numStageBoundaries; i++) {
				stageBoundariesNameData = (ArrayChar.D1) arrayStructureBB.getArray(i, nameMember);
				stageBoundaryNames.add(stageBoundariesNameData.getString().trim().toUpperCase());
			}
		} catch (IOException ioe) {
			PTMUtil.systemExit("Exception: " + ioe);
		}
	}

	/**
	 * Read the node_flow_connections table from the tidefile
	 */
	public static void readNodeFlowConnections() {
		Variable nodeFlowConnectionsVar;
		Structure structure;
		ArrayStructureBB arrayStructureBB;
		List<Member> members;
		Member nameMember, flowIndexMember, extNodeNumMember, typeMember;
		ArrayChar.D1 nameData, typeData;
		ArrayInt.D0 flowIndexData, extNodeNumData;

		qextFlowIndices = new HashMap<>();

		try {

			nodeFlowConnectionsVar = ncd.findVariable("/hydro/geometry/node_flow_connections");
			structure = (Structure) nodeFlowConnectionsVar;
			arrayStructureBB = (ArrayStructureBB) structure.read();
			members = arrayStructureBB.getMembers();
			nameMember = members.get(5);
			flowIndexMember = members.get(4);
			extNodeNumMember = members.get(2);
			typeMember = members.get(6);

			for (int i=0; i<arrayStructureBB.getSize(); i++) {
				nameData = (ArrayChar.D1) arrayStructureBB.getArray(i, nameMember);
				flowIndexData = (ArrayInt.D0) arrayStructureBB.getArray(i, flowIndexMember);
				extNodeNumData = (ArrayInt.D0) arrayStructureBB.getArray(i, extNodeNumMember);
				typeData = (ArrayChar.D1) arrayStructureBB.getArray(i, typeMember);

				if(typeData.toString().trim().equalsIgnoreCase("qext")) {
					qextFlowIndices.put(nameData.getString().trim().toUpperCase(), flowIndexData.get());
					qextExtNodeNums.put(nameData.getString().trim().toUpperCase(), extNodeNumData.get());
				}
			}
		} catch (IOException ioe) {
			PTMUtil.systemExit("Exception: " + ioe);
		}
	}

	/**
	 * Read the boundary_stage table from the tidefile
	 */
	public static void readBoundaryStage() {
		Variable boundaryStageVar;
		Structure structure;
		ArrayStructureBB arrayStructureBB;
		List<Member> members;
		Member nameMember, extNodeIDmember;
		ArrayChar.D1 nameData;
		ArrayInt.D0 extNodeIDdata;

		stageBoundaryExtNodes = new HashMap<>();

		try {

			boundaryStageVar = ncd.findVariable("/hydro/input/boundary_stage");
			structure = (Structure) boundaryStageVar;
			arrayStructureBB = (ArrayStructureBB) structure.read();
			members = arrayStructureBB.getMembers();
			nameMember = members.get(0);
			extNodeIDmember = members.get(1);

			for (int i=0; i<arrayStructureBB.getSize(); i++) {
				nameData = (ArrayChar.D1) arrayStructureBB.getArray(i, nameMember);
				extNodeIDdata = (ArrayInt.D0) arrayStructureBB.getArray(i, extNodeIDmember); 

				stageBoundaryExtNodes.put(nameData.getString().trim().toUpperCase(), extNodeIDdata.get());
			}
		} catch (IOException ioe) {
			PTMUtil.systemExit("Exception: " + ioe);
		}
	}

	/**
	 * Create the stage boundary flow indices
	 */
	public static void createStageBoundaryFlowIndices() {
		String stageBoundaryName, qextName;
		Integer extNode, qextExtNode;
		Map<String, ArrayList<Integer>> flowIndices;

		stageBoundaryFlowIndices = new HashMap<>(); 

		for(Map.Entry<String, Integer> entry : stageBoundaryExtNodes.entrySet()) {
			stageBoundaryName = entry.getKey();
			extNode = entry.getValue();

			flowIndices = new HashMap<>();
			flowIndices.put("flowIndex", new ArrayList<Integer>());
			flowIndices.put("objType", new ArrayList<Integer>());
			flowIndices.put("upDownIndex", new ArrayList<Integer>());
			stageBoundaryFlowIndices.put(stageBoundaryName, flowIndices);

			// Find channels with extNode as their upNode or downNode
			for(int i=0; i<extChanNodes.length; i++) {
				for(int upDownIndex=0; upDownIndex<2; upDownIndex++) {
					if(extChanNodes[i][upDownIndex]==extNode) {
						stageBoundaryFlowIndices.get(stageBoundaryName).get("flowIndex").add(i+1);
						stageBoundaryFlowIndices.get(stageBoundaryName).get("objType").add(Grid.OBJ_CHAN);
						stageBoundaryFlowIndices.get(stageBoundaryName).get("upDownIndex").add(upDownIndex);
					}
				}			  
			}

			// Find external flows connected at extNode
			for(Map.Entry<String, Integer> qextEntry : qextExtNodeNums.entrySet()) {
				qextName = qextEntry.getKey();
				qextExtNode = qextEntry.getValue();

				if(qextExtNode==extNode) {
					stageBoundaryFlowIndices.get(stageBoundaryName).get("flowIndex").add(qextFlowIndices.get(qextName));
					stageBoundaryFlowIndices.get(stageBoundaryName).get("objType").add(Grid.OBJ_NODE);
					stageBoundaryFlowIndices.get(stageBoundaryName).get("upDownIndex").add(0);
				}
			}
		}	  
	}

	/**
	 * Obtain the internal channel number for the specified external channel number
	 * @param extChanNum				external channel number
	 * @return						internal channel number
	 */
	public static int getIntChanNum(int extChanNum) {
		int intChanNum;
		intChanNum = extChanNums.indexOf(extChanNum);

		if(intChanNum==-1) {
			PTMUtil.systemExit("getIntChanNum: Could not find external channel number " + extChanNum + ". System exit.");
		}

		// Internal channel numbers use one-based numbering => add one
		return intChanNum+1;
	}

	/**
	 * Obtain the external channel number for the specified internal channel number
	 * @param intChanNum				internal channel number
	 * @return						external channel number
	 */
	public static int getExtChanNum(int intChanNum) {
		int extChanNum = Grid.MISSING;
		try {
			extChanNum = extChanNums.get(intChanNum-1);
		} catch (IndexOutOfBoundsException e) {
			PTMUtil.systemExit("getExtChanNum: Could not find internal channel number " + intChanNum + ". System exit.");
		}
		return extChanNum;
	}

	/**
	 * Obtain the internal node number for the specified external node number
	 * @param extNodeNum				external node number
	 * @return						internal node number
	 */
	public static int getIntNodeNum(int extNodeNum) {
		int intNodeNum;

		// Virtual nodes have numbers outside of the range of the nodes specified in the tidefile =>
		// external and internal nodeNums are the same
		if(extNodeNum>Collections.max(extNodeNums)) {
			return extNodeNum;
		}

		intNodeNum = extNodeNums.indexOf(extNodeNum);


		if(intNodeNum==-1) {
			PTMUtil.systemExit("getIntNodeNum: Could not find external node number " + extNodeNum + ". System exit.");
		}

		// Internal node numbers use one-based numbering => add one
		return intNodeNum+1;
	}

	/**
	 * Obtain the external node number for the specified internal node number
	 * @param intNodeNum				internal node number
	 * @return						external node number
	 */
	public static int getExtNodeNum(int intNodeNum) {
		int extNodeNum = Grid.MISSING;
		try {
			extNodeNum = extNodeNums.get(intNodeNum-1);
		} catch (IndexOutOfBoundsException e) {
			PTMUtil.systemExit("getExtNodeNum: Could not find internal node number " + intNodeNum + ". System exit.");
		}
		return extNodeNum;
	}

	/**
	 * Return a boolean indicating whether the specified external node number is in the list of external node numbers
	 * @param extNodeNum				external node number
	 * @return						boolean indicating whether extNodeNum is in extNodeNums
	 */
	public static boolean extNodeNumsContains(int extNodeNum) {
		return extNodeNums.contains(extNodeNum);
	}

	/**
	 * Obtain the internal reservoir number corresponding to the specified name
	 * @param reservoirName			reservoir name
	 * @return						internal reservoir number
	 */
	public static int getIntResNum(String reservoirName) {
		int intResNum;

		reservoirName = reservoirName.toUpperCase();
		intResNum = reservoirNames.indexOf(reservoirName);

		if(intResNum==-1) {
			PTMUtil.systemExit("getIntResNum: Could not find reservoir name " + reservoirName + ". System exit.");
		}

		// Internal waterbody numbers use one-based numbering => add one
		return getMaxNumberOfChannels() + intResNum + 1;
	}

	/**
	 * Obtain the internal stage boundary number corresponding to the specified name
	 * @param stageBoundaryName		stage boundary name
	 * @return						internal stage boundary number
	 */
	public static int getIntStageBoundaryNum(String stageBoundaryName) {
		int intStageBoundaryNum;
		intStageBoundaryNum = stageBoundaryNames.indexOf(stageBoundaryName);

		if(intStageBoundaryNum==-1) {
			PTMUtil.systemExit("getIntStageBoundaryNum: Could not find stage boundary name " + stageBoundaryName + ". System exit.");
		}

		// Internal waterbody numbers use one-based numbering => add one
		return getMaxNumberOfChannels() + getMaxNumberOfReservoirs() + intStageBoundaryNum + 1;
	}

	/**
	 * Obtain the internal qext number corresponding to the specified name
	 * @param qextName			qext name
	 * @return					internal qext number
	 */
	public static int getIntQextNum(String qextName) {
		int intQextNum; 
		intQextNum = qextNames.indexOf(qextName);

		if(intQextNum==-1) {
			PTMUtil.systemExit("getIntQextNum: Could not find qext name " + qextName + ". System exit.");
		}

		// Internal waterbody numbers use one-based numbering => add one
		return getMaxNumberOfChannels() + getMaxNumberOfReservoirs() + getMaxNumberOfStageBoundaries() + intQextNum + 1;
	}

	/**
	 * Obtain the internal conveyor number corresponding to the specified name 
	 * @param conveyorName				conveyor name
	 * @return							internal conveyor number
	 */
	public static int getIntConveyorNum(String conveyorName) {
		int intConveyorNum;

		conveyorName = conveyorName.toUpperCase();
		intConveyorNum = conveyorNames.indexOf(conveyorName);

		if(intConveyorNum==-1) {
			PTMUtil.systemExit("getIntConveyorNum: Could not find conveyor name " + conveyorName + ". System exit.");
		}

		// Internal waterbody numbers use one-based numbering => add one
		return getMaxNumberOfChannels() + getMaxNumberOfReservoirs() + getMaxNumberOfStageBoundaries() + 
				getMaxNumberOfBoundaryWaterbodies() + intConveyorNum + 1; 
	}

	/**
	 * Create a hash map of FluxGroup objects using the flux group specified in the config file
	 */
	public static void createFluxGroups() {
		String thisName;
		FluxGroup thisGroup;

		fluxGroups = new HashMap<>();

		for(int i=0; i<config.groups.size(); i++) {
			thisName = config.groups.get(i).get(Config.GROUPS_NAME_INDEX).toString().toUpperCase();

			if(!fluxGroups.containsKey(thisName)) {
				thisGroup = new FluxGroup(thisName);
				fluxGroups.put(thisName, thisGroup);
			}

			thisGroup = fluxGroups.get(thisName);
			thisGroup.addObjects(config.groups.get(i).get(Config.GROUPS_TYPE_INDEX).toString(), 
					config.groups.get(i).get(Config.GROUPS_PATTERN_INDEX).toString());
		}

		// Create the "all" group
		thisGroup = new FluxGroup("ALL");
		thisGroup.addObjects("ALL", "");
		fluxGroups.put("ALL", thisGroup);

	}

	/**
	 * Obtain the list of external channel numbers
	 * @return						list of external channel numbers
	 */
	public static List<Integer> getExtChanNums() {
		return extChanNums;
	}

	/**
	 * Obtain the list of reservoir name
	 * @return						list of reservoir names
	 */
	public static List<String> getReservoirNames() {
		return reservoirNames;
	}

	/**
	 * Obtain the list of stage boundary names
	 * @return
	 */
	public static List<String> getStageBoundaryNames() {
		return stageBoundaryNames;
	}

	/**
	 * Obtain the external node number for the specified stage boundary name
	 * @param stageBoundaryName		stage boundary name
	 * @return						external node number
	 */
	public static int getStageBoundaryExtNode(String stageBoundaryName) {
		return stageBoundaryExtNodes.get(stageBoundaryName);
	}

	/**
	 * Obtain the list of qext name
	 * @return						list of qext names
	 */
	public static List<String> getQextNames() {
		return qextNames;
	}

	/**
	 * Obtain the external node number for the specified qext name
	 * @param qextName				qext name
	 * @return						external node number
	 */
	public static int getQextNodeNum(String qextName) {
		return qextExtNodeNums.get(qextName);
	}

	/**
	 * Obtain the map between qext names and external node numbers
	 * @return						map between qext names and external node numbers
	 */
	public static Map<String, Integer> getQextExtNodeNums() {
		return qextExtNodeNums;
	}

	/**
	 * Obtain the map of stage boundary information (flowIndex, objType, upDownIndex) for the specified stage boundary flow index
	 * @param envIndex				stage boundary flow index
	 * @return						map of stage boundary information
	 */
	public static Map<String, ArrayList<Integer>> getStageBoundaryFlowIndices(int envIndex) {
		String stageBoundaryName;

		stageBoundaryName = stageBoundaryNames.get(envIndex-1);

		return stageBoundaryFlowIndices.get(stageBoundaryName);
	}

	/**
	 * Add a node to a conveyor's list of node numbers
	 * @param name					name of conveyor
	 * @param nodeNum					node number
	 */
	public static void addConveyorNodeNum(String name, int nodeNum) {
		if(!conveyorNodeNums.containsKey(name)) {
			conveyorNodeNums.put(name, new ArrayList<>());
		}

		conveyorNodeNums.get(name).add(nodeNum);
	}

	/**
	 * Obtain the node number for the specified conveyor and node index
	 * @param name					conveyor name
	 * @param index					node index
	 * @return						node number
	 */
	public static int getConveyorNodeNum(String name, int index) {
		return conveyorNodeNums.get(name).get(index);
	}
	
	/**
	 * Write the config file out to a runnable YAML file
	 */
	public static void echoConfig() {
		Path outputDir;
		PTMEnv Environment;
		String echoConfigPath;
		YAMLMapper yamlMapper;
		
		Environment = Globals.Environment;
		echoConfigPath = Environment.getPTMFixedInput().getFileName("echoConfig");
		
		// Create the output folder, which is where outputs are typically, but not necessarily, saved.
		// This operation is also performed in Output.java in case echoConfigPath is not defined.
		if(!Files.exists(Paths.get("./output"))) {
			try {
				outputDir = Files.createDirectories(Paths.get("./output"));
				System.out.println("Created output directory at: " + outputDir.toAbsolutePath());
			} catch (IOException e) {
				// Just continue if the directory wasn't created
			}
		}
		
		if (!(echoConfigPath.equalsIgnoreCase(""))) {
			yamlMapper = YAMLMapper.builder()
	                .enable(YAMLGenerator.Feature.SPLIT_LINES)
	                .build();			
			try {
				yamlMapper.writerWithDefaultPrettyPrinter().writeValue(new File(echoConfigPath), config);
				System.out.println("Echoed configuration to " + echoConfigPath);
			} catch (IOException e) {
				PTMUtil.systemExit("Failed to write timestamp to output file. " + e);
			}
		}	    
	}

}
