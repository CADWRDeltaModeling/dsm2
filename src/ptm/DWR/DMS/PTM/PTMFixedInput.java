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
/*
 *  This class is contained in PTMEnv class. The main purpose of this class
 *  is reading fixed input.
 *  <p>
 *  The Waterbody and Node relationship is defined by a network structure.
 * Each Waterbody has a unique id and maybe queried as to which nodes it is
 * connected to. Each Node also has a unique id and can be queried as to which
 * waterbodies it is connected to.
 *  <p>
 *  Calls are made to fortran functions for reading fixed input files. These
 *  fortran functions read the data into common blocks. The information is then
 *  transferred with a C++-Fortran function calls
 *  <p>
 *  This class is also called upon by PTMEnv class to create Waterbody,
 *  Node and XSection objects and insert them into arrays.
 *  <p>
 * @author Nicky Sandhu
 * @version $Id: PTMFixedInput.java,v 1.7.6.4 2006/04/04 18:16:23 eli2 Exp $
 */
public class PTMFixedInput{
  /**
   *  Constructor: Opens the fixed input information files and fills
   *  the fortran common block with the information
   */
  public PTMFixedInput(String inputFilename){
    _fixedData = new PTMFixedData(inputFilename);
    //
    lFD = _fixedData.getLimitsFixedData();
    pFD = _fixedData.getParticleFixedData();
    fFD = _fixedData.getFluxFixedData();
  }
  /**
   *  Gets the number of Channels
   */
  public final int getNumberOfChannels(){
    return _fixedData.getNumberOfChannels();
  }
  /**
   *  Gets the number of Channel Groups
   */
  public final int getNumberOfChannelGroups(){
    return _fixedData.getNumberOfChannelGroups();
  }

  public final Group[] getOutputGroups(){
  	return _fixedData.getOutputGroups();
  }

  /**
   *  Gets the number of Reservoirs
   */
  public final int getNumberOfReservoirs(){
    return _fixedData.getNumberOfReservoirs();
  }
  /**
   *  Gets the number of Diversions
   */
  public final int getNumberOfDiversions(){
    return _fixedData.getNumberOfDiversions();
  }
  /**
   *  Gets the number of Pumps
   */
  public final int getNumberOfPumps(){
    return _fixedData.getNumberOfPumps();
  }
  /**
   *  Gets the number of BoundaryWaterbodies
   */
  public final int getNumberOfBoundaryWaterbodies(){
    return  _fixedData.getNumberOfBoundaryWaterbodies();
  }
  /**
   *  Gets the number of BoundaryWaterbodies
   */
  public final int getNumberOfConveyors(){
    return _fixedData.getNumberOfConveyors();
  }
  /**
   * @return the number of waterbodies
   */
  public final int getNumberOfWaterbodies(){
    return _fixedData.getNumberOfWaterbodies();
  }
  /**
   *  @return the number of nodes
   */
  public final int getNumberOfNodes(){
    return _fixedData.getNumberOfNodes();
  }
  /**
   *  the number of cross sections
   */
  public final int getNumberOfXSections(){
    return _fixedData.getNumberOfXSections();
  }
  /**
   *  Gets the maximum number of waterbodies
   */
  public final int getMaximumNumberOfWaterbodies(){
   return _fixedData.getMaximumNumberOfWaterbodies();
  }
  /**
   *  Gets the maximum number of nodes
   */
  public final int getMaximumNumberOfChannels(){
    return _fixedData.getMaximumNumberOfChannels();
  }
  /**
   *  Gets the maximum number of nodes
   */
  public final int getMaximumNumberOfDiversions(){
    return _fixedData.getMaximumNumberOfDiversions();
  }
  /**
   *  Gets the maximum number of nodes
   */
  public final int getMaximumNumberOfNodes(){
    return _fixedData.getMaximumNumberOfNodes();
  }
  /**
   *  Gets the maximum number of nodes
   */
  public final int getMaximumNumberOfReservoirs(){
    return _fixedData.getMaximumNumberOfReservoirs();
  }
  /**
   *  Gets the maximum number of nodes
   */
  public final int getMaximumNumberOfPumps(){
    return _fixedData.getMaximumNumberOfPumps();
  }
  /**
   *  Gets the maximum number of nodes
   */
  public final int getMaximumNumberOfBoundaryWaterbodies(){
    return _fixedData.getMaximumNumberOfBoundaryWaterbodies();
  }
  /**
   *  Gets the maximum number of nodes
   */
  public final int getMaximumNumberOfConveyors(){
    return PTMFixedData.getMaximumNumberOfConveyors();
  }
  /**
   *  Gets the maximum number of cross sections
   */
  public final int getMaximumNumberOfXSections(){
    return _fixedData.getMaximumNumberOfXSections();
  }

  /**
   *  Creates the Waterbody array.
   *  The assumption here is that channels are stored first,
   *  followed by reservoirs, stage boundaries, boundaries and lastly
   *  conveyors
   */
  public final Waterbody [] createWaterbodyFixedInfo(){
    Waterbody [] wbArray = new Waterbody[_fixedData.getMaximumNumberOfWaterbodies()+1];
    for(int i=1; i < wbArray.length; i++){
      Waterbody wb = null;
      switch (_fixedData.getWaterbodyType(i)){
      case Waterbody.CHANNEL:
        wb = createChannel(i);
        break;
      case Waterbody.RESERVOIR:
        wb = createReservoir(i);
        break;
      case Waterbody.BOUNDARY:
        wb = createBoundary(i);
        break;
      case Waterbody.CONVEYOR:
        wb = createConveyor(i);
        break;
      }
      wbArray[i] = wb;
      if ( wb != null ){
        wb.setObjectType( _fixedData.getWaterbodyObjectType(i) );
      }
    }
    return wbArray;
  }
  /**
   *  Create a Channel Waterbody
   */
  private final Channel createChannel(int nId){
    Channel wb = null;
    int lId = _fixedData.getLocalIdForWaterbody(nId);
    float len = _fixedData.getChannelLength(lId);
    if (len > 0) {
      int[] xSectionIds = _fixedData.getChannelXSectionIds(lId);
      float[] xSectDist = _fixedData.getChannelXSectionDistances(lId);
      int[] nodeArray = _fixedData.getNodeArrayForWaterbody(nId);
      wb = new Channel(lId, nId,
  		     xSectionIds, len,
  		     nodeArray, xSectDist);
      if (DEBUG){
        System.out.println("Created channel: " + lId);
        System.out.println("Created channel: " + nId);
        System.out.println("Created channel: " + len);
      }
    }
    return wb;
  }
  /**
   *  Create a Reservoir Waterbody
   */
  private final Reservoir createReservoir(int nId){
    Reservoir wb = null;
    int lId = _fixedData.getLocalIdForWaterbody(nId);
    float area = _fixedData.getReservoirArea(lId);
    if(area > 1e-10f) {
      String name = _fixedData.getReservoirName(lId);
      float botelv=_fixedData.getReservoirBottomElevation(lId);
      int[] nodeArray = _fixedData.getNodeArrayForWaterbody(nId);
      wb = new Reservoir(nId, lId,
                         name, area, botelv, 
                         nodeArray);
    }
    return wb;
  }
  /**
   *  create a rim Waterbody
   */
  private final Boundary createBoundary(int nId){
    Boundary wb = null;
    int[] nodeArray = _fixedData.getNodeArrayForWaterbody(nId);
    if (DEBUG) {
      for( int i=0; i < nodeArray.length; i++ ){
        System.out.println("For boundary node is: " + nodeArray[i]);
      }
    }
    if ( nodeArray != null )
      wb = new Boundary(nId, nodeArray);
    return wb;
  }
  /**
   *  create a Conveyor Waterbody
   */
  private final Conveyor createConveyor(int nId){
    Conveyor wb = null;
    int [] nodeArray = _fixedData.getNodeArrayForWaterbody( nId );
    if (nodeArray != null)
      wb = new Conveyor(nId, nodeArray);
    return wb;
  }

  /**
   *  Creates the Node array with fixed information from common
   *  block.
   * 
   *  Note however that Reservoir Node info, ag. drain and pump
   *  Node info is not directly available from the common block
   *  This Node info is present in the common block for the Reservoir
   *  and for ag. drains is the Node number itself...
   * 
   *  It is the responsibility of the PTMEnv class to calculate and
   *  update the Node object with this information.
   */
  public final Node[] createNodeFixedInfo(){
    Node [] nodeArray = new Node[_fixedData.getMaximumNumberOfNodes() + 1];
    for( int i=1; i <= nodeArray.length; i++ ){
      int nodeId = i;
      //create arrays to store Waterbody id's and type's
      int[] waterbodyIdArray = _fixedData.getWaterbodyIdArrayForNode(nodeId);
      if (waterbodyIdArray.length == 0) continue;
       //TODO this used to cause an error (below) for high Node numbers. never figured out
      // howe the code would ever work right.
      String type = _fixedData.getBoundaryTypeForNode(nodeId);
      nodeArray[i] = new Node(nodeId, waterbodyIdArray, type);
      if (DEBUG) System.out.println("Created node: " + nodeArray[i]);
    }
    return nodeArray;
  }
  /**
   *  Creates the cross secton array with fixed information from
   *  common block
   */
  public final XSection [] createXSectionFixedInfo(){
  
    XSection[] xSArray = new XSection[getMaximumNumberOfXSections()+1];
    // rigged to meet only regular cross section stuff  
    //  int numElvs = com_s_irr_geom_.irreg_geom[nId].num_elev ;
    int numElvs = 2;
    float [] width = new float[numElvs];
    float [] elevation = new float[numElvs];
    float [] area = new float[numElvs];
  
    // float dist = com_s_irr_geom_.irreg_geom[nId].dist;
    int i,j;
  
    for (i=1; i<= _fixedData.getMaximumNumberOfXSections(); i++) {
      xSArray[i] = null;
      width = _fixedData.getXSectionWidths(i); // xFD[i-1].width[0];
      if (width[0] > 0){
        //? set distance to non zero. PTMEnv will set the dist = length of Channel
        //? later.
        float dist = -1;
        //
        elevation = _fixedData.getXSectionElevations(i);
        area = _fixedData.getXSectionAreas(i);
        //
        float minElv = _fixedData.getXSectionMinimumElevation(i);
        xSArray[i] = new XSection(i, numElvs, dist,
  				width, area, elevation, 
  				minElv, false);
      }
    } 
    return xSArray;
  }
  /**
   *  updates the class ParticleFixedInfo with fixed information
   *  from common block
   */
  public final void getPTMFixedInfo(ParticleFixedInfo info) {
    // Logical true/false variables
    boolean ivert = pFD.useVerticalProfile();
    boolean itrans = pFD.useTransverseProfile();
    boolean iey = pFD.useTransverseMixing();
    boolean iez = pFD.useVerticalMixing();
    boolean iprof = pFD.doProfileOutput();
    boolean igroup = pFD.doGroupOutput();
    boolean fluxPercent = pFD.doFluxPercentage();
    boolean groupPercent = pFD.doGroupPercentage();
    boolean fluxCumulative = pFD.doFluxCumulative();
    info.setVariables(ivert,itrans,iey,iez,iprof,igroup,fluxPercent,groupPercent,fluxCumulative);
  
    // floats
    int random_seed = pFD.getRandomSeed();
    float trans_constant = pFD.getTransverseConstant();
    float vert_constant = pFD.getVerticalConstant();
    float trans_a_coef = pFD.getTransverseACoef();
    float trans_b_coef = pFD.getTransverseBCoef();
    float trans_c_coef = pFD.getTransverseCCoef();
    int animated_particles = pFD.getAnimatedParticles();
  
    info.setVariables(random_seed, trans_constant, vert_constant, 
  		    trans_a_coef, trans_b_coef, trans_c_coef,
  		    animated_particles);
  
    // particle injection
    int nInjections = pFD.getNumberOfInjections();
    int [] nNode = pFD.getLocationOfParticlesInjectedArray();
    int [] nInjected = pFD.getNumberInjectedArray();
    int [] startJulmin = pFD.getInjectionStartJulminArray();
    int [] lengthJulmin = pFD.getInjectionLengthJulminArray();
  
    info.setVariables(nInjections, nNode, nInjected, startJulmin, lengthJulmin);
  
    boolean qBinary = pFD.getBinaryExistance();
    int ngroups = pFD.getNumberOfGroups();
    String[] qNames = pFD.getQualityNames();
  
    info.setVariables(ngroups,qBinary,qNames);
  
  }
  /**
   *  Gets the start run time in julian minutes
   */
  public final int getStartTime(){
    return _fixedData.getModelStartTime();
  }
  /**
   *  Gets the end run time in julian minutes
   */
  public final int getEndTime(){
    return _fixedData.getModelEndTime();
  }
  /**
   *  Gets the run length in julian minutes
   */
  public final int getRunLength(){
    return getEndTime()-getStartTime();
  }
  /**
   *  Gets the time step in minutes
   */
  public final int getPTMTimeStep(){
    return _fixedData.getPTMTimeStep();
  }
  /**
   *  Gets the output display interval in minutes
   */
  public final int getDisplayInterval(){
    return _fixedData.getDisplayInterval();
  }
  /**
   *  Creates FluxInfo
   */
  public final FluxInfo getfluxInfo(){
    FluxInfo fI = new FluxInfo(fFD,pFD);
    return fI;
  }

  /**
   *  Creates GroupInfo
   */
  public final GroupInfo getgroupInfo(){
    GroupInfo gI = new GroupInfo(pFD,getOutputGroups());
    return gI;
  }

  /**
   *  Gets animation file name
   */
  public final String getAnimationFileName(){
    return _fixedData.getAnimationFileName();
  }
  /**
   *  animation output interval
   */
  public final int getAnimationOutputInterval(){
    return _fixedData.getAnimationOutputInterval();
  }
  /**
   *  Gets behavior file name
   */
  public final String getBehaviorFileName(){
    return _fixedData.getBehaviorFileName();
  }
  /**
   *  Gets trace file name
   */
  public final String getTraceFileName(){
    return _fixedData.getTraceFileName();
  }
  /**
   *  Gets output restart file name
   */
  public final String getOutputRestartFileName(){
    return _fixedData.getRestartOutputFileName();
  }
  /**
   *  gets output interval for restart file
   */
  public final int getRestartOutputInterval(){
    return _fixedData.getRestartOutputInterval();
  }
  /**
   *  Gets input restart file name
   */
  public final String getInputRestartFileName(){
    return _fixedData.getRestartInputFileName();
  }
  /**
   *  checks to see if this is a restart run
   */
  public final boolean isRestartRun(){
    //  return isRestartRun;
    return false;
  }
  /**
   * get limits fixed data
   */
  public LimitsFixedData getLimitsFixedData(){
    return lFD;
  }
  /**
   * Fixed data object
   */
  private PTMFixedData _fixedData;
  /**
   * particle fixed data
   */
  private ParticleFixedData pFD;
  /**
   *
   */
  private LimitsFixedData lFD;
  /**
   * flux fixed data
   */
  private FluxFixedData[] fFD;
  /**
   *
   */
  private static boolean DEBUG = false;
}
