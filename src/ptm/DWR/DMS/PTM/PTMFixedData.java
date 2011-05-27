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
/**
 * Encapsulates the fixed information for this model
 *
 * @author Nicky Sandhu
 * @version $Id: PTMFixedData.java,v 1.6.6.5 2007/07/31 18:30:39 eli2 Exp $
 */
public class PTMFixedData {

  /**
   * constructor: loads library 
   */
  public PTMFixedData(String filename){
    initialize(filename);
  }
  
  /**
   * 
   */
  public LimitsFixedData getLimitsFixedData(){
    int maxChannels = getMaximumNumberOfChannels();            
    int maxReservoirs = getMaximumNumberOfReservoirs();           
    int maxDiversions = getMaximumNumberOfDiversions();          
    int maxPumps = getMaximumNumberOfPumps();               
    int maxBoundaryWaterbodies = getMaximumNumberOfBoundaryWaterbodies(); 
    //  int maxConveyors = getMaximumNumberOfConveyors();
    int maxNodes = getMaximumNumberOfNodes();               
    int maxXSections = getMaximumNumberOfXSections();
    
    return new LimitsFixedData(maxChannels,
                               maxReservoirs, 
                               maxDiversions, 
                               maxPumps, 
                               maxBoundaryWaterbodies, 
                               maxNodes, 
                               maxXSections);
  }
  
  /**
   *
   */
  public ParticleFixedData getParticleFixedData(){
    ParticleFixedData pFD = new ParticleFixedData();
  
    boolean[] booleanInputs = createParticleBooleanInputs();
    float[] floatInputs = getParticleFloatInputs();
    int nInjections = getParticleNumberOfInjections();
    int[] nNode = getParticleInjectionNodes();
    int[] nInjected = getParticleNumberOfParticlesInjected();
    int[] startJulmin = getParticleInjectionStartJulmin();
    int[] lengthJulmin = getParticleInjectionLengthJulmin();
    boolean qBinary = qualBinaryBooleanInput();
    int ngroups = getNumberOfChannelGroups();
    String[] qNames = getQualConstituentNames();
  
    pFD.setVariables(booleanInputs[0],booleanInputs[1],
                     booleanInputs[2],booleanInputs[3],
                     booleanInputs[4],booleanInputs[5],
                     booleanInputs[6],booleanInputs[7],
                     booleanInputs[8]);
    pFD.setVariables((int) floatInputs[0],floatInputs[1],
                     floatInputs[2],floatInputs[3],
                     floatInputs[4],floatInputs[5],
                     (int) floatInputs[6]);
    pFD.setVariables(nInjections,
                     nNode, nInjected,
                     startJulmin, lengthJulmin);
    pFD.setVariables(ngroups,qBinary,qNames);
  
    return pFD;
  }
  
  /**
   * 
   */
  public FluxFixedData[] getFluxFixedData(){
    int numberOfFluxes = getNumberOfFluxes();
    FluxFixedData [] fFD = new FluxFixedData[numberOfFluxes];
    //System.out.println("Number of fluxes: " + numberOfFluxes);
    
    for(int i=1; i<= fFD.length; i++){
      int[] inArray = getFluxIncoming(i);
      int[] outArray = getFluxOutgoing(i);
      int[] inTypeArray = getFluxIncomingType(i);
      int[] outTypeArray = getFluxOutgoingType(i);
      // String name = getFluxIncomingName(i);
      // String name = getFluxOutgoingname(i);
      String nameIn="TODO:FLUX";  //@todo: get names for printing error/info messages
      String nameOut="TODO:FLUX";
      //for (int j=0; j< outTypeArray.length; j++)System.out.println("flux: " + i+ " member: "+j+":"+outTypeArray[j]+" id: "+outArray[j]);
      fFD[i-1] = new FluxFixedData(
  	    new WaterbodyGroup(nameIn,inTypeArray, inArray),
        new WaterbodyGroup(nameOut,outTypeArray, outArray)
  	  );
    }//end for
    return fFD;
  }
  
  /**
   * 
   */
  public Group[] getOutputGroups(){
  	int numberOfGroups = getNumberOfGroupOutputs();
  	Group [] groups = new Group[numberOfGroups];
    
  	for(int i=1; i<= groups.length; i++){
  	  int[] memberArray = getGroupMemberIndex(i);
  	  int[] typeArray = getGroupMemberType(i);
  	  String name="TODO:GROUP";  //@todo: get names for printing error/info messages
  	  groups[i-1]=
  	    new WaterbodyGroup(name,typeArray,memberArray);
  	}
  	return groups;
  }

  public boolean qualBinaryBooleanInput(){
    int exist = doesQualBinaryExist();
    return exist == 0 ? false : true;
  }

  /**
    *
    */
  public boolean [] createParticleBooleanInputs(){
    int [] array = getParticleBooleanInputs();
    boolean [] barray = new boolean[array.length];
    for(int i=0; i < barray.length; i++){
      barray[i] = array[i] == 0 ? false : true;
    }
    return barray;
  }
  
  native void initialize(String filename);
  //
  public native int getNumberOfWaterbodies();
  public native int getNumberOfChannels();
  public native int getNumberOfChannelGroups();
  public native int getNumberOfReservoirs();
  public native int getNumberOfDiversions();
  public native int getNumberOfPumps();
  public native int getNumberOfBoundaryWaterbodies();
  public native int getNumberOfConveyors();
  public native int getNumberOfNodes();
  public native int getNumberOfXSections();
  //
  static native int getMaximumNumberOfWaterbodies();
  static native int getMaximumNumberOfChannels();
  static native int getMaximumNumberOfReservoirs();
  static native int getMaximumNumberOfDiversions();
  static native int getMaximumNumberOfPumps();
  static native int getMaximumNumberOfBoundaryWaterbodies();
  static native int getMaximumNumberOfStageBoundaries();
  static native int getMaximumNumberOfConveyors();
  static native int getMaximumNumberOfNodes();
  static native int getMaximumNumberOfXSections();
  static native int getMaximumNumberOfReservoirNodes();
  //
  static native int getUniqueIdForChannel(int i);
  static native int getUniqueIdForReservoir(int i);
  static native int getUniqueIdForBoundary(int i);
  static native int getUniqueIdForStageBoundary(int i);
  static native int getUniqueIdForConveyor(int i);
  //
  static native int doesQualBinaryExist();
  static native String[] getQualConstituentNames();
  //
  native int getNumberOfWaterbodiesForNode(int i);
  native int[] getWaterbodyIdArrayForNode(int i);
  native int getWaterbodyObjectType(int wbId);
  native int getWaterbodyType(int i);
  native int getWaterbodyGroup(int i);
  native int getLocalIdForWaterbody(int i);
  native int [] getNodeArrayForWaterbody(int i);
  native String getBoundaryTypeForNode(int i);
  //
  native int getChannelLength(int i);
  native int[] getChannelNodeArray(int i);
  native int[] getChannelXSectionIds(int i);
  native float[] getChannelXSectionDistances(int i);
  //
  native float getReservoirArea(int i);
  native float getReservoirBottomElevation(int i);
  native String getReservoirName(int i);
  native int[] getReservoirNodeArray(int i);
  //
  native int[] getDiversionNodeArray(int i);
  native int[] getPumpNodeArray(int i);
  native int[] getBoundaryWaterbodyNodeArray(int i);
  native int[] getConveyorNodeArray(int i);
  //
  native float[] getXSectionWidths(int i);
  native float[] getXSectionElevations(int i);
  native float[] getXSectionAreas(int i);
  native float getXSectionMinimumElevation(int i);
  //
  native int [] getParticleBooleanInputs();
  native float[] getParticleFloatInputs(); 
  native int getParticleNumberOfInjections();
  native int [] getParticleInjectionNodes();
  native int [] getParticleNumberOfParticlesInjected();
  native int [] getParticleInjectionStartJulmin();
  native int [] getParticleInjectionLengthJulmin();
  //
  native int getNumberOfFluxes();
  native int [] getFluxIncoming(int i);
  native int [] getFluxOutgoing(int i);
  native int [] getFluxIncomingType(int i);
  native int [] getFluxOutgoingType(int i);
  //
  native int getNumberOfGroupOutputs();
  native int [] getGroupMemberType(int i);
  native int [] getGroupMemberIndex(int i);
  //
  native int getModelStartTime();
  native int getModelEndTime();
  native int getPTMTimeStep();
  native int getDisplayInterval();
  //
  native String getAnimationFileName();
  native int getAnimationOutputInterval();
  native String getBehaviorFileName();
  native String getTraceFileName();
  native int getTraceOutputInterval(); 
  native String getRestartOutputFileName(); 
  native int getRestartOutputInterval();
  native String getRestartInputFileName(); 
}
