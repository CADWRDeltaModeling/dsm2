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
import java.util.ArrayList;
import java.util.HashMap;
import java.lang.Math;
/**
 * This class handles the reading of the hydro tide file using
 * the appropriate fortran functions and using a common block
 * to communicate updated values between this class and fortran
 * functions.
 * <p>
 * This information is then used to update the Waterbody object array.
 * <p>
 * 
 * @author Nicky Sandhu
 * @version $Id: PTMHydroInput.java,v 1.3.6.1 2006/04/04 18:16:24 eli2 Exp $
 */
// this class is mainly used to read hydro info from fortran for each time step and set up wbarray 
public class PTMHydroInput{

  private final static boolean DEBUG = false;
  // get fish swimming input info;
  private static float constConfProb = 0.0f;
  private static float maxConfProb = 1.0f;
  private static float confProbSlope = -0.25f;
  private static int tCycleForDir = 2;
  private HashMap<Integer, ArrayList<Float>> _flowsInTidalCycles = new HashMap<Integer,ArrayList<Float>>();
  private HashMap<Integer, Float> _preAdjFlow = new HashMap<Integer, Float>();
  private HashMap<Integer, Float> _preChange = new HashMap<Integer, Float>();
  private int _tideCount = 0;
  
  public static void setConfusionParameters(float ccProb, float mcProb, float cProbSlope, int cycles){
	  constConfProb = ccProb;
	  maxConfProb = mcProb;
	  confProbSlope = cProbSlope;
	  tCycleForDir = cycles;
			  
  }
  

  /**
   *  the next chunk of data till the next time step
   */
  public final void getNextChunk(int currentModelTime) {
    readMultTide(currentModelTime);
  }

  /**
   *  the information in the Waterbody array in PTMEnv
   */
  public final void updateWaterbodiesHydroInfo(Waterbody [] wbArray, LimitsFixedData lFD){
    //update Channel depths, flows and area of flow
    float[] depthArray = new float[2];
    float[] flowArray = new float[2];
    float[] stageArray = new float[2];
    float[] areaArray = new float[2];
    int numConst = PTMFixedData.getQualConstituentNames().length;  
    float[][] qualityArray = new float[2][numConst];
    // channelNumber is waterbody's envIndex because waterbodies start from channels
    for(int channelNumber=1; 
        channelNumber <= PTMFixedData.getMaximumNumberOfChannels(); 
        channelNumber++){

    	if (wbArray[channelNumber] !=null){
    		depthArray[Channel.UPNODE]   = getUpNodeDepth(channelNumber);
    		depthArray[Channel.DOWNNODE] = getDownNodeDepth(channelNumber);
  
    		stageArray[Channel.UPNODE]   = getUpNodeStage(channelNumber);
    		stageArray[Channel.DOWNNODE] = getDownNodeStage(channelNumber);
    		//TODO clean up
    		//flowArray[Channel.UPNODE]= getUpNodeFlow(channelNumber);
    		//flowArray[Channel.DOWNNODE] = getDownNodeFlow(channelNumber);
    		float flowUp = getUpNodeFlow(channelNumber);
    		float flowDown = getDownNodeFlow(channelNumber);
    		flowArray[Channel.UPNODE]   = flowUp;
    		flowArray[Channel.DOWNNODE] = flowDown;
  
    		areaArray[Channel.UPNODE]   = getUpNodeArea(channelNumber);
    		areaArray[Channel.DOWNNODE] = getDownNodeArea(channelNumber);
    		// no use of quality currently
    		for (int indx = 0; indx < qualityArray[0].length; indx++){
    			//TODO fixme: got rid of quality temporarily
    				qualityArray[Channel.UPNODE][0] = 0.f; //getUpNodeQuality(channelNumber,indx+1);
    				qualityArray[Channel.DOWNNODE][0] = 0.f; //getDownNodeQuality(channelNumber,indx+1);
    		}

    		Channel chan = ((Channel) wbArray[channelNumber]);
    		chan.setDepth(depthArray);
    		chan.setStage(stageArray);
    		chan.setFlow(flowArray);
    		chan.setArea(areaArray);
    		// calculate swimming related info: channel direction and confusion factor
    		float curFlow = (flowUp + flowDown)/2;
    		ArrayList<Float> flows = _flowsInTidalCycles.get(channelNumber);
    		if (flows == null){
    			flows = new ArrayList<Float>();
    			_flowsInTidalCycles.put(channelNumber, flows);
    		}
    		// For Java, Lists are passed by reference.  changing flows will also change _flowsInTidalCycles.get(channelNumber). 
    		flows.add(curFlow);
    		Float preAdjFlow = _preAdjFlow.get(channelNumber);
    		// Careful! Java passes a Float by value.  changing preAdjFlow will not change _preAdjFlow.get(channelNumber).
    		if (preAdjFlow == null)
    			preAdjFlow = curFlow;
    		float curAdjFlow = 0.1f*curFlow + 0.9f*preAdjFlow;
    		float change = curAdjFlow - preAdjFlow;
    		Float preChange =  _preChange.get(channelNumber);
    		if (preChange == null)
    			preChange = 0.0f;
    		if ((change>0.0f) && (preChange<0.0f))
    			_tideCount += 1;
    		if(_tideCount >= tCycleForDir){
    			int numFlows = flows.size();
    			if (numFlows == 0)
    				PTMUtil.systemExit("error: no flow found in the tidal cycles, system exit.");
    			float sumFlows = 0.0f;
    			for (float flow: flows)
    				sumFlows += flow;
    			if (sumFlows<0)
    				chan.setChanDir(-1);
    			else
    				chan.setChanDir(1);
    			float meanFlow = sumFlows/numFlows;
    			double sumSqD = 0;
    			for (float flow: flows){
    				float d = (flow - meanFlow);
    				sumSqD += d*d;
    			}
    			double sd = Math.sqrt(sumSqD/numFlows);
    			if (sd < Double.MIN_VALUE){
    				System.err.println("warn: flows are constant in the tidal cycle! set Stard Deviation to Double.MIN_VALUE.");
    				sd = Double.MIN_VALUE;
    			}
    			double s_n = Math.log(Math.max(1E-10, Math.abs(meanFlow/sd)));
    			double term = Math.exp(constConfProb + confProbSlope*s_n);
    			double confusionConst = maxConfProb*term/(1+term);
    			chan.setConfusionConst(confusionConst);   			
    			_tideCount = 0;
    			flows.clear();
    		}
    		_preAdjFlow.put(channelNumber, curAdjFlow);
    		_preChange.put(channelNumber, change);    		
    	}//end if (wbArray)
    }//end for (channelNumber)


    	// update Reservoir dynamic information
    	flowArray = new float[PTMFixedData.getMaximumNumberOfReservoirNodes()+1];
    	depthArray = new float[1];
    	for(int reservoirNumber=1; 
    			reservoirNumber <= PTMFixedData.getMaximumNumberOfReservoirs(); 
    			reservoirNumber++){
    		int envIndex = PTMFixedData.getUniqueIdForReservoir(reservoirNumber);

    		if(wbArray[envIndex] != null){
    			float volume = getReservoirVolume(reservoirNumber);
    			((Reservoir )wbArray[envIndex]).setVolume(volume);
    			if (DEBUG) System.out.println(wbArray[envIndex]);
        
    			//update Reservoir flows except for pumping flow which is set later
    			for(int connection=1; 
    					connection <= wbArray[envIndex].getNumberOfNodes();
    					connection++){
    				int nodeNumber = getNodeNumberForConnection(reservoirNumber, connection);
    				if (DEBUG) System.out.println("Node Number is " + nodeNumber
                                      + " for reservoir number " + reservoirNumber
                                      + " for connecton number " + connection);
    				int nodeLocalIndex = 0;
    				nodeLocalIndex = wbArray[envIndex].getNodeLocalIndex(nodeNumber);
    				if (nodeLocalIndex == -1){
    					System.out.println("PTMHydroInput.java: Node " + nodeNumber 
                             + " not found in waterbody " + envIndex);
    				}
    				flowArray[nodeLocalIndex] = 
    						getReservoirFlowForConnection(reservoirNumber, connection);
    				if (DEBUG){
    					System.out.println("Resrvoir # " + reservoirNumber
                             + " Connection #: " + connection + " flow= "
                             + getReservoirFlowForConnection(reservoirNumber, connection));
    				}
    			}//end for (connection)
        
    			if (DEBUG){
    				System.out.print("Wb EnvIndex: " + envIndex 
                         + "Reservoir local index: " + reservoirNumber);
    				for(int j=0; j < wbArray[envIndex].getNumberOfNodes(); j++)
    					System.out.println(", flow = " + flowArray[j]);
    			}
    			wbArray[envIndex].setFlow(flowArray);
    			depthArray[0] = getReservoirDepth(reservoirNumber);
    			((Reservoir )wbArray[envIndex]).setDepth(depthArray);
    		}
    	}

    	// update stage boundary flows
    	flowArray = new float[1];
    	for (int stgId = 0;
    			stgId < PTMFixedData.getMaximumNumberOfStageBoundaries();
    			stgId++){
    		int envIndex = PTMFixedData.getUniqueIdForStageBoundary(stgId);
    		if (wbArray[envIndex] != null ){
    			flowArray[0] = getStageBoundaryFlow(stgId);
    			wbArray[envIndex].setFlow(flowArray);
    		}
    	}


    	// update boundary flows
    	if (DEBUG) System.out.println("Updating external flows");
    	flowArray = new float[1];
    	for (int extId = 0 ; 
    			extId < PTMFixedData.getMaximumNumberOfBoundaryWaterbodies(); 
    			extId ++){
    		flowArray[0] = getBoundaryFlow(extId);
    		int envIndex = PTMFixedData.getUniqueIdForBoundary(extId);
    		if (DEBUG){
    			System.out.println("Wb EnvIndex: " + envIndex 
                         + "extId: " + extId + ", flow = " + flowArray[0]);
    		}
    		if (wbArray[envIndex] != null) wbArray[envIndex].setFlow(flowArray);
    	}


    	// update internal or conveyor flows
    	if (DEBUG) System.out.println("Updating internal flows");
    	flowArray = new float[2];
    	for (int intId = 0 ; intId < PTMFixedData.getMaximumNumberOfConveyors(); intId ++){
    		flowArray[0] = getConveyorFlow(intId);
    		flowArray[1] = -getConveyorFlow(intId);
    		int envIndex = PTMFixedData.getUniqueIdForConveyor(intId);
    		if (DEBUG){
    			System.out.println("Wb EnvIndex: " + envIndex 
                         + "Id: " + intId + ", flow = " 
                         + flowArray[0] + ", " + flowArray[1]);
    		}
    		if (wbArray[envIndex] != null) wbArray[envIndex].setFlow(flowArray);
    	}
    
    	// update stage boundary flows ?
    	if (DEBUG) System.out.println("Updated all flows");
  	}

  	private native void  readMultTide(int currentModelTime);
	public final void updateNodesHydroInfo(Node [] nodeArray) {
		for (Node node: nodeArray){
			if (node != null){
				node.setTotalWaterbodyInflows();
				node.setTotalAgDiversions();
			}
			
		}

	}

  //private native int   getExtFromInt(int channelNumber);
  public native static int getExtFromIntChan(int inchannelNumber);
  public native static int getExtFromIntNode(int innodeNumber);
  public native static int getIntFromExtChan(int exchannelNumber);
  public native static int getIntFromExtNode(int exnodeNumber);
  private native float getUpNodeDepth(int channelNumber);
  private native float getDownNodeDepth(int channelNumber);
  private native float getUpNodeStage(int channelNumber);
  private native float getDownNodeStage(int channelNumber);
  private native float getUpNodeFlow(int channelNumber);
  private native float getDownNodeFlow(int channelNumber);
  private native float getUpNodeArea(int channelNumber);
  private native float getDownNodeArea(int channelNumber);

  private native float getFlowForWaterbodyNode(int wbId, int nodeId);
  
  private native float getReservoirVolume(int reservoirNumber);
  private native int   getNodeNumberForConnection(int reservoirNumber, 
                                                  int connection);
  private native float getReservoirDepth(int reservoirNumber);
  private native float getReservoirFlowForConnection(int reservoirNumber, 
                                                     int connection);
  private native float getDiversionAtNode(int nodeNumber);
  private native float getReservoirPumping(int reservoirNumber);
  
  private native float getBoundaryFlow(int bId);
  private native float getStageBoundaryFlow(int bId);
  private native float getConveyorFlow(int cId);
  
  private native float getUpNodeQuality(int channelNumber, int constituent);
  private native float getDownNodeQuality(int channelNumber, int constituent);
  
}
