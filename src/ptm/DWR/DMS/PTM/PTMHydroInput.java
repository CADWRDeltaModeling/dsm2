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
import java.util.HashMap;
//import java.util.ArrayList;
//import java.util.Iterator;
//import java.util.Calendar;
/*
import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStreamReader;
import java.util.Map;
import java.util.HashMap;
import java.util.regex.*;
*/

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
  public void initializeNonphysicalBarrierOp(PTMBehaviorInputs bInputs){
	 
  }
  /*
	  //TODO need to be cleaned up
	  try{
		  FileInputStream fstream;
		  try{
			  fstream = new FileInputStream("non_physical_barrier_op.txt");
		  }
		  catch(FileNotFoundException fe){
			  return; // don't do anything if no non physical barriers
		  }
		  // xiao put if statement here to decide if the file is exist.
		  DataInputStream in = new DataInputStream(fstream);
		  BufferedReader br = new BufferedReader(new InputStreamReader(in));
		  String strLine;
		  String p_title = "([a-zA-Z\\:]+)([,\\s]+)(\\d+)([,\\s]+)([a-zA-Z\\:]+)([,\\s]+)(\\d+)";
		  String p_data = "([0-9/\\-]+)([,\\s]+)([0-9:]+)([,\\s]+)(\\d)";
		  Pattern r_title = Pattern.compile(p_title);
		  Pattern r_data = Pattern.compile(p_data);
		  // to do may not need the flowing maps and lists
		  _barrierSet = new ArrayList<NonPhysicalBarrier>();
		  //_barrierNodeIds = new ArrayList<Integer>();
		  //_barrierChannelIds = new ArrayList<Integer>();
		  NonPhysicalBarrier npb;
		  Map<BarrierOpPeriod, Integer> npbOp = null;
		  Calendar s_time = null, e_time = null;
		  int op = -1;	
		  
		  while ((strLine = br.readLine()) != null){
			  Matcher m_title = r_title.matcher(strLine);				
			  Matcher m_data = r_data.matcher(strLine);
			  if(m_title.find( )) {
				  npbOp = new HashMap<BarrierOpPeriod, Integer>();
				  int chan = Integer.parseInt(m_title.group(7));
				  int nd = Integer.parseInt(m_title.group(3));
				  npb = new NonPhysicalBarrier(getNodeIntFromExt(nd), getIntFromExt(chan), npbOp);
				  _barrierSet.add(npb);
				  //_barrierNodeIds.add(nd);
				  //_barrierChannelIds.add(chan);
			  }
			  else if(m_data.find( )){
				  String p_date = "([0-9]+)([\\/]+)([0-9]+)([\\/]+)([0-9]+)";
				  Pattern r_pdate = Pattern.compile(p_date);
				  Matcher m_pdate = r_pdate.matcher(m_data.group(1));
				  String p_time = "([0-9]+)([\\:]+)([0-9]+)";
				  Pattern r_ptime = Pattern.compile(p_time);
				  Matcher m_ptime = r_ptime.matcher(m_data.group(3));
				  if(m_pdate.find()&& m_ptime.find()){
					  e_time = Calendar.getInstance();
					  e_time.clear();
					  e_time.set(Integer.parseInt(m_pdate.group(5)), Integer.parseInt(m_pdate.group(1))-1, Integer.parseInt(m_pdate.group(3)),
							  Integer.parseInt(m_ptime.group(1)), Integer.parseInt(m_ptime.group(3)));
					  if (npbOp == null){
						  System.out.println("file read error: couldn't find node and wb ids line!");
						  System.exit(-1);
					  }
					  if ((op != -1) && s_time != null){
						  npbOp.put(new BarrierOpPeriod(s_time, e_time), op);
						  //System.out.println(s_time.getTime()+" "+e_time.getTime() + " " + op);
					  }
					  op = Integer.parseInt(m_data.group(5));
					  s_time = e_time;
				  }
				  else{
					  System.out.println("operation data NO MATCH");
				  }
			  }
			  else {
   					System.out.println("NO MATCH in the file");
			  }
		  }
		  in.close();
		  /*
		  if (_barrierNodeIds != null && _barrierChannelIds != null){
			  PTMFixedData.setBarrierNodeIds(_barrierNodeIds);
			  PTMFixedData.setBarrierChannelIds(_barrierChannelIds);
		  }
		 
	  }
	  catch (Exception e){
		  System.err.println("Error: " + e.getMessage());
	  }
	}
	
  ArrayList<NonPhysicalBarrier> getBarriers(){
	  return _barrierSet;
  }
  */
  //xiao
  /**
   *  the next chunk of data till the next time step
   */
  public final void getNextChunk(int currentModelTime) {
    readMultTide(currentModelTime);
    _currentModelTime = currentModelTime;
  }

  /**
   *  the information in the Waterbody array in PTMEnv
   */
  public final void updateWaterbodiesHydroInfo(Waterbody [] wbArray,HashMap<String, NonPhysicalBarrier> barriers,
                                               LimitsFixedData lFD){
    //update Channel depths, flows and area of flow
    float[] depthArray = new float[2];
    float[] flowArray = new float[2];
    float[] stageArray = new float[2];
    float[] areaArray = new float[2];
    int numConst = PTMFixedData.getQualConstituentNames().length;  
    float[][] qualityArray = new float[2][numConst];
    HashMap<String, NonPhysicalBarrier> bars = barriers;
    
    //@todo: external/internal numbers?
    for(int channelNumber=1; 
        channelNumber <= PTMFixedData.getMaximumNumberOfChannels(); 
        channelNumber++){

    	if (wbArray[channelNumber] !=null){
    		depthArray[Channel.UPNODE]   = getUpNodeDepth(channelNumber);
    		depthArray[Channel.DOWNNODE] = getDownNodeDepth(channelNumber);
  
    		stageArray[Channel.UPNODE]   = getUpNodeStage(channelNumber);
    		stageArray[Channel.DOWNNODE] = getDownNodeStage(channelNumber);
        
    		flowArray[Channel.UPNODE]   = getUpNodeFlow(channelNumber);
    		flowArray[Channel.DOWNNODE] = getDownNodeFlow(channelNumber);
  
    		areaArray[Channel.UPNODE]   = getUpNodeArea(channelNumber);
    		areaArray[Channel.DOWNNODE] = getDownNodeArea(channelNumber);
    		// no use of quality currently
    		for (int indx = 0; indx < qualityArray[0].length; indx++){
    			//fixme: got rid of quality temporarily
    				qualityArray[Channel.UPNODE][0] = 0.f; //getUpNodeQuality(channelNumber,indx+1);
    				qualityArray[Channel.DOWNNODE][0] = 0.f; //getDownNodeQuality(channelNumber,indx+1);
    		}

    		//      if(channelNumber == 54 || dsmNumber == 54)
    		//	System.out.println("Channel:"+dsmNumber+"upnode:"+qualityArray[0][0]);
    		//      if(channelNumber == 7)
    		//	System.out.println("Channel:"+dsmNumber+"flow:"+flowArray[1]);
    		//      if(dsmNumber == 8 || dsmNumber == 54)
    		//	System.out.println("Channel:"+channelNumber+"flow:"+flowArray[0]);
    		Channel chan = ((Channel) wbArray[channelNumber]);
    		chan.setDepth(depthArray);
    		chan.setStage(stageArray);
    		chan.setFlow(flowArray);
    		chan.setArea(areaArray);
    		int chanEnvId = chan.getEnvIndex();
    		//TODO need to be revisited xiao
    		if (chan.isBarrierAtUpNodeInstalled())
    			chan.setBarrierAtUpNodeOp(getBarrier(bars, chan.getUpNodeId(), 
    					chanEnvId).getBarrierOp(PTMUtil.convertHecTime(_currentModelTime)));
    		else if (chan.isBarrierAtDownNodeInstalled())
    			chan.setBarrierAtDownNodeOp(getBarrier(bars, chan.getDownNodeId(), 
    					chanEnvId).getBarrierOp(PTMUtil.convertHecTime(_currentModelTime)));
    		//TODO: disabled quality here
    		//((Channel) wbArray[dsmNumber]).setQuality(qualityArray);
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
  	
	  //TODO cleanup
	  /* commented out 9/4/2013
  	private int getCurrentBarrierOp(int chanEnvIndex, int nodeEnvIndex) {
  	  Iterator<NonPhysicalBarrier> it = _barrierSet.iterator();
	  while(it.hasNext()){
		  NonPhysicalBarrier b = it.next();
		  if ((b.getWaterbodyId() == chanEnvIndex) && b.getNodeId() == nodeEnvIndex)
			  return b.getCurrentOperation();
	  }
	  System.out.println("Barrier is not installed at node:"+ " " +nodeEnvIndex+"and channel:"+chanEnvIndex);
	  System.out.println("exit from PTMHydroInput line 351.");
	  System.exit(-1);
	  return -1;				  	
	  //return _currentBarrierOp.get(channelNumber);
	   
  }
  */
  /*
  	private void setCurrentBarrierOp(int currentModelTime){
	  if (_barriers == null) // don't do anything if there is no barrier
		  return;
	  // convert currentModelTime to Calendar time
	  Calendar curr = PTMUtil.convertHecTime(currentModelTime);
	  
	  
	  Iterator<NonPhysicalBarrier> it = _barrierSet.iterator();
	  while(it.hasNext()){
		  NonPhysicalBarrier b = it.next();
		  boolean sucess = b.setCurrentOperation(curr);
			  if (!sucess){
				  System.out.println("Warning! no time or date matches the model time " +
				  		"in the non-physical barrier operation list, model time:");
				  System.out.println(curr.getTime());
				  System.out.println("Barrier node id:"+ " " +b.getNodeId());
				  System.out.println("Barrier channel id:"+ " " +b.getWaterbodyId());
				  System.out.println("exit from PTMHydroInput line 374.");
				  System.exit(-1);
			  }
				  	
	  }
	  
	  
	  Iterator<Integer> it = _barrierChannelIds.iterator();
	  while(it.hasNext()){
		  int chan = it.next();
		  if (_barrierSet != null){
			  NonPhysicalBarrier b = _barrierSet.get(chan);
			  if (b == null){
				  System.out.println("Warning! no match channel found in the barrier channel list.");
				  System.exit(-1);
			  }
			  int op = b.getBarrierOp(curr);
			  if (op<0){
				  System.out.println("Warning! no time or date matches the model time " +
				  		"in the non-physical barrier operation list, model time:");
				  System.out.println(curr.getTime());
				  System.out.println("Barrier node id:"+ " " +b.getNodeId());
				  System.out.println("Barrier channel id:"+ " " +b.getWaterbodyId());
				  System.out.println("exit.");
				  System.exit(-1);
			  }
				  	
			  _currentBarrierOp.put(chan, op);
		  }
		  else{
			  System.out.println("Warning! no non-physical barrier data set, exit.");
			  System.exit(-1);
		  }
	  }
	  
  }
  */

	  //private Map<Integer, NonPhysicalBarrier> _barrierSet = null;
	  //private Map<Integer, Integer> _currentBarrierOp = null;
	  //private ArrayList<Integer> _barrierNodeIds = null;
	  //private ArrayList<Integer> _barrierChannelIds = null;
	  //TODO this method changed by Joey, may need to be checked and tested
	  //public native static int   getExtFromInt(int channelNumber);
	
	  //TODO these methods needed to be added in fortran
	  //public native int getIntFromExt(int externalChan);
	  //public native int getNodeExtFromInt (int internalNode);
	  //public native int getNodeIntFromExt(int externalNode);
	  //TODO temporary methods needs to be rewrite
	  /*
	  public static int getIntFromExt(int channelNumber){
		  switch(channelNumber){
			  case 422:
				  return 403;
			  case 423:
				  return 404;
			  case 366:
				  return 347;
			  default:
				  System.out.println("don't know how to deal with the channel, exit");
				  System.exit(-1);
				  return -99;
		  }	 
	  }
	  //TODO temporary methods need to be rewrite
	  public static int getNodeIntFromExt(int nodelNumber){
		  switch(nodelNumber){
			  case 343:
				  return 307;
			  default:
				  System.out.println("don't know how to deal with the channel, exit");
				  System.exit(-1);
				  return -99;
		  }	 
	  }
	  */
	  //xiao

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
  private NonPhysicalBarrier getBarrier(HashMap<String, NonPhysicalBarrier> bars, int nodeId, int chanId){
	NonPhysicalBarrier npb = bars.get(PTMUtil.concatNodeWbIds(nodeId, chanId));
	if (npb == null)
		PTMUtil.systemExit("Barrier is not setup, exit from PTMHydroInput line 232.");
	return npb;
  }
  private int _currentModelTime;
  
}
