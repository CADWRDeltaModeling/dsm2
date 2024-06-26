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
/**
 * Channel is a Waterbody which has two nodes and a direction of
 * flow between those nodes. In addition it has a length and other
 * properties such as cross-sections.
 *
 * @author Nicky Sandhu
 * @version $Id: Channel.java,v 1.4.6.1 2006/04/04 18:16:24 eli2 Exp $
 */
public class Channel extends Waterbody{
  /**
   *  a constant for vertical velocity profiling.
   */
  public final static float VONKARMAN = 0.4f;
  /**
   *  local index of up stream node
   */
  public final static int UPNODE = 0;
  /**
   *  local index of down stream node
   */
  public final static int DOWNNODE = 1;
  /**
   *  a constant defining the resolution of velocity profile
   */
  public final static int MAX_PROFILE = 1000;
  /**
   *  an array of coefficients for profile
   */
  public static float[] vertProfile = new float[Channel.MAX_PROFILE];
  /**
   *  use vertical profile
   */
  public static boolean useVertProfile;
  /**
   *  use transverse profile
   */
  public static boolean useTransProfile;

  /**
   *  Number of cross sections
   */
  private int nXsects;
  /**
   *  Array of XSection object indices contained in this Channel
   */
  private int[] xSectionIds;
  /**
   *  Length of Channel
   */
  private float length;
  /**
   *  Area of Channel/reservoir
   */

  /**
   *  Flow, depth, velocity, width and area information read from tide file
   */
  private float[] areaAt;
  private float[] depthAt;
  private float[] stageAt;

  /**
   *  sets fixed information for Channel
   */
  public Channel(int nId, int gnId,
                 float len,
                 int[] nodeIds){

    super(Waterbody.CHANNEL, nId, nodeIds);
    length = len;

    widthAt = new float[getNumberOfNodes()];
    areaAt = new float[getNumberOfNodes()];
    depthAt = new float[getNumberOfNodes()];
    stageAt = new float[getNumberOfNodes()];

  }

  public boolean equals(Channel chan){
	  if (chan.getEnvIndex() == this.getEnvIndex())
		  return true;
	  return false;
  }

  /**
   *  Gets the length of Channel
   */
  public final float getLength(){
    return (length);
  }

  /**
   *  Gets the width of the Channel at that particular x position
   */
  public final float getWidth(float xPos){
    float alfx = xPos/length;
    return (alfx*widthAt[1] + (1-alfx)*widthAt[0]);
  }

  /**
   *  Gets the depth of the Channel at that particular x position
   */
  public final float getDepth(float xPos){
    float depth = 0.0f;
    float alfx = xPos/length;

    depth=alfx*depthAt[DOWNNODE] + (1-alfx)*depthAt[UPNODE];
    return depth;
  }

  /**
   *  Gets the depth of the Channel at that particular x position
   */
  public final float getStage(float xPos){
    float stage = 0.0f;
    float alfx = xPos/length;

    stage = alfx*stageAt[DOWNNODE] + (1-alfx)*stageAt[UPNODE];
    return stage;
  }

  /**
   *  A more efficient calculation of velocity if average velocity, width and
   *  depth has been pre-calculated.
   */
  public final float getVelocity(float xPos, float yPos, float zPos,
                                 float averageVelocity, float width, float depth){
	float vp=1.0f, tp=1.0f;
    if(useVertProfile) vp = calcVertProfile(zPos, depth);
    if(useTransProfile) tp = calcTransProfile(yPos, width);
    return  (averageVelocity*vp*tp);
  }

  /**
   *  the flow at that particular x position
   */
  public final float getFlow(float xPos){
    float flow = 0.0f;
    float alfx = xPos/length;

    flow = alfx*flowAt[DOWNNODE] + (1-alfx)*flowAt[UPNODE];
    return flow;
  }

  /**
   *  Gets the type from particle's point of view
   */
  @Override
  public int getPTMType(){
    return Waterbody.CHANNEL;
  }

  /**
   *  Returns the hydrodynamic type of Channel
   */
  public int getHydroType(){
    return FlowTypes.channell;
  }

  /**
   *  Gets the EnvIndex of the upstream node
   */
  public final int getUpNodeId(){
    return(getNodeEnvIndex(UPNODE));
  }
  public final Node getUpNode(){
	  return getNode(UPNODE);
  }
  /**
   *  Gets the EnvIndex of the down node
   */
  public final int getDownNodeId(){
    return(getNodeEnvIndex(DOWNNODE));
  }
  public final Node getDownNode(){
	  return getNode(DOWNNODE);
  }
  /**
   *  Gets the Transverse velocity A coefficient
   */
   public float getTransverseACoef(){
     return Globals.Environment.pInfo.getTransverseACoef();
   }

  /**
   *  Gets the Transverse velocity B coefficient
   */
   public float getTransverseBCoef(){
     return Globals.Environment.pInfo.getTransverseBCoef();
   }

  /**
   *  Gets the Transverse velocity A coefficient
   */
   public float getTransverseCCoef(){
     return Globals.Environment.pInfo.getTransverseCCoef();
   }

  /**
   *  Return flow direction sign
   *  INFLOW (flow into water body) = 1 if node is upstream node
   *  OUTFLoW (flow out water body) = -1 if downstream node
   *  in tidal situation, if flow reverses (flow from downstream),
   *  flow at down node will be multiplied by -1 to be positive
   *  flow at down node will stay negative
   */
  public int flowType( int nodeId ){
    if (nodeId == UPNODE)
      return INFLOW;
    else if (nodeId == DOWNNODE)
      return OUTFLOW;
    else{
      throw new IllegalArgumentException();
    }
  }

  // this channel doesn't know the exact value of the swimming velocity.
  // SV has to be passed from a particle
  public float getInflowWSV(int nodeEnvId, float sv){
	int nodeId = getNodeLocalIndex(nodeEnvId);
	//at gate flow == 0
	if (Math.abs(flowAt[nodeId]) < Float.MIN_VALUE)
		return 0.0f;
	if (flowType(nodeId) == OUTFLOW)
	  return -1.0f*(flowAt[nodeId]+sv*getFlowArea(length));
	return flowAt[nodeId]+sv*getFlowArea(0.0f);
  }
  public boolean isAgSeep(){ return false;}
  public boolean isAgDiv(){ return false;}

  /**
   *  vertical profile multiplier
   */
  private final float calcVertProfile(float z, float depth){
    float zfrac = z/depth*(MAX_PROFILE-1);
    return Math.max(0.0f, vertProfile[(int)zfrac]);
  }

  /**
   *  transverse profile multiplier
   */
  private final float calcTransProfile(float y, float width){
    float yfrac = 2.0f*y/width;
    float yfrac2 = yfrac*yfrac;
    float a = getTransverseACoef();
    float b = getTransverseBCoef();
    float c = getTransverseCCoef();
    return a+b*yfrac2+c*yfrac2*yfrac2; // quartic profile across Channel width
  }

  /**
   *  returns the number of cross sections
   */
  public final int getNumberOfXSections(){
    return(nXsects);
  }

  /**
   *  Gets the EnvIndex of cross sections given the local index of the
   *  cross section
   */
  public final int getXSectionEnvIndex(int localIndex){
    return(xSectionIds[localIndex]);
  }


  /**
   *  Set depth information
   */
  public final void setDepth(float[] depthArray){
    depthAt[UPNODE] = depthArray[0];
    depthAt[DOWNNODE] = depthArray[1];
    if (Globals.currentModelTime == Globals.Environment.getStartTime()){
	  //TODO why 0.6?
      depthAt[UPNODE] = depthAt[UPNODE]/0.6f;
      depthAt[DOWNNODE] = depthAt[DOWNNODE]/0.6f;
    }
  }

  /**
   *  Set depth information
   */
  public final void setStage(float[] stageArray){
    stageAt[UPNODE] = stageArray[0];
    stageAt[DOWNNODE] = stageArray[1];
    if (Globals.currentModelTime == Globals.Environment.getStartTime()){
      stageAt[UPNODE] = stageAt[UPNODE]/0.6f;
      stageAt[DOWNNODE] = stageAt[DOWNNODE]/0.6f;
    }
  }

  /**
   *  Set area information
   */
  public final void setArea(float[] areaArray){
    areaAt[UPNODE] = areaArray[0];
    areaAt[DOWNNODE] = areaArray[1];
    if (Globals.currentModelTime == Globals.Environment.getStartTime()){
      areaAt[UPNODE] = areaAt[UPNODE]/0.6f;
      areaAt[DOWNNODE] = areaAt[DOWNNODE]/0.6f;
    }
    widthAt[UPNODE] = areaAt[UPNODE]/depthAt[UPNODE];
    widthAt[DOWNNODE] = areaAt[DOWNNODE]/depthAt[DOWNNODE];
  }

  /**
   *  Get the flow area
   */
  public final float getFlowArea(float xpos){
    return getDepth(xpos)*getWidth(xpos);
  }

  /**
   *  An efficient way of calculating all Channel parameters i.e.
   *  length, width, depth, average velocity and area all at once.
   */
  public final void updateChannelParameters(float xPos,
                                            float [] channelLength,
                                            float [] channelWidth,
                                            float [] channelDepth,
                                            float [] channelVave,
                                            float [] channelArea){
    channelLength[0] = this.length;

    float alfx = xPos/this.length;
    float nalfx = 1.0f - alfx;

    channelDepth[0] = alfx*depthAt[DOWNNODE] + nalfx*depthAt[UPNODE];
    channelWidth[0] = alfx*widthAt[DOWNNODE] + nalfx*widthAt[UPNODE];
    channelArea[0] = channelDepth[0]*channelWidth[0];

    float Vave = (alfx*flowAt[DOWNNODE] + nalfx*flowAt[UPNODE])/channelArea[0];
    if ((Vave > 0 && Vave < 0.001f) || Vave == 0)
    		Vave = 0.001f;
    if (Vave < 0 && Vave > -0.001f)
    		Vave = -0.001f;
    channelVave[0] = Vave;
  }


  /**
   *  calculate profile
   */
  public final static void constructProfile(){
    vertProfile[0] = (float) (1.0f + 0.1f*(1.0f + Math.log((0.01f)/MAX_PROFILE))/VONKARMAN);
    for(int i=1; i<MAX_PROFILE; i++)
      vertProfile[i] = (float) (1.0f + (0.1f/VONKARMAN)*(1.0f + Math.log(((float)i)/MAX_PROFILE)));
  }
}
