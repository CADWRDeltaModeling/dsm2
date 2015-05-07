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
import java.lang.*;
/*
 * Reservoir is a Waterbody with a large volume. A Reservoir
 * is modeled as storage for water with no velocity fields within it.
 *
 * @author Nicky Sandhu
 * @version $Id: Reservoir.java,v 1.2 2000/08/07 17:00:35 miller Exp $
 */
public class Reservoir extends Waterbody{
  /**
   *  Set fixed information for Reservoir
   */
  public Reservoir(int nId, int hId, String wbName,
                   float resArea, float botelv, 
                   int[] nodeArray){
    super(Waterbody.RESERVOIR, nId, nodeArray);
    name = wbName;
    this.area = resArea;
    bottomElevation = botelv;
  }
  /**
   *  Return flow direction sign
   *  always opposite from H5 flow sign
   */
  public int flowType(int nodeId){return OUTFLOW;}
  public boolean isAgSeep(){ return false;}
  public boolean isAgDiv(){ return false;}
  /**
   *  Return the hydrodynamic type of Reservoir
   */
  public int getHydroType(){return FlowTypes.reservoirr;}
  /**
   *  Get the total volume of water in Reservoir else returns 0.
   */
  public final float getTotalVolume(float timeStep){return volume;}
  /**
   *  Get the total volume outflow to local nodeId in a certain time step
   */
  public final float getVolumeOutflow(int nodeId, float timeStep){
      return (flowAt[nodeId]*timeStep);
  }
  /**
   *  Set current Reservoir volume from HYDRO input
   */
  public final void setVolume(float currentVolume){
    volume = currentVolume;
  }
  /**
   *  Set current depth information from HYDRO input
   */
  public final void setDepth(float[] depthArray){
    depthAt[0] = depthArray[0];
  }
  public final String getName(){return name;}
  public float getInflow(int nodeEnvId){
		int nodeId = getNodeLocalIndex(nodeEnvId);
	    if (flowType(nodeId) == OUTFLOW) 
	      return (-flowAt[nodeId]);
	    return (flowAt[nodeId]);
	  }
  public float getInflowWSV(int nodeEnvId, float sv){ 
	  return getInflow(nodeEnvId);
  }
  public void setOutputDistance(int distance){
	  if (distance != 0)
		  PTMUtil.systemExit("a reservoir doesn't have distance, please check the behavior input file to make sure output location info is set properly, system exit.");
  }
  public int getOutputDistance(){return 0;}
  /**
   *  String containing the name
   */
  private String name;
  /**
   *  Area of channel/Reservoir
   */
  private float area;
  /**
   *  Volume of Reservoir
   */
  private float volume;
  /**
   *  Bottom elevation of channel or Reservoir
   */
  private float bottomElevation;
}

