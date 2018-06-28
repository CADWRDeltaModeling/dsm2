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
 * CLASS
 * pump
 * This class encapuslates the pumping at a node
 *<p>
 */
public class pump extends Waterbody {
  /**
   * sets fixed information for pumps or pumps or boundary waterbodies
   */
public pump(int nId, int[] ndArray){
   super(Waterbody.PUMP, nId, ndArray);
};

  /***
  * gets direction of flow 
  * Returns flow type ie.
  * inflow if node is upstream node
  * outflow if downstream node
  */
public int flowType(int nodeId){return INFLOW;}
  
  /**
   * Gets the type from particle's point of view
   */
public int getPTMType(){ return Waterbody.SINK;}
  
  /***
   * Returns the hydrodynamic type of pump
   */
public int getHydroType(){ return FlowTypes.pump;}

}
