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
 * Boundary is a Waterbody at the Boundary of the system
 * A Boundary is connected to exactly one node
 * Particles entering this Waterbody are taken out of the system.
 * In other words a Boundary is like a source/sink
 */
class Boundary extends Waterbody {
  /**
   *  Set fixed information for boundaries or pumps or Boundary Waterbodies
   */
  public Boundary(int nId, int[] ndArray, String boundaryName){
    super(Waterbody.BOUNDARY, nId, ndArray);
    String upName = boundaryName.toUpperCase();
    _name = upName;
    if (upName.contains("DICU")){
    	if(upName.contains("SEEP")){
    		_boundaryName = "AG_SEEP";
    		_isSeep = true;
    		_isDiv = false;
    	}
    	else if (upName.contains("DIV")){
    		_boundaryName = "AG_DIV";
    		_isSeep = false;
    		_isDiv = true;
    	}
    	else if  (upName.contains("DRAIN")){
    		_boundaryName = "AG_DRAIN";
    		_isSeep = false;
    		_isDiv = false;
    	}
    	else {PTMUtil.systemExit("Wrong DICU Type encountered: "+boundaryName);}
    }
    else{ _boundaryName = boundaryName;}
  }
  /**
   *  Return flow direction sign
   *  always opposite from H5 flow sign
   */
  public int flowType(int nodeId){return OUTFLOW;}
  public boolean isAgSeep(){ return _isSeep;}
  public boolean isAgDiv(){ return _isDiv;}
  /**
   *  Get the type from particle's point of view
   */
  @Override
  public int getPTMType(){return Waterbody.BOUNDARY;}
  /**
   *  Return the hydrodynamic type of Boundary
   */
  public int getHydroType(){return FlowTypes.rim;}

  public String getBoundaryName(){return _boundaryName;}
  public String getName() {return _name;}

  public float getInflowWSV(int nodeEnvId, float sv){
	//TODO implement unique inflow with swimming velocity later
	  return getInflow(nodeEnvId);
  }
  private String _boundaryName, _name;
  private boolean _isSeep = false, _isDiv = false;
}
