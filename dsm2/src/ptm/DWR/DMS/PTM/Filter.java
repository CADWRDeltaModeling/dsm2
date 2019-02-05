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
 * A Filter is attached with one node, reside on one waterbody, e.g. channel, reservoir, boundary(ag) 
 * Filter is 2-directional, with different functions respectively
 * at node enter side, filter functions as a catching net only if efficiency = 0, block and keep particles from entering 
 * at node exit side, filter functions as decision making adjustment if node has multiple target waterbodies
 * @author Yu Zhou
 * @version 2010/09/20 18:16:24
 */
public class Filter {
public Filter(int filterIndex,
              String filterName,
              int filterNode, 
              int filterWb,
              int filterWbType){

this.filterIndex = filterIndex;
this.filterName = filterName;
this.filterNode = filterNode; 
this.filterWb = filterWb; 
this.filterWbType = filterWbType; 

}

public String toString(){
  return "filter: \n";
}

public String getFilterName(){
  return filterName;
}
public int getFilterNode(){
  return filterNode;
}
public int getFilterWb(){
  return filterWb;
}
public int getFilterWbType(){
  return filterWbType;
}

/**
 * gets the Filter Operation at the specified timestamp
 */
//TODO Xiao commented out to deal with it later
/*
public float getFilterOp(){
  float[] filterOps = Globals.Environment.getFilterOps();
  filterOp = filterOps[filterIndex];
  return filterOp;
}
*/

private int filterIndex;
private String filterName;
protected int filterNode; 
protected int filterWb; 
protected int filterWbType; 
private float filterOp;   //Filter Operation at the specified timestamp: 1-on; 0-off
}
