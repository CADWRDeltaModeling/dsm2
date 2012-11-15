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
/**
 * Fixed Data for Filters
 * Read from DSM2 Fortran & C++ input system   
 * @author Yu Zhou
 * @version 2012/07/3 18:16:24
 */
class FiltersFixedData {
public FiltersFixedData(int nFilters,
                        int[] filterIndices,
                        String [] filterNames,
                        int[] filterNodes, 
                        int[] filterWbs,
                        int[] filterWbTypes){

  this.nFilters = nFilters; 
  this.filterIndices = filterIndices;
  this.filterNames = filterNames;
  this.filterNodes = filterNodes; 
  this.filterWbs = filterWbs; 
  this.filterWbTypes = filterWbTypes; 
}

public String toString(){
  return "filtersFixedData: \n";
}

public int getNumberOfFilters(){
  return nFilters;
}
public int getFilterIndex(int i){
  return filterIndices[i];
}
public String getFilterName(int i){
  return filterNames[i];
}
public int getFilterNode(int i){
  return filterNodes[i];
}
public int getFilterWb(int i){
  return filterWbs[i];
}
public int getFilterWbType(int i){
  return filterWbTypes[i];
}


public int[] getIndicesOfFilters(){
  return filterIndices;
}
public String[] getNamesOfFilters(){
  return filterNames;
}
public int[] getNodesOfFilters(){
  return filterNodes;
}
public int[] getwaterbodiesOfFilters(){
  return filterWbs;
}
public int[] getWaterbodyTypesOfFilters(){
  return filterWbTypes;
}
/**
 * check existence of particle filter for node and waterbody
 */
public boolean filterExist(Node nd, Waterbody wb) {
    boolean filterExist = false;
	String key = ""+nd.getEnvIndex()+","
	           + ""+wb.getEnvIndex();
	if(filterArr.containsKey(key)){//TODO
		filterExist = true;
	}
	return filterExist;
}

//static public float[] getFilterOps(int time){
//	filterOps = PTMFixedData.getOpsOfFilters(time);
//	return filterOps;
//}
/**
 * Fills the node with filters' array
 */
//static public void setFilterArr(String key, Filter filter) {
//  filterArr.put(key,filter);
//}

protected int nFilters; 
private int[] filterIndices;
private String[] filterNames;
protected int[] filterNodes; 
protected int[] filterWbs; 
protected int[] filterWbTypes; 
private float [] filterOps;   //on-1;off-0.

/**
 * filters' array on the specified node
 */
private HashMap<String,Filter> filterArr;
}
