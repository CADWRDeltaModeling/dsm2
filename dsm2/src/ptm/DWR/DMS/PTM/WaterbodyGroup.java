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
import java.util.Arrays;
import java.util.Comparator;
/**
 *  
 */
public class WaterbodyGroup implements Group {

  private String name;
  private final WaterbodyGroupMember[] members;
  
  /**
   *  Constructor
   */
  public WaterbodyGroup(String name, int[] types, int[] indexes) {
    if (types == null)throw new NullPointerException("array of wb types is null");
    if (indexes == null)throw new NullPointerException("array of wb indexes is null");		
    if (indexes.length != types.length)
      throw new IllegalArgumentException(
        "type and index arguments do not match in size"
      );
    
    int nmember=types.length;		
    members = new WaterbodyGroupMember[nmember];
    for (int i = 0; i < nmember; i++) {
      members[i]=new WaterbodyGroupMember(types[i],indexes[i]);
    }
    this.name = name;
    Arrays.sort(members);
  }
  
  /**
   * 
   */
  public boolean containsWaterbody(Waterbody wb) {
  	//@todo: this isn't necessarily right -- what about "any" Group?
  	// if (wb == null) return false;
  	//if (wb == null) throw new NullPointerException("Null Waterbody");
  	int contains = indexOfWaterBody(wb);
    return contains >= 0;
  }
  
  //boolean done=false;
  
  /**
   * 
   */
  private int indexOfWaterBody(Waterbody wb) {
  	// if (wb == null) throw new NullPointerException("Null waterbody");
  	if (members == null) throw new IllegalStateException("Empty group");
      if (members.length == 0 ) throw new IllegalStateException("Empty group");
  
      int ndx = -1;
      if ( wb != null ) {
        ndx = Arrays.binarySearch(members, new WaterbodyGroupMember(wb
  			.getType(), wb.getEnvIndex()));
      }
      else {
        if ( members[0].getMemberIndex() == -9998 ) {
          ndx = 0;
        }
      }
  	//if (ndx >= 0)System.out.println("Found wb. Index in Group="+ndx);
  	return ndx;
  }
  
  /**
   *  get the name of this waterbody group
   */
  public String getName() {
  	return this.name;
  }
  
  /**
   *  String representation
   */
  public String toString() {
  	return getName();
  }

  /**
   * sub-class of waterbody group member
   */
  public class WaterbodyGroupMember implements Comparable {
  
    public static final int ANY_INDEX = -9998;
    public static final int ANY_TYPE = -9999;
    private int memberType;
    private int memberNdx;
    
    /**
     *  sub-class Constructor
     */   
    public WaterbodyGroupMember(int type, int localNdx) {
    	memberType = type;
    	memberNdx = localNdx;
    }
    
    public int getMemberType(){return memberType;}
    public int getMemberIndex(){return memberNdx;}
    
    /**
     * 
     */
    private int compareWithWildcard(int a, int b, int wild) {
      if (a == wild || b == wild){
      	// one of the two is a wildcard, matches anything
      	return 0;
      }
      // standard comparison
      // return 1 if a is bigger, -1 if smaller, else 0
      return a > b ? 1 : (a < b ? -1 : 0);
    }
      
    /**
     * 
     */
    public int compareTo(Object o) {
      if (!(o instanceof WaterbodyGroupMember))
        throw new IllegalArgumentException(
      	  "Water body group member only comparable to other members");
      WaterbodyGroupMember otherMember = (WaterbodyGroupMember) o;
      int typeComp = 0;
      
      if (memberType == ANY_TYPE || otherMember.memberType == ANY_TYPE) {
      	return 0;
      } else {
      	int typeMatch = compareWithWildcard(memberType, otherMember.memberType, ANY_TYPE);
      	//System.out.println("Comparison: type: " + memberType + " vs "otherMember.memberType + " gave "+typeMatch);
      	if (typeMatch != 0)
      	  return typeMatch;
      	else
      	  return compareWithWildcard(memberNdx, otherMember.memberNdx, ANY_INDEX);
      }
    }
    
  }//end of class WaterbodyGroupMember
}//end of class WaterbodyGroup
