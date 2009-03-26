/*
 * Created on Nov 17, 2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package DWR.DMS.PTM;

import java.util.Arrays;
import java.util.Comparator;

public class WaterbodyGroup implements Group {
	private String name;

	private final WaterbodyGroupMember[] members;

	public WaterbodyGroup(String name, int[] types,int[] indexes) {
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

	public boolean containsWaterbody(Waterbody wb) {
		//@todo: this isn't necessarily right -- what about "any" Group?
		// if (wb == null) return false;
		//if (wb == null) throw new NullPointerException("Null Waterbody");
		int contains=indexOfWaterBody(wb);
                return contains >= 0;
	}
        boolean done=false;
	private int indexOfWaterBody(Waterbody wb) {
		// if (wb == null) throw new NullPointerException("Null waterbody");
		if (members == null) throw new IllegalStateException("Empty group");
        int ndx = -1;
        if ( wb != null ) {
            ndx= Arrays.binarySearch(members, new WaterbodyGroupMember(wb
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

	public String getName() {
		return this.name;
	}

	public String toString() {
		return getName();
	}

	public class WaterbodyGroupMember implements Comparable {
		public static final int ANY_INDEX = -9998;

		public static final int ANY_TYPE = -9999;

		private int memberType;

		private int memberNdx;


		
		public WaterbodyGroupMember(int type, int localNdx) {
			memberType = type;
			memberNdx = localNdx;
		}

		public int getMemberType(){return memberType;}
		public int getMemberIndex(){return memberNdx;}		
		
		
		private int compareWithWildcard(int a, int b, int wild) {
			if (a == wild || b == wild){
				// one of the two is a wildcard, matches anything
				return 0;
			}
			// standard comparison
			// return 1 if a is bigger, -1 if smaller, else 0
			return a > b ? 1 : (a < b ? -1 : 0);
		}

		public int compareTo(Object o) {
			if (!(o instanceof WaterbodyGroupMember))
				throw new IllegalArgumentException(
						"Water body group member only comparable to other members");
			WaterbodyGroupMember otherMember = (WaterbodyGroupMember) o;
			int typeComp = 0;
			if (memberType == ANY_TYPE || otherMember.memberType == ANY_TYPE) {
				return 0;
			} else {
				int typeMatch = compareWithWildcard(memberType,
						otherMember.memberType, ANY_TYPE);
				//System.out.println("Comparison: type: " + memberType + " vs "otherMember.memberType + " gave "+typeMatch);
				if (typeMatch != 0)
					return typeMatch;
				else
					return compareWithWildcard(memberNdx,
							otherMember.memberNdx, ANY_INDEX);
			}
		}
	}
}
