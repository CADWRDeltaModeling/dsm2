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
class LimitsFixedData {
public LimitsFixedData(int maxChannels,
		       int maxReservoirs,
		       int maxDiversions,
		       int maxPumps,
		       int maxBoundaryWaterbodies,
		       int maxNodes,
		       int maxXSections){
  this.maxChannels =  maxChannels;
  this.maxReservoirs =  maxReservoirs;
  this.maxDiversions =  maxDiversions;
  this.maxPumps =  maxPumps;
  this.maxBoundaryWaterbodies =  maxBoundaryWaterbodies;
  this.maxNodes =  maxNodes;
  this.maxXSections =  maxXSections;
}

public String toString(){
  return "limitsFixedData: \n";
}

public int getMaximumNumberOfChannels(){
  return maxChannels;
}
public int getMaximumNumberOfReservoirs(){
  return maxReservoirs;
}
public int getMaximumNumberOfDiversions(){
  return maxDiversions;
}
public int getMaximumNumberOfPumps(){
  return maxPumps;
}
public int getMaximumNumberOfNodes(){
  return maxNodes;
}
public int getMaximumNumberOfBoundaryWaterbodies(){
  return maxBoundaryWaterbodies;
}
public int getMaximumNumberOfXSections(){
  return maxXSections;
}
public int getMaximumNumberOfReservoirNodes(){
  return 20;
}
protected int maxChannels;
protected int maxReservoirs;
protected int maxDiversions;
protected int maxPumps;
protected int maxBoundaryWaterbodies;
protected int maxNodes;
protected int maxXSections;
}
