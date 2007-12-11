//    Copyright (C) 1996 State of California, Department of Water
//    Resources.
//
//    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
//    numerical model.  No protection claimed in original FOURPT and
//    Branched Lagrangian Transport Model (BLTM) code written by the
//    United States Geological Survey.  Protection claimed in the
//    routines and files listed in the accompanying file "Protect.txt".
//    If you did not receive a copy of this file contact Dr. Paul
//    Hutton, below.
//
//    This program is licensed to you under the terms of the GNU General
//    Public License, version 2, as published by the Free Software
//    Foundation.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, contact Dr. Paul Hutton, below,
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
//    Dr. Paul Hutton
//    California Dept. of Water Resources
//    Division of Planning, Delta Modeling Section
//    1416 Ninth Street
//    Sacramento, CA  95814
//    916-653-5601
//    hutton@water.ca.gov
//
//    or see our home page: http://wwwdelmod.water.ca.gov/

//$Id: xSection.java,v 1.2 2000/08/07 17:00:36 miller Exp $

package DWR.DMS.PTM;
import java.lang.*;

/**
 *  CLASS
 * 
 *  xSection
 * 
 *  This class encapsulates the data for cross Section information
 *  The actual manipulation to calculate depth, width information in
 *  a waterbody relies on this data.
 * 
 *  Currently all width, depth information is being calculated from the
 *  tide file input
 */
class xSection{
  
  /**
   *  Constructor
   */
public xSection(int nId, int numElvs, float dist,
		float[] w, float[] a, float[] elv, float minElv,
		boolean irreg){
  EnvIndex = nId;
  channelNumber = -1;
  irregular = irreg;
  distance = dist;
  numberOfElevations = numElvs;
  minimumElevation = minElv;
  width = new float[numberOfElevations];
  area = new float[numberOfElevations];
  elevation = new float[numberOfElevations];
  for (int i=0 ; i < numberOfElevations; i++) {
    width[i] = w[i];
    area[i] = a[i];
    elevation[i] = elv[i];
  }
}

  /**
   *  returns the index of xSection array in PTMEnv
   */
public final int getEnvIndex(){
  return(EnvIndex);
}


  /**
   *  returns distance of cross section from up stream end
   */
public final float getDistance(){
  return (distance);
}


  /**
   *  sets distance of cross section from up stream end
   */
public final void setDistance(float d){
  distance = d;
}


  /**
   *  returns type of cross section: rectangular or irregular
   */
public final boolean isIrregular(){
  return(irregular);
}


  /**
   *  sets the channel number EnvIndex for this cross section
   */
public final void setChannelNumber(int channelIndex){
  channelNumber = channelIndex;
}



  /**
   *  to xSection array in PTMEnv
   */
private int EnvIndex;

  /**
   *  channel to which the cross section belongs
   */
private int channelNumber;

  /**
   *  flag to check if cross section is irregular
   */
private boolean irregular;

  /**
   *  number of elevations defined in the cross section
   */
private int numberOfElevations;

  /**
   *  minimum elevation for cross section
   */
private float minimumElevation;
  /**
   *  array of elevation at which width and area are defined
   */
private float[] elevation;
  /**
   *  array of widths at each elevation
   */
private float[] width;

  /**
   *  array of areas at each elevation
   */
private float[] area;

  /**
   *  distance of cross section from upstream end
   */
private float distance;
}

