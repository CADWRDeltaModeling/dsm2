C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.
C!
C!    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
C!    numerical model.  No protection claimed in original FOURPT and
C!    Branched Lagrangian Transport Model (BLTM) code written by the
C!    United States Geological Survey.  Protection claimed in the
C!    routines and files listed in the accompanying file "Protect.txt".
C!    If you did not receive a copy of this file contact Dr. Paul
C!    Hutton, below.
C!
C!    This program is licensed to you under the terms of the GNU General
C!    Public License, version 2, as published by the Free Software
C!    Foundation.
C!
C!    You should have received a copy of the GNU General Public License
C!    along with this program; if not, contact Dr. Paul Hutton, below,
C!    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
C!    02139, USA.
C!
C!    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
C!    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
C!    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
C!    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
C!    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
C!    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
C!    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
C!    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
C!    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
C!    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
C!    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
C!    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
C!    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
C!    DAMAGE.
C!
C!    For more information about DSM2, contact:
C!
C!    Dr. Paul Hutton
C!    California Dept. of Water Resources
C!    Division of Planning, Delta Modeling Section
C!    1416 Ninth Street
C!    Sacramento, CA  95814
C!    916-653-5601
C!    hutton@water.ca.gov
C!
C!    or see our home page: http://wwwdelmod.water.ca.gov/

*===== BOF chinitcd.inc ==================================================
*   Version 93.01, January, 1993

*   Note: This include, if used, must appear after "Network.inc".

      INTEGER ChannelNumber(MaxChannels), Locations(MaxChannels)
      INTEGER FirstLocation(MaxChannels),NUserInitLocations(MaxChannels)
      INTEGER InitialConditionIndex(MaxChannels)
      REAL*8 InitialX(MaxLocations)
      REAL*8 InitialWS(MaxLocations), InitialQ(MaxLocations)
      LOGICAL InitCndInitialized
      COMMON / ChInitialFlow_I / ChannelNumber, Locations, FirstLocation,NUserInitLocations,
     &     InitialConditionIndex,
     &     InitCndInitialized

      COMMON / ChInitialFlow_r / 
     &     InitialX, InitialWS, InitialQ
      SAVE / ChInitialFlow_i /, /ChInitialFlow_r/


*   Definitions:
*     ChannelNumber(j) - channel number corresponding to sequence number "j".
*     Locations(j) - number of locations in ChannelNumber(j).
*     FirstLocation(j) - sequence number of first location in ChannelNumber(j).
*     InitialConditionIndex( ChannelNumber(j) ) - sequence number corresponding
*                                                 to ChannelNumber(j).
*     InitialX(i) - stream distance at initial location "i".
*     InitialWS(i) - water surface elevation at initial location "i".
*     InitialQ(i) - volumetric discharge at initial location "i".

*===== EOF chinitcd.inc ==================================================
