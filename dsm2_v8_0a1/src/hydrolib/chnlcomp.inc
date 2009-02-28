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

*===== BOF chncomp.inc ================================================
*   Version 93.01, January, 1993

*   Note: This include, when used, must follow  after "Network.inc".

      INTEGER TotalCompLocations
      INTEGER NumberOfCompLocations(MaxChannels)
      INTEGER UpCompPointer(MaxChannels)
      INTEGER DownCompPointer(MaxChannels)
      REAL*8    CompLocation(MaxLocations)
      REAL*8    DummyArray(MaxLocations), DummyArray2(MaxLocations)
      CHARACTER*16 DummyCharArray(MaxLocations)
      COMMON /ChCompLocations_i/
     &     TotalCompLocations,
     &     NumberOfCompLocations,
     &     UpCompPointer, DownCompPointer
      COMMON /ChCompLocations_r/
     &     CompLocation,
     &     DummyArray, DummyArray2
      COMMON / DummyChar / DummyCharArray
      SAVE / ChCompLocations_i /, / ChCompLocations_r /, / DummyChar / 

*   Definitions:
*     TotalCompLocations - total number of computational locations
*                          in the current application.

*           for the current channel, i,
*     MaxChannels - maximum number of channels.
*     NumberOfCompLocations(i) - number of computational locations.
*     UpCompPointer(i) - global sequence number of most upstream
*                        location.
*     DownCompPointer(i) - global sequence number of most downstream
*                          location.

*            for the global sequence number, j,
*     CompLocation(j) - downstream distance coordinate.
*     DummyArray(j) - a REAL*8 value dependent upon context.
*     DummyArray2(j) - a REAL*8 value dependent upon context.
*     DummyCharArray(j) - a CHARACTER value dependent upon context.

*===== EOF chncomp.inc ================================================
