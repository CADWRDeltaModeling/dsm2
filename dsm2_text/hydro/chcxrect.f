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

*==== BOF chcxrect =====================================================

************************************************************************
*
*     This file is a FORTRAN module for the computation of the geometric
*     and hydraulic properties of multiple, rectangular, non-prismatic
*     channels.   Manning's "n" is assumed constant with depth and downstream
*     distance within a branch.    A branch is represented by  two
*     rectangular cross sections at its extremities.  Width,
*     depth, and wetted perimeter are interpolated linearly.  Area and
*     conveyance are subsequently computed from these interpolated
*     values.  Two data lines are input for each branch.  All input
*     is free-field.
*
*       line 0:
*       PrintOption (integer)
*
*       line 1:
*       ChannelNumber (integer)
*
*       line 2:
*       X1 , Width1, Btm1, X2, Width2, Btm2, Eta  (all real),
*
*          (Repeat above lines 1 and 2 for additional branches.)
*
*       where,
*
*     PrintOption - index for writing to standard output device.
*          [0] input data will not be written to standard output.
*          [1] input data will be echoed to standard output.
*     ChannelNumber - sequence number of channel, monotonically increasing,
*          beginning with 1.
*     X1   - downstream reference distance for upstream extent of branch
*     Width1   - channel width corresponding to X1
*     Btm1 - channel-bottom elevation corresponding to X1
*     X2   - downstream reference distance for downstream extent of branch
*     Width2   - channel width corresponding to X2
*     Btm2 - channel-bottom elevation corresponding to X2
*     Eta  - effective Manning's n for the branch
*
*
*
*     Module note: Only functions or subroutines marked "public" should be
*                   used outside of this module as those marked "private"
*                   may not be supported by future revisions of this module
*                   or replacement modules.  Likewise, no data or common
*                   blocks contained within this module should be accessed
*                   by routines outside of this module accept through the
*                   use of "public" functions or subroutines contained
*                   within this module.
*
*     Module note: The first function called from this module must be the
*                   initiallization function.  All other functions assume
*                   the module has been initialized.
*
*     Non-standard usage: Symbolic names in this module may be represented by
*                         as many as 31 characters in order to provide better
*                         definition directly in the code.  Standard FORTRAN
*                         allows only 6 characters, but this restriction is
*                         generally extended to 32 characters by most compilers.
*
*
*     Public functions:
*
*     LOGICAL FUNCTION InitializeChannelProperties()
*            - begin initialization of module.
*
*     REAL FUNCTION BtmSlope(X)
*            - returns channel-bottom slope.
*
*     REAL FUNCTION Beta(X,H)
*            - returns momentum coefficient.
*
*     REAL FUNCTION ChannelWidth(X,H)
*            - returns channel width.
*
*     REAL FUNCTION CxArea(X,H)
*            - returns cross-sectional area.
*
*     REAL FUNCTION Conveyance(X,H)
*            - returns sinuosity-weighted conveyance.
*
*     REAL FUNCTION dConveyance(X,H)
*            - returns d(Conveyance)/dH.
*
*     REAL FUNCTION UserStreamDistance(LocationNumber)
*            - returns downstream distance of LocationNumber.
*
*     REAL FUNCTION CxCentroid (X,H)
*            - returns distance from water surface to centroid.
*
*     REAL FUNCTION dCxCentroid (X,H)
*            - returns d(CxCentroid)/dH
*
*     REAL FUNCTION AreaWtSinuosity (X,H)
*            - returns area-weighted sinuosity.
*
*     REAL FUNCTION dAreaWtSinuosity (X,H)
*            - returns d(AreaWtSinuosity)/dH.
*
*     REAL FUNCTION QWtSinuosity (X,H)
*            - returns discharge-weighted sinuosity.
*
*     REAL FUNCTION dQWtSinuosity (X,H)
*            - returns d(QWtSinuosity)/dH.
*
*     Public subroutines:
*
*     SUBROUTINE UserStreamLocations( NumberOfCoordinates, Coordinate )
*            - returns an array of downstream distances of user locations.
*
*     Arguments:
*       fUnit  - FORTRAN unit number for proerties input data file
*       FileName - properties input data file name
*       NumberOfChannels   - number of branches in the current network
*       X      - current downstream distance
*       H      - current depth of flow
*
*
*     Module data:
*      'chcxrec1.inc'
*       X1(i)     - downstream reference distance,
*                   at upstream extent of channel "i".
*       Width1(i) - channel width at X(i).
*       Btm1(i)   - channel-bottom elevation at X(i).
*       X2(i)    - downstream reference distance,
*                   at downstream extent of channel "i".
*       Width2(i) - channel width at X(i).
*       Btm2(i)   - channel-bottom elevation at X(i).
*       Eta(i)    - effective Manning's n for channel "i".
*       i         - channel sequence number.
*
*      'chcxrec2.inc'
*       fUnit     - FORTRAN unit number for data input.
*       PrintOption - option for printing input data,
*            [0] do not print, or
*            [1] print.
*
************************************************************************

*== Public (BtmSlope) ==================================================

      REAL FUNCTION BtmSlope(X)

      IMPLICIT NONE

*   Purpose:  Compute bottom slope.

*   Arguments:
      REAL X

*   Argument definitions:
*     X      - downstream distance within channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chcxrec1.inc'

*   Local variable:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation ------------------------------------------------------------

      BtmSlope      = ( Btm2(Branch) - Btm1(Branch) )/
     &     (X2(Branch)-X1(Branch))

      RETURN
      END

*== Public (BtmElev) ===================================================

      REAL FUNCTION BtmElev(X)

      IMPLICIT NONE

*   Purpose:  Return bottom elevation.

*   Arguments:
      REAL X

*   Argument definitions:
*     X      - downstream distance within channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chcxrec1.inc'

*   Local variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation ------------------------------------------------------------

      BtmElev = (
     &     (X2(Branch)-X)*Btm1(Branch)+(X-X1(Branch))*Btm2(Branch)
     &     ) / (X2(Branch)-X1(Branch))

      RETURN
      END

*== Public (Beta) ======================================================

      REAL FUNCTION Beta(X,H)

      IMPLICIT NONE

*   Purpose:  Return momentum coefficient, assumed equal to 1.0.

*   Arguments:
      REAL X, H

*   Argument definitions:
*     X      - downstream distance within channel.
*     H      - depth of flow.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chcxrec1.inc'

*   Local variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation ------------------------------------------------------------

      Beta = 1.0

      RETURN
      END

*== Public (dBeta) =====================================================

      REAL FUNCTION dBeta(X,H)

      IMPLICIT NONE

*   Purpose:  Return d(Beta)/dX, assumed equal to 0.0.

*   Arguments:
      REAL X, H

*   Argument definitions:
*     X      - downstream distance within channel.
*     H      - depth of flow.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chcxrec1.inc'

*   Local variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation ------------------------------------------------------------

      dBeta = 0.0

      RETURN
      END

*== Public (Conveyance) ==============================================

      REAL FUNCTION Conveyance(X,H)

      IMPLICIT NONE

*   Purpose:  Compute conveyance.

*   Arguments:
      REAL X,H

*   Argument definitions:
*     X      - downstream distance.
*     H      - depth of flow.

*   Module data:
      INCLUDE 'network.inc'

      include '../input/fixed/common.f'

*   Local variables:
      REAL R53,R23
      PARAMETER (R53 = 5.0/3.0, R23 = 2.0/3.0)

*   Routines by module:

***** Locals:
      REAL     CxArea,WetPerimeter,ManningN
      EXTERNAL CxArea,WetPerimeter,ManningN

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -------------------------------------------------

 610  format(/' Error...  Negative depth of',f10.3,
     &     ' found in channel ',i3
     &     /' Model Time ',a)

      if (h.le.0.) then
         write(unit_error,610) h,int2ext(branch),current_dt
         call exit(1)
      endif
      Conveyance = 1.486*(CxArea(X,H)**R53)
     &     /(WetPerimeter(X,H)**R23)
     &     /ManningN(X,H)

      RETURN
      END

*== Public (dConveyance) =============================================

      REAL FUNCTION dConveyance(X,H)

      IMPLICIT NONE

*   Purpose:  Compute d(Conveyance)/dH.

*   Arguments:
      REAL X,H

*   Argument definitions:
*     X      - downstream distance.
*     H      - depth of flow.

*   Module data:
      INCLUDE 'network.inc'

*   Local variables:
      REAL R53,R23
      PARAMETER (R53 = 5.0/3.0, R23 = 2.0/3.0)

*   Routines by module:

***** Locals:
      REAL     ChannelWidth,CxArea,WetPerimeter,dWetPerimeter
      EXTERNAL ChannelWidth,CxArea,WetPerimeter,dWetPerimeter

      REAL     ManningN,dManningN,Conveyance
      EXTERNAL ManningN,dManningN,Conveyance

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -------------------------------------------------

      dConveyance = R53 * 1.486 * ChannelWidth(X,H) * CxArea(X,H)**R23
     &     / ManningN(X,H) / WetPerimeter(X,H)**R23
     &     - 1.486 * R23 * dWetPerimeter(X,H) * CxArea(X,H)**R53
     &     / ManningN(X,H) / WetPerimeter(X,H)**R53

      RETURN
      END

*== Public (ChannelWidth) ==============================================

      REAL FUNCTION ChannelWidth(X,H)

      IMPLICIT NONE

*   Purpose:  Compute width of channel.

*   Arguments:
      REAL X,H

*   Argument definitions:
*     X      - downstream distance.
*     H      - depth of flow.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chcxrec1.inc'

*   Local variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation ------------------------------------------------------------

      ChannelWidth = (
     &     (X2(Branch)-X)*Width1(Branch)+(X-X1(Branch))*Width2(Branch)
     &     ) / (X2(Branch)-X1(Branch))

      RETURN
      END

*=====Public (dChannelWidth)========================================================

      REAL FUNCTION dChannelWidth(X,H)

*     Purpose:   Compute side slope of channel.

      IMPLICIT NONE

*     Arguments:
      REAL X,H

*     Definitions:
*       X      - downstream distance.
*       H      - depth of flow.

*   Module data:
      INCLUDE 'network.inc'

*-----Implementation -----------------------------------------------------

      dChannelWidth = 0.0

      RETURN
      END

*== Public (CxArea) ====================================================

      REAL FUNCTION CxArea(X,H)

      IMPLICIT NONE

*   Purpose:  Compute cross-sectional area.

*   Arguments:
      REAL X, H

*   Argument definitions:
*     X      - downstream distance.
*     H      - depth of flow.

*   Module data:
      INCLUDE 'network.inc'

*   Local variables:

*   Routines by module:

***** Local:
      REAL     ChannelWidth
      EXTERNAL ChannelWidth

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation ------------------------------------------------------------

      CxArea = H*ChannelWidth(X,H)

      RETURN
      END

*== Private (WetPerimeter) =============================================

      REAL FUNCTION WetPerimeter(X,H)

      IMPLICIT NONE

*   Purpose:  Compute wetted perimeter.

*   Arguments:
      REAL X,H

*   Argument definitions:
*     X      - downstream distance.
*     H      - depth of flow.

*   Module data:
      INCLUDE 'network.inc'

*   Local variables:

*   Routines by module:

*****-Locals:
      REAL     ChannelWidth
      EXTERNAL ChannelWidth

*   Programmed by:
*   Date:
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation ------------------------------------------------------------

      WetPerimeter = 2.0*H+ChannelWidth(X,H)

      RETURN
      END

*== Private (dWetPerimeter) ============================================

      REAL FUNCTION dWetPerimeter(X,H)

      IMPLICIT NONE

*   Purpose:  Compute d(wp)/dH.

*   Arguments:
      REAL X,H

*   Argument definitions:
*     X      - downstream distance within channel.
*     H      - depth of flow.

*   Module data:
      INCLUDE 'network.inc'

*   Local variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation ------------------------------------------------------------

      dWetPerimeter = 2.0

      RETURN
      END

*== Private (ManningN) =================================================

      REAL FUNCTION ManningN(X,H)

      IMPLICIT NONE

*   Purpose:  Compute effective flow-resistance coefficient.

*   Arguments:
      REAL X,H

*   Argument definitions:
*     X      - downstream distance.
*     H      - depth of flow.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chcxrec1.inc'

*   Local variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation ------------------------------------------------------------

      ManningN = Eta(Branch)

      RETURN
      END

*== Private (dManningN) ================================================

      REAL FUNCTION dManningN(X,H)

      IMPLICIT NONE

*   Purpose:  Compute d(ManningN)/dH.

*   Arguments:
      REAL X,H

*   Argument definitions:
*     X      - downstream distance.
*     H      - depth of flow.

*   Module data:
      INCLUDE 'network.inc'

*   Local variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation ------------------------------------------------------------

      dManningN = 0.0

      RETURN
      END

*== Public (ReadHydraulicProperties) ===================================

      LOGICAL FUNCTION ReadHydraulicProperties(ChannelNumber)

      IMPLICIT NONE

*   Purpose:  Read geometric and hydraulic properties.

*   Arguments:
      INTEGER ChannelNumber

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chcxrec1.inc'
      INCLUDE 'chcxrec2.inc'

      include '../input/fixed/misc.f'

*   Local variables:
      INTEGER Brn, I

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation ------------------------------------------------------------

 2001 FORMAT(/'             Cross-sectional properties...'/)
 2002 FORMAT('               Distance     Width   Btm_elev'/
     &     ' Branch',I3/
     &     ' Upstream.....',3F10.2/
     &     ' Downstream...',3F10.2/
     &     ' Effective n..',F6.3/)

      ReadHydraulicProperties = .FALSE.

      IF(PrintOption.GT.0) THEN
         WRITE(UNIT_ERROR,*) ' '
         WRITE(UNIT_ERROR,*) ' Channel ',int2ext(ChannelNumber),'...'
         WRITE(UNIT_ERROR,2001)
      END IF

      Brn = ChannelNumber
      READ(fUnit,*,END=200) I

      IF(I .NE. Brn ) THEN
         WRITE(*,*) ' Channel sequence error...'
         WRITE(*,*) ' Read number', I
         WRITE(*,*) ' Changed to ',Brn
      END IF

      READ(fUnit,*,END=200) Eta(Brn)

      READ(fUnit,'(A16)',END=200) UpStreamID(Brn)
      READ(fUnit,*,END=200)
     &     X1(Brn),Width1(Brn),Btm1(Brn)
      READ(fUnit,'(A16)',END=200) DownStreamID(Brn)
      READ(fUnit,*,END=200)
     &     X2(Brn),Width2(Brn),Btm2(Brn)

      IF(PrintOption.GT.0) THEN
         WRITE(*,2002) Brn,
     &        X1(Brn),Width1(Brn),Btm1(Brn),
     &        X2(Brn),Width2(Brn),Btm2(Brn),Eta(Brn)
      END IF

      ReadHydraulicProperties = .TRUE.

      RETURN

 200  CONTINUE

      WRITE(UNIT_ERROR,*) ' End of file...reading Channel',Brn

      RETURN
      END

*== Public (UserStreamLocations) =======================================

      SUBROUTINE UserStreamLocations(
     &     NumberOfCoordinates,
     &     ID, Coordinate
     &     )

      IMPLICIT NONE

*   Purpose:  Return coordinates of user-supplied location IDs
*             and locations in current channel.

*   Arguments:
      INTEGER NumberOfCoordinates
      REAL    Coordinate(NumberOfCoordinates)
      CHARACTER*16 ID(NumberOfCoordinates)

*   Argument definitions:
*     NumberOfCoordinates - dimension of following array.
*     Coordinates(i) - downstream distances of locations in current channel.
*     ID(i) - location identifiers.
*     i - location index, 1 to NumberOfLocations, increasing downstream.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chcxrec1.inc'

      include '../input/fixed/misc.f'

*   Local Variables:

*   Routines by module:

***** Channel schematic:
      EXTERNAL GetUserStreamLocationIDs

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      CALL GetUserStreamLocationIDs(NumberOfCoordinates, ID)

      IF( Branch .GT. 0 ) THEN
         IF( ID(1) .EQ. UpStreamID(Branch) ) THEN
            Coordinate(1) = X1(Branch)
C-----------WRITE(*,*) '  Upstream coordinate...',X1(Branch)
         ELSE
            WRITE(UNIT_ERROR,*) ' ***ERROR (UserStreamLocations)...'
            WRITE(UNIT_ERROR,*) ' ID mismatch...upstream...channel ',Branch
            WRITE(UNIT_ERROR,'(A16,2X,A16)') ID(1), UpStreamID(Branch)
         END IF

         IF( ID(2) .EQ. DownStreamID(Branch) ) THEN
            Coordinate(2) = X2(Branch)
C-----------WRITE(*,*) '  Downstream coordinate...',X2(Branch)
         ELSE
            WRITE(UNIT_ERROR,*) ' ***ERROR (UserStreamLocations)...'
            WRITE(UNIT_ERROR,*) ' ID mismatch...downstream...channel ',Branch
            WRITE(UNIT_ERROR,'(A16,2X,A16)') ID(2), DownStreamID(Branch)
         END IF

      ELSE
         WRITE(UNIT_ERROR,*) ' CurrentChannel not set (UserStreamLocations)...'
         CALL EXIT(1)
      END IF

      RETURN
      END

*== Public (UserStreamDistance) ========================================

      REAL FUNCTION UserStreamDistance(LocationNumber)

      IMPLICIT NONE

*   Purpose:  Return the downstream distance of the user-supplied
*             location corresponding to the "LocationNumber"
*             location counted from the upstream end of the current
*             channel.

*   Arguments:
      INTEGER LocationNumber

*   Argument definitions:
*     LocationNumber - sequential location number within the current
*                       channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chcxrec1.inc'

      include '../input/fixed/misc.f'

*   Local variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      IF( LocationNumber .EQ. Branch*2 ) THEN

*--------Requested location is at downstream end of current branch.
         UserStreamDistance = X2(Branch)

      ELSE IF( LocationNumber .EQ. Branch*2-1 ) THEN

*--------Requested location is at upstream end of current branch.
         UserStreamDistance = X1(Branch)

      ELSE

*--------Requested location does not fall within current branch,
*       return upstream end with warning.

         WRITE(UNIT_ERROR,*) ' Requested user-supplied location does not'
         WRITE(UNIT_ERROR,*) ' fall within current branch...(',Branch,')'
         WRITE(UNIT_ERROR,*) ' (UserStreamDistance)'
         WRITE(UNIT_ERROR,*) ' Upstream most location returned...'
         UserStreamDistance = X1(Branch)

      END IF

      RETURN
      END

*== Public (AreaWtSinuosity) ================================================

      REAL FUNCTION AreaWtSinuosity(X,H)

      IMPLICIT NONE

*   Purpose:
*     Estimate area-weighted sinuosity in the current channel,
*     at X downstream distance, for the area limited by the lowest point
*     in the channel and a distance H above the lowest point.
*     This routine is a stub, returning only 1.0.

*   Arguments:
      REAL X, H

*   Argument definitions:
*     X      - downstream distance within channel.
*     H      - depth of flow.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chcxrec1.inc'

*   Local Variables:

*   Routines by module:

***** Local:

*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          October 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      AreaWtSinuosity = 1.0

      RETURN
      END

*== Public ( dAreaWtSinuosity) ================================================

      REAL FUNCTION  dAreaWtSinuosity(X,H)

      IMPLICIT NONE

*   Purpose:
*     Estimate d(area-weighted sinuosity)/dH in the current channel,
*     at X downstream distance, for the area limited by the lowest point
*     in the channel and a distance H above the lowest point.
*     This routine is a stub, returning only 0.0.

*   Arguments:
      REAL X, H

*   Argument definitions:
*     X      - downstream distance within channel.
*     H      - depth of flow.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chcxrec1.inc'

*   Local Variables:

*   Routines by module:

***** Local:

*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          October 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      dAreaWtSinuosity = 0.0

      RETURN
      END

*== Public ( QWtSinuosity) ================================================

      REAL FUNCTION  QWtSinuosity(X,H)

      IMPLICIT NONE

*   Purpose:
*     Estimate Q-weighted sinuosity in the current channel,
*     at X downstream distance, for the area limited by the lowest point
*     in the channel and a distance H above the lowest point.
*     This routine is a stub, returning only 1.0.

*   Arguments:
      REAL X, H

*   Argument definitions:
*     X      - downstream distance within channel.
*     H      - depth of flow.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chcxrec1.inc'

*   Local Variables:

*   Routines by module:

***** Local:

*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          October 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      QWtSinuosity = 1.0

      RETURN
      END

*== Public ( dQWtSinuosity) ================================================

      REAL FUNCTION  dQWtSinuosity(X,H)

      IMPLICIT NONE

*   Purpose:
*     Estimate d(Q-weighted sinuosity)/dH in the current channel,
*     at X downstream distance, for the area limited by the lowest point
*     in the channel and a distance H above the lowest point.
*     This routine is a stub, returning only 0.0.

*   Arguments:
      REAL X, H

*   Argument definitions:
*     X      - downstream distance within channel.
*     H      - depth of flow.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chcxrec1.inc'

*   Local Variables:

*   Routines by module:

***** Local:

*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          October 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      dQWtSinuosity = 0.0

      RETURN
      END

*== Public (CxCentroid) ====================================================

      REAL FUNCTION CxCentroid(X,H)

      IMPLICIT NONE

*   Purpose:
*     Estimate distance from the water surface to the cross-section
*     centroid, in the current channel, at X downstream distance,
*     limited by the lowest point in the channel and a
*     distance H above the lowest point.

*   Arguments:
      REAL X, H

*   Argument definitions:
*     X      - downstream distance.
*     H      - depth of flow.

*   Module data:
      INCLUDE 'network.inc'

*   Local variables:

*   Routines by module:

***** Local:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation ------------------------------------------------------------

      CxCentroid = H * 0.5

      RETURN
      END

*== Public (dCxCentroid) ====================================================

      REAL FUNCTION dCxCentroid(X,H)

      IMPLICIT NONE

*   Purpose:
*     Estimate d(CxCentroid)/dH, in the current channel,
*     at X downstream distance, limited by the lowest point
*     in the channel and a distance H above the lowest point.

*   Arguments:
      REAL X, H

*   Argument definitions:
*     X      - downstream distance.
*     H      - depth of flow.

*   Module data:
      INCLUDE 'network.inc'

*   Local variables:

*   Routines by module:

***** Local:

*   Programmed by: Lew DeLong
*   Date:          November 1990
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation ------------------------------------------------------------

      dCxCentroid = 0.5

      RETURN
      END

*==== EOF chcxrect =====================================================
