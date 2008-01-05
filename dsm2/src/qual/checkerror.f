C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public !<license as published by
C!    the Free Software Foundation, either version 3 of the !<license, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public !<license for more details.

C!    You should have received a copy of the GNU General Public !<license
C!    along with DSM2.  If not, see <http://www.gnu.org/!<licenses/>.
C!</license>

      SUBROUTINE CHECKERROR

C-----This subroutine checks for error in the input
      use io_units
      use grid_data
      IMPLICIT NONE
      INCLUDE 'param.inc'

      INCLUDE 'bltm1.inc'
      INCLUDE 'bltm3.inc'
      INCLUDE 'bltm2.inc'

C-----Local variables

      INTEGER JN
      LOGICAL LERROR

      LERROR=.FALSE.

      DO JN=1,NJUNC
         IF(JUNCFLG(JN).NE.1)THEN
            WRITE(unit_error,951)JN
 951        FORMAT(' ERROR JUNCTION #: ',I3,' IS NOT USED')
            LERROR=.TRUE.
         ENDIF
      ENDDO

      IF (LERROR) THEN
         call exit(2)
      endif

      RETURN
      END

