C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    The Delta Simulation Model 2 (DSM2) is free software: 
C!    you can redistribute it and/or modify
C!    it under the terms of the GNU General Public License as published by
C!    the Free Software Foundation, either version 3 of the License, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public License for more details.

C!    You should have received a copy of the GNU General Public License
C!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
C!</license>


      INTEGER NOBR,NOSC,NOPR
      PARAMETER (NOBR=800, NOSC=15, NOPR=24)
      INTEGER MAXNUMRES,MAXRESJUNC
      PARAMETER (MAXNUMRES=30,MAXRESJUNC=15)
      integer ihout, irhout
      parameter (ihout=50,irhout=51)
      integer idoout, irdoout
      parameter (idoout=52,irdoout=53)
      integer ialgout, iralgout
      parameter (ialgout=54,iralgout=55)
