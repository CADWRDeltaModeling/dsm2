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

*==== BOF master.inc ==========================================================
*   Version 93.01, January, 1993
*******************************************************************************
*
*     INCLUDE FILE FOR MASTER FILE MODULE
*
*******************************************************************************

      INTEGER       MaxFiles, NameLength

      PARAMETER    (
     &     MaxFiles   = 26,
     &     NameLength = 12
     &     )

      CHARACTER*12  DefaultName(MaxFiles)
      CHARACTER*12  UserName(MaxFiles)
      LOGICAL       FileUsed(MaxFiles)
      CHARACTER*12  MasterFileName
      INTEGER       DefaultUnit(MaxFiles)
      INTEGER       UserUnit(MaxFiles)
      INTEGER       MasterUnit
      LOGICAL       MasterInit

      COMMON /MasterFiles/  DefaultName, UserName,
     &     DefaultUnit, UserUnit,
     &     MasterFileName, MasterUnit,
     &     MasterInit, FileUsed

      SAVE /MasterFiles/

*==== EOF master.inc ==========================================================
