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

*===== BOF chconnec.inc ================================================
*   Version 93.01, January, 1993

*   Note: This include, when used, must appear after "Network.inc".
      REAL*8    Yres
      REAL*8    QRes
      REAL*8    Ares
      REAL*8    Dres
      REAL*8    ReservoirCoeff
      REAL*8    YResOld
      REAL*8    QresOld
      INTEGER ResConnectingChannels
	      COMMON /IntReservoirProperties/
     &     ResConnectingChannels(MaxNres,MaxResConnectChannel)
       SAVE   /IntReservoirProperties/

	   COMMON /RealReservoirProperties/
     &     Ares(MaxNres),
     &     Yres(MaxNres),
     &     Dres(MaxNres),
     &     ReservoirCoeff(MaxNres,MaxResConnectChannel,2),
     &     QRes(MaxNres,MaxResConnectChannel),     
     &     YResOld(MaxNres),
     &     QResOld(MaxNres,MaxResConnectChannel)
		SAVE /RealReservoirProperties/
 


      INTEGER UpBoundaryCode(MaxChannels)
      INTEGER UpNumberOfConnections(MaxChannels)
      INTEGER UpConnection(MaxChannels*MaxConnectingChannels)
      INTEGER DownBoundaryCode(MaxChannels)
      INTEGER DownNumberOfConnections(MaxChannels)
      INTEGER DownConnection(MaxChannels*MaxConnectingChannels)
      INTEGER nodeSumQChan(MaxNodes)
      REAL*8   dX(MaxChannels)

      COMMON /ChannelConnections_I/
     &     UpBoundaryCode, UpNumberOfConnections,
     &     UpConnection,
     &     DownBoundaryCode, DownNumberOfConnections,
     &     DownConnection,
     &     nodeSumQChan

	        COMMON /ChannelConnections_R/ dX
      SAVE / ChannelConnections_I /, /ChannelConnections_R/


*===== EOF chconnec.inc ================================================
