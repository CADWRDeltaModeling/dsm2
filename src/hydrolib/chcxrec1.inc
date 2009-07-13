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

*===== BOF chcxrec1.inc ================================================
*   Version 93.01, January, 1993

*   Note: If used, this include must appear after "Network.inc".

      REAL*8 X1(MaxChannels),Width1(MaxChannels),Btm1(MaxChannels)
      REAL*8 X2(MaxChannels),Width2(MaxChannels),Btm2(MaxChannels)
      REAL*8 Eta(MaxChannels)
      CHARACTER*16 UpStreamID(MaxChannels), DownStreamID(MaxChannels)
      COMMON  /ChannelID/ UpStreamID, DownStreamID
      COMMON  /RectangularChannelProperties/
     &     X1,Width1,Btm1,X2,Width2,Btm2,Eta
      SAVE /RectangularChannelProperties/, /ChannelID/

*   Definitions:
*       MaxChannels - maximum number of channels.
*       X1(i)     - downstream reference distance,
*                   at upstream extent of channel "i".
*       Width1(i) - channel width at X(i).
*       Btm1(i)   - channel-bottom elevation at X(i).
*       X2(i)    - downstream reference distance,
*                   at downstream extent of channel "i".
*       Width2(i) - channel width at X(i).
*       Btm2(i)   - channel-bottom elevation at X(i).
*       Eta(i)    - effective Manning's n for channel "i".
*       UpStreamID(i) - character string ID of upstream cross section,
*                       channel i.
*       DownStreamID(i) - character string ID of downstream cross section,
*                         channel i.
*       i         - channel sequence number.

*===== EOF chcxrec1.inc ================================================
