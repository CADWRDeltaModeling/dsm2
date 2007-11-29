C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.
C!
C!    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
C!    numerical model.  No protection claimed in original FOURPT and
C!    Branched Lagrangian Transport Model (BLTM) code written by the
C!    United States Geological Survey.  Protection claimed in the
C!    routines and files listed in the accompanying file "Protect.txt".
C!    If you did not receive a copy of this file contact Tara Smith,
C!    below.
C!
C!    This program is licensed to you under the terms of the GNU General
C!    Public License, version 2, as published by the Free Software
C!    Foundation.
C!
C!    You should have received a copy of the GNU General Public License
C!    along with this program; if not, contact Tara Smith, below,
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
C!    Tara Smith
C!    California Dept. of Water Resources
C!    Division of Planning, Delta Modeling Section
C!    1416 Ninth Street
C!    Sacramento, CA  95814
C!    916-653-9885
C!    tara@water.ca.gov
C!
C!    or see our home page: http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/


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
