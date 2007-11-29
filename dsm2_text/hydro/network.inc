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


*===== BOF network.inc ==================================================
*   Version 93.01, January, 1993

      INTEGER MaxChannels
      PARAMETER( MaxChannels = 800 ) ! fixme: should be same as max_channels
      INTEGER MaxNres,MaxResConnectChannel,MaxNgate
      PARAMETER (MaxNres = 25)  ! fixme: should be same as max_reservoirs
      PARAMETER (MaxResConnectChannel = 10)
      PARAMETER (MaxNGate = 30)

      INTEGER NumCh, Branch
      INTEGER  MaxLocations
      PARAMETER ( MaxLocations = 3500)
      INTEGER MaxQuadPts
      PARAMETER ( MaxQuadPts = 3 )
      INTEGER MaxConnectingChannels
      PARAMETER ( MaxConnectingChannels = 5 )
      COMMON  /ChannelNetworkData/ NumCh, Branch
      SAVE / ChannelNetworkData /

*   Definitions:
*     MaxChannels - maximum number of channels.
*     NumCh - current number of channels.
*     Branch - current selected or active channel.
*     MaxLocations - maximum number of computational or user locations.
*     MaxQuadPts - maximum number of quadrature points allowed
*                  for numerical spatial integration between
*                  adjacent computational cross sections.
*     MaxConnectingChannels - maximum number of channels that may be
*                             connected at a junction of channels.

*===== EOF network.inc =================================================
