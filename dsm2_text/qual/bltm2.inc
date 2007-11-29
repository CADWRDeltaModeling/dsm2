C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.

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

      CHARACTER*4 LABEL
      COMMON/CHARS/LABEL(11)

      INTEGER IOUT,JCD,JNCD,JNCU,JGO,JPO,
     &     NHR,ITDDS,NBC
      REAL*8    FLOW,PX

      REAL*8 B,X
      INTEGER NUMUP,NUMDOWN,LISTUP,LISTDOWN
      INTEGER MAXPARCEL

      COMMON/BLK1_R/FLOW(NOBR,4,NOSC),PX(NOPR,NOBR),
     &     B(NOBR),X(NOBR,NOSC)
	  COMMON/BLK1_I/MAXPARCEL(NOBR),
     &     NUMUP(NOBR),NUMDOWN(NOBR),LISTUP(NOBR,5),LISTDOWN(NOBR,5),
     &     NHR,ITDDS,NBC,JGO,JPO,
     &     IOUT(NOBR,NOSC)
     &     ,JNCD(NOBR),JNCU(NOBR),JCD(MAX_NODES)

      CHARACTER*130 FILENAME
      CHARACTER*80 PATH
      INTEGER MAXNUMFLOWFILES
      PARAMETER(MAXNUMFLOWFILES=40)
      COMMON/FLOWFILES/FILENAME(MAXNUMFLOWFILES),PATH(MAXNUMFLOWFILES)

c-----For mass tracking
      !INTEGER MAXHOURS
      !PARAMETER (MAXHOURS=201)
      REAL * 8    qnode ! , QRIM,CRIM, !,qag,cag !,QCHSEEP,QCHDIV
      !INTEGER NODESEQ2QUAL
      logical upnode_flag_of_chan,downnode_flag_of_chan
 
      COMMON/FLOWS/qnode(max_nodes) !,QRIM(MAX_NODES),CRIM(MAX_NODES,MAX_CONSTITUENT)
     &     !,QAG(MAX_NODES),CAG(MAX_NODES, MAX_CONSTITUENT)
     &     !,QCHSEEP(MAX_NODES),QCHDIV(MAX_NODES)
     &     !,NODESEQ2QUAL(MAX_NODES)
     &     ,upnode_flag_of_chan(max_nodes) 
     &     ,downnode_flag_of_chan(max_nodes) 
