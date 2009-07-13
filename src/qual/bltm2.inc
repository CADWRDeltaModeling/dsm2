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
      REAL*8    qnode ! , QRIM,CRIM, !,qag,cag !,QCHSEEP,QCHDIV
      !INTEGER NODESEQ2QUAL
      logical upnode_flag_of_chan,downnode_flag_of_chan
 
      COMMON/FLOWS/qnode(max_nodes) !,QRIM(MAX_NODES),CRIM(MAX_NODES,MAX_CONSTITUENT)
     &     !,QAG(MAX_NODES),CAG(MAX_NODES, MAX_CONSTITUENT)
     &     !,QCHSEEP(MAX_NODES),QCHDIV(MAX_NODES)
     &     !,NODESEQ2QUAL(MAX_NODES)
     &     ,upnode_flag_of_chan(max_nodes) 
     &     ,downnode_flag_of_chan(max_nodes) 
