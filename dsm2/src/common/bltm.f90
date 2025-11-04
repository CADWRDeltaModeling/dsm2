!!<license>
!!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!!    Department of Water Resources.
!!    This file is part of DSM2.

!!    The Delta Simulation Model 2 (DSM2) is free software:
!!    you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License as published by
!!    the Free Software Foundation, either version 3 of the License, or
!!    (at your option) any later version.

!!    DSM2 is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License for more details.

!!    You should have received a copy of the GNU General Public License
!!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!!</license>

module bltm
    use array_limits
    use grid_data, only: MAX_NODES
    use qual_param
    implicit none

    ! Reservoir arrays (previously in COMMON/RESERVOIR_R and COMMON/RESERVOIR_I)
    integer :: NRESJUNC(MAXNUMRES)
    integer :: LRESJUNC(MAXNUMRES, MAXRESJUNC)
    integer :: NCONRES(MAX_NODES)
    integer :: LCONRES(MAX_NODES, MAXRESJUNC, 2)
    integer :: IRESPICK

    real*8 :: QRES(MAXNUMRES, MAXRESJUNC)
    real*8 :: ERES(MAXNUMRES)
    real*8 :: RESVOL(MAXNUMRES)
    real*8 :: ARES(MAXNUMRES)
    real*8 :: HRES(MAXNUMRES)
    real*8 :: RESCHGMASS(MAXNUMRES, MAX_CONSTITUENT)
    real*8 :: RESCHGVOL(MAXNUMRES)
    real*8 :: CRES(MAXNUMRES, MAX_CONSTITUENT)

    ! mass tracking arrays (previously COMMON/MASS_TRACK1)
    real*8 :: qpumpres(MAXNUMRES)
    real*8 :: reschgconc(MAXNUMRES, MAX_CONSTITUENT)

    integer, parameter :: MAX_EXPORT_NODES = 10

    ! Masstracking summary arrays (previously COMMON/MASSTRACKING)
    real*8 :: TOTCONSTIT(MAX_CONSTITUENT)
    real*8 :: TOTOLDCONSTIT(MAX_CONSTITUENT)
    real*8 :: TOTRESCONSTIT(MAX_CONSTITUENT)
    real*8 :: TOTCHCONSTIT(MAX_CONSTITUENT)
    real*8 :: TOTCONSTITENTER(MAX_CONSTITUENT)
    real*8 :: TOTCONSTITBND(MAX_CONSTITUENT)
    real*8 :: AMOUNTDECAYED(MAX_CONSTITUENT)
    real*8 :: TOTCONSTITCHDEP(MAX_CONSTITUENT)
    real*8 :: TOTCONSTITPUMP(MAX_CONSTITUENT)
    real*8 :: TOTCONSTITEXPORT(MAX_CONSTITUENT)
    real*8 :: MASS_EXPORT(MAX_EXPORT_NODES, MAX_CONSTITUENT)
    integer :: LIST_EXPORT_NODES(MAX_EXPORT_NODES)
    integer :: NUM_EXPORT_NODES
    integer :: NODE_EXPORT_ID(MAX_NODES)
    character*8 :: EXPORT_LABELS(MAX_EXPORT_NODES)

    integer :: num_masstrack_regions
    integer, parameter :: max_masstrack_regions = 20
    integer, parameter :: max_chan_in_masstrack_region = 500
    integer, parameter :: max_res_in_masstrack_region = 10

    integer :: num_masstrack_chan_in_region(max_masstrack_regions)
    integer :: num_masstrack_res_in_region(max_masstrack_regions)
    integer :: list_masstrack_chan_in_region(max_masstrack_regions, max_chan_in_masstrack_region)
    integer :: list_masstrack_res_in_region(max_masstrack_regions, max_res_in_masstrack_region)

    real*8 :: totmass_in_region(max_masstrack_regions, MAX_CONSTITUENT)
    real*8 :: pct_mass_in_region(max_masstrack_regions, MAX_CONSTITUENT)
    real*8 :: sum_pct_mass_in_region(max_masstrack_regions)

    integer, parameter :: max_num_boundary_lines = 20
    integer, parameter :: max_num_ch_in_boundary_line = 20
    integer :: num_masstrack_boundary_lines
    integer :: num_chan_in_masstrack_boundary_line(max_num_boundary_lines)
    integer :: list_ch_in_masstrack_boundary_line(max_num_boundary_lines, max_num_ch_in_boundary_line)
    logical :: masstrack_chan_boundary_upstream(max_num_boundary_lines, max_num_ch_in_boundary_line)
    logical :: masstrack_chan_boundary_positive_direction(max_num_boundary_lines, max_num_ch_in_boundary_line)

    real*8 :: mass_passed_through_boundary_ch(max_num_boundary_lines, max_num_ch_in_boundary_line, MAX_CONSTITUENT)
    real*8 :: totmass_passed_through_boundary_line(max_num_boundary_lines, MAX_CONSTITUENT)

    real*8 :: mass_correction_factor(MAX_CONSTITUENT)
!----------------------------------------------------------------------------

    INTEGER NIPX(NOBR, NOPR), NKAI(NOBR, NOSC), LR(MAX_CONSTITUENT), NS(NOBR), NXSEC(NOBR)
    INTEGER NBRCH, NEQ, NUMNODE
    INTEGER JTS, JHR, IHR, NJUNC, JUNCFLG(MAX_NODES)
    REAL * 8 DQQ(NOBR), DVD(NOBR), DVU(NOBR)
    REAL * 8 GPT(MAX_CONSTITUENT, NOPR, NOBR)
    REAL * 8 CSTART(NOBR, MAX_CONSTITUENT)
    REAL * 8 GPTD(MAX_CONSTITUENT, NOBR), GPTU(MAX_CONSTITUENT, NOBR)
    REAL * 8 GPV(NOBR, NOPR)
    REAL * 8 GTRIB(MAX_CONSTITUENT, NOSC, NOBR)
    REAL * 8 GVU(NOBR, NOSC)
    REAL * 8 CJ(MAX_CONSTITUENT, MAX_NODES)
    REAL * 8 cj_prev(max_constituent, max_nodes)  ! node concentration from last timestep or iteration
    REAL * 8 QI, QT(NOSC), DQV

    REAL*8 :: TOPW(NOBR, 2)
    INTEGER :: BLTMID(NOBR)
    INTEGER :: RESCHJUNC(MAXNUMRES, MAXRESJUNC)

    ! bltm3
    integer chan_res          ! pointer whether channel or reservoir for kinetics

    integer jtime, irc, mx, nrev, irev
    real*8 :: dt, DTT, a(nosc), w(nosc), q(nosc), &
              resdepth

    INTEGER MALG, MORGN, MNH3, MNO2, MNO3, MORGP, &
        MPO4, MBOD, MDO, MTEMP, MTDS, MCL &
        , MEC

    REAL * 8 SOLAR, &
        TSOLHR, HA, DRYBLB, WETBLB, &
        CLOUD, WIND, &
        ATMPR

    REAL * 8 STS, STB, STR, DELTSL, EQTIME, RR
    REAL * 8 DECLIN, ELEXP, CS, VPAIR

    INTEGER NL, IDAY

    ! bltm2
    CHARACTER(len=4) LABEL(11)
    real*8 :: FLOW(NOBR, 4, NOSC), PX(NOPR, NOBR), &
              B(NOBR), X(NOBR, NOSC)
    integer :: MAXPARCEL(NOBR), &
               NUMUP(NOBR), NUMDOWN(NOBR), LISTUP(NOBR, 5), LISTDOWN(NOBR, 5), &
               NHR, ITDDS, NBC, JGO, JPO, &
               IOUT(NOBR, NOSC) &
               , JNCD(NOBR), JNCU(NOBR), JCD(MAX_NODES)

    INTEGER MAXNUMFLOWFILES
    PARAMETER(MAXNUMFLOWFILES=40)
    CHARACTER * 130 FILENAME(MAXNUMFLOWFILES)
    CHARACTER * 80 PATH(MAXNUMFLOWFILES)

!-----For mass tracking
    !INTEGER MAXHOURS
    !PARAMETER (MAXHOURS=201)

    integer iprnt_mass

    real*8 :: qnode(max_nodes)
        !,QAG(MAX_NODES),CAG(MAX_NODES, MAX_CONSTITUENT) &
        !,QCHSEEP(MAX_NODES),QCHDIV(MAX_NODES) &
        !,NODESEQ2QUAL(MAX_NODES) &
    logical :: upnode_flag_of_chan(max_nodes) &
        , downnode_flag_of_chan(max_nodes)

    ! kinetic
    integer iskip

    real * 8 depth, vel

    real * 8 scsk(max_constituent), dtsub


end module bltm
