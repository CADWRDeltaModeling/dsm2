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

      INTEGER NRESJUNC, LRESJUNC, NCONRES, LCONRES, IRESPICK
      real*8 QRES, ERES, RESVOL, ARES, CRES, HRES
      real*8 RESCHGMASS, RESCHGVOL
      real*8 qpumpres, reschgconc

      COMMON/RESERVOIR_R/QRES(MAXNUMRES, MAXRESJUNC), &
          ERES(MAXNUMRES), RESVOL(MAXNUMRES), ARES(MAXNUMRES), &
          HRES(MAXNUMRES), RESCHGMASS(MAXNUMRES, MAX_CONSTITUENT), &
          RESCHGVOL(MAXNUMRES), &
          CRES(MAXNUMRES, MAX_CONSTITUENT)

      COMMON/RESERVOIR_I/NRESJUNC(MAXNUMRES), &
          LRESJUNC(MAXNUMRES, MAXRESJUNC), NCONRES(MAX_NODES), &
          LCONRES(MAX_NODES, MAXRESJUNC, 2), IRESPICK

!-----------------------------------------------------------------------------------
      common/MASS_TRACK1/qpumpres(maxnumres), reschgconc(maxnumres, max_constituent)

      real*8 TOTCONSTIT, TOTOLDCONSTIT, TOTCONSTITENTER, TOTCONSTITBND
      real*8 TOTRESCONSTIT, TOTCHCONSTIT, TOTCONSTITCHDEP
      real*8 AMOUNTDECAYED, TOTCONSTITPUMP, TOTCONSTITEXPORT, MASS_EXPORT
      integer list_export_nodes, num_export_nodes, node_export_id
      integer MAX_EXPORT_NODES
      parameter(max_export_nodes=10)
      character*8 export_labels

      common/MASSTRACKING/ &
          TOTCONSTIT(MAX_CONSTITUENT), &       ! total mass of each constituent
          TOTOLDCONSTIT(MAX_CONSTITUENT), &    ! previous time step's total mass
          TOTRESCONSTIT(MAX_CONSTITUENT), &    ! total reservoir mass
          TOTCHCONSTIT(MAX_CONSTITUENT), &     ! total channel mass
          TOTCONSTITENTER(MAX_CONSTITUENT), &  ! total mass entering (leaving) with external flows
          TOTCONSTITBND(MAX_CONSTITUENT), &    ! total mass entering (leaving) via stage boundary
          AMOUNTDECAYED(MAX_CONSTITUENT), &    ! total mass decayed (non-conservative constituents)
          TOTCONSTITCHDEP(MAX_CONSTITUENT), &
          TOTCONSTITPUMP(MAX_CONSTITUENT), &
          TOTCONSTITEXPORT(MAX_CONSTITUENT), &
          MASS_EXPORT(MAX_EXPORT_NODES, MAX_CONSTITUENT), &
          LIST_EXPORT_NODES(MAX_EXPORT_NODES), NUM_EXPORT_NODES, &
          NODE_EXPORT_ID(MAX_NODES), &
          EXPORT_LABELS(MAX_EXPORT_NODES)

      integer num_masstrack_regions, max_masstrack_regions
      integer max_chan_in_masstrack_region, max_res_in_masstrack_region
      parameter(max_masstrack_regions=20)
      parameter(max_chan_in_masstrack_region=500)
      parameter(max_res_in_masstrack_region=10)

      integer num_masstrack_chan_in_region, &
          num_masstrack_res_in_region, &
          list_masstrack_chan_in_region, &
          list_masstrack_res_in_region

      real*8 totmass_in_region, pct_mass_in_region, sum_pct_mass_in_region

      common/masstrack_regions/ &
          totmass_in_region(max_masstrack_regions, MAX_CONSTITUENT), &
          pct_mass_in_region(max_masstrack_regions, MAX_CONSTITUENT), &
          sum_pct_mass_in_region(max_masstrack_regions), &
          num_masstrack_chan_in_region(max_masstrack_regions), &
          num_masstrack_res_in_region(max_masstrack_regions), &
          list_masstrack_chan_in_region(max_masstrack_regions, max_chan_in_masstrack_region), &
          list_masstrack_res_in_region(max_masstrack_regions, max_res_in_masstrack_region), &
          num_masstrack_regions

      real*8 mass_passed_through_boundary_ch, &
          totmass_passed_through_boundary_line
      integer max_num_boundary_lines, &
          max_num_ch_in_boundary_line
      parameter(max_num_boundary_lines=20)
      parameter(max_num_ch_in_boundary_line=20)
      integer num_masstrack_boundary_lines, &
          num_chan_in_masstrack_boundary_line, &
          list_ch_in_masstrack_boundary_line

      logical masstrack_chan_boundary_upstream, &
          masstrack_chan_boundary_positive_direction

      common/masstrack_boundary_lines/ &
          mass_passed_through_boundary_ch(max_num_boundary_lines, &
                                          max_num_ch_in_boundary_line, MAX_CONSTITUENT), &
          totmass_passed_through_boundary_line(max_num_boundary_lines, &
                                               MAX_CONSTITUENT), &
          num_masstrack_boundary_lines, &
          num_chan_in_masstrack_boundary_line(max_num_boundary_lines), &
          list_ch_in_masstrack_boundary_line(max_num_boundary_lines, &
                                             max_num_ch_in_boundary_line), &
          masstrack_chan_boundary_upstream(max_num_boundary_lines, &
                                           max_num_ch_in_boundary_line), &
          masstrack_chan_boundary_positive_direction(max_num_boundary_lines, &
                                                     max_num_ch_in_boundary_line)

      real*8 mass_correction_factor
      common/flow_correction_factor_for_masstrack/ &
          mass_correction_factor(MAX_CONSTITUENT)
!----------------------------------------------------------------------------

      INTEGER NIPX, NKAI, LR, NS, NXSEC, NBRCH, NEQ, NUMNODE
      INTEGER JTS, JHR, IHR, NJUNC, JUNCFLG
      REAL*8 DQQ, DVD, DVU, GPT, CSTART
      REAL*8 GPTD, GPTU, GPV, GTRIB, GVU, CJ, QI, QT, DQV
      REAL*8 cj_prev           ! node concentration from last timestep or iteration

      COMMON/QUALITY_R/DQQ(NOBR), DVD(NOBR), DVU(NOBR), &
          GPT(MAX_CONSTITUENT, NOPR, NOBR), CSTART(NOBR, MAX_CONSTITUENT), &
          GPTD(MAX_CONSTITUENT, NOBR), &
          GPTU(MAX_CONSTITUENT, NOBR), GPV(NOBR, NOPR), &
          GTRIB(MAX_CONSTITUENT, NOSC, NOBR), GVU(NOBR, NOSC), &
          CJ(MAX_CONSTITUENT, MAX_NODES), cj_prev(max_constituent, max_nodes), &
          QI, QT(NOSC), &
          DQV
      COMMON/QUALITY_I/ &
          NIPX(NOBR, NOPR), NKAI(NOBR, NOSC), LR(MAX_CONSTITUENT), NS(NOBR), &
          NXSEC(NOBR), JUNCFLG(MAX_NODES), &
          NBRCH, NEQ, JTS, JHR, IHR, &
          NJUNC, NUMNODE

      REAL*8 TOPW
      INTEGER BLTMID, RESCHJUNC

      COMMON/TIDE/ &
          TOPW(NOBR, 2), &
          RESCHJUNC(MAXNUMRES, MAXRESJUNC), &
          BLTMID(NOBR)
