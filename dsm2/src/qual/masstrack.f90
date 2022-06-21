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

      SUBROUTINE MASSTRACK (COMMAND)

!-----This subroutine performs a set of calculation to be used
!-----as a diagnostic test or mass tracking
!-----The tasks completed here is based an COMMAND
      use io_units
      use logging
      use grid_data
      use constants

      IMPLICIT NONE
      INTEGER COMMAND

!-----COMMAND=1   Calculate the total (constituent) mass stored
!-----------------in all the channels and reservoirs.

!-----=2   Calculate the amount of constituent entering and leaving

      INCLUDE 'param.inc'

      INCLUDE 'bltm1.inc'
      INCLUDE 'bltm3.inc'
      INCLUDE 'bltm2.inc'

!-----LOCAL VARIABLES
      INTEGER  I,K, CONS_NO &
          ,intnode &             ! int node number
          ,branch              ! qual branch (channel) number

      real*8 objflow, massrate(max_constituent) ! flow and massrate at object

      integer j,  export_num
      real*8 mass_entry

      integer res_num_clfct

      integer nbanode, loop_channel

      IF(COMMAND.NE.1 .and. COMMAND.NE.2 )THEN
         PRINT*,' ERROR... WRONG VALUE FOR COMMAND SENT TO'
         PRINT*,'          SUBROUTINE MASSTRACK'
         print*,'          IT COULD HAVE ONLY TWO VALUES: 1 or 2'
         print*,'          PLEASE TRY AGAIN'
         Return
      ENDIF

      IF(COMMAND.EQ.1)THEN     ! mass in storage
         DO CONS_NO=1,NEQ
            TOTCONSTIT(CONS_NO) = 0.0
            TOTRESCONSTIT(CONS_NO) = 0.0
            TOTCHCONSTIT(CONS_NO) = 0.0
            do i=1,num_masstrack_regions
               totmass_in_region(i,CONS_NO) = 0.
            enddo

            DO I=1,num_masstrack_regions
               do j=1,num_masstrack_res_in_region(i)
                  k=list_masstrack_res_in_region(i,j)
                  TOTRESCONSTIT(CONS_NO) = TOTRESCONSTIT(CONS_NO)  &
                   + RESVOL(k) * CRES(k,CONS_NO)
               enddo
            ENDDO

            ! Mass tracking by region
            do i=1,num_masstrack_regions ! start loop for mass tracking
               ! Amount of mass in channels in each region
               do j = 1,num_masstrack_chan_in_region(i) ! start loop for channels in region
                  n = list_masstrack_chan_in_region(i,j)
                  do k = 1, ns(n) ! start loop for tot channels in reg.
                     totmass_in_region(i,CONS_NO) =  &
                         totmass_in_region(i,CONS_NO)  &
                         +  GPV(N,K) * GPT(CONS_NO,K,N) ! parcel vol * conc of const
                  enddo         ! end of loop, no. of channels within the regional
               enddo            ! end of loop, no. of mass track channel in the region

               ! Amount of mass in reservoirs in each region
               do j = 1,num_masstrack_res_in_region(i) ! loop for no of reservoirs
                  n = list_masstrack_res_in_region(i,j)
                  totmass_in_region(i,CONS_NO) = totmass_in_region(i,CONS_NO)  &
                      + resvol(n) * cres(n,CONS_NO)
               enddo            ! end of loop for the reservoirs in the region
            enddo               ! end of loop for the mass track region

!-----------Modified by ganesh
            DO loop_channel = 1,num_masstrack_chan_in_region(num_masstrack_regions)    ! hard coded
               N=list_masstrack_chan_in_region(num_masstrack_regions,loop_channel)     ! generalize!!!
               DO K=1,NS(N)                                            ! parcels in the Channel
                  TOTCHCONSTIT(CONS_NO) = TOTCHCONSTIT(CONS_NO)  &
                      + GPV(N,K) * GPT(CONS_NO,K,N)
               ENDDO
            ENDDO
 9011       format('totoldmass=',1pe15.7)

            TOTCONSTIT(CONS_NO) = TOTCHCONSTIT(CONS_NO)+TOTRESCONSTIT(CONS_NO)

            IF(PRINT_LEVEL.EQ.42)THEN
               WRITE(*,901) CONS_NO, TOTCHCONSTIT(CONS_NO), TOTRESCONSTIT(CONS_NO), &
                   TOTCONSTIT(CONS_NO)
 901           FORMAT(' CONSTIT.:', I2,'  CH:', 1PE12.5, ' RES=', 1PE12.5, &
                   ' TOT=', 1PE12.5)
            ENDIF
         ENDDO                  ! end of loop for NEQ, total constituents modeled
      ENDIF           ! end of If for COMMAND value 1

      IF(COMMAND.EQ.2)THEN   ! Compu. the amount of const. entering and leaving the system
         do CONS_NO=1,neq
            IF(Jtime.EQ.1)THEN                                ! Initialize
               TOTCONSTITENTER(CONS_NO)=0.
               TOTCONSTITBND(CONS_NO)=0.
               TOTCONSTITCHDEP(CONS_NO)=0.

               do intnode=1,nnodes
                  export_num=node_export_id(intnode)
               enddo
            ENDIF
         enddo

!--------Clifton Court Hard-coded
         if(res_num_clfct.eq.0)then
            do i=1,nreser
               if(res_geom(i).name .eq.'clfct')then     ! hard coded
                  res_num_clfct=i
               endif
            enddo
            if(res_num_clfct.eq.0)then
               write(unit_error,*)' Error... could not find info for Clifton Court'
               call exit(2)
            endif
         endif

!--------nodes
         do intnode=1,nnodes
               if( upnode_flag_of_chan(intnode)  &
                   .or. downnode_flag_of_chan(intnode) ) then ! flag for the node is on
                  call node_rate(intnode,TO_OBJ,QEXT_FLOWS,objflow,massrate)
                  do CONS_NO=1,neq
                     totconstitenter(CONS_NO) = totconstitenter(CONS_NO)  &
                         + massrate(CONS_NO)*dtt
                  enddo
                  call node_rate(intnode,FROM_OBJ,QEXT_FLOWS,objflow,massrate)
                  do CONS_NO=1,neq
                     export_num=node_export_id(intnode)
                     if(export_num.eq.3) then
                        nbanode=intnode
                     endif

                     if(export_num.gt.0) then ! This node is labeled as export
                        mass_export(export_num,CONS_NO) =  &
                            mass_export(export_num,CONS_NO)-massrate(CONS_NO)*dtt
                        totconstitexport(CONS_NO) = totconstitexport(CONS_NO)  &
                            - massrate(CONS_NO)*dtt
                     else
                        TOTCONSTITCHDEP(CONS_NO)=TOTCONSTITCHDEP(CONS_NO)  &
                            - massrate(CONS_NO)*dtt
                     endif
                  enddo
                                ! Stage condition (boundary) node (e.g. Martinez)
                  do i=1,nstgbnd
                     if (intnode .eq. stgbnd(i).node) then
                        branch=listdown(intnode,1)
                        if (flow(branch,1,nxsec(branch)) .le. 0.) then
                           do CONS_NO=1,neq
                              totconstitbnd(CONS_NO) = totconstitbnd(CONS_NO) + &
                                  flow(branch,1,nxsec(branch)) * dtt * &
                                  gtrib(CONS_NO,nxsec(branch),branch)
                           enddo
                        else
                                ! Constituents leaving the delta toward the ocean
                           do CONS_NO=1,neq
                              totconstitbnd(CONS_NO) = totconstitbnd(CONS_NO) - gptd(CONS_NO,branch)
                           enddo
                        endif
                     endif
                  enddo
               endif
         enddo      ! end of loop for constituents

         do CONS_NO=1,NEQ
!-----------Mass tracking for boundary lines
            do i=1,num_masstrack_boundary_lines
               do j=1,num_chan_in_masstrack_boundary_line(i)
                  n=list_ch_in_masstrack_boundary_line(i,j)
                  if(masstrack_chan_boundary_upstream(i,j)) then
!--------------------boundary line at the upstream end of the channel
                     if(flow(n,1,1).ge.0.)then
!-----------------------flow in the positive direction
                        if(node_geom(jncu(n)).qual_int)then
!--------------------------internal node. Use junction mixed value
                           mass_entry = flow(n,1,1) * dtt * cj(CONS_NO,jncu(n))
                        else
!--------------------------external node. Use first parcel info
                           mass_entry = gpv(n,1) * gpt(CONS_NO,1,n)
                        endif
                     else
!-----------------------flow in the negative direction
                        mass_entry=-gptu(CONS_NO,n)
                     endif
                  else
!--------------------boundary at the downstream end of the channel
                     if(flow(n,1,nxsec(n)).ge.0.)then
!-----------------------flow in the positive direction
                        mass_entry=gptd(CONS_NO,n)
                     else
!-----------------------flow in the negative direction
                        if(node_geom(jncd(n)).qual_int)then
!--------------------------internal node. Use junction mixed value
                           mass_entry=flow(n,1,nxsec(n)) * dtt * cj(CONS_NO,jncd(n))
                        else
!--------------------------external node. Use first parcel info
                           mass_entry=-gpv(n,ns(n)) * gpt(CONS_NO,ns(n),n)
                        endif
                     endif
                  endif
                  if(masstrack_chan_boundary_positive_direction(i,j)) then
!--------------------user has specified the positive direction for
!--------------------mass-track boundary line for this channel to be
!--------------------the same as what the grid suggests.
!--------------------No need to change the sign
                  else
!--------------------need to change the sign. See above
                     mass_entry=-mass_entry
                  endif
                  mass_passed_through_boundary_ch(i,j,CONS_NO)= &
                      mass_passed_through_boundary_ch(i,j,CONS_NO) + mass_entry
                  totmass_passed_through_boundary_line(i,CONS_NO)=  &
                      totmass_passed_through_boundary_line(i,CONS_NO) + mass_entry
               enddo
            enddo
         enddo
      endif
      RETURN
      END

!-------------------------------------------------open_masstrack_output_files
      subroutine open_masstrack_output_files(CONS_NO)
      Use IO_Units
      use logging
      use grid_data
      use common_qual
      implicit none
      INCLUDE 'param.inc'

      INCLUDE 'bltm1.inc'

                                ! local variables
      integer i,j, CONS_NO, iun
      character * 80 fle

!-----The main mass tracking output for the Delta
!-----Names are hard-coded
      if(CONS_NO.le.9)then
         write(fle,964) CONS_NO
 964     format('pct_mass_',i1,'.out')
      else
         write(fle,966) CONS_NO
 966     format('pct_mass_',i2,'.out')
      endif
      iun=150+CONS_NO
      open(file=fle,unit=iun,status='unknown')
!@@@      write(iun,967) trim(constit_format(constituents(CONS_NO)))
 967  format('Constituent:',a)
      write(iun,968) (export_labels(i),i=1,num_export_nodes)
      if(print_level.ge.2)then
         write(unit_screen,968) (export_labels(i),i=1,num_export_nodes)
      endif
!-----Mass-tracking output header (tidal day hard-coded)
 968  format(/ &
          'Tday    Delta      Islands      SWP         Chipps   '1x,3(2x,a8),'    Decay    ', &
          '      Total')
!-----initialize masstrack through boundary lines
      do i=1,num_masstrack_boundary_lines
         iun=100+i
         if(i.le.9)then
            write(fle,961) i
 961        format('boundary_line',i1,'.out')
         else
            write(fle,962) i
 962        format('boundary_line',i2,'.out')
         endif
         open(file=fle,unit=iun,status='unknown')
         write(iun,963) i, (int2ext(list_ch_in_masstrack_boundary_line(i,j)), &
             j = 1,num_chan_in_masstrack_boundary_line(i))
 963     format(10x,'Mass-tracking through boundary line #',i3/ &
             5x,10(6x,i4), '  Total ')
         totmass_passed_through_boundary_line(i,CONS_NO)=0.
         do j=1,num_chan_in_masstrack_boundary_line(i)
            mass_passed_through_boundary_ch(i,j,CONS_NO)=0.
         enddo
      enddo

      return
      end

!-----reservoir_salinity_update_formtk
      subroutine reservoir_salinity_update_formtk(res_num_clfct)
      use IO_Units
      use grid_data
      use logging
      use runtime_data
      implicit none

      INCLUDE 'param.inc'

      INCLUDE 'bltm1.inc'
      INCLUDE 'bltm3.inc'
      INCLUDE 'bltm2.inc'

      integer res_num_clfct

!-----local variables
      integer i,j, CONS_NO,k
      real*8 masspump

!-----Clifton Court Hard-coded
      if(res_num_clfct.eq.0)then
         do i=1,nreser
            if(res_geom(i).name .eq.'clfct')then
               res_num_clfct=i
            endif
         enddo
         if(res_num_clfct.eq.0)then
            write(unit_error,918)
 918        format(' Error... could not find info for Clifton Court')
            call exit(2)
         endif
      endif

      do CONS_NO=1,neq
         do j=1,res_geom(res_num_clfct).nnodes
            k=lresjunc(res_num_clfct,j) ! connecting nodes to Clifton Court
            if(qres(res_num_clfct,j).le.0.)then
!--------------Flow into the forebay
               masspump=-qres(res_num_clfct,j)*dtt*cj(CONS_NO,k)
               TOTCONSTITPUMP(CONS_NO)=TOTCONSTITPUMP(CONS_NO)+MASSPUMP
            elseif(qres(res_num_clfct,j).le.1.e-1)then
!--------------roundoff error. Not a big deal
            else
               write(unit_error,914)qres(res_num_clfct,j)
 914           format('Error ...Flow out of the forebay is not allowed',/ &
                   'flow=',1pe12.5)
               call exit(2)
            endif
         enddo
      enddo

      return
      end

!-----print_results_for_masstracking
      subroutine print_results_for_masstracking
      Use IO_Units
      use logging
      use runtime_data
      use grid_data
      implicit none
      INCLUDE 'param.inc'

      INCLUDE 'bltm1.inc'
      INCLUDE 'bltm3.inc'
      INCLUDE 'bltm2.inc'

                                ! local variables
      integer i,j, CONS_NO, iun

      real*8 net_mass_enter, sum_pct_mass, pct_mass_export
      real*8 pct_mass_delta, pct_mass_chdep,pct_mass_pump
      real*8 pct_mass_bnd,pct_decay,peror,t1
      real*8 pct_mass(max_num_ch_in_boundary_line)
      real*8 pct_mass_export_node(max_export_nodes)

      integer iprnt_mass
      integer iwarn(max_constituent),itday,export_num
      common /mass_tracking_1/ iprnt_mass

      CALL MASSTRACK(1)
      CALL MASSTRACK(2)

      DO CONS_NO=1,NEQ
         NET_MASS_ENTER = TOTOLDCONSTIT(CONS_NO) + TOTCONSTITENTER(CONS_NO)
         IF(NET_MASS_ENTER.EQ.0.)THEN

            IF(IWARN(CONS_NO).EQ.0) WRITE(UNIT_ERROR,1210)
!@@@     &           constit_format(constituents(CONS_NO))
 1210       format(/'---------------------------------------------------------------'/ &
                'Warning... Mass-tracking is not possible for '/ &
                '           Constituent:',a,/ &
                '           No incoming mass is detected'/ &
                '---------------------------------------------------------------'/)
            NET_MASS_ENTER=1.E-3 ! Without this there will be division by zero
            IWARN(CONS_NO)=1
         ENDIF
         PCT_MASS_EXPORT = TOTCONSTITEXPORT(CONS_NO) / NET_MASS_ENTER*100.
         PCT_DECAY = AMOUNTDECAYED(CONS_NO) / NET_MASS_ENTER * 100.
         if(pct_decay .lt. 1.0e-05) then
            pct_decay=0.        ! hard coded, but could be a small number
         endif

!--------This is hard-coded...
!--------Delta is defined using the last region, which includes
!--------all the channels and open water areas east of Chipps island,
!--------except Clifton Court Forebay itself.

         PCT_MASS_DELTA = totmass_in_region(num_masstrack_regions,CONS_NO) / NET_MASS_ENTER*100.

         do export_num = 1,NUM_EXPORT_NODES
            pct_mass_export_node(export_num) = mass_export(export_num,CONS_NO)/NET_MASS_ENTER*100.
         enddo
         PCT_MASS_CHDEP = TOTCONSTITCHDEP(CONS_NO) / NET_MASS_ENTER*100.
         PCT_MASS_PUMP=TOTCONSTITPUMP(CONS_NO) / NET_MASS_ENTER*100.

!--------This is hard-coded
!--------Chipps Island is defined by Boundary Line#1
         PCT_MASS_BND=totmass_passed_through_boundary_line(1,CONS_NO) / NET_MASS_ENTER*100.

         t1 = PCT_MASS_DELTA + PCT_MASS_CHDEP+PCT_MASS_PUMP + PCT_MASS_BND+PCT_MASS_EXPORT+PCT_DECAY

         PEROR=(100.-t1)
         if(print_level.ge.2)then
            itday=int((julmin-start_julmin-time_step)/iprnt_mass)+1
            WRITE(unit_screen,1946)itday,CONS_NO,PCT_MASS_DELTA,PCT_MASS_CHDEP, &
                PCT_MASS_PUMP,PCT_MASS_BND, &
                (PCT_MASS_EXPORT_NODE(export_num),export_num=1,num_export_nodes), &
                PCT_DECAY,t1
         endif
         if(mod((julmin-start_julmin),iprnt_mass).eq.0)then
!-----------print mass_tracking info
            iun=150+CONS_NO     ! This is hard-coded
            itday=(julmin-start_julmin)/iprnt_mass
            WRITE(iun,946)itday,PCT_MASS_DELTA,PCT_MASS_CHDEP, &
                PCT_MASS_PUMP,PCT_MASS_BND, &
                (PCT_MASS_EXPORT_NODE(export_num),export_num=1,num_export_nodes), &
                PCT_DECAY,t1

 946        FORMAT(i4,1x,19(e11.4,1X))
 1946       FORMAT(i4,1x,i2,1x,8(1pe10.3,1x),1pe12.5)
         endif

         sum_pct_mass_in_region(CONS_NO)=0.
         do i=1,num_masstrack_regions
!-----------mass tracking by region
            pct_mass_in_region(i,CONS_NO)=totmass_in_region(i,CONS_NO)/NET_MASS_ENTER*100.
            if(i.ne.num_masstrack_regions) then
               sum_pct_mass_in_region(CONS_NO) = sum_pct_mass_in_region(CONS_NO) &
                   + pct_mass_in_region(i,CONS_NO)
            endif
         enddo
         if(num_masstrack_regions.gt.1)then
            if(print_level.ge.3)then
               itday=int((julmin-start_julmin-time_step)/iprnt_mass)+1
               write(unit_screen,955) itday,(pct_mass_in_region(i,CONS_NO), &
                   i=1,num_masstrack_regions-1),sum_pct_mass_in_region(CONS_NO)
            endif
            if(mod((julmin-start_julmin),iprnt_mass).eq.0)then
               iun=CONS_NO+190
               itday=(julmin-start_julmin)/iprnt_mass
               write(iun,955) itday,(pct_mass_in_region(i,CONS_NO), &
                   i=1,num_masstrack_regions-1),sum_pct_mass_in_region(CONS_NO)
 955           format(i4,14(f7.3,1x))
            endif
         endif
         do i=1,num_masstrack_boundary_lines
            iun=100+i           ! This is hard-coded
            sum_pct_mass=0.0

            do j=1,num_chan_in_masstrack_boundary_line(i)
               pct_mass(j)=mass_passed_through_boundary_ch(i,j,CONS_NO)/NET_MASS_ENTER*100.
               sum_pct_mass=sum_pct_mass+pct_mass(j)
            enddo
            if(mod((julmin-start_julmin),iprnt_mass).eq.0)then
               write(iun,965)itday,(pct_mass(j), &
                   j=1,num_chan_in_masstrack_boundary_line(i)), &
                   sum_pct_mass
 965           format(i4,1x,10f10.4,5x,f10.4)
            endif
         enddo
      ENDDO
      RETURN
      end

!-----read_input_data_for_masstracking
      subroutine read_input_data_for_masstracking
      Use IO_Units
      use common_qual
      use logging
      implicit none
      INCLUDE 'param.inc'

      INCLUDE 'bltm1.inc'
      INCLUDE 'bltm3.inc'
      INCLUDE 'bltm2.inc'

                                ! local variables
      integer i,j, CONS_NO, iun, reg_num
      character * 20 line_in, ch_loc_flag, ch_sign_flag
      character * 80 fle

      integer iprnt_mass
      integer ext2int
      external ext2int
      common /mass_tracking_1/ iprnt_mass

!-----Hard code frquency of the output (Most likely will not change) (25hours)

      iprnt_mass=25*60          ! Frequency of output in minutes

!-----Assign three export locations as default values.
!-----These values will be over-written by input file, if any.
      num_export_nodes = 3      ! output to be reported from 3 pts
      list_export_nodes(1) = 181 ! export node no. 1
      export_labels(1)='CVP'    ! Name of export location.
      list_export_nodes(2) = 206
      export_labels(2) = 'CCC'
      list_export_nodes(3) = 273
      export_labels(3) = 'NBA'

!-----If there are new locations in the input file, they will overwrite
!-----the dafault values set earlier.
      open(file='export_locations',unit=81,status='old',err=685)
      read(81,*) num_export_nodes
      if(num_export_nodes .gt. MAX_EXPORT_NODES) then
         write(unit_error,9010) num_export_nodes, MAX_EXPORT_NODES
 9010    format('Error.... Array dimension exceeded'/ &
             '          num_export_nodes=',i5,/ &
             '          MAX_EXPORT_NODES=',i5)
         call exit(2)
      else
         do i=1,num_export_nodes
            read(81,*) list_export_nodes(i), export_labels(i)
         enddo
      endif
      close(81)                 ! close export location input files
 685  continue

      do i=1,num_export_nodes
!--------Reverse translation (not sure about it, whether it is reverse
!--------translations or not --ganesh).
         node_export_id(list_export_nodes(i))=i
      enddo

!-----Check to see if mass tracking by region is requested
!-----(Basically see if the input file is present)
!-----Here are the assumptions in the input files:
!-----1- Each region has at least one channel
!-----2- Once a new region is declared, no new channels or reservoirs
!-----are introduced for a previously defined region (Any region
!-----defined is a new region.
!-----3- There can be no missing region number
!-----4- channel numbers are read with i3 format, and
!-----reservoir numbers are read with i2 format (**FORMATED INPUT**)

      open(file='region_ch_all.inp',unit=81,status='old',err=990)
 655  read(81,'(a)',end=980) line_in
      if(line_in(1:3).eq.'Reg' .or. line_in(1:3).eq.'reg')then
!--------A new region is defined
         read(line_in(8:11),*) reg_num
         num_masstrack_regions=num_masstrack_regions+1
         if(num_masstrack_regions.gt.max_masstrack_regions)then
            write(*,9011) num_masstrack_regions, MAX_MASSTRACK_REGIONS
 9011       format('Error.... Array dimension exceeded'/ &
                '          num_masstrack_regions=',i5,/ &
                '          MAX_MASSTRACK_REGIONS=',i5)
            stop 'Array size exceeded. Increase max_masstrack_regions'
         endif
      else
!--------channel number specified
         read(line_in,'(i3)') n ! external number of the specified channel
         if(n.gt.0) then
            num_masstrack_chan_in_region(reg_num) = num_masstrack_chan_in_region(reg_num)+1
            if(num_masstrack_chan_in_region(reg_num).gt. &
                max_chan_in_masstrack_region)then
               stop 'Array size exceeded. Increase max_chan_in_masstrack_region'
            endif
            list_masstrack_chan_in_region(reg_num, &
                num_masstrack_chan_in_region(reg_num))=ext2int(n) ! convert external to internal
         endif
      endif
      goto 655
 980  close(81)

      open(file='region_res',unit=81,status='old',err=990)
 660  read(81,'(a)',end=990) line_in
      if(line_in(1:3).eq.'Reg' .or. line_in(1:3).eq.'reg')then
!--------A new region is defined
         read(line_in(8:11),*) reg_num
         if(num_masstrack_regions.gt.MAX_masstrack_regions)then
            stop 'Array size exceeded. Increase max_masstrack_regions'
         endif
      else
!--------reservoir number specified
         read(line_in,'(i2)') n ! number of specified reservoir
         num_masstrack_res_in_region(reg_num)=num_masstrack_res_in_region(reg_num) + 1
         if(num_masstrack_res_in_region(reg_num).gt. &
             max_res_in_masstrack_region)then
            stop 'Array size exceeded. Increase max_res_in_masstrack_region'
         endif
         list_masstrack_res_in_region(reg_num, &
             num_masstrack_res_in_region(reg_num))=n
      endif
      goto 660
 990  close(81)

      do CONS_NO=1,neq
!--------mass-tracking by region output-filenames hard-coded
         iun=190+CONS_NO
         if(CONS_NO.le.9)then
            write(fle,974) CONS_NO
 974        format('pct_mass_region_const_',i1,'.out')
         else
            write(fle,977) CONS_NO
 977        format('pct_mass_region_const_',i2,'.out')
         endif
         open(file=fle,unit=iun,status='unknown') ! open output file

         write(iun,978)(i,i=1,num_masstrack_regions)
!--------header hard-coded with tidal day and 11 regions
 978     format('Tday',11(1x,'Reg:',i2,1x),' Total')
      enddo

      if(print_level.gt.3)then
         write(*,991) num_masstrack_regions
 991     format(i5,' Mass tracking regions defined')

         do i=1,num_masstrack_regions
            write(*,992) i,num_masstrack_chan_in_region(i)
 992        format(5x,'Region ',i4,' :'/10x,i4,' channels')
            do j=1,num_masstrack_chan_in_region(i)
               write(*,993) list_masstrack_chan_in_region(i,j)
 993           format(15x,' Chan:',i5)
            enddo
            write(*,994) num_masstrack_res_in_region(i)
 994        format(10x,i4,' Reservoirs')
            do j=1,num_masstrack_res_in_region(i)
               write(*,995) list_masstrack_res_in_region(i,j)
 995           format(15x,' Reservoir:',i5)
            enddo
         enddo
      endif

!-----See if any Boundary lines defined
      num_masstrack_boundary_lines=0
!-----masstrack input file defining the boundary lines(filename hard-coded)
!-----minimum of 1 boundary line is needed to define Chipps Island
!-----(Chipps Island should be  defined as the first boundary line (hard-coded)
      open(file='masstrack_boundary_lines',unit=81,status='old',err=905)
      read(81,*) num_masstrack_boundary_lines

      if(num_masstrack_boundary_lines.gt.max_num_boundary_lines)then
         write(unit_error,925) num_masstrack_boundary_lines, &
             max_num_boundary_lines
 925     format('ERROR... Array dimension exceeded'/ &
             '         number of boundary lines specified=',i5/ &
             '         max_num_boundary_lines=',i5)
         call exit(1)
      endif
      do i=1,num_masstrack_boundary_lines ! nos of mass track bdr lines
         read(81,*) num_chan_in_masstrack_boundary_line(i)
         if(num_chan_in_masstrack_boundary_line(i).gt. &
             max_num_ch_in_boundary_line)then
            write(unit_error,926)i,num_chan_in_masstrack_boundary_line(i), &
                max_num_ch_in_boundary_line
 926        format('ERROR... Array dimension exceeded'/ &
                '         For boundary line #',i5, &
                '         Number of channels specified=',i5/ &
                '         max_num_ch_in_boundary_line=',i5)
            call exit(1)
         endif
         do j=1,num_chan_in_masstrack_boundary_line(i)
            read(81,*) list_ch_in_masstrack_boundary_line(i,j), &
                ch_loc_flag,ch_sign_flag

            list_ch_in_masstrack_boundary_line(i,j)= &
                ext2int(list_ch_in_masstrack_boundary_line(i,j))
            if(ch_loc_flag.eq.'upstream')then
               masstrack_chan_boundary_upstream(i,j)=.true.
            elseif(ch_loc_flag.eq.'downstream')then
               masstrack_chan_boundary_upstream(i,j)=.false.
            else
               write(unit_error,981) ch_loc_flag
 981           format(' file=masstrack_boundary_lines'/ &
                   ' invalid ch_loc_flag=',a/ &
                   ' should be  upstream or downstream')
               call exit(2)
            endif
            if(ch_sign_flag.eq.'positive')then
               masstrack_chan_boundary_positive_direction(i,j)=.true.
            elseif(ch_sign_flag.eq.'negative')then
               masstrack_chan_boundary_positive_direction(i,j)=.false.
            else
               write(unit_error,982) ch_sign_flag
 982           format(' file=masstrack_boundary_lines'/ &
                   ' invalid ch_sign_flag=',a/ &
                   ' should be  positive or negative')
               call exit(2)
            endif
         enddo
      enddo
      close(81)
 905  continue

      return
      end

!-----read_nodeflags_for_masstracking_region
      subroutine read_nodeflags_for_masstracking_region
!-----This subroutine records the flags for the nodes contained within the
!-----mass tracking region.
!-----The flags are set true if they are located on either end of the channel.
      use grid_data
      implicit none

      INCLUDE 'param.inc'


      INCLUDE 'bltm1.inc'
      INCLUDE 'bltm3.inc'
      INCLUDE 'bltm2.inc'

      integer ichan, inode, ch_no, upnode_of_chan,downnode_of_chan

      do inode=1,max_nodes
         upnode_flag_of_chan(inode)=.false.
         downnode_flag_of_chan(inode)=.false.
      enddo

      do ichan=1,num_masstrack_chan_in_region(num_masstrack_regions)
         ch_no=list_masstrack_chan_in_region(num_masstrack_regions,ichan)
         upnode_of_chan=chan_geom(ch_no).upnode
         downnode_of_chan=chan_geom(ch_no).downnode
         upnode_flag_of_chan(upnode_of_chan)=.true.
         downnode_flag_of_chan(downnode_of_chan)=.true.
      enddo

      return
      end

!-----update_resvol_for_masstracking_region
      subroutine update_resvol_for_masstracking_region
      Use IO_Units
!-----This subroutine records the flags for the nodes contained within the
!-----mass tracking region.
!-----The flags are set true if they are located on either end of the channel.
      use common_tide
      implicit none

      INCLUDE 'param.inc'

      INCLUDE 'bltm1.inc'
      INCLUDE 'bltm3.inc'
      INCLUDE 'bltm2.inc'

      real*8 objflow,massrate(max_constituent) ! flow and massrate at object
      real*8 ERZ
      integer i, cons_no

                                ! put this portion into a subroutine too
      DO I=1,NRESER
         call res_rate(i,TO_OBJ,0,objflow,massrate)
         reschgvol(i)=OBJFLOW*DTT
         DO CONS_NO=1,NEQ
            reschgmass(i,CONS_NO)=MASSRATE(CONS_NO)*DTT
         ENDDO

         call res_rate(i,FROM_OBJ,0,objflow,massrate)
         reschgvol(i)=reschgvol(i)+OBJFLOW*DTT
         DO CONS_NO=1,NEQ
            reschgmass(i,CONS_NO)=reschgmass(i,CONS_NO)+MASSRATE(CONS_NO)*DTT
         ENDDO

         DO CONS_NO=1,NEQ
            CRES(I,CONS_NO) = (RESVOL(I) * CRES(I,CONS_NO) +  &
                reschgmass(i,CONS_NO)) / (RESVOL(I) + reschgvol(i))
         ENDDO

         IF(RESVOL(I) .LT. 1.0D-03)THEN
            ERZ=RESVOL(I)/ARES(I)
            WRITE(UNIT_ERROR,922)res_geom(i).name,ERZ
 922        FORMAT(' ERROR ... RESERVOIR: ',a,' HAS NEGATIVE', &
                ' DEPTH =', 1PE12.5)
            call exit(2)
         ENDIF
      ENDDO

      return
      end

      subroutine determine_mass_in_delta(totconstit_adjust)
      use common_qual
      IMPLICIT NONE

      INCLUDE 'param.inc'
      INCLUDE 'bltm1.inc'
      INCLUDE 'bltm3.inc'
      INCLUDE 'bltm2.inc'
      real*8 totconstit_adjust(MAX_CONSTITUENT)

!-----LOCAL VARIABLES
      INTEGER  I,K, CONS_NO

      integer j

      integer loop_channel

      IF(MASS_TRACKING)THEN
         DO CONS_NO=1,NEQ
            TOTCONSTIT(CONS_NO) = 0.0
            TOTRESCONSTIT(CONS_NO) = 0.0
            TOTCHCONSTIT(CONS_NO) = 0.0
            i=num_masstrack_regions
            totmass_in_region(i,CONS_NO) = 0.

            i=num_masstrack_regions
            do j=1,num_masstrack_res_in_region(i)
               k=list_masstrack_res_in_region(i,j)
               TOTRESCONSTIT(CONS_NO) = TOTRESCONSTIT(CONS_NO)  &
                   + RESVOL(k) * CRES(k,CONS_NO)
            enddo

            i=num_masstrack_regions
            DO loop_channel = 1,num_masstrack_chan_in_region(i)
               N=list_masstrack_chan_in_region(i,loop_channel)
               DO K=1,NS(N)
                  TOTCHCONSTIT(CONS_NO) = TOTCHCONSTIT(CONS_NO)  &
                      + GPV(N,K) * GPT(CONS_NO,K,N)
               ENDDO
            ENDDO

            TOTCONSTIT(CONS_NO) = TOTCHCONSTIT(CONS_NO)+TOTRESCONSTIT(CONS_NO)

         ENDDO                  ! end of loop for NEQ, total constituents modeled

         do cons_no=1,NEQ
            totconstit_adjust(cons_no)=totconstit(cons_no)
         enddo
      else
                                ! no need to update mass in Channels and Reservoirs
         return
      endif

      return
      end
