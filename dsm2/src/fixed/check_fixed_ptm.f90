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

subroutine check_fixed_ptm(istat)

!-----Check the PTM fixed input for omissions and errors before starting
!-----the model run.  Supply default values where possible.
      use constants
      use IO_Units
      use type_defs

      use grid_data
      use runtime_data
      use common_tide
      use common_ptm
      use common_qual_bin
      use groups, only: WriteGroupMembers2File
      use network
      use netcntrl_common
      use utilities, only : incr_intvl, diff2dates, cdt2jmin, jmin2cdt
      implicit none

!-----Local variables

      logical &
          nodeexist  &          ! true if node exists in network
          ,filterexist          ! true if filter exists in network

      integer &
          istat  &              ! status of call (returned)
           ,m, n  &             ! indices !TODO
           ,nodeindex  &        ! node index used for iterating through nodes
           ,chanindex &         ! chan index used for iterating through channels
          ,resindex &           ! res index used for iterating through reservoirs
          ,qextindex &          ! qext index used for iterating through source flow
          ,stgbndindex &        ! stgbnd index used for iterating through stage boundary
          ,obj2objindex &       ! obj2obj index used for iterating through obj2obj transfer
          ,ext2intnode

      character &
          tmpdate*14 &          ! temporary date for comparison
          ,tmpstr*32            ! temporary string for name comparison

 605  format(/a,' date incorrect: ',a)

 606  format(/'Invalid ',a,' date of ',a,' in tidefile:' &
          /a)

 607  format(/'Warning - Value for ',a,' not supplied - set to ',l5)

 608  format(/'Warning - Value for ',a,' not supplied - set to ',f8.3)

 609  format(/'Warning - Value for ',a,' not supplied - set to ',i5)

 620  format(/'Too many upstream channels at node ',i3,' :',i5, &
          ' max allowed: ',i4)

 621  format(/'Too many downstream channels at node ',i3,' :',i5, &
          ' max allowed: ',i4)

 642  format(/a,i4,' does not have a name or channel number.')

 643  format(/'Invalid insertion at node ',i3,': node does not exist.')

 644  format(/'Path name ',a,' does not have a translation.')

 645  format(/a,' does not have a translation.')

 646  format(/a,a)

 661  format(/'Invalide filter: ',a,': node or waterbody does not exist, or not connected.')

 647  format(/'Qaul binary starts on ',a,' it does not contain the date :',a)
! 647  format(/'Warning - Value for ',a,' not supplied - set to ',a)
! 648  format(/'Warning - Value for ',a,' not supplied - set to ',a)

 648  format(/'Qaul binary ends on ',a,' it does not contain the date :',a)


!-----adjust areas
!      do m=1,max_reservoirs
!         res_geom(m).area = res_geom(m).area*1e06
!      enddo

!-----adjust totals
!-----npass_node=npass_node-1
!      nchanres=nchanres-1   ! KJ: Is this variable ever used?
!      npartno=npartno-1     ! KJ: Not necessary in the text version.

!-----Check scalar variables

      if (ptm_time_step_int .eq. 0) then
         ptm_time_step = 15
         write(unit_error,607)'ptm_time_step  ',ptm_time_step
      else
         ptm_time_step = incr_intvl(0,time_step_intvl_ptm,IGNORE_BOUNDARY)
      endif

      if (ptm_ivert_int .eq. 0) then
         ptm_ivert=.true.
         write(unit_error,607)'ptm_ivert ',ptm_ivert
      endif

      if (ptm_itrans_int .eq. 0) then
         ptm_itrans=.true.
         write(unit_error,607)'ptm_itrans ',ptm_itrans
      endif

      if (ptm_iey_int .eq. 0) then
         ptm_iey=.true.
         write(unit_error,607)'ptm_iey',ptm_iey
      endif

      if (ptm_iez_int .eq. 0) then
         ptm_iez=.true.
         write(unit_error,607)'ptm_iez',ptm_iez
      endif

      if (ptm_flux_percent_int .eq. 0) then
         ptm_flux_percent=.true.
         write(unit_error,607)'ptm_flux_percent',ptm_flux_percent
      endif

      if (ptm_group_percent_int .eq. 0) then
         ptm_group_percent=.true.
         write(unit_error,607)'ptm_group_percent',ptm_group_percent
      endif

      if (ptm_flux_cumulative_int .eq. 0) then
         ptm_flux_cumulative=.true.
         write(unit_error,607)'ptm_flux_cumulative',ptm_flux_cumulative
      endif

      if (ptm_iprof_int .eq. 0) then
         ptm_iprof=.false.
      endif

      if (ptm_igroup_int .eq. 0) then
         ptm_igroup=.false.
      endif

      if (ptm_no_animated .eq. 0) then
         ptm_no_animated=100
         write(unit_error,609)'ptm_no_animated',ptm_no_animated
      endif

      if (ptm_trans_constant_int .eq. 0) then
         ptm_trans_constant=0.60
         write(unit_error,608)'ptm_trans_constant',ptm_trans_constant
      endif

      if (ptm_vert_constant_int .eq. 0) then
         ptm_vert_constant=0.40
         write(unit_error,608)'ptm_vert_constant',ptm_vert_constant
      endif

      if (ptm_trans_a_coef_int .eq. 0) then
         ptm_trans_a_coef=1.62
         write(unit_error,608)'ptm_trans_a_coef',ptm_trans_a_coef
      endif

      if (ptm_trans_b_coef_int .eq. 0) then
         ptm_trans_b_coef=-2.22
         write(unit_error,608)'ptm_trans_b_coef',ptm_trans_b_coef
      endif

      if (ptm_trans_c_coef_int .eq. 0) then
         ptm_trans_c_coef=0.60
         write(unit_error,608)'ptm_trans_c_coef',ptm_trans_c_coef
      endif



!-----Check times for injection
!-----calculate ending time if injection length, rather than
!-----start/end injection times are given
      do m=1,npartno
!--------Commented for testing purposes Aaron Miller----------
         if (part_injection(m).start_date(:3) .eq. 'run') then
            part_injection(m).start_date=run_start_date
         endif
!-------------------------------------------------------------
         if (part_injection(m).slength .ne. ' ') then
!-----------injection start length should be in form: '20hour' or '5day'
!-----------or, 'runtime' means no offset length
            if (part_injection(m).slength(:3) .eq. 'run') then
               part_injection(m).start_date=run_start_date
            else
               part_injection(m).start_date= &
                   diff2dates(run_start_date,part_injection(m).slength)
            endif
         endif
         if (part_injection(m).length .ne. ' ') then
!-----------injection length should be in form: '20hour' or '5day'
            part_injection(m).start_julmin= &
                cdt2jmin(part_injection(m).start_date)
            part_injection(m).end_date= &
                diff2dates(part_injection(m).start_date,part_injection(m).length)
         endif
         part_injection(m).start_julmin=cdt2jmin(part_injection(m).start_date)
!--------check if injection date is before model start date;
!--------if so, zero out the injected particles
         if (part_injection(m).start_julmin .lt. start_julmin) then
            part_injection(m).nparts=0
         endif
         part_injection(m).end_julmin=cdt2jmin(part_injection(m).end_date)
         part_injection(m).length_julmin= &
             part_injection(m).end_julmin-part_injection(m).start_julmin
!--------convert injection nodes to internal and check if injection node exists
         nodeexist = .false.
!----    do nodeindex=1,nchans
	     do nodeindex=1,nnodes
	  !    print *,"node_geom: ",node_geom(nodeindex).node_id
		!  fixme: could just use node_geom
!----       if ((part_injection(m).node .eq. chan_geom(nodeindex).upnode) .or.
!----&           (part_injection(m).node .eq. chan_geom(nodeindex).downnode)) then
            if (part_injection(m).node .eq. node_geom(nodeindex).node_id) then
        !  fixme: to delete after check
               write (unit_screen,'(a,i3)') 'check inject node ',part_injection(m).node
               nodeexist = .true.
               exit
	        end if
	     end do
         if (.not. nodeexist) then
            write (unit_error, 643) node_geom(part_injection(m).node).node_id
            goto 900
         endif
         part_injection(m).node = ext2intnode(part_injection(m).node)
         !print *,"inject nd: ",part_injection(m).node
      enddo

!-----Check particle filters existence
      do m=1,nfilter
         filterexist = .false.
         ! on channel
         if (part_filter(m).at_wb_type .eq. obj_channel) then
            do chanindex=1,nchans
               if ((part_filter(m).at_wb_id .eq. chanindex) .and. &
                  ((part_filter(m).node .eq. chan_geom(chanindex).upnode) .or. &
                   (part_filter(m).node .eq. chan_geom(chanindex).downnode))) then
                  write (unit_screen,'(a,a,a,i3,a,a)') 'check filter ',trim(part_filter(m).name), &
                       ', at node ',nodelist(part_filter(m).node),', at wb ',part_filter(m).at_wb
                  filterexist = .true.
                  exit
               endif
            enddo
            if (.not. filterexist) then
               write (unit_error, 661) part_filter(m).name
               goto 900
            endif

         ! on reservoir
         else if (part_filter(m).at_wb_type .eq. obj_reservoir) then
            if (part_filter(m).at_wb_ndx .le. nreser) then
               resindex = part_filter(m).at_wb_ndx
               if (trim(part_filter(m).at_wb(5:)) .eq. trim(res_geom(resindex).name)) then
                  do n=1,res_geom(resindex).nnodes
                     if (part_filter(m).node .eq. res_geom(resindex).node_no(n)) then
                        write (unit_screen,'(a,a,a,i3,a,a)') 'check filter ',trim(part_filter(m).name), &
                             ', at node ',nodelist(part_filter(m).node),', at wb ',part_filter(m).at_wb
                        filterexist = .true.
                        exit
                     endif
                  enddo
               endif
            endif
            if (.not. filterexist) then
               write (unit_error, 661) part_filter(m).name
               goto 900
            endif

         ! on diversion & boundary stage, 2 conditions: div-nd, div-res
         else if (part_filter(m).at_wb_type .eq. obj_qext) then
            if (part_filter(m).at_wb_ndx .le. nqext) then
               qextindex = part_filter(m).at_wb_ndx
               if ((qext(qextindex).attach_obj_type .eq. obj_node) .and. &
                  (part_filter(m).node .eq. qext(qextindex).attach_obj_no)) then  !ext node no
                   write (unit_screen,'(a,a,a,i3,a,a)') 'check filter ',trim(part_filter(m).name), &
                        ', at node ',nodelist(part_filter(m).node),', at wb ',part_filter(m).at_wb
                   filterexist = .true.
                   cycle
               else if ((qext(qextindex).attach_obj_type .eq. obj_reservoir) .and. &
                       (part_filter(m).resname .eq. qext(qextindex).attach_obj_name)) then
                        write (unit_screen,'(a,a,a,a,a,a)') 'check filter ',trim(part_filter(m).name), &
                             ', at reservoir ',trim(part_filter(m).resname),', at wb ',part_filter(m).at_wb
                   filterexist = .true.
                   cycle
               endif
            endif
            if (.not. filterexist) then
               write (unit_error, 661) part_filter(m).name
               goto 900
            endif

         ! on stage boundary
         else if (part_filter(m).at_wb_type .eq. obj_stage) then
            if (part_filter(m).at_wb_ndx .le. nstgbnd) then
               stgbndindex = part_filter(m).at_wb_ndx
               if ((trim(part_filter(m).at_wb(7:)) .eq. trim(stgbnd(stgbndindex).name)) .and. &
                  (part_filter(m).node .eq. stgbnd(stgbndindex).node)) then
                  write (unit_screen,'(a,a,a,i3,a,a)') 'check filter ',trim(part_filter(m).name), &
                       ', at node ',nodelist(part_filter(m).node),', at wb ',part_filter(m).at_wb
                  filterexist = .true.
                  cycle
               endif
            endif
            if (.not. filterexist) then
               write (unit_error, 661) part_filter(m).name
               goto 900
            endif

         ! on obj2obj
         else if (part_filter(m).at_wb_type .eq. obj_obj2obj) then
            if (part_filter(m).at_wb_ndx .le. nobj2obj) then
               obj2objindex = part_filter(m).at_wb_ndx

               if (((obj2obj(obj2objindex).from_obj.obj_type .eq. obj_node) .and. &
                   (obj2obj(obj2objindex).from_obj.obj_no .eq. part_filter(m).node)) .or. &
                  ((obj2obj(obj2objindex).to_obj.obj_type .eq. obj_node) .and. &
                   (obj2obj(obj2objindex).to_obj.obj_no .eq. part_filter(m).node))) then
                   write (unit_screen,'(a,a,a,i3,a,a)') 'check filter ',trim(part_filter(m).name), &
                        ', at node ',nodelist(part_filter(m).node),', at wb ',part_filter(m).at_wb
                   filterexist = .true.
                   cycle
               else if (((obj2obj(obj2objindex).from_obj.obj_type .eq. obj_reservoir) .and. &
                        (obj2obj(obj2objindex).from_obj.obj_name .eq. part_filter(m).resname)) .or. &
                       ((obj2obj(obj2objindex).to_obj.obj_type .eq. obj_reservoir) .and. &
                       (obj2obj(obj2objindex).to_obj.obj_name .eq. part_filter(m).resname))) then
                   write (unit_screen,'(a,a,a,a,a,a)') 'check filter ',trim(part_filter(m).name), &
                        ', at reservoir ',trim(part_filter(m).resname),', at wb ',part_filter(m).at_wb
                   filterexist = .true.
                   cycle
               endif

            endif

            if (.not. filterexist) then
               write (unit_error, 661) part_filter(m).name
               goto 900
            endif

         ! others err
         else
            write (unit_error, 661) part_filter(m).name
            print *,'wrong obj for filter setting'
            goto 900
         end if

      enddo

!-----check that quality tide file includes full runtime

      if (qual_bin_file.filename .ne. ' ') then
         if(qual_bin_file.start_julmin_file .gt. start_julmin) then
            tmpdate = jmin2cdt(start_julmin)
            write (unit_error, 647) qual_bin_file.start_date,tmpdate
            goto 900
         elseif(qual_bin_file.end_julmin_file .lt. end_julmin) then
            tmpdate = jmin2cdt(end_julmin)
            write (unit_error, 648) qual_bin_file.end_date,tmpdate
            goto 900
         endif
      endif

      !open(unit=911,file='group_member.out',status='unknown',err=890)
      !call WriteGroupMembers2File(911)
      !close(unit=911)




 890  return

 900  continue                  !here for fatal error
      istat= -1

      return
end



!       function getTypeWBNumber(name, Type)

!       implicit none

!       integer*2 getTypeWBNumber, getReservoirNumber, getBoundaryNumber
!       include 'common.f'
!       integer ri,rn
!       integer lnblnk
!       integer multp
!       include '../../ptm/ptm-fortran.inc'
!       integer i,shift
!       byte Type
!       character*(*) name
!       if (name(1:1) .eq. '-') then
!          multp = -1
!          name=name(2:lnblnk(name))
!       else
!          multp = 1
!       endif
!       getTypeWBNumber=0
!       if (Type .eq. diversion) then
!          shift = max_channels+max_reservoirs
!       else if(Type .eq. pump) then
!          shift = max_channels+max_reservoirs+max_nodes
!       else if(Type .eq. rim) then
!          shift =  max_channels+max_reservoirs+max_nodes+max_reservoirs
!       else if (Type .eq. reservoirr) then
!          shift = max_channels
!       endif
!       if (Type .eq. reservoirr) then
!          getTypeWBNumber = getReservoirNumber(name)+max_channels
!       else
!          do i=1, transNumber
!             if (translationInfo(i).type .eq. Type) then
!                ri = lnblnk(translationInfo(i).name)
!                rn = lnblnk(name)
!                if (translationInfo(i).name(1:ri) .eq. name(1:rn)) then
!                   if(type .eq. pump) then
!                      getTypeWBNumber =
!      &                    translationInfo(i).reservoirNumber+shift
!                   else if (type .eq. rim) then
!                      getTypeWBNumber =
!      &                    getBoundaryNumber(i)+shift
!                   else
!                      getTypeWBNumber =
!      &                    translationInfo(i).nodeNumber+shift
!                   endif
!                endif
!             endif
!          enddo
!       endif
!       getTypeWBNumber = multp*getTypeWBNumber
!       end

!       integer function getBoundaryNumber( k)
!       implicit none
!       integer k
!       include 'common.f'
!       include '../../ptm/ptm-fortran.inc'
!       integer i
!       getBoundaryNumber=0
!       do i=1,k
!          if(translationInfo(i).type .eq. rim)
!      &        getBoundaryNumber = getBoundaryNumber+1
!       enddo

!       end
