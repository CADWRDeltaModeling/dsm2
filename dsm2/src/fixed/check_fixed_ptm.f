C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public !<license as published by
C!    the Free Software Foundation, either version 3 of the !<license, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public !<license for more details.

C!    You should have received a copy of the GNU General Public !<license
C!    along with DSM2.  If not, see <http://www.gnu.org/!<licenses/>.
C!</license>

      subroutine check_fixed_ptm(istat)

c-----Check the PTM fixed input for omissions and errors before starting
c-----the model run.  Supply default values where possible.
      use constants
      use IO_Units
      use type_defs

      use grid_data
      use runtime_data
      use common_tide
      use common_ptm
      use common_qual_bin
      implicit none

      
      include '../hydrolib/network.inc'
      include '../hydrolib/netcntrl.inc'
      include '../hydrolib/chconnec.inc'
      include '../hydrolib/chnluser.inc'
      include '../hydrolib/chcxrec1.inc'
      include '../timevar/dss.inc'
      include '../timevar/readdss.inc'
      include '../timevar/writedss.inc'


c-----Local variables

      logical
     &     nodeexist            ! true if node exists in network

      integer
     &     istat                ! status of call (returned)
     &     ,m                   ! indices
     &     ,nodeindex           ! node index used for iterating through nodes
     &     ,ext2intnode

      integer*4
     &     cdt2jmin             ! character date/time to julian minute
     &     ,incr_intvl          ! increment julian minute by interval function

      character
     &     diff2dates*14        ! return DSS date given start and diff
     &     ,jmin2cdt*14         ! julian minute to character date/time function
     &     ,tmpdate*14          ! temporary date for comparison

 605  format(/a,' date incorrect: ',a)

 606  format(/'Invalid ',a,' date of ',a,' in tidefile:'
     &     /a)

 607  format(/'Warning - Value for ',a,' not supplied - set to ',l5)

 608  format(/'Warning - Value for ',a,' not supplied - set to ',f8.3)

 609  format(/'Warning - Value for ',a,' not supplied - set to ',i5)

 620  format(/'Too many upstream channels at node ',i3,' :',i5,
     &     ' max allowed: ',i4)

 621  format(/'Too many downstream channels at node ',i3,' :',i5,
     &     ' max allowed: ',i4)

 642  format(/a,i4,' does not have a name or channel number.')

 643  format(/'Invalide insertion at node ',i3,': node does not exist.') 

 644  format(/'Path name ',a,' does not have a translation.')

 645  format(/a,' does not have a translation.')

 646  format(/a,a)

 647  format(/'Qaul binary starts on ',a,' it does not contain the date :',a)
! 647  format(/'Warning - Value for ',a,' not supplied - set to ',a)
! 648  format(/'Warning - Value for ',a,' not supplied - set to ',a)

 648  format(/'Qaul binary ends on ',a,' it does not contain the date :',a)


c-----adjust areas
      do m=1,max_reservoirs
         res_geom(m).area = res_geom(m).area*1e06
      enddo

c-----adjust totals
c-----npass_node=npass_node-1
      nchanres=nchanres-1
      npartno=npartno-1

c-----Check scalar variables

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



c-----Check times for injection
c-----calculate ending time if injection length, rather than
c-----start/end injection times are given
      do m=1,npartno
c--------Commented for testing purposes Aaron Miller----------
         if (part_injection(m).start_date(:3) .eq. 'run') then
            part_injection(m).start_date=run_start_date
         endif
c-------------------------------------------------------------
         if (part_injection(m).slength .ne. ' ') then
c-----------injection start length should be in form: '20hour' or '5day'
c-----------or, 'runtime' means no offset length
            if (part_injection(m).slength(:3) .eq. 'run') then
               part_injection(m).start_date=run_start_date
            else
               part_injection(m).start_date=
     &              diff2dates(run_start_date,part_injection(m).slength)
            endif
         endif
         if (part_injection(m).length .ne. ' ') then
c-----------injection length should be in form: '20hour' or '5day'
            part_injection(m).start_julmin=
     &           cdt2jmin(part_injection(m).start_date)
            part_injection(m).end_date=
     &           diff2dates(part_injection(m).start_date,part_injection(m).length)
         endif
         part_injection(m).start_julmin=cdt2jmin(part_injection(m).start_date)
c--------check if injection date is before model start date;
c--------if so, zero out the injected particles
         if (part_injection(m).start_julmin .lt. start_julmin) then
            part_injection(m).nparts=0
         endif
         part_injection(m).end_julmin=cdt2jmin(part_injection(m).end_date)
         part_injection(m).length_julmin=
     &        part_injection(m).end_julmin-part_injection(m).start_julmin
c--------convert injection nodes to internal and check if injection node exists
         print*,"Injection node",part_injection(m).node
         part_injection(m).node = ext2intnode(part_injection(m).node)
	   print*,"Converted to internal node: ",part_injection(m).node
         nodeexist = .false.
	   do nodeindex=1,nchans
		!  fixme: could just use node_geom
	      if ((part_injection(m).node .eq. chan_geom(nodeindex).upnode) .or.
     &           (part_injection(m).node .eq. chan_geom(nodeindex).downnode)) then
               nodeexist = .true.
               exit
	       end if
	   end do
         if (.not. nodeexist) then
            write (unit_error, 643) node_geom(part_injection(m).node).node_id
            goto 900
         endif
      enddo

c-----check that quality tide file includes full runtime

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



      return

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
