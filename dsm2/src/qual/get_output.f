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

      real*8 function get_output(ptr)

c-----Get the desired output variable from the particular DSM module
      Use io_units
	Use groups, Only: groupArray
      use common_tide
      use grid_data
      use iopath_data
      implicit none

c-----arguments

      integer
     &     ptr                  ! output path pointer

c-----global variables

      include 'param.inc'
      include 'bltm1.inc'
      include 'bltm2.inc'

c-----local variables

      byte object               ! object type (node, channel, ...)

      integer
     &     qualchan             ! dsm and bltm channel numbers
     &     ,object_no           ! object number
     &     ,hydro_res_no        ! hydro reservoir number
     &     ,hydro_node_no       ! hydro node number
     &     ,nx                  ! number of cross sections for channel
     &     ,i                   ! loop index
     &     ,lnblnk              ! last non blank intrinsic function

      integer
     &     const_no             ! constituent number

      real*8
     &     chan_qual            ! Qual function to return channel concentration
     &     ,node_qual           ! Qual function to return node concentration
     &     ,val_x               ! interpolated value statement function
     &     ,val_up,val_down     ! value at upstream and downstream end of chan
     &     ,chan_dist           ! distance along channel
     &     ,chan_len            ! channel length
     &     ,objflow,massrate(max_constituent) ! flow and massrate at an object

c-----statement function to interpolate value along channel
      val_x(val_up,val_down,chan_dist,chan_len)=val_up-(val_up
     &     -val_down)*(chan_dist/chan_len)

      object=pathoutput(ptr).obj_type
      object_no=pathoutput(ptr).obj_no
      if (object .eq. obj_channel) then ! output at channel requested
         qualchan=object_no
      endif
      const_no=pathoutput(ptr).const_ndx
      if (const_no. gt. 0) then ! water quality constituent requested
         if (object .eq. obj_channel) then ! output at channel requested
            get_output=chan_qual(qualchan,pathoutput(ptr).chan_dist,const_no)
         else if (object .eq. obj_node) then ! output at node requested
            get_output=node_qual(object_no,const_no)
         else if (object .eq. obj_reservoir) then ! output in reservoir requested
            get_output=cres(object_no,const_no)
         elseif (index(pathoutput(ptr).meas_type, 'mass') .gt. 0) then
         else                   ! error, can't figure out where this output is
            write(unit_error, '(a/a/a)') 'Unable to produce output for path:',
     &           '  ' // pathoutput(ptr).path(:lnblnk(pathoutput(ptr).path)),
     &           '  No location given.'
            call exit(2)
         endif
      elseif (pathoutput(ptr).meas_type .eq. 'stage') then
         if (object .eq. obj_channel) then ! channel output
            nx=chan_geom(object_no).nxsect ! last X-Section (i.e. downstream)
	      nx=2 !  fixme: the whole chan_geom(..).nxsect and .bottomelev is bogus and
	      ! should be removed
            get_output=val_x(
     &           ychan(qualchan,1)+chan_geom(object_no).bottomelev(1),
     &           ychan(qualchan,2)+chan_geom(object_no).bottomelev(nx),
     &           dble(pathoutput(ptr).chan_dist),
     &           dble(chan_geom(object_no).length))
         else if (object .eq. obj_reservoir) then ! reservoir output
            hydro_res_no=pathoutput(ptr).obj_no
            get_output=eresv(hydro_res_no)
         endif
      elseif (pathoutput(ptr).meas_type .eq. 'flow') then
         if (object .eq. obj_channel) then
            get_output=val_x(qchan(qualchan,1),qchan(qualchan,2),
     &           dble(pathoutput(ptr).chan_dist),
     &           dble(chan_geom(object_no).length))
         else if (object .eq. obj_reservoir) then
            hydro_res_no=pathoutput(ptr).obj_no
            hydro_node_no=pathoutput(ptr).res_node_no
            get_output=-qres(hydro_res_no,hydro_node_no) ! + qres: flow from res to chan
         endif
      elseif (pathoutput(ptr).meas_type .eq. 'pump') then ! all reservoir pumping
         call res_rate(object_no,FROM_OBJ,NO_CONNECT,objflow,massrate)
         get_output=objflow
      elseif (pathoutput(ptr).meas_type .eq. 'flow-net') then ! net reservoir pump+gate flow
         if (object .eq. obj_reservoir) then
            get_output=0.0
            do i=1,res_geom(pathoutput(ptr).obj_no).nnodes
               hydro_node_no=res_geom(pathoutput(ptr).obj_no).node_no(i)
               get_output=get_output-qres(hydro_res_no,i) ! + qres: flow from res to chan
            enddo

            call res_rate(object_no,FROM_OBJ,NO_CONNECT,objflow,massrate)
            get_output=get_output+objflow
         endif
      else
         write(unit_error, 600)
     &        trim(pathoutput(ptr).meas_type),
     &        trim(groupArray(pathoutput(ptr).source_group_ndx).name)
 600     format ('Error: The following constituent'
     &        ' is not simulated in this run; run stopped'
     &        /'Constituent name: ',a
     &        /'Constituent source group: ',a)
         call exit(1)

      endif

      return
      end

      real*8 function chan_qual(qualchan, chan_distance, const_no)
      Use IO_Units
      use common_qual
      
c-----this routine reports water quality of DSM2-Qual channel

      implicit none

c-----subroutine arguments

      integer
     &     qualchan             ! channel no.
     &     ,chan_distance       ! location within a channel

      integer
     &     const_no             ! constituent number

c-----common blocks

      include 'param.inc'
      include 'bltm1.inc'
      include 'bltm2.inc'
      include 'bltm3.inc'

c-----local variables

      integer
     &     k                   ! loop index

      real*8
     &     xdist

c	fixme: qualchan --- what is it?
      xdist=dble(chan_distance)/dble(chan_geom(qualchan).length)
     &     *(nxsec(qualchan)-1)+1.
      if (chan_distance.eq.0) then
c--------pick the first parcel concentration
         chan_qual=gpt(const_no,1,qualchan)
         return
      else if (xdist.ge.px(ns(qualchan),qualchan)) then
c--------pick the last parcel concentration
         chan_qual=gpt(const_no,ns(qualchan),qualchan)
         return
      else
         do k=2,ns(qualchan)
            if (xdist. le. px(k,qualchan)) then
               chan_qual=gpt(const_no,k-1,qualchan)
               return
            end if
         end do
         write(unit_error, *)
     &        'Error inside chan_qual.f; run stopped.'
         call exit(1)
      end if

      return
      end

      real*8 function node_qual(intnode,const_no)
      Use IO_Units
      use grid_data
c-----Return the mixed quality at a node

      implicit none

c-----arguments

      integer
     &     intnode              ! internal node number
      integer
     &     const_no             ! constituent number

c-----common blocks

      include 'param.inc'
      include 'bltm1.inc'
      include 'bltm2.inc'
      include 'bltm3.inc'

      if (node_geom(intnode).qual_int) then   ! internal node (and already mixed)
         node_qual=cj(const_no,intnode)
      else                      ! external node
         if (numup(intnode) .eq. 1) then ! upstream boundary node
            node_qual=gpt(const_no,1,listup(intnode,1))
         else if (numdown(intnode) .eq. 1) then ! downstream boundary node
            node_qual=gpt(const_no,ns(listdown(intnode,1)), listdown(intnode,1))
         else
            write(unit_error, 610) node_geom(intnode).node_ID
 610        format(/'Error in node_qual, DSM2 node number ',i3)
            call exit(1)
         endif
      endif

      return
      end
