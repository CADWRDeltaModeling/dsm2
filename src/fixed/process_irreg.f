<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public License as published by
C!    the Free Software Foundation, either version 3 of the License, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public License for more details.

C!    You should have received a copy of the GNU General Public License
C!    along with DSM2.  If not, see <http://www.gnu.org/licenses/>.
</license>

      subroutine process_irreg

c-----Assign cross-sections to channels
      use io_units
      use grid_data
      use logging
      use common_xsect
      implicit none

c-----local variables
      integer maxf              ! maximum number of fields in data files
      parameter (maxf=7)

      integer
     &     h                    ! do loop counters
     &     ,j
     &     ,m
     &     ,channo              ! DSM channel number


c-----arguments for DSS function FINDLM

      do j=1,nirg
c--------convert distance
         channo=irreg_geom(j).chan_no
         irreg_geom(j).dist_actual = irreg_geom(j).dist_ratio*float(
     &        chan_geom(channo).length)
      enddo
      

c--------subtract minimum elevation to convert elevation from datum
c--------to height above channel bottom
c-----fixme: this is very strange. Why do it?
      do h=1,nirg
         do m=1,irreg_geom(h).num_elev
            irreg_geom(h).elevation(m)=
     &           irreg_geom(h).elevation(m)-
     &           irreg_geom(h).min_elev
            irreg_geom(h).z_centroid(m)=
     &           irreg_geom(h).z_centroid(m)
     &           -irreg_geom(h).min_elev
         enddo
         irreg_geom(h).z_centroid(1)=0.0
      enddo

      if (print_level .ge. 4) then
         write(unit_screen,*)
         write(unit_screen,*) 
     &        '-------------------------------------------------------------------',
     &        'The following irregular cross-section files have only one or zero  ',
     &        'layer. Please verify these cross-sections. ',
     &        '-------------------------------------------------------------------' 
         do h=1,nirg
            if (irreg_geom(h).num_elev .lt. 2) then
               write(unit_screen,*) 
     &              "Channel: ",chan_geom(irreg_geom(h).chan_no).chan_no,"Distance (fraction):", irreg_geom(h).dist_ratio
            endif               
         enddo


         write(unit_screen,*)
         write(unit_screen,*) 
     &        '-------------------------------------------------------------------',
     &        'The maximum and minimum elevations in the following irregular ',
     &        'cross-sections are more than 100 ft apart.  Please verify ',
     &        'that these cross-sections should have layers > 100 ft apart.',
     &        '-------------------------------------------------------------------' 
         do h=1,nirg
            if ((irreg_geom(h).elevation(irreg_geom(h).num_elev)
     &           -irreg_geom(h).min_elev) .gt. 100.0) then
               write(unit_screen,*) 
     &              "Channel: ",chan_geom(irreg_geom(h).chan_no).chan_no,"Distance (fraction):", irreg_geom(h).dist_ratio
            endif
         enddo
      endif

      call assign_sections

      return                    ! normal return

      end

      subroutine assign_sections
   
c-----This subroutine assigns the irregular cross-sections to the channels.
c-----Cross-sections that are within a specified distance from the end of a channel
c-----are moved to the end.  Cross-sections are also copied to the ends of channels
c-----if it will not be possible to interpolate.
      Use IO_Units
      use grid_data
      use common_xsect
      implicit none


c-----local variables

      integer
     &     i                    ! do loop counters
     &     ,channo              ! current channel number


c-----Loop through sections, count sections assigned to channels.
c-----Assign channels to assignment structure based on location
c-----(defined in the input file) only.

      do i=1,nirg
         channo=irreg_geom(i).chan_no
         xsect_assg(channo).num_sec_orig=xsect_assg(channo).num_sec_orig+1
         xsect_assg(channo).num_sec_assg=xsect_assg(channo).num_sec_orig
         xsect_assg(channo).sec_index(xsect_assg(channo).num_sec_assg) = i
         xsect_assg(channo).original(
     &        xsect_assg(channo).num_sec_assg) = .true.
      enddo

      call sort_assignments
      call move_xsects
      call copy_xsects
      call assign_adjacent_xsects

      return
      end

      subroutine sort_assignments

c-----sort assignments by dist from upstream end
      use common_xsect
      implicit none


c-----local variables
c-----do loop counters
      integer
     &     i
     &     ,j
     &     ,k
     &     ,channo
c-----sorting variables
      integer
     &     first
     &     ,last
     &     ,ptr
      real*8
     &     hold

      do i=1,nirg
         channo=irreg_geom(i).chan_no
         last=xsect_assg(channo).num_sec_orig
         do j=1,last-1
            ptr=j
            first=j+1
            do k=first,last
               if (irreg_geom(xsect_assg(channo).sec_index(k)).dist_actual .lt.
     &              irreg_geom(
     &              xsect_assg(channo).sec_index(ptr)).dist_actual) ptr=k
            enddo
            hold=xsect_assg(channo).sec_index(j)
            xsect_assg(channo).sec_index(j) =
     &           xsect_assg(channo).sec_index(ptr)
            xsect_assg(channo).sec_index(ptr)=hold

            hold=xsect_assg(channo).original(j)
            xsect_assg(channo).original(j) =
     &           xsect_assg(channo).original(ptr)
            xsect_assg(channo).original(ptr)=hold
         enddo
      enddo

      return
      end

      subroutine move_xsects

c-----Move cross-sections to channel endpoints if within specified dist from end.
c-----Cross-sections are moved by changing the value of xsect_assg().dist.
      use common_xsect
      use grid_data

      implicit none


      integer
     &     i
     &     ,channo              ! do loop counters
     &     ,upstream_sec        ! number of upstream xsect
     &     ,downstream_sec      ! number of downstream xsect
     &     ,upstream_index      ! index of upstream irregular xsect
     &     ,downstream_index    ! index of downstream irregular xsect
     &     ,upstream_node       ! node at upstream end of chan
     &     ,downstream_node     ! node at downstream end of chan
     &     ,upstream_chan       ! upstream channel
     &     ,downstream_chan     ! downstream channel
     &     ,upstr_adj_near_end  ! end of the adj. channel that is adj.
                                ! to the upstream end of the current channel
     &     ,downstr_adj_near_end ! same for downstream
     &     ,nupstream           ! # of channels connected to the upstream node
     &     ,ndownstream         ! # of channels connected to downstream node
      real*8
     &     channel_length       ! length of current channel

      common /com_xsect_numbers/
     &     upstream_node
     &     ,downstream_node
     &     ,upstream_chan
     &     ,downstream_chan
     &     ,upstr_adj_near_end
     &     ,downstr_adj_near_end
     &     ,nupstream
     &     ,ndownstream
      
      do channo=1,nchans
c--------if distance of first and last sec is with specified percentage of channel
c--------length from node, move cross-section to the end of the channel by
c--------changing distance to 0 or length
c----fixme: why is this done
         if ( chan_geom(channo).length .gt. 0 .and. 
     &        xsect_assg(channo).num_sec_orig .gt. 0 ) then
            call xsect_numbers(channo)
            
            upstream_sec=1
            downstream_sec=xsect_assg(channo).num_sec_orig
            upstream_index=xsect_assg(channo).sec_index(upstream_sec)
            downstream_index=xsect_assg(channo).sec_index(downstream_sec)
            channel_length=float(chan_geom(channo).length)
            
            if (irreg_geom(upstream_index).dist_ratio .le.
     &           max_dist_ratio) then
               irreg_geom(upstream_index).dist_actual = 0.0
               irreg_geom(upstream_index).dist_ratio = 0.0
            endif
            if (irreg_geom(downstream_index).dist_ratio .ge.
     &           (1.0-max_dist_ratio)) then
               irreg_geom(downstream_index).dist_actual = channel_length
               irreg_geom(downstream_index).dist_ratio = 1.0
            endif
         endif
      enddo

c-----copy distances from irreg_geom() to xsect_assg()
      do channo=1, nchans
         do i=1,xsect_assg(channo).num_sec_assg
            xsect_assg(channo).dist(i) = 
     &           irreg_geom(xsect_assg(channo).sec_index(i)).dist_actual
         enddo
      enddo

      return
      end

      subroutine copy_xsects

c-----Copy cross-sections to ends of channels
      use grid_data
      use common_xsect
      implicit none


      integer
     &     i
     &     ,channo              ! do loop counters
     &     ,upstream_sec        ! number (within chan) of upstream xsect
     &     ,downstream_sec      ! number (within chan) of downstream xsect
     &     ,upstream_index      ! index of upstream irregular xsect
     &     ,downstream_index    ! index of downstream irregular xsect
     &     ,upstream_node       ! node at upstream end of chan
     &     ,downstream_node     ! node at downstream end of chan
     &     ,upstream_chan       ! upstream channel
     &     ,downstream_chan     ! downstream channel
     &     ,upstr_adj_near_end  ! end of the adj. channel that is adj.
                                ! to the upstream end of the current channel
     &     ,downstr_adj_near_end ! same for downstream
     &     ,nupstream           ! # of channels connected the upstream node
     &     ,ndownstream         ! # of channels connected to the downstream node
      real*8
     &     channel_length       ! length of current channel

      common /com_xsect_numbers/
     &     upstream_node
     &     ,downstream_node
     &     ,upstream_chan
     &     ,downstream_chan
     &     ,upstr_adj_near_end
     &     ,downstr_adj_near_end
     &     ,nupstream
     &     ,ndownstream
      

      do channo=1,nchans
         if ( chan_geom(channo).length .gt. 0 .and.
     &        xsect_assg(channo).num_sec_assg .ge. 1 )then
            call xsect_numbers(channo)
            
            upstream_sec=1
            downstream_sec=xsect_assg(channo).num_sec_orig
            upstream_index=xsect_assg(channo).sec_index(upstream_sec)
            downstream_index=xsect_assg(channo).sec_index(downstream_sec)
            channel_length=float(chan_geom(channo).length)
c-----------if 
c-----------1.  current channel has irregular xsect(s)
c-----------2.  nearest cross-section in chan is out of specified range for moving &
c-----------3.  there is <> one adj. channel or there are no irregular xsects 
c-----------    in adj chan
c-----------then COPY cross-section to end.

c-----------UPSTREAM
            if ( (irreg_geom(upstream_index).dist_ratio .gt. max_dist_ratio) .and.
     &           ( (nupstream .ne. 2) .or. 
     &           (xsect_assg(upstream_chan).num_sec_assg .eq. 0) ) ) then
               xsect_assg(channo).num_sec_assg =
     &              xsect_assg(channo).num_sec_assg+1
               
               do i=xsect_assg(channo).num_sec_assg,2,-1
                  xsect_assg(channo).sec_index(i)=
     &                 xsect_assg(channo).sec_index(i-1)
                  xsect_assg(channo).dist(i)=
     &                 xsect_assg(channo).dist(i-1)
                  xsect_assg(channo).original(i)=
     &                 xsect_assg(channo).original(i-1)
               enddo
               xsect_assg(channo).original(1)=.false.
               xsect_assg(channo).dist(1)=0.0
            endif
c-----------DOWNSTREAM
            if ( (irreg_geom(downstream_index).dist_ratio .lt. 
     &           (1.0-max_dist_ratio)) .and. 
     &           ( (ndownstream .ne. 2) .or. 
     &           (xsect_assg(downstream_chan).num_sec_assg .eq. 0) ) ) then
               
               xsect_assg(channo).num_sec_assg =
     &              xsect_assg(channo).num_sec_assg+1
               
               xsect_assg(channo).sec_index(
     &              xsect_assg(channo).num_sec_assg) =
     &              xsect_assg(channo).sec_index(
     &              xsect_assg(channo).num_sec_assg-1)
               xsect_assg(channo).dist(
     &              xsect_assg(channo).num_sec_assg) =
     &              channel_length
               xsect_assg(channo).original(xsect_assg(channo).
     &              num_sec_assg)=.false.
            endif
         endif
      enddo

      return
      end

      subroutine assign_adjacent_xsects

c-----Assign cross-sections in adjacent channels to current channel
c-----(for interpolation).  Upstream distance will be < 0; downstream distance will
c-----be > channel length.
      Use IO_Units
      use grid_data
      use common_xsect
      use constants
      implicit none


      integer
     &     i
     &     ,channo              ! do loop counters
     &     ,upstream_sec        ! number (within chan) of upstream xsect
     &     ,downstream_sec      ! number (within chan) of downstream xsect
     &     ,upstream_index      ! index of upstream irregular xsect
     &     ,downstream_index    ! index of downstream irregular xsect
     &     ,upstream_node       ! node at upstream end of chan
     &     ,downstream_node     ! node at downstream end of chan
     &     ,upstream_chan       ! upstream channel
     &     ,downstream_chan     ! downstream channel
     &     ,upstr_adj_near_end  ! end of the adj. channel that is adj.
                                ! to the upstream end of the current channel
     &     ,downstr_adj_near_end ! same for downstream
     &     ,nupstream           ! # of channels connected to upstream node
     &     ,ndownstream         ! # of channels connected to downstream node
     &     ,oindex              ! index of nearest original (not copied) irr. xsect
      real*8
     &     channel_length       ! length of current channel

      common /com_xsect_numbers/
     &     upstream_node
     &     ,downstream_node
     &     ,upstream_chan
     &     ,downstream_chan
     &     ,upstr_adj_near_end
     &     ,downstr_adj_near_end
     &     ,nupstream
     &     ,ndownstream

      do channo=1,max_channels
         if ( chan_geom(channo).length .gt. 0 .and.
     &        xsect_assg(channo).num_sec_assg .ge. 1 ) then
            call xsect_numbers(channo)

            upstream_sec=1
            downstream_sec=xsect_assg(channo).num_sec_orig
            upstream_index=xsect_assg(channo).sec_index(upstream_sec)
            downstream_index=xsect_assg(channo).sec_index(downstream_sec)
            channel_length=float(chan_geom(channo).length)

c-----------assign from upstream channel--only if upstream sec is not at node
            if ( (nupstream .eq. 2) .and.
     &           (xsect_assg(upstream_chan).num_sec_assg .ge. 1) .and.
     &           (xsect_assg(channo).dist(1) .gt. 0) ) then
               xsect_assg(channo).num_sec_assg =
     &              xsect_assg(channo).num_sec_assg+1

               do i=xsect_assg(channo).num_sec_assg,2,-1
                  xsect_assg(channo).sec_index(i)=
     &                 xsect_assg(channo).sec_index(i-1)
                  xsect_assg(channo).dist(i)=
     &                 xsect_assg(channo).dist(i-1)
                  xsect_assg(channo).original(i)=
     &                 xsect_assg(channo).original(i-1)
               enddo

c--------------find index of nearest original xsect in adjacent channel
               oindex=0
               if (upstr_adj_near_end .eq. chan_down) then
                  do i=1,xsect_assg(upstream_chan).num_sec_assg
                     if (xsect_assg(upstream_chan).original(i)) oindex=i
                  enddo
               elseif (upstr_adj_near_end .eq. chan_up) then
                  do i=xsect_assg(upstream_chan).num_sec_assg,1,-1
                     if (xsect_assg(upstream_chan).original(i)) oindex=i
                  enddo
               else
                  write(unit_error,*) 'Error:  upstream_adj_near_end=',upstr_adj_near_end
                  call exit(2)
               endif
               if (oindex .eq. 0) then
                  write(unit_error,*)
     &                 'Error:  unable to assign upstream cross-section for channel ',channo
                  call exit(2)
               endif

               xsect_assg(channo).sec_index(1)=
     &              xsect_assg(upstream_chan).sec_index(oindex)
               if (upstr_adj_near_end .eq. chan_down) then
                  xsect_assg(channo).dist(1)=
     &                 -( float(chan_geom(upstream_chan).length)-
     &                 xsect_assg(upstream_chan).dist(oindex) )
               elseif (upstr_adj_near_end .eq. chan_up) then
                  xsect_assg(channo).dist(1)=
     &                 -(xsect_assg(upstream_chan).dist(oindex))
               else
                  write(unit_error,*) 'Error:  upstr_adj_near_end=',upstr_adj_near_end
                  call exit(2)
               endif

               xsect_assg(channo).original(1)=.false.
               if (xsect_assg(channo).dist(1) .gt. 0) then
                  write(unit_error,*)
     &                 'Error in upstream external cross-section distance calculation for channel ', channo
                  call exit(2)
               endif
            endif

c-----------assign from downstream channel--only if downstream sec is not at node
            if ( (ndownstream .eq. 2) .and.
     &           (xsect_assg(downstream_chan).num_sec_assg .ge. 1) .and.
     &           (xsect_assg(channo).dist(
     &           xsect_assg(channo).num_sec_assg) .lt.
     &           float(chan_geom(channo).length)) ) then
               xsect_assg(channo).num_sec_assg =
     &              xsect_assg(channo).num_sec_assg+1

               oindex=0
               if (downstr_adj_near_end .eq. chan_up) then
                  do i=xsect_assg(downstream_chan).num_sec_assg,1,-1
                     if (xsect_assg(downstream_chan).original(i)) oindex=i
                  enddo
               elseif (downstr_adj_near_end .eq. chan_down) then
                  do i=1,xsect_assg(downstream_chan).num_sec_assg
                     if (xsect_assg(downstream_chan).original(i)) oindex=i
                  enddo
               else
                  write(unit_error,*) 'Error:  downstream_adj_near_end=',downstr_adj_near_end
                  call exit(2)
               endif

               if (oindex .eq. 0) then
                  write(unit_error,*)
     &                 'Error:  unable to assign downstream cross-section for channel ',channo
                  call exit(2)
               endif

               xsect_assg(channo).sec_index(
     &              xsect_assg(channo).num_sec_assg)=
     &              xsect_assg(downstream_chan).sec_index(oindex)
               if (downstr_adj_near_end .eq. chan_up) then
                  xsect_assg(channo).dist(
     &                 xsect_assg(channo).num_sec_assg)=
     &                 channel_length+
     &                 xsect_assg(downstream_chan).dist(oindex)
               elseif (downstr_adj_near_end .eq. chan_down) then
                  xsect_assg(channo).dist(
     &                 xsect_assg(channo).num_sec_assg)=
     &                 channel_length+
     &                 real(chan_geom(downstream_chan).length)-      !todo: change to dble()
     &                 xsect_assg(downstream_chan).dist(oindex)
               else
                  write(unit_error,*) 'Error:  downstr_adj_near_end=',downstr_adj_near_end
                  call exit(2)
               endif

               xsect_assg(channo).original(
     &              xsect_assg(channo).num_sec_assg)=.false.
               if (xsect_assg(channo).dist(
     &              xsect_assg(channo).num_sec_assg) .lt. channel_length) then
                  write(unit_error,*)
     &                 'Error in downstream external cross-section distance calculation for channel ', channo
                  call exit(2)
               endif
            endif
         endif
      enddo

      return
      end

      subroutine errmsg(
     &     channo
     &     ,adj_chan
     &     ,adj_end
     &     ,adj_near_end
     &     ,subname
     &     )

c-----This subroutine prints an error message when the rectangular cross-sections
c-----that are assigned to both sides of a node connected to two channels are
c-----different.
      use IO_Units
      use grid_data
      use common_xsect
      implicit none


      integer
     &     channo               ! current channel number
     &     ,adj_chan            ! number of adjacent channel
     &     ,adj_end             ! current end of current channel
     &     ,adj_near_end        ! end of adjacent channel that is adjacent to
                                ! the current end of the current channel

      character
     &     location1*10         ! location in current channel
     &     ,location2*10        ! location in adjacent channel
     &     ,subname*30          ! name of subroutine that found error

      if(adj_end .eq. 1) then
         location1='upstream'
      elseif (adj_end .gt. 1) then
         location1='downstream'
      endif

      if(adj_near_end .eq. 1) then
         location2 = 'upstream'
      elseif (adj_near_end .gt. 1) then
         location2 = 'downstream'
      endif

c-----only check upstream ends if there are two channels connected to a node
c-----and the channels are both pointing away from the node.

c-----print message if
c-----1. two channels are pointing away from node
c-----2. two channels are pointing toward node
c-----3. adj_end is upstream and adj_near_end is downstream
c-----don't print message if adj_end is downstream and adj_near_end is upstream
      if ( ((adj_end .eq. 1) .and. (adj_near_end .eq. 1)) .or.
     &     ((adj_end .eq. 1) .and. (adj_near_end .gt. 1)) .or.
     &     ((adj_end .gt. 1) .and. (adj_near_end .gt. 1)) ) then
         write(unit_output,'(2(i8,3x,a,2x),a)')
     &        channo
     &        ,trim(location1)
     &        ,adj_chan
     &        ,trim(location2)
     &        ,trim(subname)
      endif

      return
      end

      subroutine geom_output

c-----This subroutine prints a list of all the irregular cross-sections and a list
c-----of the cross-section assignments.
      use IO_Units
      use grid_data
      use common_xsect
      implicit none


      integer
     &     j
     &     ,i
     &     ,channo              ! do loop counters
      character
     &     all_xsect_indices*(5+max_assg_sec*5) ! stores all assigned xsect indices
     &     ,xsect_index*5       ! index of current xsect

      do j=1,nirg
         write(unit_output,*)
         write(unit_output,*) 'IRREGULAR CROSS-SECTIONS (INPUT)'
         write(unit_output,14) chan_geom(irreg_geom(j).chan_no).chan_no
 14      format('DSM2 Channel Number = ',i4)
         write(unit_output,*)
         write(unit_output,*) 'Number of elevations = ',irreg_geom(j).num_elev
         write(unit_output,'(a,1f5.1)') 'Minimum elevation = ', irreg_geom(j).min_elev
         write(unit_output,*) ' elevation       area      wet_p      width   h_radius x_centroid z_centroid'
         write(unit_output,*) '----------------------------------------------------------------------------'

         do i=1,irreg_geom(j).num_elev
            write(unit_output,15)
     &           irreg_geom(j).elevation(i)
     &           ,irreg_geom(j).area(i)
     &           ,irreg_geom(j).wet_p(i)
     &           ,irreg_geom(j).width(i)
     &           ,irreg_geom(j).h_radius(i)
c     &           ,irreg_geom(j).x_centroid(i)
c     &           ,irreg_geom(j).z_centroid(i)
 15         format(f10.2,1x,4(f10.1,1x))
         enddo
      enddo

      write(unit_output,*)
      write(unit_output,*) 'CROSS-SECTION ASSIGNMENTS'
      write(unit_output,*)
     &     'Chan  Cross-section indices: "O" means original, "C" means copy'
     &     ,'A cross-section number of zero means that a rectangular sec was used'
     &     ,'--------------------------------------------------------------------'
c-----this will only work for channel numbers <= 999.  If >=1000, 1st digit will
c-----be overwritten
      do channo=1,nchans
         if (chan_geom(channo).length .gt. 0) then
            write(all_xsect_indices,'(i6)') chan_geom(channo).chan_no
            do j=1,xsect_assg(channo).num_sec_assg
               write(xsect_index,'(i5)') xsect_assg(channo).sec_index(j)
               all_xsect_indices((j*5)+1:(j*5)+5)=xsect_index
               if (xsect_assg(channo).original(j)) then
                  all_xsect_indices((j*5)+2:(j*5)+2) = 'O'
               elseif (.not. xsect_assg(channo).original(j)) then
                  all_xsect_indices(j*5+2:j*5+2) = 'C'
               endif
            enddo
            i=5*(1+xsect_assg(channo).num_sec_assg)
            write(unit_output,'(a)') all_xsect_indices(1:i)
         endif
      enddo

      return
      end


      subroutine xsect_numbers(channo)

c-----This subroutine calculates the numbers of the upstream and
c-----downstream ends of the current channel, the number of channels
c-----attached to the upstream and downstream ends, the numbers of the
c-----channels that are attached to the upstream and downstream ends,
c-----and the numbers of the cross-sections that are assigned to the
c-----adjacent ends of the adjacent channels.
      use grid_data
      use common_xsect
      use constants
      implicit none

      
c-----local variables
      
      integer
     &     channo               ! current channel number
     &     ,upstream_node       ! the upstream node in the current channel
     &     ,downstream_node     ! the downstream node in the current channel
     &     ,upstream_chan       ! the upstream channel from the current channel
     &     ,downstream_chan     ! the downstream channel from the current channel
     &     ,upstr_adj_near_end  ! end of the adj. channel that is adj.
                                ! to the upstream end of the current channel
     &     ,downstr_adj_near_end ! same for downstream
     &     ,nupstream           ! # of channels connected the upstream node
     &     ,ndownstream         ! # of channels connected to downstream node
      
      common /com_xsect_numbers/
     &     upstream_node
     &     ,downstream_node
     &     ,upstream_chan
     &     ,downstream_chan
     &     ,upstr_adj_near_end
     &     ,downstr_adj_near_end
     &     ,nupstream
     &     ,ndownstream
      
      upstream_node = chan_geom(channo).upnode
      downstream_node = chan_geom(channo).downnode
      
c-----If there are more than 2 channels connected to the upstream or downstream
c-----node, then the channel numbers are not needed because no copying will occur
c-----if the upstream channel points toward the node
      if (node_geom(upstream_node).downstream(1) .ne. 0) then
         if (node_geom(upstream_node).downstream(1) .ne. channo) then
            upstream_chan=node_geom(upstream_node).downstream(1)
         else if (node_geom(upstream_node).downstream(2) .ne. channo) then
            upstream_chan=node_geom(upstream_node).downstream(2)
         endif
c--------if the upstream channel points away from the node
      elseif (node_geom(upstream_node).downstream(1) .eq. 0) then
         if (node_geom(upstream_node).upstream(1) .ne. channo) then
            upstream_chan=node_geom(upstream_node).upstream(1)
         elseif (node_geom(upstream_node).upstream(2) .ne. channo) then
            upstream_chan=node_geom(upstream_node).upstream(2)
         endif
c--------there is no upstream channel
      else
         upstream_chan=0
      endif
      
c-----if the downstream channel points away from the node
      if (node_geom(downstream_node).upstream(1) .ne. 0) then
         if (node_geom(downstream_node).upstream(1) .ne. channo) then
            downstream_chan=node_geom(downstream_node).upstream(1)
         elseif (node_geom(downstream_node).upstream(2) .ne. channo) then
            downstream_chan=node_geom(downstream_node).upstream(2)
         endif
c--------if the downstream channel points toward the node
      elseif (node_geom(downstream_node).upstream(1) .eq. 0) then
         if (node_geom(downstream_node).downstream(1) .ne. channo) then
            downstream_chan=node_geom(downstream_node).downstream(1)
         elseif (node_geom(downstream_node).downstream(2) .ne. channo) then
            downstream_chan=node_geom(downstream_node).downstream(2)
         endif
c--------there is no downstream channel
      else
         downstream_chan=0
      endif
      
c-----determine upstream and downstream adjacent ends of adjacent
c-----channels.
      if ( chan_geom(upstream_chan).downnode .eq.
     &     chan_geom(channo).upnode ) then
         upstr_adj_near_end = chan_down
      elseif ( chan_geom(upstream_chan).upnode .eq.
     &        chan_geom(channo).upnode ) then
         upstr_adj_near_end = chan_up
      else
      endif
      
      if ( chan_geom(downstream_chan).upnode .eq.
     &     chan_geom(channo).downnode ) then
         downstr_adj_near_end = chan_up
      elseif ( chan_geom(downstream_chan).downnode .eq.
     &        chan_geom(channo).downnode ) then
         downstr_adj_near_end = chan_down
      else
      endif
      
c-----find number of channels adjacent to each end of the
c-----current channel.
      nupstream=node_geom(upstream_node).nup+
     &     node_geom(upstream_node).ndown
      ndownstream=node_geom(downstream_node).nup+
     &     node_geom(downstream_node).ndown
      
      return
      end
      
