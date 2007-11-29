C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.
C!
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

      subroutine readirreg

c-----Calls subroutines that read all of the data in the irregular cross-section
c-----files into a structure and assign the cross-sections to channels.

      implicit none

      include 'common.f'
      include 'common_irreg_geom.f'

c-----local variables
      integer maxf              ! maximum number of fields in data files
      parameter (maxf=7)
      integer
     &     i
     &     ,j                   ! do loop counters

c-----initialize variables
      do i=1,max_irr_xsects
         irreg_geom(i).dist_actual=init_small_r
         irreg_geom(i).secno=init_small_i
         irreg_geom(i).num_elev=init_small_i
         irreg_geom(i).min_elev=init_big_r
         do j=1,max_elevations
            irreg_geom(i).elevation(j)=init_small_r
            irreg_geom(i).area(j)=init_small_r
            irreg_geom(i).wet_p(j)=init_small_r
            irreg_geom(i).width(j)=init_small_r
            irreg_geom(i).h_radius(j)=init_small_r
            irreg_geom(i).x_centroid(j)=init_small_r
            irreg_geom(i).z_centroid(j)=init_small_r
         enddo
      enddo

      do i=0,max_channels
         irreg_geom_assg(i).num_sec_orig=init_small_i
         irreg_geom_assg(i).num_sec_assg=init_small_i
         do j=1,max_assg_sec
            irreg_geom_assg(i).sec_index(j)=init_small_i
            irreg_geom_assg(i).original(j) = .false.
            irreg_geom_assg(i).rect(j) = .false.
            irreg_geom_assg(i).dist(j)=init_small_r
         enddo
      enddo

      do i=1,max_channels
         chan_index(i)=init_small_i
         num_virt_sec(i)=init_small_i
         num_layers(i)=init_small_i
         elev_index(i)=init_small_i
         virt_deltax(i)=init_small_r
      enddo

      do i=1,max_layers
         virt_area(i)=init_small_r
         virt_wet_p(i)=init_small_r
         virt_width(i)=init_small_r
         virt_z_centroid(i)=init_small_r
      enddo

      do i=1, max_total_elevations
         virt_elevation(i)=init_small_r
      enddo

      do i=1,max_virt_xsects
         virt_min_elev(i)=init_big_r
         minelev_index(i)=init_small_i
      enddo

      call readgeom
      call assign_sections

      return
      end

      subroutine readgeom
 
c-----This subroutine reads in all the irregular cross sections,
c-----assigns cross-sections to channels such that each end of each channel has
c-----a rectangular or irregular cross-section.

      implicit none

      include 'common.f'
      include 'common_irreg_geom.f'

c-----local variables
      integer maxf              ! maximum number of fields in data files
      parameter (maxf=7)

      integer
     &     h                    ! do loop counters
     &     ,j
     &     ,k
     &     ,m
     &     ,lnblnk              ! last nonblank intrinsic function
     &     ,channo              ! DSM channel number

      character
     &     one_line*90          ! used to separate each line into fields
     &     ,cerr_msg*10         ! error message
     &     ,is_it_done*5        ! used to skip headers in input files

c-----sorting variables
      integer
     &     first
     &     ,last
     &     ,ptr

      real*8
     &     hold
c-----arguments for DSS function FINDLM

      integer nbeg              ! position in one_line to begin the search
     &     ,nlen                ! number of characters to search
     &     ,nfield              ! number of fields to search
     &     ,ibegf(maxf)         ! array of beginning positions of fields
     &     ,ilenf(maxf)         ! array of widths of fields
     &     ,idelmt(maxf)        ! array of delimiter types
     &     ,idelmp(maxf)        ! array of delimeter positions
     &     ,itbl(128)           ! array of information on the delimeters set

      do j=1,nirg
c--------convert distance
         channo=irreg_geom(j).chan_no
         irreg_geom(j).dist_actual = irreg_geom(j).dist_ratio*float(
     &        chan_geom(channo).length)

c--------open the geometry viewer output file
         open (
     &        unit_input,
     &        file=irreg_geom(j).filename,
     &        status="old",
     &        err=810
     &        )

         is_it_done = '     '
c--------go down past the headers
         do while(is_it_done .ne. '=====')
            read(
     &           unit_input,
     &           '(a5)') 
     &           is_it_done
         enddo

c--------read a line and determine column widths with FINDLM
         read (
     &        unit_input,
     &        '(a90)'
     &        )
     &        one_line

         m=1
         irreg_geom(j).elevation(1)=0.0
         do while (lnblnk(one_line) .gt. 0)
            nbeg=1
            nlen=90
            nfield=-7
            call findlm(one_line, nbeg, nlen, nfield, ibegf, ilenf,
     &           idelmt, idelmp, itbl)
c-----------parse the line into the structure using results from
c-----------FINDLM.
            cerr_msg='elevation'
c-----------here, "elevation" is wrt NGVD (i.e. same as stage); gets
c-----------changed to a depth reference later
c-----------Changed by Ganesh Pandey 04/03/00
            read(one_line(ibegf(1):ilenf(1)+ibegf(1)-1)
     &           ,'(f12.0)',err=900) irreg_geom(j).elevation(m)
               cerr_msg='cross-section area'
               read(one_line(ibegf(2):ilenf(2)+ibegf(2)-1)
     &              ,'(f12.0)',err=900) irreg_geom(j).area(m)
               cerr_msg='wetted perimeter'
               read(one_line(ibegf(3):ilenf(3)+ibegf(3)-1)
     &              ,'(f12.0)',err=900) irreg_geom(j).wet_p(m)
               cerr_msg='width'
               read(one_line(ibegf(4):ilenf(4)+ibegf(4)-1)
     &              ,'(f12.0)',err=900) irreg_geom(j).width(m)
               cerr_msg='hydraulic radius'
               read(one_line(ibegf(5):ilenf(5)+ibegf(5)-1)
     &              ,'(f12.0)',err=900) irreg_geom(j).h_radius(m)
               cerr_msg='x-centroid'
               read(one_line(ibegf(6):ilenf(6)+ibegf(6)-1)
     &              ,'(f12.0)',err=900) irreg_geom(j).x_centroid(m)
               cerr_msg='z-centroid'
               read(one_line(ibegf(7):ilenf(7)+ibegf(7)-1)
     &              ,'(f12.0)',err=900) irreg_geom(j).z_centroid(m)
c-----------find minimum elevation
               irreg_geom(j).min_elev = min(
     &              irreg_geom(j).elevation(m),irreg_geom(j).min_elev)
               m=m+1
            read(unit_input,'(a)',end=850) one_line
         enddo
         irreg_geom(j).num_elev = m-1
         close(unit_input)
      enddo

c-----sort cross-section properties by elevation in ascending order
      do h=1,nirg
         last=irreg_geom(h).num_elev
         do j=1,last-1
            ptr=j
            first=j+1
            do k=first,last
               if (irreg_geom(h).elevation(k) .lt. 
     &              irreg_geom(h).elevation(ptr)) ptr=k
            enddo

            hold=irreg_geom(h).elevation(j)
            irreg_geom(h).elevation(j)=irreg_geom(h).elevation(ptr)
            irreg_geom(h).elevation(ptr)=hold

            hold=irreg_geom(h).area(j)
            irreg_geom(h).area(j)=irreg_geom(h).area(ptr)
            irreg_geom(h).area(ptr)=hold

            hold=irreg_geom(h).wet_p(j)
            irreg_geom(h).wet_p(j)=irreg_geom(h).wet_p(ptr)
            irreg_geom(h).wet_p(ptr)=hold

            hold=irreg_geom(h).width(j)
            irreg_geom(h).width(j)=irreg_geom(h).width(ptr)
            irreg_geom(h).width(ptr)=hold

            hold=irreg_geom(h).h_radius(j)
            irreg_geom(h).h_radius(j)=irreg_geom(h).h_radius(ptr)
            irreg_geom(h).h_radius(ptr)=hold

            hold=irreg_geom(h).x_centroid(j)
            irreg_geom(h).x_centroid(j)=irreg_geom(h).x_centroid(ptr)
            irreg_geom(h).x_centroid(ptr)=hold

            hold=irreg_geom(h).z_centroid(j)
            irreg_geom(h).z_centroid(j)=irreg_geom(h).z_centroid(ptr)
            irreg_geom(h).z_centroid(ptr)=hold
         enddo
c--------subtract minimum elevation to convert elevation from NGVD
c--------to height above channel bottom at xsect (i.e. depth)
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
     &        'The following irregular cross-section files have only one layer. ',
     &        'Please verify that these cross-sections should only have one layer ',
     &        '-------------------------------------------------------------------' 
         do h=1,nirg
            if (irreg_geom(h).num_elev .lt. 2) then
               write(unit_screen,*) 
     &              irreg_geom(h).filename(1:lnblnk(irreg_geom(h).filename) )
            endif               
         enddo

         write(unit_screen,*)
         write(unit_screen,*) 
     &        '-------------------------------------------------------------------',
     &        'The maximum and minimum elevations in the following irregular ',
     &        'cross-section files are more than 100 ft apart.  Please verify ',
     &        'that these cross-sections should have layers > 100 ft apart.',
     &        '-------------------------------------------------------------------' 
         do h=1,nirg
            if ((irreg_geom(h).elevation(irreg_geom(h).num_elev)
     &           -irreg_geom(h).min_elev) .gt. 100.0) then
               write(unit_screen,*) 
     &              irreg_geom(h).filename(1:lnblnk(irreg_geom(h).filename) )
            endif
         enddo
      endif

      return                    ! normal return

 810  continue                  ! file open error
      write(unit_error, 605) irreg_geom(j).filename
     &     (1:lnblnk(irreg_geom(j).filename))
 605  format(/'Could not open file: ',a)
      call exit(2)

 850  continue                  ! here for premature eof
      write(unit_error, 610) irreg_geom(j).filename
     &     (1:lnblnk(irreg_geom(j).filename))
 610  format(/'Premature end-of-file for irregular geometry file: ',a)
      call exit(2)

 900  continue                  ! error on reading data from line
      write(unit_error, 620) cerr_msg(1:lnblnk(cerr_msg)),
     &     irreg_geom(j).filename(1:lnblnk(irreg_geom(j).filename)),one_line
 620  format(/'Conversion error on ', a,
     &     ' field for irregular geometry file: ', a,'one line =',a)
      call exit(2)

      end

      subroutine assign_sections

c-----This subroutine assigns the irregular cross-sections to the channels.
c-----Cross-sections that are within a specified distance from the end of a channel
c-----are moved to the end.  Cross-sections are also copied to the ends of channels
c-----if it will not be possible to interpolate.

      implicit none

      include 'common.f'
      include 'common_irreg_geom.f'

c-----local variables

      integer
     &     i                    ! do loop counters
     &     ,channo              ! current channel number
     &     ,upstream_chan       ! the upstream channel from the current channel
     &     ,downstream_chan     ! the downstream channel from the current channel
     &     ,upstr_adj_near_end  ! end of the adj. channel that is adj.
                                ! to the upstream end of the current channel
     &     ,downstr_adj_near_end ! same for downstream
     &     ,nupstream           ! # of channels connected to the upstream node
     &     ,ndownstream         ! # of channels connected the downstream node
     &     ,upstream_rect_sec   ! the number of the upstream rectangular xsect
     &     ,downstream_rect_sec ! the number of the downstream rectangular xsect

c-----Loop through sections, count sections assigned to channels.
c-----Assign channels to assignment structure based on location
c-----(defined in the input file) only.

      do i=1,nirg
         channo=irreg_geom(i).chan_no
         irreg_geom_assg(channo).num_sec_orig=irreg_geom_assg(channo).num_sec_orig+1
         irreg_geom_assg(channo).num_sec_assg=irreg_geom_assg(channo).num_sec_orig
         irreg_geom_assg(channo).sec_index(irreg_geom_assg(channo).num_sec_assg) = i
         irreg_geom_assg(channo).original(
     &        irreg_geom_assg(channo).num_sec_assg) = .true.
      enddo

      call sort_assignments
      call move_xsects
      call copy_xsects
      call assign_adjacent_xsects

      if (print_level .ge. 4) then
         write(unit_screen,*) 
     &        '-------------------------------------------------------------------------'
     &        ,'Comparison of rectangular cross-sections assigned to both sides' 
     &        ,'of a node connected to two channels'
     &        ,'Cross-sections assigned to the following locations are different'
     &        ,'-------------------------------------------------------------------------'
     &        ,'Channel     Location   Channel     Location  Called From Subroutine'
     &        ,'-------------------------------------------------------------------------'
      endif

      do channo=1, max_channels
         if (chan_geom(channo).length .gt. 0) then
            call xsect_numbers(channo)
            upstream_rect_sec = 1 
            downstream_rect_sec = chan_geom(channo).nxsect

            if (nupstream .eq. 2) then
               call compare_assigned_rectangular(
     &              channo
     &              ,upstream_rect_sec
     &              ,upstream_chan
     &              ,upstr_adj_near_end
     &              ,nupstream
     &              )
            endif
            if (ndownstream .eq. 2) then
               call compare_assigned_rectangular(
     &              channo
     &              ,downstream_rect_sec
     &              ,downstream_chan
     &              ,downstr_adj_near_end
     &              ,ndownstream
     &              )
            endif
         endif
      enddo

      return
      end

      subroutine sort_assignments

c-----sort assignments by dist from upstream end

      implicit none

      include 'common.f'
      include 'common_irreg_geom.f'

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
         last=irreg_geom_assg(channo).num_sec_orig
         do j=1,last-1
            ptr=j
            first=j+1
            do k=first,last
               if (irreg_geom(irreg_geom_assg(channo).sec_index(k)).dist_actual .lt.
     &              irreg_geom(
     &              irreg_geom_assg(channo).sec_index(ptr)).dist_actual) ptr=k
            enddo
            hold=irreg_geom_assg(channo).sec_index(j)
            irreg_geom_assg(channo).sec_index(j) =
     &           irreg_geom_assg(channo).sec_index(ptr)
            irreg_geom_assg(channo).sec_index(ptr)=hold

            hold=irreg_geom_assg(channo).original(j)
            irreg_geom_assg(channo).original(j) =
     &           irreg_geom_assg(channo).original(ptr)
            irreg_geom_assg(channo).original(ptr)=hold
         enddo
      enddo

      return
      end

      subroutine move_xsects

c-----Move cross-sections to channel endpoints if within specified dist from end.
c-----Cross-sections are moved by changing the value of irreg_geom_assg().dist.

      implicit none

      include 'common.f'
      include 'common_irreg_geom.f'

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
      
      do channo=1,max_channels
c--------if distance of first and last sec is with specified percentage of channel
c--------length from node, move cross-section to the end of the channel by
c--------changing distance to 0 or length
         if ( chan_geom(channo).length .gt. 0 .and. 
     &        irreg_geom_assg(channo).num_sec_orig .gt. 0 ) then
            call xsect_numbers(channo)
            
            upstream_sec=1
            downstream_sec=irreg_geom_assg(channo).num_sec_orig
            upstream_index=irreg_geom_assg(channo).sec_index(upstream_sec)
            downstream_index=irreg_geom_assg(channo).sec_index(downstream_sec)
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

c-----copy distances from irreg_geom() to irreg_geom_assg()
      do channo=1, max_channels
         do i=1,irreg_geom_assg(channo).num_sec_assg
            irreg_geom_assg(channo).dist(i) = 
     &           irreg_geom(irreg_geom_assg(channo).sec_index(i)).dist_actual
         enddo
      enddo

      return
      end

      subroutine copy_xsects

c-----Copy cross-sections to ends of channels

      implicit none

      include 'common.f'
      include 'common_irreg_geom.f'

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
      

      do channo=1,max_channels
         if ( chan_geom(channo).length .gt. 0 .and.
     &        irreg_geom_assg(channo).num_sec_assg .ge. 1 )then
            call xsect_numbers(channo)
            
            upstream_sec=1
            downstream_sec=irreg_geom_assg(channo).num_sec_orig
            upstream_index=irreg_geom_assg(channo).sec_index(upstream_sec)
            downstream_index=irreg_geom_assg(channo).sec_index(downstream_sec)
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
     &           (irreg_geom_assg(upstream_chan).num_sec_assg .eq. 0) ) ) then
               irreg_geom_assg(channo).num_sec_assg =
     &              irreg_geom_assg(channo).num_sec_assg+1
               
               do i=irreg_geom_assg(channo).num_sec_assg,2,-1
                  irreg_geom_assg(channo).sec_index(i)=
     &                 irreg_geom_assg(channo).sec_index(i-1)
                  irreg_geom_assg(channo).dist(i)=
     &                 irreg_geom_assg(channo).dist(i-1)
                  irreg_geom_assg(channo).original(i)=
     &                 irreg_geom_assg(channo).original(i-1)
               enddo
               irreg_geom_assg(channo).original(1)=.false.
               irreg_geom_assg(channo).dist(1)=0.0
            endif
c-----------DOWNSTREAM
            if ( (irreg_geom(downstream_index).dist_ratio .lt. 
     &           (1.0-max_dist_ratio)) .and. 
     &           ( (ndownstream .ne. 2) .or. 
     &           (irreg_geom_assg(downstream_chan).num_sec_assg .eq. 0) ) ) then
               
               irreg_geom_assg(channo).num_sec_assg =
     &              irreg_geom_assg(channo).num_sec_assg+1
               
               irreg_geom_assg(channo).sec_index(
     &              irreg_geom_assg(channo).num_sec_assg) =
     &              irreg_geom_assg(channo).sec_index(
     &              irreg_geom_assg(channo).num_sec_assg-1)
               irreg_geom_assg(channo).dist(
     &              irreg_geom_assg(channo).num_sec_assg) =
     &              channel_length
               irreg_geom_assg(channo).original(irreg_geom_assg(channo).
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

      implicit none

      include 'common.f'
      include 'common_irreg_geom.f'

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
     &        irreg_geom_assg(channo).num_sec_assg .ge. 1 ) then
            call xsect_numbers(channo)

            upstream_sec=1
            downstream_sec=irreg_geom_assg(channo).num_sec_orig
            upstream_index=irreg_geom_assg(channo).sec_index(upstream_sec)
            downstream_index=irreg_geom_assg(channo).sec_index(downstream_sec)
            channel_length=float(chan_geom(channo).length)

c-----------assign from upstream channel--only if upstream sec is not at node
            if ( (nupstream .eq. 2) .and.
     &           (irreg_geom_assg(upstream_chan).num_sec_assg .ge. 1) .and.
     &           (irreg_geom_assg(channo).dist(1) .gt. 0) ) then
               irreg_geom_assg(channo).num_sec_assg =
     &              irreg_geom_assg(channo).num_sec_assg+1

               do i=irreg_geom_assg(channo).num_sec_assg,2,-1
                  irreg_geom_assg(channo).sec_index(i)=
     &                 irreg_geom_assg(channo).sec_index(i-1)
                  irreg_geom_assg(channo).dist(i)=
     &                 irreg_geom_assg(channo).dist(i-1)
                  irreg_geom_assg(channo).original(i)=
     &                 irreg_geom_assg(channo).original(i-1)
               enddo

c--------------find index of nearest original xsect in adjacent channel
               oindex=0
               if (upstr_adj_near_end .eq. chan_down) then
                  do i=1,irreg_geom_assg(upstream_chan).num_sec_assg
                     if (irreg_geom_assg(upstream_chan).original(i)) oindex=i
                  enddo
               elseif (upstr_adj_near_end .eq. chan_up) then
                  do i=irreg_geom_assg(upstream_chan).num_sec_assg,1,-1
                     if (irreg_geom_assg(upstream_chan).original(i)) oindex=i
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

               irreg_geom_assg(channo).sec_index(1)=
     &              irreg_geom_assg(upstream_chan).sec_index(oindex)
               if (upstr_adj_near_end .eq. chan_down) then
                  irreg_geom_assg(channo).dist(1)=
     &                 -( float(chan_geom(upstream_chan).length)-
     &                 irreg_geom_assg(upstream_chan).dist(oindex) )
               elseif (upstr_adj_near_end .eq. chan_up) then
                  irreg_geom_assg(channo).dist(1)=
     &                 -(irreg_geom_assg(upstream_chan).dist(oindex))
               else
                  write(unit_error,*) 'Error:  upstr_adj_near_end=',upstr_adj_near_end
                  call exit(2)
               endif

               irreg_geom_assg(channo).original(1)=.false.
               if (irreg_geom_assg(channo).dist(1) .gt. 0) then
                  write(unit_error,*)
     &                 'Error in upstream external cross-section distance calculation for channel ', channo
                  call exit(2)
               endif
            endif

c-----------assign from downstream channel--only if downstream sec is not at node
            if ( (ndownstream .eq. 2) .and.
     &           (irreg_geom_assg(downstream_chan).num_sec_assg .ge. 1) .and.
     &           (irreg_geom_assg(channo).dist(
     &           irreg_geom_assg(channo).num_sec_assg) .lt.
     &           float(chan_geom(channo).length)) ) then
               irreg_geom_assg(channo).num_sec_assg =
     &              irreg_geom_assg(channo).num_sec_assg+1

               oindex=0
               if (downstr_adj_near_end .eq. chan_up) then
                  do i=irreg_geom_assg(downstream_chan).num_sec_assg,1,-1
                     if (irreg_geom_assg(downstream_chan).original(i)) oindex=i
                  enddo
               elseif (downstr_adj_near_end .eq. chan_down) then
                  do i=1,irreg_geom_assg(downstream_chan).num_sec_assg
                     if (irreg_geom_assg(downstream_chan).original(i)) oindex=i
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

               irreg_geom_assg(channo).sec_index(
     &              irreg_geom_assg(channo).num_sec_assg)=
     &              irreg_geom_assg(downstream_chan).sec_index(oindex)
               if (downstr_adj_near_end .eq. chan_up) then
                  irreg_geom_assg(channo).dist(
     &                 irreg_geom_assg(channo).num_sec_assg)=
     &                 channel_length+
     &                 irreg_geom_assg(downstream_chan).dist(oindex)
               elseif (downstr_adj_near_end .eq. chan_down) then
                  irreg_geom_assg(channo).dist(
     &                 irreg_geom_assg(channo).num_sec_assg)=
     &                 channel_length+
     &                 float(chan_geom(downstream_chan).length)-
     &                 irreg_geom_assg(downstream_chan).dist(oindex)
               else
                  write(unit_error,*) 'Error:  downstr_adj_near_end=',downstr_adj_near_end
                  call exit(2)
               endif

               irreg_geom_assg(channo).original(
     &              irreg_geom_assg(channo).num_sec_assg)=.false.
               if (irreg_geom_assg(channo).dist(
     &              irreg_geom_assg(channo).num_sec_assg) .lt. channel_length) then
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

      implicit none

      include 'common.f'
      include 'common_irreg_geom.f'

      integer
     &     channo               ! current channel number
     &     ,adj_chan            ! number of adjacent channel
     &     ,adj_end             ! current end of current channel
     &     ,adj_near_end        ! end of adjacent channel that is adjacent to
                                ! the current end of the current channel
     &     ,lnblnk              ! last nonblank intrinsic function

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
     &        ,location1(1:lnblnk(location1))
     &        ,adj_chan
     &        ,location2(1:lnblnk(location2))
     &        ,subname(1:lnblnk(subname))
      endif

      return
      end

      subroutine geom_output

c-----This subroutine prints a list of all the irregular cross-sections and a list
c-----of the cross-section assignments.

      implicit none

      include 'common.f'
      include 'common_irreg_geom.f'

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
         write(unit_output,14) irreg_geom(j).chan_no, irreg_geom(j).filename
 14      format('DSM2 Channel Number = ',i4,1x,'filename = ',a60)
         write(unit_output,*)
         write(unit_output,*) 'Number of elevations = ',irreg_geom(j).num_elev
         write(unit_output,*) 'Minimum elevation = ',irreg_geom(j).min_elev
         write(unit_output,*) ' elevation       area      wet_p      width   h_radius x_centroid z_centroid'
         write(unit_output,*) '----------------------------------------------------------------------------'

         do i=1,irreg_geom(j).num_elev
            write(unit_output,15)
     &           irreg_geom(j).elevation(i)
     &           ,irreg_geom(j).area(i)
     &           ,irreg_geom(j).wet_p(i)
     &           ,irreg_geom(j).width(i)
     &           ,irreg_geom(j).h_radius(i)
     &           ,irreg_geom(j).x_centroid(i)
     &           ,irreg_geom(j).z_centroid(i)
 15         format(f10.2,1x,6(f10.1,1x))
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
      do channo=1,max_channels
         if (chan_geom(channo).length .gt. 0) then
            write(all_xsect_indices,'(i5)') channo
            do j=1,irreg_geom_assg(channo).num_sec_assg
               write(xsect_index,'(i5)') irreg_geom_assg(channo).sec_index(j)
               all_xsect_indices((j*5)+1:(j*5)+5)=xsect_index
               if (irreg_geom_assg(channo).original(j)) then
                  all_xsect_indices((j*5)+2:(j*5)+2) = 'O'
               elseif (.not. irreg_geom_assg(channo).original(j)) then
                  all_xsect_indices(j*5+2:j*5+2) = 'C'
               endif
            enddo
            i=5*(1+irreg_geom_assg(channo).num_sec_assg)
            write(unit_output,'(a)') all_xsect_indices(1:i)
         endif
      enddo

      return
      end

      subroutine compare_assigned_rectangular(
     &     channo
     &     ,adj_end
     &     ,adj_chan
     &     ,adj_near_end
     &     ,num_adj_chan
     &     )
      
c-----This subroutine checks rectangular cross-sections assigned on both sides of
c-----nodes that are connected to two channels.  If the cross-sections have 
c-----different widths or bottom elevations, print an error message.
      
      implicit none

      include 'common.f'
      include 'common_irreg_geom.f'

c-----local variables
      integer
     &     channo               ! DSM channel number
     &     ,adj_end             ! current end of current channel
     &     ,adj_near_end        ! end of adj. channel that is adj. to adj_end
     &     ,adj_chan            ! channel connected to node that is conn. to adj_end
     &     ,num_adj_chan        ! number of channels conn. to adj node
     &     ,rsecno_adj          ! number of rectangular xsect at adj_end
     &     ,rsecno_adj_near     ! number of rectangular xsect at adj_near_end

      if ( (irreg_geom_assg(channo).sec_index(adj_end) .eq. 0) .and. 
     &     (irreg_geom_assg(adj_chan).sec_index(adj_near_end).eq.0))then
         if (num_adj_chan .eq. 2) then
            rsecno_adj = chan_geom(channo).xsect(adj_end)
            if (adj_near_end .eq. 1) then
               rsecno_adj_near = chan_geom(adj_chan).xsect(1)
            elseif (adj_near_end .gt. 1) then
               rsecno_adj_near = 
     &              chan_geom(adj_chan).xsect(chan_geom(adj_chan).nxsect)
            endif
            if ( ( xsect_geom(rsecno_adj).width .ne. 
     &           xsect_geom(rsecno_adj_near).width ) .or.
     &           ( xsect_geom(rsecno_adj).botelv .ne.
     &           xsect_geom(rsecno_adj_near).botelv ) ) then
               if (print_level .ge. 3) then
                  call errmsg(
     &                 channo
     &                 ,adj_chan
     &                 ,adj_end
     &                 ,adj_near_end
     &                 ,'compare_assigned_rectangular  '
     &                 )
               endif
            endif
         endif
      endif

      return
      end

      subroutine xsect_numbers(channo)

c-----This subroutine calculates the numbers of the upstream and
c-----downstream ends of the current channel, the number of channels
c-----attached to the upstream and downstream ends, the numbers of the
c-----channels that are attached to the upstream and downstream ends,
c-----and the numbers of the cross-sections that are assigned to the
c-----adjacent ends of the adjacent channels.

      implicit none

      include 'common.f'
      include 'common_irreg_geom.f'
      
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
      
