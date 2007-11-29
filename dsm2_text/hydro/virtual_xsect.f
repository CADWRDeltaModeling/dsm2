C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.
C!
C!    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
C!    numerical model.  No protection claimed in original FOURPT and
C!    Branched Lagrangian Transport Model (BLTM) code written by the
C!    United States Geological Survey.  Protection claimed in the
C!    routines and files listed in the accompanying file "Protect.txt".
C!    If you did not receive a copy of this file contact Tara Smith,
C!    below.
C!
C!    This program is licensed to you under the terms of the GNU General
C!    Public License, version 2, as published by the Free Software
C!    Foundation.
C!
C!    You should have received a copy of the GNU General Public License
C!    along with this program; if not, contact Tara Smith, below,
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
C!    Tara Smith
C!    California Dept. of Water Resources
C!    Division of Planning, Delta Modeling Section
C!    1416 Ninth Street
C!    Sacramento, CA  95814
C!    916-653-9885
C!    tara@water.ca.gov
C!
C!    or see our home page: http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/


      subroutine virtual_xsect

c-----Creates virtual cross-section lookup table:
c-----Interpolates within cross-sections to make tables for all unique elev.
c-----then interpolates in x-direction to make virtual cross-sections
c-----which are then used as lookup tables.
c-----Elevations are now heights of layers.  A layer consists of all cross-section
c-----properties in a cross-section that have the same elevation (or height).
c-----The height is the distance above the lowest point in the cross-section.

      implicit none

      include '../input/fixed/common.f'
      include '../input/fixed/common_irreg_geom.f'
	include 'virt_xsect.inc'


      integer
     &     channo               ! dsm channel number
     &     ,vsecno              ! number of virtual xsect within chan. (upstream=1)
     &     ,i                   ! do loop counters
     &     ,j
     &     ,virtelev            ! number of layer (within xsect)



c-----function required by qsort function
      external compar
      integer*2 compar


c-----initialize index arrays
      do vsecno=1,max_virt_xsects
         virt_min_elev(vsecno)=miss_val_r
      enddo
      do channo=1,max_channels
         chan_index(channo)=0
         elev_index(channo)=0
         minelev_index(channo)=0
      enddo
      elev_index(1)=1
      chan_index(1)=1
      minelev_index(1)=1

c-----used by interp_width
      if (levee_slope .le. 0) then
         levee_slope = 0.5      ! default levee slope
      endif

c-----used by check_area
      if (area_tolerance .le. 0) then
         area_tolerance = 2.0   ! default area tolerance
      endif

      do channo=1,max_channels
c--------initialize temporary data arrays--these must be re-initialized for each
c--------channel because of the way the arrays are sorted
         do i=1,max_elevations
            temp_all_elev(i)=0
            do j=1,max_assg_sec
               temparea(j,i)=0
               tempwet_p(j,i)=0
               tempwidth(j,i)=0
               tempz_centroid(j,i)=0
            enddo
         enddo
c--------indices must be cumulative:  add current indices to indices for next chan
         if (channo .lt. max_channels) then
            if ( chan_geom(channo).length .le. 0 .or.
     &           irreg_geom_assg(channo).num_sec_assg .le. 0 ) then
               elev_index(channo+1)=elev_index(channo+1)+elev_index(channo)
               chan_index(channo+1)=chan_index(channo+1)+chan_index(channo)
               minelev_index(channo+1)=minelev_index(channo+1)+minelev_index(channo)
            endif
         endif

c--------ALL ELEVATIONS ARE NOW > 0 (THEY ARE ACTUALLY HEIGHTS of layers above
c--------the lowest point in the cross-section.  A layer consists of all
c--------cross-section properties in a cross-section that have the same height.
c--------Elevations were converted to heights in readirreg by subtracting the
c--------bottom elevation.

         if (chan_geom(channo).length .gt. 0) then
            call combine_heights(channo)

c-----------calculate elevation index for next channel
            if (channo+1 .le. max_channels) then
               elev_index(channo+1) = elev_index(channo) + num_layers(channo)
            endif

            call first_interpolation(channo)
            call second_interpolation(channo)
         endif
      enddo

c-----copy bottom elevations at ends of channel to chan_geom().BottomElev.
c-----This is done for the use of qual and ptm only; hydro doesn't need this.
      do channo=1,max_channels
         chan_geom(channo).BottomElev(1) = virt_min_elev(minelev_index(channo))
         chan_geom(channo).BottomElev(2) = virt_min_elev(minelev_index(channo)+
     &        num_virt_sec(channo)-1)
      enddo

      return
      end

      subroutine combine_heights(channo)

c-----This subroutine makes a list of all of the unique layer heights in a channel
c-----by making a list of all heights from real cross-sections in the channel,
c-----and sorting and filtering the list.  A layer consists of all cross-section
c-----properties in a cross-section that have the same  height.
c-----If the maximum height in the list is < = the specified maximum layer height,
c-----then a top height defined by the user in common_irreg_geom.f is
c-----added to the list.

      implicit none

      include '../input/fixed/common.f'
      include '../input/fixed/common_irreg_geom.f'
	include 'virt_xsect.inc'


      integer
     &     channo               ! dsm channel number
     &     ,secno               ! number of current cross-section (not within chan)
     &     ,vsecno              ! number of virtual xsect within chan. (upstream=1)
     &     ,i                   ! do loop counters
     &     ,j
     &     ,k
     &     ,virtelev            ! number of layer (within xsect)
     &     ,numvirtelev         ! # of virt elev. in current chan., incl. duplicates
     &     ,xs                  ! rectangular cross-section number
     &     ,num_layers_total    ! total number of layers assigned to this channel



c-----function required by qsort function
      external compar
      integer*2 compar



c-----check array bounds
      if(irreg_geom_assg(channo).num_sec_assg .gt. max_assg_sec) then
         write(unit_error,*) 'Too many irregular cross-sections assigned
     &        to channel ',channo
      endif

c-----count number of elevations assigned to channel (total number of points
c-----in all irregular cross-sections).  Add 2 to the number (elevations are
c-----added at 100ft and at MSL) and compare to max_elevations.  Print
c-----error message if too large

      num_layers_total=0
      do i=1,irreg_geom_assg(channo).num_sec_assg
         secno = irreg_geom_assg(channo).sec_index(i)
         num_layers_total=num_layers_total+irreg_geom(secno).num_elev
      enddo

      if(num_layers_total+2*irreg_geom_assg(channo).num_sec_assg
     &     .gt. max_elevations) then
         call array_bounds_exceeded(channo)
      endif

c-----combine all elev (for all real xsect in the chan) including duplicates
      k=1
      do i=1,irreg_geom_assg(channo).num_sec_assg
         secno=irreg_geom_assg(channo).sec_index(i)
         do j=1,irreg_geom(secno).num_elev
            temp_all_elev(k)=irreg_geom(secno).elevation(j)
            k=k+1
         enddo
      enddo
c-----if first and last cross-sec are not at nodes or outside channel, 
c-----assign elev for rectangular section
      if (irreg_geom_assg(channo).num_sec_assg .eq. 0) then
         temp_all_elev(k)=0.0
         k=k+1
         temp_all_elev(k)=max_layer_height
         k=k+1
      endif
      numvirtelev=k-1
      
c-----add top height
      if (temp_all_elev(numvirtelev) .lt. max_layer_height) then
         temp_all_elev(numvirtelev+1)=max_layer_height
         numvirtelev=numvirtelev+1
      endif

c-----add elevations at MSL to compare areas at MSL to rectangular area @ MSL
      do i=1,irreg_geom_assg(channo).num_sec_assg
         secno=irreg_geom_assg(channo).sec_index(i)
c--------don't add elev if the bottom elev is above MSL
         if (irreg_geom(secno).min_elev .le. 0) then
            temp_all_elev(numvirtelev+1)=-irreg_geom(secno).min_elev
            numvirtelev = numvirtelev+1
         endif
      enddo
      if (irreg_geom_assg(channo).num_sec_assg .ge. 1) then
         if (irreg_geom_assg(channo).dist(1) .gt. 0.0) then
            xs=chan_geom(channo).xsect(1)
            if (xsect_geom(xs).botelv .le. 0) then
               temp_all_elev(numvirtelev+1)=-xsect_geom(xs).botelv
               numvirtelev = numvirtelev+1
            endif
         endif
         if (irreg_geom_assg(channo).dist(irreg_geom_assg(channo).
     &        num_sec_assg) .lt. chan_geom(channo).length) then
            xs=chan_geom(channo).xsect(2)
            if (xsect_geom(xs).botelv .le. 0) then
               temp_all_elev(numvirtelev+1)=-xsect_geom(xs).botelv
               numvirtelev = numvirtelev+1
            endif
         endif
      elseif (irreg_geom_assg(channo).num_sec_assg .eq. 0) then
         xs=chan_geom(channo).xsect(1)
         if (xsect_geom(xs).botelv .le. 0) then
            temp_all_elev(numvirtelev+1)=-xsect_geom(xs).botelv
            numvirtelev = numvirtelev+1
         endif
         if (xsect_geom(xs+1).botelv .le. 0) then
            temp_all_elev(numvirtelev+1)=-xsect_geom(xs+1).botelv
            numvirtelev = numvirtelev+1
         endif
      endif

c-----sort list of elevations in the channel:
c-----ascending order with zeros at end (see function compar)
      call qsort(temp_all_elev,max_elevations,REAL_PRECISION,compar)

c-----copy unique values onto virt_elevation()
      virt_elevation(elev_index(channo))=0.0
      j=0
      do i=1,numvirtelev
         if ( temp_all_elev(i) .ne.
     &        virt_elevation(elev_index(channo)+j) ) then
            virt_elevation(elev_index(channo)+j+1) = temp_all_elev(i)
            j=j+1
         endif
      enddo
      num_layers(channo)=j

      return
      end

      subroutine first_interpolation(channo)

c-----This is the first of three stages of interpolation.  The first two stages
c-----occur before the run begins.  ALL ELEVATIONS ARE NOW HEIGHTS OF LAYERS.
c-----A layer consists of all cross-section properties in a cross-section that have
c-----the same elevation (or height).
c-----The first interpolation uses the list of all unique heights in the channel
c-----(which is made in combine_elevations) to interpolate cross-section
c-----properties within each real cross-section.  Cross-section properties are
c-----extrapolated if the maximum height of any layer in the channel is greater than
c-----the maximum height in the current cross-section.  Extrapolation is done by
c-----calculating a top width using the user-specified value levee_slope, which is
c-----rise over run.

      implicit none

      include '../input/fixed/common.f'
      include '../input/fixed/common_irreg_geom.f'
	include 'virt_xsect.inc'


      integer
     &     channo               ! dsm channel number
     &     ,secno               ! number of current cross-section (not within chan)
     &     ,vsecno              ! number of virt xsect within channel (upstream=1)
     &     ,rsecno              ! number of temporary real cross-section
     &     ,i                   ! loop counter
     &     ,assgindex           ! the index of the arrays in the irreg_geom str.
     &     ,virtelev            ! number of layer (within xsect)
     &     ,eindex              ! function:  index of elevation array
     &     ,ei                  ! stores value of eindex

      REAL*8
     &     height

c-----function required by qsort function
      external compar
      integer*2 compar
c-----statement function to calculate indices of virtual data arrays
      eindex(channo,virtelev)
     &     =elev_index(channo)+virtelev-1

c-----loop through all assigned sections
      do i=1,irreg_geom_assg(channo).num_sec_assg
         secno=irreg_geom_assg(channo).sec_index(i)
         assgindex=1
         do virtelev=1,num_layers(channo)
c-----------if the elevation matches, copy data, don't interpolate
            ei = eindex(channo,virtelev)
            if ( virt_elevation(ei) .eq.
     &           irreg_geom(secno).elevation(assgindex) ) then
               temparea(i,virtelev)=irreg_geom(secno).area(assgindex)
               tempwet_p(i,virtelev)=irreg_geom(secno).wet_p(assgindex)
               tempwidth(i,virtelev)=irreg_geom(secno).width(assgindex)
               if (virt_elevation(ei).eq.0) then
                  tempz_centroid(i,virtelev)=0
               else
                  tempz_centroid(i,virtelev)=
     &                 irreg_geom(secno).z_centroid(assgindex)
               endif
               assgindex=assgindex+1
c--------------if the elevation doesn't match, interpolate or extrapolate
            elseif ( virt_elevation(ei) .ne.
     &              irreg_geom(secno).elevation(assgindex) ) then
               call interp_width(
     &              secno
     &              ,channo
     &              ,virtelev
     &              ,tempwidth(i,virtelev-1)
     &              ,tempwidth(i,virtelev))
               height=virt_elevation(ei)-virt_elevation(ei-1)
               call interp_area(
     &              temparea(i,virtelev-1)
     &              ,tempwidth(i,virtelev-1)
     &              ,tempwidth(i,virtelev)
     &              ,height
     &              ,temparea(i,virtelev))
               call interp_wet_p(
     &              secno
     &              ,channo
     &              ,virtelev
     &              ,tempwet_p(i,virtelev))
               rsecno=i
                if (virt_elevation(ei).eq.0) then
                   write(unit_error,*) 'Software error 1 in virtual_xsect.f'
                   write(unit_error,*) 'channel, secno, virtelev=',
     &                  channo,secno, virtelev
                   write(unit_error,*) 
     &                  'There may be too many layers in this channel'
                 call exit(2)
                else
                  call interp_z_centroid(
     &                 rsecno
     &                 ,secno
     &                 ,channo
     &                 ,assgindex
     &                 ,virtelev
     &                 ,tempz_centroid(i,virtelev))
                  assgindex=assgindex+1
                endif
            endif
         enddo
      enddo
c-----assign rectangular xsect if none available for interpolation at chan. ends
      if (irreg_geom_assg(channo).num_sec_assg .eq. 0) then
         call assgrect(channo,chan_up)
         call assgrect(channo,chan_down)
      endif

      return
      end

      subroutine second_interpolation(channo)

c-----This is the second of three stages of interpolation.  The first two stages
c-----occur before the run begins.  ALL ELEVATIONS ARE NOW HEIGHTS OF LAYERS.
c-----A layer consists of all cross-section properties in a cross-section that have
c-----the same height.
c-----The second interpolation makes the virtual cross-sections which are used by
c-----the model.  The cross-section properties are interpolated in the
c-----x direction (along the length of the channel) to make virtual cross-sections
c-----at each computational point.  Computational points are located at the ends of
c-----the channel and at distances along the channel length defined by (deltax/2).

      implicit none

      include '../input/fixed/common.f'
      include '../input/fixed/common_irreg_geom.f'

      integer
     &     channo               ! dsm channel number
     &     ,vsecno              ! number of virt xsect within channel (upstream=1)

      REAL*8
     &     dx_r                ! stores value of deltax_requested


c-----find the virtual deltax (requested value divided by 2)
      if (deltax_requested .eq. 0) then
         dx_r = chan_geom(channo).length
      elseif (deltax_requested .ne. 0) then
         dx_r = deltax_requested
      endif

c-----find number of virtual cross-sections that will be assigned to the channel
      if (float(chan_geom(channo).length) .le. dx_r) then
         num_virt_sec(channo) = 3
      elseif (float(chan_geom(channo).length) .gt. dx_r) then
         num_virt_sec(channo) =
     &        2* int( float(chan_geom(channo).length) /dx_r) +1
      endif

      if (num_virt_sec(channo) .lt. 3) then
         write (unit_error,*) 'Error:  num_virt_sec(channo) < 3.'
      endif
      virt_deltax(channo) = dfloat(chan_geom(channo).length) /
     &     (num_virt_sec(channo)-1)

c-----if the current channel is not the last channel number, calculate indices
      if (channo+1 .le. max_channels) then
         chan_index(channo+1) = chan_index(channo)
     &        + num_layers(channo) * num_virt_sec(channo)
         minelev_index(channo+1) = minelev_index(channo)+num_virt_sec(channo)
      endif

c-----interpolate bottom elevations and all cross-section properties in the
c-----horizontal direction
      call interp_botelv_horizontal(channo)
      call interp_xs_prop_horizontal(channo)

      return
      end

      subroutine interp_botelv_horizontal(channo)

c-----Bottom elevations--interpolate from 2 nearest xsect.  xsects at channel ends
c-----could be outside channel (dist could be < 0 or > channel length).  If no xsect
c-----upstream or downstream of virtual xsect, then use rectangular bottom elev.
c-----upstream end

      implicit none

      include '../input/fixed/common.f'
      include '../input/fixed/common_irreg_geom.f'
	include 'virt_xsect.inc'


      REAL*8
     &     interp              ! interpolation function

      integer
     &     channo               ! dsm channel number
     &     ,i                   ! do loop counter
     &     ,vsecno              ! number of virt xsect within channel (upstream=1)
     &     ,upindex             ! index of temporary data arrays:  upstream sec
     &     ,downindex           ! index of temporary data arrays:  downstream sec
     &     ,num_sec             ! number of real cross-sections in current chan
     &     ,xs                  ! rectangular cross-section number

c-----statement functions to interpolate and extrapolate wrt two points
      interp(x1,x2,y1,y2,x) =-((y2-y1)/(x2-x1))*(x2-x) + y2

      x=0
      if (irreg_geom_assg(channo).rect(1)) then
         xs=chan_geom(channo).xsect(1)
         virt_min_elev(minelev_index(channo)) = xsect_geom(xs).botelv
      else
         x1=irreg_geom_assg(channo).dist(1)
         y1=irreg_geom(irreg_geom_assg(channo).sec_index(1)).min_elev
         x2=irreg_geom_assg(channo).dist(2)
         y2=irreg_geom(irreg_geom_assg(channo).sec_index(2)).min_elev
         virt_min_elev(minelev_index(channo)) = interp(x1,x2,y1,y2,x)
      endif
c-----downstream end
      x=float(chan_geom(channo).length)
      num_sec=irreg_geom_assg(channo).num_sec_assg
      vsecno=num_virt_sec(channo)
      if (irreg_geom_assg(channo).rect(irreg_geom_assg(channo).num_sec_assg)) then
         xs=chan_geom(channo).xsect(2)
         virt_min_elev(minelev_index(channo)+vsecno-1) = xsect_geom(xs).botelv
      else
         if ( irreg_geom_assg(channo).dist(num_sec) .lt. 
     &        chan_geom(channo).length) then
            xs=chan_geom(channo).xsect(2)
            virt_min_elev(minelev_index(channo)+vsecno-1) = xsect_geom(xs).botelv
         elseif(irreg_geom_assg(channo).dist(num_sec) .ge.
     &           chan_geom(channo).length) then
            x1=irreg_geom_assg(channo).dist(num_sec-1)
            x2=irreg_geom_assg(channo).dist(num_sec)
            y1=irreg_geom(irreg_geom_assg(channo).sec_index(num_sec-1)).min_elev
            y2=irreg_geom(irreg_geom_assg(channo).sec_index(num_sec)).min_elev
            virt_min_elev(minelev_index(channo)+vsecno-1) = interp(x1,x2,y1,y2,x)
         endif
      endif

c-----interpolate interior virtual xsect bottom elevations
      do vsecno=2,num_virt_sec(channo)-1
         x = (float(vsecno)-1)*(virt_deltax(channo))
         num_sec=irreg_geom_assg(channo).num_sec_assg
         upindex=0
         downindex=0
         do i=1,num_sec
            if ( irreg_geom_assg(channo).dist(i) .le. x .and. 
     &           .not. irreg_geom_assg(channo).rect(i) ) upindex=i
         enddo
         do i=num_sec,1,-1
            if ( irreg_geom_assg(channo).dist(i) .ge. x .and. 
     &           .not. irreg_geom_assg(channo).rect(i) ) downindex=i
         enddo
c--------if there is a xsect with dist=x, then both indices will be the same
         if  ( (upindex .eq. downindex) .and. 
     &        (upindex .ge. 1) .and. (downindex .ge. 1) ) then
            virt_min_elev(minelev_index(channo)+vsecno-1) = 
     &           irreg_geom(irreg_geom_assg(channo).sec_index(upindex)).min_elev
         else
            if (upindex .eq. 0) then
               x1=0
               xs=chan_geom(channo).xsect(1)
               y1=xsect_geom(xs).botelv
            elseif (upindex .gt. 0) then
               x1=irreg_geom_assg(channo).dist(upindex)
               y1=irreg_geom(irreg_geom_assg(channo).sec_index(upindex)).min_elev
            endif
            if (downindex .eq. 0) then
               x2=float(chan_geom(channo).length)
               xs=chan_geom(channo).xsect(2)
               y2=xsect_geom(xs).botelv
            elseif (downindex .gt. 0) then
               x2=irreg_geom_assg(channo).dist(downindex)
               y2=irreg_geom(irreg_geom_assg(channo).sec_index(downindex)).min_elev
            endif
            virt_min_elev(minelev_index(channo)+vsecno-1) = interp(x1,x2,y1,y2,x)
         endif
      enddo

      return
      end

      subroutine interp_xs_prop_horizontal(channo)

c-----interpolate cross-section properties in the x direction

      implicit none

      include '../input/fixed/common.f'
      include '../input/fixed/common_irreg_geom.f'
      include 'virt_xsect.inc'

      integer
     &     channo               ! dsm channel number
     &     ,i                   ! do loop counter
     &     ,vsecno              ! number of virt xsect within channel (upstream=1)
     &     ,virtelev            ! number of the current layer (within xsect)
     &     ,upindex             ! index of temporary data arrays:  upstream sec
     &     ,downindex           ! index of temporary data arrays:  downstream sec
     &     ,dindex              ! function:  index of permanent data arrays
     &     ,di                  ! stores value of dindex
     &     ,num_sec             ! number of real cross-sections in current chan

      REAL*8
     &     y3           ! extra interpolation variables
     &     ,y4
     &     ,y5
     &     ,y6
     &     ,y7
     &     ,y8
     &     ,interp              ! interpolation function
     &     ,extrap              ! extrapolation function

c-----statement functions to interpolate and extrapolate wrt two points
      interp(x1,x2,y1,y2,x) =-((y2-y1)/(x2-x1))*(x2-x) + y2
      extrap(x1,x2,y1,y2,x) = ((y2-y1)/(x2-x1))*(x-x1) + y1
c-----statement function to calculate indices of virtual data arrays
      dindex(channo,vsecno,virtelev)
     &     =chan_index(channo) + (vsecno-1)*num_layers(channo) + virtelev-1

      do vsecno=1, num_virt_sec(channo)
         do virtelev=1,num_layers(channo)
c-----------find upindex, downindex,x1,x2,y1-8 
            x = (dfloat(vsecno)-1)*(virt_deltax(channo))
            num_sec=irreg_geom_assg(channo).num_sec_assg
            upindex=0
            downindex=0
            do i=1,num_sec
               if (irreg_geom_assg(channo).dist(i) .le. x) upindex=i
            enddo
            do i=num_sec,1,-1
               if (irreg_geom_assg(channo).dist(i) .ge. x) downindex=i
            enddo
c-----------print error if upstream or downstream xsect not found
            if (upindex .le. 0) then
               write(unit_error,*) 'Error in second xsect property interpolation: 
     &              channel,vsecno,virtelev,upindex'
     &              ,channo,vsecno,virtelev,upindex
               call exit(2)
            elseif (downindex .le. 0) then
               write(unit_error,*) 'Error in second xsect property interpolation:  
     &              channel,vsecno,virtelev,downindex='
     &              ,channo,vsecno,virtelev,downindex
               call exit(2)
            endif

            x1=irreg_geom_assg(channo).dist(upindex)
            x2=irreg_geom_assg(channo).dist(downindex)

            if ( (x .lt. 0) .or. (x .gt. float(chan_geom(channo).length)) ) then
               write(unit_error,*)
     &              'Desired cross-section is outside channel ',
     &              int2ext(channo)
               call exit(2)
            endif
            
            y1=tempwidth(upindex,virtelev)
            y2=tempwidth(downindex,virtelev)
            y3=temparea(upindex,virtelev)
            y4=temparea(downindex,virtelev)
            y5=tempwet_p(upindex,virtelev)
            y6=tempwet_p(downindex,virtelev)
            y7=tempz_centroid(upindex,virtelev)
            y8=tempz_centroid(downindex,virtelev)
c-----------calculate xsect property array index for current layer
            di = dindex(channo,vsecno,virtelev)
            if (x .le. x2) then
               if ( (x1 .eq. x2) .and. (upindex .gt. 0) .and. 
     &              (downindex .gt. 0) )then
                  virt_width(di)=y1
                  virt_area(di)=y3
                  virt_wet_p(di)=y5
                  virt_z_centroid(di)=y7
               else
                  virt_width(di)=interp(x1,x2,y1,y2,x)
                  virt_area(di)=interp(x1,x2,y3,y4,x)
                  virt_wet_p(di)=interp(x1,x2,y5,y6,x)
                  virt_z_centroid(di)=interp(x1,x2,y7,y8,x)
               endif
            elseif (x .gt. x2) then
               write(unit_error,*)
     &              'Should not be extrapolating in the X direction!'
               virt_width(di)=extrap(x1,x2,y1,y2,x)
               virt_area(di)=extrap(x1,x2,y3,y4,x)
               virt_wet_p(di)=extrap(x1,x2,y5,y6,x)
               virt_z_centroid(di)=extrap(x1,x2,y7,y8,x)
            endif
         enddo
      enddo

      return
      end

      subroutine interp_width(secno,channo,virtelev,previous_width,w)

c-----This subroutine interpolates or extrapolates width values in the vertical
c-----direction.

      implicit none

      include '../input/fixed/common.f'
      include '../input/fixed/common_irreg_geom.f'
	include 'virt_xsect.inc'

	
      integer
     &     secno                ! real cross-section number
     &     ,channo              ! dsm channel number
     &     ,assgindex           ! index of real cross-section data arrays
     &     ,virtelev            ! number of current layer (within xsect)
     &     ,i                   ! do loop counter
     &     ,eindex              ! function: calcultes index of elevation array
     &     ,ei                  ! stores value of eindex

      REAL*8
     &     w
     &     ,interp              ! interpolation function
     &     ,previous_width      ! width of lower layer

c-----statement functions to interpolate/extrapolate wrt to two points
      interp(x1,x2,y1,y2,x) =-((y2-y1)/(x2-x1))*(x2-x) + y2


c-----statement function to calculate index of elevation array
      eindex(channo,virtelev)=elev_index(channo)+virtelev-1

c-----if the elevation index is greater than the number of elevations in the
c-----section, then extrapolate
c-----find the index of the largest real elevation which is <= current virt elev
      ei = eindex(channo,virtelev)
c-----increment assgindex until the real elevation is le the current virt elev.
      assgindex=1
      do i=1,irreg_geom(secno).num_elev
         if ( irreg_geom(secno).elevation(assgindex) .lt.
     &        virt_elevation(ei) ) then
            assgindex=assgindex+1
         endif
      enddo
      assgindex=assgindex-1
      if (assgindex .ge. irreg_geom(secno).num_elev) then
c--------calculate widths using specified levee slope
         w=2.0*(1.0/levee_slope)*(virt_elevation(ei)
     &        -virt_elevation(ei-1)) +
     &        previous_width

c--------otherwise, interpolate
      elseif (assgindex .lt. irreg_geom(secno).num_elev) then
         x1=irreg_geom(secno).elevation(assgindex)
         x2=irreg_geom(secno).elevation(assgindex+1)
         y1=irreg_geom(secno).width(assgindex)
         y2=irreg_geom(secno).width(assgindex+1)

         x=virt_elevation(ei)
         w = interp(x1,x2,y1,y2,x)
      endif

      return
      end

      subroutine interp_area(a1,b1,b2,h,a)

c-----This subroutine interpolates area values in the vertical direction.  This
c-----subroutine is always called after interp_width.

      implicit none
      include '../input/fixed/common.f'
      include '../input/fixed/common_irreg_geom.f'

      REAL*8
     &     a                    ! interpolated area
     &     ,a1                  ! area at lower layer
     &     ,b1                  ! lower base for trapezoidal area calculation
     &     ,b2                  ! upper base for trapezoidal area calculation
     &     ,h                   ! height of trapezoid

c-----to interpolate area, add previous area to area of trapezoid between current
c-----and previous layers

      a = a1+(.5*(b1+b2))*h

      return
      end

      subroutine interp_wet_p(secno,channo,virtelev,w)

c-----This subroutine interpolates or extrapolates wetted perimeter values in the 
c-----vertical direction.

      implicit none

      include '../input/fixed/common.f'
      include '../input/fixed/common_irreg_geom.f'
	include 'virt_xsect.inc'




      integer
     &     secno                ! real cross-section number
     &     ,channo              ! dsm channel number
     &     ,assgindex           ! index of real cross-section data arrays
     &     ,virtelev            ! number of layer (within xsect)
     &     ,i                   ! do loop counter
     &     ,eindex              ! function:  calculates index of elevation array
     &     ,ei                  ! stores value of eindex

      REAL*8
     &     w
     &     ,interp              ! interpolation function
     &     ,extrap              ! extrapolation function

c-----statement functions to interpolate/extrapolate wrt to two points
      extrap(x1,x2,y1,y2,x) = ((y2-y1)/(x2-x1))*(x-x1) + y1
      interp(x1,x2,y1,y2,x) =-((y2-y1)/(x2-x1))*(x2-x) + y2
c-----statement function to calculate index of elevation array
      eindex(channo,virtelev)
     &     =elev_index(channo)+virtelev-1

c-----if only one elevation (layer), then return that value
      if (irreg_geom(secno).num_elev .eq. 1) then
         w=irreg_geom(secno).wet_p(1)
         return
      endif

c-----if the elevation index is greater than the number of elevations in the
c-----section, then extrapolate
c-----find the index of the largest real elevation which is <= current virt elev
      ei = eindex(channo,virtelev)
c-----decrement assgindex until the real elevation is le the current virt elev.
      assgindex=1
      do i=1,irreg_geom(secno).num_elev
         if ( irreg_geom(secno).elevation(assgindex) .lt.
     &        virt_elevation(ei) )then
            assgindex=assgindex+1
         endif
      enddo
      assgindex=assgindex-1
c-----if it's outside, extrapolate
      if (assgindex .ge. irreg_geom(secno).num_elev) then
         x1=irreg_geom(secno).elevation(assgindex-1)
         x2=irreg_geom(secno).elevation(assgindex)
         y1=irreg_geom(secno).wet_p(assgindex-1)
         y2=irreg_geom(secno).wet_p(assgindex)

         x=virt_elevation(ei)
         w = extrap(x1,x2,y1,y2,x)
c--------otherwise, interpolate
      elseif (assgindex .lt. irreg_geom(secno).num_elev) then
         x1=irreg_geom(secno).elevation(assgindex)
         x2=irreg_geom(secno).elevation(assgindex+1)
         y1=irreg_geom(secno).wet_p(assgindex)
         y2=irreg_geom(secno).wet_p(assgindex+1)

         x=virt_elevation(ei)
         w = interp(x1,x2,y1,y2,x)
      endif

      return
      end

      subroutine interp_z_centroid(rsecno,secno,channo,assgindex,virtelev,zc)

c-----This subroutine interpolates z centroid values in the vertical direction.

      implicit none

      include '../input/fixed/common.f'
      include '../input/fixed/common_irreg_geom.f'
	include 'virt_xsect.inc'


      integer
     &     rsecno               ! real cross-section number
     &     ,secno
     &     ,channo              ! dsm channel number
     &     ,assgindex           ! index of real cross-section property arrays
     &     ,virtelev            ! number of layer (within xsect)
     &     ,eindex              ! function to calculate elevation array index
     &     ,ei                  ! stores value of eindex

      REAL*8
     &     zc                   ! z centroid at current elevation
     &     ,aprev               ! area at previous width
     &     ,Arect               ! area of rectangular portion of current trapezoid
     &     ,Atria               ! area of triangular portion of current trapezoid
     &     ,Cprev               ! Z centroid at previous width
     &     ,Crect               ! Z centroid of rectangular portion of current trap.
     &     ,Ctria               ! Z centroid of triangular portion of current trap.
     &     ,trap_height         ! height of current trapezoid


c-----statement function to calculate index of elevation array
      eindex(channo,virtelev)
     &     =elev_index(channo)+virtelev-1

      ei = eindex(channo,virtelev)
      trap_height=virt_elevation(ei)-virt_elevation(ei-1)
c-----decrement assgindex until the real elevation is le the current virt elev.
      do while (assgindex .gt. 1 .and.
     &     (irreg_geom(secno).elevation(assgindex) .gt. virt_elevation(ei) .or.
     &     irreg_geom(secno).elevation(assgindex) .eq. 0.0) )
         assgindex=assgindex-1
      enddo

c-----to interpolate z centroid values, find centroids and areas of
c-----rectangular and triangular portions of area between the two layers.
      Aprev = temparea(rsecno,virtelev-1)
      Arect = min(tempwidth(rsecno,virtelev),tempwidth(rsecno,virtelev-1)) *
     &     trap_height
      Atria = abs( 0.5 * (tempwidth(rsecno,virtelev)-tempwidth(rsecno,virtelev-1))
     &     * trap_height )
      Cprev = tempz_centroid(rsecno,virtelev-1)
      Crect = virt_elevation(ei) + trap_height/2.

      if ( tempwidth(rsecno,virtelev) .gt. tempwidth(rsecno,virtelev-1) ) then
         Ctria = virt_elevation(ei) + (2./3.)*trap_height
      elseif ( tempwidth(rsecno,virtelev) .lt.
     &        tempwidth(rsecno,virtelev-1) ) then
         Ctria = virt_elevation(ei) + (1./3.)*trap_height
      elseif ( tempwidth(rsecno,virtelev) .eq.
     &        tempwidth(rsecno,virtelev-1) ) then
         Ctria = 0.
      endif
      zc = ((Aprev*Cprev) + (Arect*Crect) + (Atria*Ctria)) / (Aprev+Arect+Atria)

      return
      end

      integer*2 function compar(a,b)

c-----this function is required by the qsort function (a FORTRAN library function)
c-----values will be sorted in ascending order with one exception:zeros will be last

      REAL*8 a,b
      if (a .lt. b) compar=-1
      if (a .eq. b) compar= 0
      if (a .gt. b) compar= 1

      if (a .eq. 0) compar= 1
      if (b .eq. 0) compar=-1
      return
      end

      subroutine assgrect(channo,loc)

c-----This subroutine calculates cross-section properties for rectangular sections.

      implicit none

      include '../input/fixed/common.f'
      include '../input/fixed/common_irreg_geom.f'
	include 'virt_xsect.inc'


      integer
     &     channo               ! dsm channel number
     &     ,rsecno              ! number of rectangular cross-section
     &     ,sec                 ! number of cross-section in channel
     &     ,virtelev            ! number of layer (within xsect)
     &     ,eindex              ! function: calculates index of elevation array
     &     ,ei                  ! stores value of eindex
     &     ,i                   ! do loop counter
     &     ,loc                 ! upstream or downstream end of channel


c-----statement function to calculate index of elevation array
      eindex(channo,virtelev)
     &     =elev_index(channo)+virtelev-1

      if (loc .eq. chan_up) then
         sec=1
c--------increment number of assigned sections
         irreg_geom_assg(channo).num_sec_assg=
     &        irreg_geom_assg(channo).num_sec_assg + 1
         irreg_geom_assg(channo).rect(sec)=.true.
         rsecno=chan_geom(channo).xsect(1)
         do i=irreg_geom_assg(channo).num_sec_assg,2,-1
            irreg_geom_assg(channo).sec_index(i)=
     &           irreg_geom_assg(channo).sec_index(i-1)
            irreg_geom_assg(channo).dist(i)=
     &           irreg_geom_assg(channo).dist(i-1)
            irreg_geom_assg(channo).original(i)=
     &           irreg_geom_assg(channo).original(i-1)
         enddo
         irreg_geom_assg(channo).original(1)=.false.
         irreg_geom_assg(channo).dist(1)=0.0

      elseif (loc .eq. chan_down) then
c--------increment number of assigned sections
         irreg_geom_assg(channo).num_sec_assg=
     &        irreg_geom_assg(channo).num_sec_assg + 1
         sec=irreg_geom_assg(channo).num_sec_assg
         if (sec .eq. 1) then
            sec=2
            irreg_geom_assg(channo).rect(sec)=.true.
         elseif (sec .ge. 2) then
            irreg_geom_assg(channo).rect(sec)=.true.
            rsecno=chan_geom(channo).xsect(chan_geom(channo).nxsect)
            irreg_geom_assg(channo).sec_index(
     &           irreg_geom_assg(channo).num_sec_assg) =
     &           irreg_geom_assg(channo).sec_index(
     &           irreg_geom_assg(channo).num_sec_assg-1)
            irreg_geom_assg(channo).dist(
     &           irreg_geom_assg(channo).num_sec_assg) =
     &           float(chan_geom(channo).length)
            irreg_geom_assg(channo).original(irreg_geom_assg(channo).
     &           num_sec_assg)=.false.
         else
            write(unit_error,*)
     &           'Subroutine assgrect called, but end of channel undefined'
            call exit(2)
         endif
      endif

c-----use width to calculate area and wetted perimeter
      do virtelev=1,num_layers(channo)
      ei = eindex(channo,virtelev)
         tempwidth(sec,virtelev)=xsect_geom(rsecno).width
         temparea(sec,virtelev)=tempwidth(sec,virtelev)*virt_elevation(ei)
         tempwet_p(sec,virtelev)=tempwidth(sec,virtelev)+
     &        2*(virt_elevation(ei))
         tempz_centroid(sec,virtelev)=virt_elevation(ei) / 2
      enddo

      return
      end

      subroutine virt_output

c-----This subroutine prints a list of all cross-section properties for all layers
c-----of all virtual cross-sections.

      implicit none
      include '../input/fixed/common.f'
      include '../input/fixed/common_irreg_geom.f'

      integer
     &     channo               ! dsm channel number
     &     ,vsecno              ! number of cross-section within channel
     &     ,dindex              ! function: calculates index of data arrays
     &     ,di                  ! stores value of dindex
     &     ,virtelev            ! number of layer (within xsect)
     &     ,eindex              ! function: calculates index of elevation array
     &     ,ei                  ! stores value of eindex
     &     ,mindex              ! function: calculates index of min. elev. array
     &     ,mi                  ! stores value of mindex

c-----statement function to calculate indices of virtual data arrays
      dindex(channo,vsecno,virtelev)
     &     =chan_index(channo) + (vsecno-1)*num_layers(channo) + virtelev-1
c-----statement function to calculate index of elevation array
      eindex(channo,virtelev)
     &     =elev_index(channo)+virtelev-1
c-----statement function to calculate index of minimum elevation array
      mindex(channo,vsecno)
     &     =minelev_index(channo)+vsecno-1

      write(unit_output,*) 'VIRTUAL CROSS-SECTION LOOKUP TABLE'
      do channo=1,max_channels
         if ( (chan_geom(channo).length .gt. 0) .and.
     &        (irreg_geom_assg(channo).num_sec_assg .gt. 0) ) then
            do vsecno=1,num_virt_sec(channo)
               write(unit_output,*) 'Channel ',channo, ',Virtual Section ',vsecno
               write(unit_output,*)
     &              ' Height        Width          Area         Wet_p    Z Centroid      min_elev'
               write(unit_output,*)
     &              '---------------------------------------------------------------------------'
               mi = mindex(channo,vsecno)
               do virtelev=1,num_layers(channo)
                  di = dindex(channo,vsecno,virtelev)
                  ei = eindex(channo,virtelev)
                  write(unit_output,20)
     &                 virt_elevation(ei)
     &                 ,virt_width(di)
     &                 ,virt_area(di)
     &                 ,virt_wet_p(di)
     &                 ,virt_z_centroid(di)
     &                 ,virt_min_elev(mi)
               enddo
            enddo
         endif
      enddo
 20   format(f6.2,4x,5(f10.2,4x))

      return
      end

      subroutine check_dconveyance

c-----This subroutine checks every layer of every cross-section for
c-----negative derivative of conveyance wrt layer height.  If negative
c-----dconveyance is found, the cross-section number and elevation are
c-----printed so the user can change the cross-section.

      implicit none
      include '../input/fixed/common.f'
      include '../input/fixed/common_irreg_geom.f'

      integer
     &     channo               ! dsm channel number
     &     ,vsecno              ! number of virtual section within channel
     &     ,dindex              ! function: calculates index of data arrays
     &     ,di                  ! stores value of dindex
     &     ,virtelev            ! number of layer (within xsect)
     &     ,eindex              ! function: calculates index of elevation array
     &     ,ei                  ! stores value of eindex
     &     ,mindex              ! function: calculates index of min. elev. array
     &     ,mi                  ! stores value of mindex

      REAL*8
     &     dp0                  ! change in wetted perimeter wrt depth, below
     &     ,dz0                 ! change in depth, below current layer
     &     ,dp1                 ! change in wetted perimeter wrt depth, above
     &     ,dz1                 ! change in depth, above current layer
     &     ,condition0          ! condition for negative dk/dz, below current layer
     &     ,condition1          ! condition for negative dk/dz, above current layer
     &     ,elevation

      logical
     &     condition2           ! true if elevation is between -5 and 15
c-----statement function to calculate indices of virtual data arrays
      dindex(channo,vsecno,virtelev)
     &     =chan_index(channo) + (vsecno-1)*num_layers(channo) + virtelev-1
c-----statement function to calculate index of elevation array
      eindex(channo,virtelev)
     &     =elev_index(channo)+virtelev-1
c-----statement function to calculate index of minimum elevation array
      mindex(channo,vsecno)
     &     =minelev_index(channo)+vsecno-1

c-----this subroutine checks every virtual cross-section to see if the
c-----derivative of conveyance wrt depth is less than zero at any depth.
c-----Warnings are printed only for even-numbered cross-sections because
c-----dconveyance is only calculated for these cross-sections.  However,
c-----many even-numbered cross- sections are not real;  they are created
c-----by interpolating in the x direction.

      write(unit_error,610)
 610  format('Warning:  Some cross-sections may have negative dconveyance,'
     &     /'check output file for details.')

      write(unit_output,*) 'Warning:  The following cross-sections have negative dconveyance'
      write(unit_output,*) 'channo   vsecno    depth     min_elev   elevation    cond 0    cond 1'
      write(unit_output,*) '---------------------------------------------------------------------'
      do channo=1,max_channels
         do vsecno=1,num_virt_sec(channo)
            do virtelev=2,num_layers(channo)-1
               di = dindex(channo,vsecno,virtelev)
               ei = eindex(channo,virtelev)
               mi = mindex(channo,vsecno)
               dp0 = virt_wet_p(di)-virt_wet_p(di-1)
               dp1 = virt_wet_p(di+1)-virt_wet_p(di)
               dz0 = virt_elevation(ei)-virt_elevation(ei-1)
               dz1 = virt_elevation(ei+1)-virt_elevation(ei)

               condition0 = 5*virt_width(di) - 2*(virt_area(di)/virt_wet_p(di))*
     &              (dp0/dz0)
               condition1 = 5*virt_width(di) - 2*(virt_area(di)/virt_wet_p(di))*
     &              (dp1/dz1)
               elevation=virt_elevation(ei)+
     &              virt_min_elev(mi)

               if ( (elevation .le. -7.) .or. (elevation .ge. 15.) ) then
                  condition2=.false.
               else
                  condition2=.true.
               endif
               if ( ( (condition0 .lt. 0) .or.
     &              (condition1 .lt. 0) ) .and.
     &              (condition2) ) then
                  write(unit_output,'(i10,i10,5(f10.2))')
     &                 channo
     &                 ,vsecno
     &                 ,virt_elevation(ei)
     &                 ,virt_min_elev(mi)
     &                 ,elevation
     &                 ,condition0
     &                 ,condition1
               endif
            enddo
         enddo
      enddo

      return
      end

      subroutine compare_rect_area

c-----This subroutine is not currently being used.
c-----This subroutine compares the MSL area of virtual cross-sections to the MSL
c-----area of the rectangular cross-sections they replaced.

      implicit none

      include '../input/fixed/common.f'
      include '../input/fixed/common_irreg_geom.f'

      integer
     &     channo               ! do loop counters
     &     ,vsecno              ! number of virtual cross-section (upstream=1)
     &     ,rsecno              ! number of rectangular sec (upstr=1,downstr=2)
     &     ,virtelev            ! number of layer (within xsect)
     &     ,dindex              ! statement function to calculate data array index
     &     ,di                  ! calculated data array index
     &     ,mindex              ! function: calculates index of min. elev. array
     &     ,mi                  ! stores value of mindex

      REAL*8
     &     rect_width           ! width of rectangular section
     &     ,rect_area           ! area of rectangular section at MSL
     &     ,rect_min_elev       ! minimum elevation of rectangular section

c-----statement function to calculate index of data arrays
      dindex(channo,vsecno,virtelev)
     &     =chan_index(channo) + (vsecno-1)*num_layers(channo) + virtelev-1
c-----statement function to calculate index of minimum elevation array
      mindex(channo,vsecno)
     &     =minelev_index(channo)+vsecno-1

c-----this subroutine checks every virtual cross-section to see if the area at MSL
c-----is within the range of 0.5 to 2 times the area at MSL of the rectangular
c-----cross-section that was replaced by the virtual cross-section

      write(unit_error,620)
 620  format('Warning:  Some cross-sections may have significant differences in'
     &     ,'rectangular area(MSL) and irregular area(MSL);'
     &     /'check output file for details.')

      write(unit_output,*)
     &     'Warning:  The following cross-sections have significant differences in'
     &     ,'rectangular area(MSL) and irregular area(MSL).'
      write(unit_output,*)
     &     'channo vsecno  MSLheightI   MSLheightR   rect_area  irreg_area       ratio  rect_width'
      write(unit_output,*)
     &     '--------------------------------------------------------------------------------------'
      do channo=1,max_channels
         do vsecno=1,num_virt_sec(channo)
            mi = mindex(channo,vsecno)
            do virtelev=2,num_layers(channo)-1
               di = dindex(channo,vsecno,virtelev)
c--------------if the elevation is at MSL, compare areas.
               if ( virt_elevation(elev_index(channo)+virtelev-1) .eq.
     &              -virt_min_elev(mi) ) then
                  if (vsecno .gt. 1) then
                     rsecno=2
                  else
                     rsecno=1
                  endif
                  rect_width=xsect_geom(chan_geom(channo).xsect(rsecno)).width
                  rect_min_elev=xsect_geom(chan_geom(channo).xsect(rsecno)).botelv
                  rect_area=abs(rect_width*rect_min_elev)
                  if ( (virt_area(di) .gt. area_tolerance*rect_area) .or.
     &                 (virt_area(di) .lt. (1/area_tolerance)*rect_area) ) then
                     write(unit_output,'(i7,2x,i6,2x,2(f10.2,2x),4(f10.2,2x))')
     &                    channo
     &                    ,vsecno
     &                    ,-virt_min_elev(mi)
     &                    ,-rect_min_elev
     &                    ,rect_area
     &                    ,virt_area(di)
     &                    ,max(
     &                    virt_area(di) / rect_area
     &                    ,rect_area /virt_area(di) )
     &                    ,rect_width
                  endif
               endif
            enddo
         enddo
      enddo

      return
      end

      subroutine check_area

c-----This subroutine compares the MSL areas of virtual cross-sections within
c-----channels.  If the ratio is > a user-specified area tolerance, an error
c-----message is printed with the names of the cross-sections.

      implicit none

      include '../input/fixed/common.f'
      include '../input/fixed/common_irreg_geom.f'

      integer
     &     channo               ! do loop counters
     &     ,vsecno              ! number of virtual cross-section (upstream=1)
     &     ,virtelev            ! number of layer (within xsect)
     &     ,dindex              ! statement function to calculate data array index
     &     ,msl_area(max_assg_virtsec) ! MSL area for all virt. sec in current chan
     &     ,max_vsecno          ! number of largest xsect 
     &     ,min_vsecno          ! number of smallest xsect 
c-----output variables
      integer
     &     veindex             ! index of elevation that is at or below MSL
     &     ,di                  ! virtual xsect data index

      REAL*8
     &     max_area            ! area of largest xsect in current chan
     &     ,min_area            ! area of largest xsect in current chan
     &     ,x1                  ! interpolation variables
     &     ,x2
     &     ,b1
     &     ,b2
     &     ,a1

c-----statement function to calculate index of data arrays
      dindex(channo,vsecno,virtelev)
     &     =chan_index(channo) + (vsecno-1)*num_layers(channo) + virtelev-1

c-----this subroutine checks every real cross-section to see if the area at MSL
c-----is within the range of 0.5 to 2 times the area at MSL of the rectangular
c-----cross-section that was replaced by the virtual cross-section

      write(unit_error,620)
 620  format('Warning:  Some channels may have unacceptable changes in area(MSL),'
     &     /'check output file for details.')
      write(unit_output,*)
     &     'Warning:  The following channels have unacceptable changes in area(MSL)'
      write(unit_output,*)
     &     'based on a comparison of largest and smallest xsect in each channel.'
      write(unit_output,*)
     &     'channo  vsecnoA  vsecnoB   MSLheight1  MSLheight2    MSLarea1    MSLarea2       ratio'
      write(unit_output,*)
     &     '-------------------------------------------------------------------------------------'
      do channo=1,max_channels
         if (chan_geom(channo).length .gt. 0) then
c-----------find index of layer which is at MSL
            max_area=-901
            min_area=999999.9
            max_vsecno=-901
            min_vsecno=-901
            do vsecno=1,num_virt_sec(channo)
               virtelev=1
               do while ( elev_index(channo)+virtelev .lt. max_total_elevations .and.
     &              virtelev .le. num_layers(channo) .and.
     &              -virt_min_elev(minelev_index(channo)+vsecno-1) .ge.
     &              virt_elevation(elev_index(channo)+virtelev) )
                  virtelev=virtelev+1
               enddo
               veindex=elev_index(channo)+virtelev-1
               di=dindex(channo,vsecno,virtelev)
               x1=virt_elevation(veindex)
               x2=-virt_min_elev(minelev_index(channo)+vsecno-1)
               a1=virt_area(di)
               b1=virt_width(di)
               b2=virt_width(di+1)
               msl_area(vsecno) = a1+(0.5*(b1+b2))*(x2-x1)
               if (min_area .gt. msl_area(vsecno)) then
                  min_area = msl_area(vsecno)
                  min_vsecno = vsecno
               elseif (max_area .lt. msl_area(vsecno)) then
                  max_area = msl_area(vsecno)
                  max_vsecno = vsecno
               endif
            enddo
c-----------compare area(MSL) of all xsect--find max & min

 700        format(i7,3x,2(i6,3x),2(f10.2,2x),3(f10.2,2x))
            if (max_area .gt. min_area * area_tolerance) then
               if (min_area .gt. 0) then
                  write(unit_output,700) 
     &                 channo
     &                 ,max_vsecno
     &                 , min_vsecno
     &                 ,-virt_min_elev(minelev_index(channo)+max_vsecno-1)
     &                 ,-virt_min_elev(minelev_index(channo)+min_vsecno-1)
     &                 ,max_area
     &                 ,min_area
     &                 ,max_area/min_area
               else
                  write(unit_output,*)
     &                 'Cross-section in channel ',channo,' has min_elev > MSL.  Unable to calculate area(MSL)'
               endif
            endif
         endif
      enddo

      return
      end

      subroutine array_bounds_exceeded(channo)
      include '../input/fixed/common.f'
      include '../input/fixed/common_irreg_geom.f'

      integer channo

      write(unit_error,*) 'Too many elevations assigned to channel '
      write(unit_error,*) channo, '.  Maximum number of elevations'
      write(unit_error,*) 'allowed=',max_elevations,'. Try reducing'
      write(unit_error,*) 'the number of points in the cross-sections'
      write(unit_error,*) 'assigned to this channel'
      call exit(2)

      return
      end

