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
      
      subroutine process_channel(
     &                           extcounter,
     &                           id,     
     &                           channo,
     &                           chan_len,
     &                           chan_manning,
     &                           chan_dispersion,
     &                           chan_upnode,
     &                           chan_downnode)
     
     
      use logging
      use grid_data     
      implicit none
      integer
     &     id
     &     ,channo              ! channel number
     &     ,prev_channo         ! track same channel numbers
     &     ,chan_len            ! channel length
     &     ,chan_downnode       ! channel downstream node
     &     ,chan_upnode         ! channel upstream node
     &     ,extcounter          ! channel count to be returned
      integer,save :: counter = 0


      real*8
     &     chan_manning
     &     ,chan_dispersion
     
      counter = nchans
      counter=counter+1
      chan_geom(counter).id=id
      chan_geom(counter).chan_no=channo
      chan_geom(counter).inUse=.true.
      chan_geom(counter).length=chan_len
      chan_geom(counter).manning=chan_manning
      chan_geom(counter).disp=chan_dispersion
                                ! These node numbers are external, and will be made
                                ! internal later in the call to order_nodes
      chan_geom(counter).downnode=chan_downnode
      chan_geom(counter).upnode=chan_upnode
      int2ext(counter)=channo
      nchans = counter
      extcounter = counter
      return 
      end subroutine
      
c=======================================      
      
      subroutine process_xsect(channo,chan_fdist,xsectid,xsectno)
      use grid_data
      use common_xsect
      use logging
      use io_units
      implicit none
      integer :: channo
      real*8  :: chan_fdist
      integer :: xsectid  ! database id
      integer,intent(out) :: xsectno  ! xsect index
      integer :: i
      integer, external :: ext2int
      integer :: prev_chan
      integer :: intchan
      if (chan_fdist .le. max_dist_ratio) then
        chan_fdist = 0.0d0
      endif
      if (chan_fdist .ge. (1.0-max_dist_ratio)) then
        chan_fdist = 1.0d0
      endif
      intchan=ext2int(channo)
      
      ! nirg has not been incremented yet it is the previous one
      if (irreg_geom(nirg).chan_no .eq. intchan) then
c-----------------search for similar xsect distance
         if (chan_fdist .eq. irreg_geom(nirg).dist_ratio) then
             ! This is not a new cross section return the old index
             xsectno = nirg
             return
         end if
      end if

      nirg=nirg+1
      irreg_geom(nirg).ID = xsectid
      irreg_geom(nirg).chan_no = intchan
      irreg_geom(nirg).dist_ratio=chan_fdist
      xsectno = nirg
      if (print_level .ge. 3)
     &    write(unit_screen,'(a,i10,i10,i10,i10,i10)')
     &      'Add xsect ',nirg, xsectid, channo, chan_fdist
      
      return
      end subroutine
      
      
      
      subroutine process_xsect_layer_full(chan_no,dist,elev,area,width,wetperim)
      implicit none
      integer :: chan_no
      integer :: xsectno
      integer :: dummy_id = 0
      integer :: nl
      real*8 ::  dist,elev,area,width,wetperim
      call process_xsect(chan_no,dist,dummy_id,xsectno)
      call process_xsect_layer(xsectno,elev,area,width,wetperim)
      return
      end subroutine
      
      subroutine process_xsect_layer(xsectno,elev,area,width,wetperim)
      use grid_data
      use logging
      use io_units
      use common_xsect
      implicit none
      integer :: xsectno
      integer :: nl
      real*8 ::  elev,area,width,wetperim
      real*8 :: prev_area,prev_width,prev_elev,calc_area
      real*8,parameter :: VERT_RESOLUTION = 0.001
      real*8,parameter :: AREA_PRECISION = 0.0001
       
	!@todo: if CSDP gets fixed, make below 0.2
      real*8,parameter :: AREA_READ_PRECISION = 10000.        
      
      
c-----------no duplicate or deleted layers are allowed; create a new
c-----------cross section instead
      irreg_geom(xsectno).num_elev=irreg_geom(xsectno).num_elev+1
      nl=irreg_geom(xsectno).num_elev
      irreg_geom(xsectno).elevation(nl)=elev
      irreg_geom(xsectno).min_elev=
     &       min(irreg_geom(xsectno).elevation(nl),
     &       irreg_geom(xsectno).min_elev)
      irreg_geom(xsectno).width(nl)=width
c-----------adjust area to make sure:
c-----------upper layer area=lower layer area+trapezoidal area between them 
      if (nl .gt. 1) then
	   prev_area = irreg_geom(xsectno).area(nl-1)
	   prev_width = irreg_geom(xsectno).width(nl-1)
	   prev_elev = irreg_geom(xsectno).elevation(nl-1)
	   if (area .lt. prev_area) then
            write(unit_error,'(a,i5)')
     &		  "Channel areas decreasing with elevation in channel ",
     &         chan_geom(irreg_geom(xsectno).chan_no).chan_no
	         call exit(-3)
	         return
	   end if
	   if (width .lt. prev_width) then
            write(unit_error,'(a,i5)')
     &       "Channel width decreasing with elevation in channel ",
     &        chan_geom(irreg_geom(xsectno).chan_no).chan_no
	        call exit(-3)
	      return
	   end if

	   calc_area=prev_area + 
     &    (elev-prev_elev)*0.5*(width+prev_width)
	   if ( abs(area - calc_area ) .gt. AREA_PRECISION) then
	      if ( abs(area - calc_area ) .gt. AREA_READ_PRECISION) then
              write(unit_error,'(a,i5,a,2f13.5)')
     &        "Area-width relationship grossly wrong in channel ",
     &        chan_geom(irreg_geom(xsectno).chan_no).chan_no,": area, calc area: ",
     &	      area, calc_area
            end if
	      area=calc_area
	   end if
       end if
       irreg_geom(xsectno).area(nl)=area
       irreg_geom(xsectno).wet_p(nl)=wetperim
	 if (wetperim .ne. 0.0d0) then
        irreg_geom(xsectno).h_radius(nl)=area/wetperim
	 else
	  irreg_geom(xsectno).h_radius(nl)=0.0d0
	 endif
! todo: log for high print level?
      
      return
      end subroutine
      




c/////////////////////////////////////////////////////////////      
c-----Order nodes in node_geom in a way that is compatible
c-----with hydro and qual. The function also changes 
c-----chan_geom.upnode and chan_geom.downnode from external to internal
      logical function order_nodes()
      use grid_data
      use io_units
      implicit none
      integer nn,n,node
      integer intchan
      integer ext2intnode
      integer compareInt
      external compareInt
      order_nodes=.true.
c     compile list of all nodes and sort them in numerical order

      nn=0

      do intchan=1,nchans
         if (chan_geom(intchan).inUse) then
            nn=nn+1
            nodelist(nn)=chan_geom(intchan).upnode
            nn=nn+1
            nodelist(nn)=chan_geom(intchan).downnode
         end if
      end do
      ! now sort
      call qsort (nodelist(1), nn, int4(4), compareInt) 
      
      node=nn
c     add internal nodes to node_geom, in order
      n=0
      do nn=1,node
         if (nodelist(nn) .ne. miss_val_i .and. ! not junk
     &        nodelist(nn) .ne. node_geom(n).node_ID .and. ! not already done
     &        (nodelist(nn) .eq. nodelist(nn+1) .or. ! internal (repeated in nodelist)
     &        nodelist(nn) .eq. nodelist(nn-1))) then
            n=n+1
            node_geom(n).node_ID=nodelist(nn)
         end if
      end do
      nintnodes=n

c     add external nodes to node_geom, in order
      do nn=1,node
         if (nodelist(nn) .ne. miss_val_i .and. ! not junk
     &        nodelist(nn) .ne. node_geom(n).node_ID .and. ! not already handled
     &        nodelist(nn) .ne. nodelist(nn+1) .and. ! external
     &        nodelist(nn) .ne. nodelist(nn-1)) then
            n=n+1
            node_geom(n).node_ID=nodelist(nn)
         end if
      end do      
      nnodes=n

c-----now repair nodelist to reflect new order
      nodelist=miss_val_i      
      do n=1,nnodes
         nodelist(n)=node_geom(n).node_ID
      end do

c-----add network connectivity of nodes and channels to node_geom and chan_geom
      do intchan=1,nchans
c--------upstream node
         node=ext2intnode(chan_geom(intchan).upnode)
         node_geom(node).nup=node_geom(node).nup+1
         if(node_geom(node).nup .gt. max_cpn)then
             write(unit_error,"(a,i)")
     &         "Too many upstream channel connections node "
     &          ,node_geom(node).node_id
             order_nodes=.false.
             return
         end if
         node_geom(node).upstream(node_geom(node).nup)=intchan
         chan_geom(intchan).upnode=node
c--------downstream node
         node=ext2intnode(chan_geom(intchan).downnode)
         node_geom(node).ndown=node_geom(node).ndown+1
         if(node_geom(node).ndown .gt. max_cpn)then
             write(unit_error,"(a,i)")
     &         "Too many downstream channel connections node "
     &          ,node_geom(node).node_id
             order_nodes=.false.
             return
         end if
         node_geom(node).downstream(node_geom(node).ndown)=intchan
         chan_geom(intchan).downnode=node
      enddo

      return
      end function      
      
      
      
      
      subroutine process_xsect_csdp(channo, fdist, filename)
c-----Transfer buffer contents from xsect to xsect layer. This routine does not
c     actually do any data processing. There are two requirements:
c     i.  it must be called after prioritize_buffers(), otherwise the items
c         added to the xsect_layer buffer here will disappear -- they have no layer number
c     ii. it must be called before process_xsect_layer gets done. Otherwise the items
c          added to the xsect_layer buffer will never be processed 
      use grid_data
      use input_storage_fortran
      use common_xsect
      use logging
      use io_units
c-----This subroutine reads in all the irregular cross sections,
c-----assigns cross-sections to channels such that each end of each channel has
c-----a rectangular or irregular cross-section.

      implicit none
c-----args
      integer :: channo              ! DSM channel number
      real*8  :: fdist
      character*128 :: filename 
      integer :: ierror = 0
c----- buffer
      type(cross_section_t) buffer


c-----local variables
      integer maxf              ! maximum number of fields in data files
      parameter (maxf=7)

      real*8  elev,area,width,wet_p
      real*8  ::dummy_xcentroid = 0.D0
      real*8  ::dummy_ycentroid = 0.D0
      real*8  ::dummy_zcentroid = 0.D0
      real*8  ::dummy_hrad = 0.D0


      integer
     &     h                    ! do loop counters
     &     ,j
     &     ,k
     &     ,m
     &     ,lnblnk              ! last nonblank intrinsic function
 
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
     &     ,istat
       integer xsectno

c--------convert distance

c      call process_xsect(channo,fdist,miss_val_i,xsectno)  ! this will cause double processing
c--------open the geometry viewer output file
      open (
     &    unit_input,
     &    file=filename,
     &    status="old",
     &    err=810
     &    )

      is_it_done = '     '
c--------go down past the headers
      do while(is_it_done .ne. '=====')
          read(
     &       unit_input,
     &        '(a5)') 
     &       is_it_done
      enddo

c--------read a line and determine column widths with FINDLM
      read (
     &    unit_input,
     &    '(a90)'
     &    )
     &    one_line

      m=0
      do while (lnblnk(one_line) .gt. 0)
         m=m+1
         if (m .gt. max_elevations)then
            write(unit_error,120)max_elevations,channo,fdist
            call exit(-2)
 120     format("Maximum number of elevations per xsect (",i4,
     &          ") exceeded in channel",1x,"Distance",1x,f8.3)
         end if
         nbeg=1
         nlen=90
         nfield=-7
         call findlm(one_line, nbeg, nlen, nfield, ibegf, ilenf,
     &       idelmt, idelmp, itbl)
c-----------parse the line into the structure using results from
c-----------FINDLM.
            cerr_msg='elevation'
c-----------here, "elevation" is wrt NGVD (i.e. same as stage); gets
c-----------changed to a depth reference later
c-----------Changed by Ganesh Pandey 04/03/00
         read(one_line(ibegf(1):ilenf(1)+ibegf(1)-1)
     &           ,'(f12.0)',err=900)buffer.elevation(m)
         cerr_msg='cross-section area'
         read(one_line(ibegf(2):ilenf(2)+ibegf(2)-1)
     &       ,'(f12.0)',err=900) buffer.area(m)
         cerr_msg='wetted perimeter'
         read(one_line(ibegf(3):ilenf(3)+ibegf(3)-1)
     &       ,'(f12.0)',err=900) buffer.wet_p(m)
         cerr_msg='width'
         read(one_line(ibegf(4):ilenf(4)+ibegf(4)-1)
     &       ,'(f12.0)',err=900) buffer.width(m)
         cerr_msg='hydraulic radius'
         read(one_line(ibegf(5):ilenf(5)+ibegf(5)-1)
     &       ,'(f12.0)',err=900) buffer.h_radius(m)
         cerr_msg='x-centroid'
         read(one_line(ibegf(6):ilenf(6)+ibegf(6)-1)
     &       ,'(f12.0)',err=900) buffer.x_centroid(m)
         cerr_msg='z-centroid'
         read(one_line(ibegf(7):ilenf(7)+ibegf(7)-1)
     &       ,'(f12.0)',err=900) buffer.z_centroid(m)
         read(unit_input,'(a)',end=850) one_line
      enddo
      buffer.num_elev = m      
      ! they were read in decreasing elevation, append in increasing
      do m=buffer.num_elev,1,-1
c-----------find minimum elevation
         call xsect_layer_append_to_buffer(channo,
     &                                     fdist,
     &                                     buffer.elevation(m),
     &                                     buffer.area(m),
     &                                     buffer.width(m),
     &                                     buffer.wet_p(m),
     &                                     ierror)

      end do
      close(unit_input)
      return                    ! normal return

 810  continue                  ! file open error
      write(unit_error, 605) trim(filename)
 605  format(/'Could not open file: ',a)
      call exit(2)

 850  continue                  ! here for premature eof
      write(unit_error, 610) trim(filename)
 610  format(/'Premature end-of-file for irregular geometry file: ',a)
      call exit(2)

 900  continue                  ! error on reading data from line
      write(unit_error, 620) trim(cerr_msg),one_line
 620  format(/'Conversion error on ', a,
     &     ' field for irregular geometry file: ', a,'one line =',a)
      call exit(2)

      end subroutine
      
  