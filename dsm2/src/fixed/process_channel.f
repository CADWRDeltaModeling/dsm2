      
      
      subroutine process_channel(
     &                           extcounter,
     &                           id,     
     &                           channo,
     &                           chan_len,
     &                           chan_manning,
     &                           chan_dispersion,
     &                           chan_downnode,
     &                           chan_upnode)
     
     
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

!     todo: get rid of real*4. Time to get past the 1990s
      real*4
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
      
      subroutine process_xsect(channo,chan_fdist,xsect_id)
      use f90SQLConstants
      use f90SQL
      use grid_data
      !use constants
      use common_xsect
      use logging
      integer :: channo
      real*8  :: chan_fdist
      integer :: xsect_id
      if (chan_fdist .le. max_dist_ratio) then
        chan_fdist = 0.0d0
      endif
      if (chan_fdist .ge. (1.0-max_dist_ratio)) then
        chan_fdist = 1.0d0
      endif

c-----------------search for similar xsect distance
      if (chan_fdist .ne. 0.0d0) then
        do i=1,nirg
            if (irreg_geom(i).chan_no .eq. channo .and.
     &          irreg_geom(i).dist_ratio/chan_fdist .lt. 1.01d0 .and.
     &          irreg_geom(i).dist_ratio/chan_fdist .gt. 0.99d0) then
                exit
             endif
        enddo
      else
        do i=1,nirg
            if (irreg_geom(i).chan_no .eq. channo .and.
     &          irreg_geom(i).dist_ratio .eq. 0.0d0) then
                exit
             endif
         enddo
      endif
      if (i .le. nirg) then ! similar xsect distance found
         write(unit_error,'(a/a,i5,a,i5,i5/a,2f10.3)')
     &         'Warning in load_channel_xsects_SQL; similar xsect distance found',
     &         'Channel ', channo, ' xsect IDs ', irreg_geom(i).ID, xsectID,
     &         'distances ', irreg_geom(i).dist_ratio, chan_fdist
      endif

      nirg=nirg+1
      irreg_geom(nirg).ID = xsectID
      irreg_geom(nirg).chan_no = ext2int(channo)
      irreg_geom(nirg).dist_ratio=chan_fdist
      if (print_level .ge. 3)
     &    write(unit_screen,'(a,i10,i10,i10,i10,i10)')
     &      'Add xsect ',nirg, xsectid, channo, chan_fdist
      
      return
      end subroutine
      
      
      subroutine process_xsect_layer(xsectno,elev,area,width,wetperim)
      use f90SQLConstants
      use f90SQL
      use grid_data
      use common_xsect  
      integer :: xsectno
      real*8 ::  elev,area,width,wetperim
c-----------no duplicate or deleted layers are allowed; create a new
c-----------cross section instead
      irreg_geom(xsectno).num_elev=irreg_geom(xsectno).num_elev+1
      nl=irreg_geom(xsectno).num_elev
      nl_gbl=nl_gbl+1
      irreg_geom(xsectno).elevation(nl)=elev
      irreg_geom(xsectno).min_elev=
     &       min(irreg_geom(xsectno).elevation(nl),
     &       irreg_geom(xsectno).min_elev)
      irreg_geom(xsectno).width(nl)=width
c-----------adjust area to make sure:
c-----------upper layer area=lower layer area+trapezoidal area between them 
      if (irreg_geom(xsectno).num_elev .gt. 0) then
	   if (area .lt. irreg_geom(xsectno).area(nl)) then
            write(unit_error,'(a,i5)')
     &		  "Channel areas decreasing with elevation in channel ",
     &         chan_geom(irreg_geom(xsectno).chan_no).chan_no
	         istat=-3
	         return
	   end if
	   if (width .lt. irreg_geom(xsectno-1).width(nl)) then
            write(unit_error,'(a,i5)')
     &       "Channel width decreasing with elevation in channel ",
     &        chan_geom(irreg_geom(xsectno).chan_no).chan_no
	        istat=-3
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
       if (print_level .ge. 5)
     &   write(unit_screen,'(a,4i10)') 'Add xsect layer ',
     &   nl, nl_gbl, xsectID, irreg_geom(xsectno).elevation(nl)
      
      return
      end subroutine
      




c/////////////////////////////////////////////////////////////      
c-----Order nodes in node_geom in a way that is compatible
c-----with hydro and qual. The function also changes 
c-----chan_geom.upnode and chan_geom.downnode from external to internal
      logical function order_nodes()
      use grid_data
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
         node_geom(node).upstream(node_geom(node).nup)=intchan
         chan_geom(intchan).upnode=node
c--------downstream node
         node=ext2intnode(chan_geom(intchan).downnode)
         node_geom(node).ndown=node_geom(node).ndown+1
         node_geom(node).downstream(node_geom(node).ndown)=intchan
         chan_geom(intchan).downnode=node
      enddo

      return
      end function      
      
      
  