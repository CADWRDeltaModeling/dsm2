
      subroutine read_boundary_values
      use iopath_data
      use grid_data
      implicit none

c-----read time-varying data arrays for Qual

c-----subroutine arguments

c-----common blocks

      include 'param.inc'
      include 'bltm1.inc'
      include 'bltm2.inc'

      include '../timevar/dss.inc'
      include '../timevar/readdss.inc'
      include '../timevar/writedss.inc'

      integer
     &     chan,grid,constituent_no ! array indices

      do chan=1,nobr
         do grid=1,nosc
            do constituent_no=1,max_constituent
                 gtrib(constituent_no,grid,chan)=0.0
            enddo
         enddo
      enddo

      if (npthsin_min15 .gt. 0) then
         call readtvd(max_inp_min,mins15,npthsin_min15,ptin_min15,
     &        datain_15min)
      endif

      if (npthsin_hour1 .gt. 0) then
         call readtvd(max_inp_hour,hrs,npthsin_hour1, ptin_hour1,
     &        datain_1hour)
      endif

      if (npthsin_day1 .gt. 0) then
         call readtvd(max_inp_day,dys,npthsin_day1,ptin_day1,
     &        datain_1day)
      endif

      if (npthsin_month1 .gt. 0) then
         call readtvd(max_inp_month,mths,npthsin_month1,ptin_month1,
     &        datain_1month)
      endif

      if (npthsin_year1 .gt. 0) then
         call readtvd(max_inp_year,yrs,npthsin_year1,ptin_year1,
     &        datain_1year)
      endif

      if (npthsin_irr .gt. 0) then
         call readtvd(max_inp_irr,irrs,npthsin_irr,ptin_irr,
     &        datain_irr)
      endif

      call store_values

      return
      end

      subroutine store_values
      use common_qual
      use iopath_data
      implicit none
c-----Fill time-varying data arrays for Qual

c-----common blocks

      include 'param.inc'
      include 'bltm1.inc'
      include 'bltm2.inc'
      include 'bltm3.inc'

c-----local variables

      integer
     &     ptr,i,ic             ! array indices
     &     ,intchan             ! internal channel numbering
     &     ,intnode             !  node numbering
     &     ,constituent_no      ! constituent number

      do ptr=1,ninpaths

c--------don't use input paths which are only for replacement
c--------(priority 2 or higher)

         call get_inp_data(ptr) ! get input data from buffers

c--------meteorological values
         if (pathinput(ptr).variable .eq. 'cloud') then
            cloud=pathinput(ptr).value
         else if (pathinput(ptr).variable .eq. 'dryblb') then
            dryblb=pathinput(ptr).value
         else if (pathinput(ptr).variable .eq. 'wetblb') then
            wetblb=pathinput(ptr).value
         else if (pathinput(ptr).variable .eq. 'wind') then
            wind=pathinput(ptr).value
         else if (pathinput(ptr).variable .eq. 'atmpr') then
            atmpr=pathinput(ptr).value
         else
c-----------water quality constituent
            if (pathinput(ptr).obj_type .eq. obj_node) then
               intnode=pathinput(ptr).obj_no
c--------------only the stage type boundary concentration is used later from this section;
c--------------flow BCs are handled in node_rate and res_rate
               do i=1,nstgbnd
                  if (intnode .eq. stgbnd(i).node) then
                     !downstream stage boundary node
                     if ((node_geom(intnode).Nup .eq. 0) .and. (node_geom(intnode).Ndown .eq. 1)) then
                         intchan=node_geom(intnode).downstream(1)                     
                         do ic=1,pathinput(ptr).n_consts
                             constituent_no = pathinput(ptr).const_ndx(ic)
                             gtrib(constituent_no,nxsec(intchan),intchan)=
     &                       pathinput(ptr).value
                         enddo
                     !upstream stage boundary node
                     elseif ((node_geom(intnode).Nup .eq. 1) .and. (node_geom(intnode).Ndown .eq. 0)) then
                         intchan=node_geom(intnode).upstream(1)
                         do ic=1,pathinput(ptr).n_consts
                             constituent_no = pathinput(ptr).const_ndx(ic)
                             gtrib(constituent_no,1,intchan)=
     &                       pathinput(ptr).value
                         enddo
                     endif                     
                  endif
               enddo
            endif
         endif
 100     continue
      enddo

      return
      end subroutine



