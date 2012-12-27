C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    The Delta Simulation Model 2 (DSM2) is free software: 
C!    you can redistribute it and/or modify
C!    it under the terms of the GNU General Public License as published by
C!    the Free Software Foundation, either version 3 of the License, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public License for more details.

C!    You should have received a copy of the GNU General Public License
C!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
C!</license>
 
      subroutine process_tide(
     &     new_tidefile,
     &     first_used_tidefile,
     &     tidefile_ndx)

c-----Process Hydro tidefile data into arrays needed by Qual.
      Use IO_Units
      use common_qual
      use common_tide
      use logging
      use runtime_data
      use grid_data      
      use network
      implicit none

      include 'param.inc'


      include 'bltm1.inc'
      include 'bltm3.inc'
      include 'bltm2.inc'

c-----arguments
      logical
     &     new_tidefile         ! true if new tidefile (INPUT)
      integer
     &     first_used_tidefile  ! first used tidefile number (INPUT)
     &     ,tidefile_ndx    ! current tidefile number (INPUT)
      integer :: iconnect

c-----local variables
      integer
     &     i,j,k                ! array indices


      logical
     &     smoothing_needed     ! smoothing needed between tidefiles

      real*8
     &     correct_fact,totv,totnewv
     &     ,totsysv
     &     ,totsysnewv

      real*8 mass_before(MAX_CONSTITUENT),mass_after(MAX_CONSTITUENT)

      integer cons_no

      ! initialize the mass_correction_factor
      do i=1,neq
         mass_correction_factor(i)=1.
      enddo

c-----smoothing is needed if a new tidefile is read

      smoothing_needed=(new_tidefile .and.
     &     tidefile_ndx-first_used_tidefile .gt. 0)
     
      if (new_tidefile) then
	   !@fixme is this the right condition? was same as smoothing_needed
         do i=1,nreser
            resvol(i)=(eresv(i)-hres(i))*ares(i)
         enddo
      endif

c--------assign flows to reservoirs. Couldn't do this before
c        in ReadReservoirData because of anothe variable called qres
c        fixme: this is incredibly confusing
         iconnect = 0
         do j=1, nreser
            do k=1, res_geom(j).nnodes
               iconnect = iconnect+1
               qres(j,k)=dble(qresv(iconnect))
            enddo
         enddo

      if (smoothing_needed) then
         IF(MASS_TRACKING)THEN
            call determine_mass_in_delta(mass_before)
         endif
         if (print_level .ge. 3) then
            write(unit_screen,*) 'Smoothing tidefile at time ',current_date
         endif

         totsysv=0.
         totsysnewv=0.0
         do n=1,nbrch
            totv=0.
            correct_fact=1.
            do k=1,ns(n)
               totv=totv+gpv(n,k)
            enddo
            totnewv=achan_avg(n) * chan_geom(n).length
            totsysv=totsysv+totv
            totsysnewv=totsysnewv+totnewv
	      if (totv .gt. 0.) then !@todo prevents a floating point error
               correct_fact=totnewv/totv
            else 
	         correct_fact=1.0
            end if
            do k=1,ns(n)
              gpv(n,k)=gpv(n,k)*correct_fact
            enddo
            do i=1,nxsec(n)
              gvu(n,i)=gvu(n,i)*correct_fact
            enddo
            if (print_level.ge.7) then
               write(unit_output,960)chan_geom(n).chan_no,totv,totnewv,correct_fact
 960           format(1P ' ADJUSTING VOL  FOR CH:',i4,' OLD V=',e14.7,
     &           ' NEW V=',e14.7,' CORRECTION FACT=',0p f10.7)
	      end if
         enddo

         if (print_level .ge. 4) then
            write(unit_screen,965) totsysv,totsysnewv,totsysnewv/totsysv
 965        format(1P ' OLD SYS V=',e14.7,' NEW SYS V=',e14.7,
     &           ' CORRECTION FACT=',0P f10.7)
         endif

c--------Now adjust the reservoirs

         do i=1,nreser
            resvol(i)=(eresv(i)-hres(i))*ares(i)
         enddo
         IF(MASS_TRACKING)THEN
            call determine_mass_in_delta(mass_after)
            do cons_no=1,neq
               mass_correction_factor(cons_no)=mass_after(cons_no)/mass_before(cons_no)
               TOTOLDCONSTIT(CONS_NO)=TOTOLDCONSTIT(CONS_NO)*mass_correction_factor(cons_no)
               TOTCONSTITENTER(CONS_NO)=TOTCONSTITENTER(CONS_NO)*mass_correction_factor(cons_no)
               TOTCONSTITEXPORT(CONS_NO)=TOTCONSTITEXPORT(CONS_NO)*mass_correction_factor(cons_no)
               TOTCONSTITCHDEP(CONS_NO)=TOTCONSTITCHDEP(CONS_NO)*mass_correction_factor(cons_no)
               TOTCONSTITPUMP(CONS_NO)=TOTCONSTITPUMP(CONS_NO)*mass_correction_factor(cons_no)
               do i=1, num_masstrack_boundary_lines
                  totmass_passed_through_boundary_line(i,CONS_NO)=
     &                 totmass_passed_through_boundary_line(i,CONS_NO)*
     &                 mass_correction_factor(cons_no)
               enddo
            enddo
         endif
      endif
      

      return
      end

      subroutine check_tidefile(dim_res,dim_chan,n_res,n_chan
     &     ,tidefile)

c-----Check Hydro tidefile for size compatibility with Qual.
      use io_units
      use grid_data
      implicit none

      include 'param.inc'
      include 'bltm1.inc'
      include 'bltm3.inc'
      include 'bltm2.inc'

c-----arguments
      integer
     &     dim_res,dim_chan     ! reservoir and channel array dimensions
     &     ,n_res,n_chan        ! reservoir and channels used in tidefile

      character*(*) tidefile    ! tidefile name

 610  format(/'Incorrect ',a,' ',a,' in tidefile:'/a
     &     /'tidefile was ',i4,', Qual uses ',i4)

      if (dim_res .ne. maxnumres) then
         write(unit_error,610) 'reservoir', 'dimension',
     &        tidefile,dim_res,maxnumres
         call exit(2)
      else if (dim_chan .ne. nobr) then
         write(unit_error,610) 'channel', 'dimension',
     &        tidefile,dim_chan,nobr
         call exit(2)
      else if (n_res .ne. nreser) then
         write(unit_error,610) 'reservoir', 'number',
     &        tidefile,n_res,nreser
         call exit(2)
      else if (n_chan .ne. nbrch) then
         write(unit_error,610) 'channel', 'number',
     &        tidefile,n_chan,nbrch
         call exit(2)
      endif

      return
      end
