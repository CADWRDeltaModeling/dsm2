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

      subroutine kinetic(c)
      use common_qual
      implicit none
      include 'param.inc'
      include 'bltm3.inc'
      include 'kinetic1.inc'
      
      integer   nhigh(max_constituent)
      integer nrepeat
      real*8 c(max_constituent)
      real*8 cp(max_constituent),del_c,tol_c,delta_c
      logical converged, firsttime
      logical,save :: startprint = .false.
c-----local variables---------------------------

      real*8 c1(max_constituent),scsk1(max_constituent)
      real*8 rirdt, idtm,  nsmalldtt

      integer i,ii
      real*8 dt_rem, dt_react

c	if (julmin .eq. 50916600)startprint = .true.


      firsttime = .true.
      if (irc. eq. 1) then
c--------fixme This setup is temporary. It will be changing

         del_c=10
         del_c=del_c/100.
         tol_c=5
         tol_c=tol_c/100.
      end if

      irc = 0
      nrepeat=0
 40      continue

      if (chan_res. eq. 1) then
         if (firsttime) dt_rem = dtsub
         dt_react = dt_rem

         if (dt_react. lt. 0.05) go to 900

         call calscsk(c)
c-----reservoir kinetics

      elseif (chan_res. eq. 2) then
         dt_react = dt
         call calscsk(c)
      endif


c-----update constituent concentrations
      do  ii = 1, no_of_nonconserve_constituent
         i = nonconserve_ptr(ii)
         c1(i) = c(i)
         cp(i) = c(i) + scsk(i)*dt_react
         scsk1(i) = scsk(i)
      enddo


c-----iskip is used to avoid calling heat and reading met data
c-----more than once for the same time step

      iskip = iskip+1

      call calscsk (cp)

      converged=.true.
      do  ii = 1, no_of_nonconserve_constituent
         i = nonconserve_ptr(ii)

         delta_c=0.5*(scsk(i)+scsk1(i))*dt_react
         c(i) = c1(i)+delta_c

        if(c1(i).lt.0.000001) c1(i)=0.000001

          if(abs(delta_c)/c1(i) .gt. del_c)then
c-----------tolerance not met
            converged=.false.
          endif

         if (abs((scsk1(i))).ge.0.0001) then
            if(abs((scsk(i)-scsk1(i))/scsk1(i)). gt. 20.)then
               nhigh(i) = nhigh(i) + 1
            end if
         endif
      enddo
      if(converged) goto 78
 65   continue
      do  ii = 1, no_of_nonconserve_constituent
         i = nonconserve_ptr(ii)
         cp(i) = c(i)
      enddo
      call calscsk (cp)

c-----the next (write) line is only temporarily commented out; do not remove;
c-----needed for diagnosis related to accuracy checks
c-----write(iscr, *)'          call to SCSK repeats'

      converged=.true.
c-----nrepeat counts repetitions of 2nd call to calscsk
      nrepeat = nrepeat + 1
      if(nrepeat.gt.2) go to 77
      do  ii = 1, no_of_nonconserve_constituent
         i = nonconserve_ptr(ii)
         c(i) = c1(i)+0.5*(scsk(i)+scsk1(i))*dt_react

         if(cp(i).lt.0.000001) cp(i)=0.000001

          delta_c=abs((c(i)-cp(i))/cp(i))
 
c--------test to see if more repetitions needed (within tolerance)

         if(delta_c.gt.tol_c)then
            converged=.false.
         endif
      enddo
      if(.not.converged) goto 65 

      if (chan_res. eq. 2) go to 200 ! reservoir kinetics
      go to 78
 77   continue
c-----the next (write) line is only temporarily commented out; do not remove
c-----needed for diagnosis related to accuracy checks
c-----write(iscr,*)'tol. not met',' with 2 repeat',' reduce step'
 78   if (chan_res. eq. 2) go to 200 ! reservoir kinetics
      rirdt=int(dt_rem*100.0+0.5)
      idtm=int(dt_react*100.0+0.5)
      if (idtm.eq.rirdt) go to 100
      dt_rem = dt_rem - dt_react
      firsttime = .false.
      go to 40
 900  nsmalldtt = nsmalldtt + 1
 200  if (chan_res. eq. 2) iskip = 0

 100  return
      end

