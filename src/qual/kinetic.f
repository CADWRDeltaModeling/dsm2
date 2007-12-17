C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.

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

      subroutine kinetic(c)

      include '../fixed/common.f'
      include '../fixed/common_qual.inc'
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

      integer i
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

	if (startprint)then
	  print*,"c1", c(2),c(3),c(4),c(5)
	  print*,"scsk1:",scsk(2),scsk(3),scsk(4),scsk(5)

	  print*,"cp", cp(2),cp(3),cp(4),cp(5)
	  print*,"scsk:",scsk(2),scsk(3),scsk(4),scsk(5)
      end if
c-----iskip is used to avoid calling heat and reading met data
c-----more than once for the same time step

      iskip = iskip+1

      call calscsk (cp)

      converged=.true.
      do  ii = 1, no_of_nonconserve_constituent
         i = nonconserve_ptr(ii)

         delta_c=0.5*(scsk(i)+scsk1(i))*dt_react
         c(i) = c1(i)+delta_c

c         if(c(i).lt.0.000001) c(i)=0

		if(startprint)then
		  	  print*,"c(2)", c(2),c(3),c(4),c(5)
	          print*,"scsk:",scsk(2),scsk(3),scsk(4),scsk(5)
	   end if
        if(c1(i).lt.0.000001) c1(i)=0.000001
c         if(c1(i).lt.0.000001) c1(i)=0.00000

c	   if(abs(c1(i)).ge.0.000001) then
          if(abs(delta_c)/c1(i) .gt. del_c)then
c-----------tolerance not met
            converged=.false.
          endif
c	   endif

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

