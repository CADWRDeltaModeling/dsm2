!!<license>
!!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!!    Department of Water Resources.
!!    This file is part of DSM2.

!!    The Delta Simulation Model 2 (DSM2) is free software:
!!    you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License as published by
!!    the Free Software Foundation, either version 3 of the License, or
!!    (at your option) any later version.

!!    DSM2 is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License for more details.

!!    You should have received a copy of the GNU General Public License
!!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!!</license>

      ! Controls the integration of source terms
      ! Uses the Runge-Kutta(2,3) method of Bogacki and Shampine
      ! The method adapts in time based on an error estimate that comes from the
      ! difference of the 2nd and 3rd order result.
      ! There is an attempt at some extra protection against negative concentration
      subroutine kinetic(c)
      use common_qual
      use io_units
      implicit none
      include 'param.inc'
      include 'bltm3.inc'
      include 'kinetic1.inc'

!-----arguments---------------------------
      real*8 :: c(max_constituent)

!-----local variables---------------------------

      real*8 :: cnew(max_constituent)
      real*8 :: scsk1(max_constituent)
      real*8 :: scsk2(max_constituent)
      real*8 :: scsk3(max_constituent)
      real*8 :: scsk4(max_constituent)
      real*8, parameter :: rtol = 1.d-3  ! relative tolerance for error
      real*8, parameter :: atol = 1.d-6  ! absolute tolerance for error
      real*8, parameter :: threshold = atol/rtol
      real*8, parameter :: minstepeps = 1.6d-8  ! epsilon for step going to zero, should be d-15
                                                ! but want to see if this ever gets tripped
      real*8, parameter :: nonneg_tstep = 0.02d0  ! smallest step to try to enforce nonnegativity with time refinement
      real*8, parameter :: smallc = 1.d-12      ! very small non-negative concentration
      real*8 :: dt_rem   ! time left to react during this invocation
      real*8 :: dt_react ! total time to react this invocation (from dsm2 qual)
      real*8 :: tstep    ! time step for this reaction subcycle
      real*8 :: timept   ! relative time progress between zero and dt_react
      real*8 :: minstep  ! minimum stepsize... lower than this is failure
      real*8 :: maxstep  ! maximum stepsize... in practice, always the full dsm2 step
      real*8 :: newstep  ! calculated version of new time step, not including positivity hack
      real*8 :: errvec(max_constituent) ! vector of error estimates from the BG23 scheme
      real*8 :: err      ! max relative error
      real*8 :: trialerr ! local for calculating err
      integer i,ii
      logical all_nonnegative,ignore_nonnegative

!     Establish the amount of time to integrate.
      if (chan_res .eq. 1) then
         dt_react = dtsub  ! channels do a dsm2-qual sub-timestep based on advection/mixing
      elseif (chan_res .eq. 2) then
         dt_react = dt     ! reservoirs do whole time step
      endif
      timept = 0.d0
      dt_rem = dt_react - timept

      maxstep = dt_react
      call calscsk(c)
      scsk1 = scsk
      iskip = iskip+1   !todo: don't think this is really hooked up

!     Try to do the whole thing
      tstep=dt_react
      ignore_nonnegative = .false.

      do while(dt_rem > 0)

          !! Determine time to integrate
          minstep = dt_rem*minstepeps
          tstep = min(tstep, maxstep)
          tstep = max(tstep, minstep)
          ! Stretch the step if getting close to end
          if (1.1d0*abs(tstep) >= dt_rem)then
              tstep = dt_rem
          end if

          !! Integrate in four stages (we are on stage 2, stage 1 is
          !! done during integration or last subcycle
	    ! time = t0 + tstep/2.d0
	    cnew = c + tstep*0.5d0*scsk1

	    call calscsk(cnew)
	    scsk2 = scsk
	    ! time = t0 + 0.75d0*tstep
	    cnew = c + 0.75d0*tstep*scsk2
	    call calscsk(cnew)
	    scsk3 = scsk

	    ! tnew = t0 + tstep
	    cnew = c + tstep*(2.d0*scsk1 + 3.d0*scsk2 + 4.d0*scsk3)/9.d0
	    call calscsk(cnew)
	    scsk4 = scsk

    	    !! Error estimate
	    errvec = tstep*(-5.d0*scsk1 + 6.d0*scsk2 + 8.d0*scsk3 - 9.d0*scsk4)/72.d0
	    err = 0.d0
	    do  ii = 1, no_of_nonconserve_constituent
		      i = nonconserve_ptr(ii)
		      trialerr = errvec(i)/max(abs(cnew(i)),abs(c(i)),threshold)
		      if (trialerr > err) err = trialerr
	    end do
	    err = err+tiny(err)

          ! Check for positivity
   	    all_nonnegative = (minval(cnew) >= 0.d0)

          !! Time step adaptation
	    newstep = tstep*min(5.d0, 0.8d0*(rtol/err)**0.33d0)
	    if (all_nonnegative)then
		      tstep = newstep
	    else
		      tstep = min(newstep, tstep/2.d0)
		      if (err <= rtol .and. tstep < nonneg_tstep)then
			       ! time step is small and only because of negative value
			       ! so...floor it and move on
			      ignore_nonnegative = .true.
			      do  ii = 1, no_of_nonconserve_constituent
				      i = nonconserve_ptr(ii)
                      ! zero concentrations will be exactly zero and we don't want to
                      ! mess with those. Negative ones will have "crossed over"
				      if (cnew(i) .lt. 0.d0) cnew(i) = smallc
			      end do
			      tstep = newstep
		      end if
	    end if

          !! Advancement check
	    if (err <= rtol .and. (all_nonnegative .or. ignore_nonnegative))then
		      timept = timept + tstep
		      dt_rem = dt_react - timept
		      c = cnew
		      scsk1 = scsk4
		      ignore_nonnegative = .false.
	    end if


	    if (abs(tstep) <= minstep)then
		      write(unit_error,*) "Nonconservative reaction failed (adaptive time step too small)"
		      call exit(2)
	    end if

      end do


      if (chan_res .eq. 2) iskip = 0
 100  return
      end subroutine
