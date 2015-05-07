!<license>
!    Copyright (C) 2015 State of California,
!    Department of Water Resources.
!    This file is part of DSM2-GTM.
!
!    The Delta Simulation Model 2 (DSM2) - General Transport Model (GTM) 
!    is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    DSM2 is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!</license>

!> This module is used to open DSS file.
!>@ingroup process_io
module gtm_dss_open

    contains

    !> Process DSS input files
    subroutine opendss(ifltab,        & ! ifltab_in
                       num_dssfiles,  & ! number of dss files
                       dssfilenames)    ! dss filenames
                       
        use common_variables, only : unit_error
        implicit none
        integer, intent(in):: num_dssfiles
        character(len=130), intent(in) :: dssfilenames(num_dssfiles)
        integer, intent(out) :: ifltab(600,num_dssfiles) ! ifltab must have a length of 600 short integer words (DSS documentation, this is not an arbitrary number)
        character(len=150) :: ctmp
        logical :: lstat
        integer :: nlen, istat, i
       
        ! Open the DSS files for reading
        do i =1, num_dssfiles
            call zfname (trim(dssfilenames(i)), ctmp, nlen, lstat)
            if (.not. lstat) then
                write(unit_error, '(a/a/a)') 'Fatal error - DSS input file',  &
                                             ctmp(:nlen), 'does not exist.'
            endif
            call zopen (ifltab(1,i), trim(dssfilenames(i)), istat)
            if (istat .gt. 0) then
                 write(unit_error, '(a,a)') 'Unable to open the file ', dssfilenames(i)
            endif
        enddo    
        return
    end subroutine
    
end module        