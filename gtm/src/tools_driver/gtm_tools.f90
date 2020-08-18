!<license>
!    Copyright (C) 2017 State of California,
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
!@ingroup gtm_tools
program gtm_tools

    use create_restart
    use create_synth_tide
    use mass_calculate
    implicit none
    integer :: tool_no
    character(len=128) :: restart_file
    character(len=128) :: gtm_tidefile
    character(len=14) :: datetime
    
    write(*,'(//5x,a52)') "****************************************************"
    write(*,'(5x,a52)')   "*                                                  *"    
    write(*,'(5x,a52)')   "*                 DSM2-GTM TOOLS                   *"
    write(*,'(5x,a52)')   "*                                                  *"      
    write(*,'(5x,a52/)')  "****************************************************"    
    write(*,*) ""
    write(*,*) "Please select the tools you want to use:"
    write(*,*) "(1) Create restart file from DSM2-GTM tidefile"
    write(*,*) "(2) Mass calculate"
    
    read(*,*) tool_no
    if (tool_no .eq. 1) then
        write(*,*) "You select (1) Create restart file from DSM2-GTM tidefile"
        write(*,*) "Please type in output restart file name:"        
        !read(*,*) restart_file
        write(*,*) "Please type in DSM2-GTM tidefile name:"
        !read(*,*) gtm_tidefile
        write(*,*) "Please type in time to output (format: DDMMMYYYY HHMM):"
        !read(*,'(a14)') datetime
        !call create_restart_file(trim(restart_file), trim(gtm_tidefile), trim(datetime))
        !call create_restart_file("test.txt", "K:\Calibration_SSC\gtm_calib_03\test_a\test_a_gtm.h5", "01FEB2011 0900")
        !todo:: not working, not sure why. It is working in unit test.
    elseif (tool_no .eq. 2) then
        call calculate_mass("Mass_calculate_2011.txt","K:\Calibration_SSC\gtm_calib_03\test_a\test_a_gtm.h5","01OCT2010 0000","01OCT2011 0000", &
                            "K:\Calibration_SSC\gtm_calib_03\test_a\exclude_budget.txt")
        call calculate_mass("Mass_calculate_2012.txt","K:\Calibration_SSC\gtm_calib_03\test_a\test_a_gtm.h5","01OCT2011 0000","01OCT2012 0000", &
                            "K:\Calibration_SSC\gtm_calib_03\test_a\exclude_budget.txt")
        call calculate_mass("Mass_calculate_2013.txt","K:\Calibration_SSC\gtm_calib_03\test_a\test_a_gtm.h5","01OCT2012 0000","01OCT2013 0000", &
                            "K:\Calibration_SSC\gtm_calib_03\test_a\exclude_budget.txt")
        call calculate_mass("Mass_calculate_2014.txt","K:\Calibration_SSC\gtm_calib_03\test_a\test_a_gtm.h5","01OCT2013 0000","01OCT2014 0000", &
                            "K:\Calibration_SSC\gtm_calib_03\test_a\exclude_budget.txt")
        call calculate_mass("Mass_calculate_2015.txt","K:\Calibration_SSC\gtm_calib_03\test_a\test_a_gtm.h5","01OCT2014 0000","01OCT2015 0000", &
                            "K:\Calibration_SSC\gtm_calib_03\test_a\exclude_budget.txt")
    else
        write(*,*) "Please select a tool!"
    end if       
   
    stop
end program