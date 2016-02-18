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
!@ingroup gtm_tools
program gtm_tools

    use create_restart
    use create_synth_tide
    use select_cell_ts
    use read_unit_test
    implicit none
    integer :: tool_no
    character(len=128) :: restart_file
    character(len=128) :: gtm_tidefile
    character(len=14) :: datetime
    integer :: n_select_cell, ncell
    integer, allocatable :: arr(:)
    
    write(*,'(//5x,a52)') "****************************************************"
    write(*,'(5x,a52)')   "*                                                  *"    
    write(*,'(5x,a52)')   "*                 DSM2-GTM TOOLS                   *"
    write(*,'(5x,a52)')   "*                                                  *"      
    write(*,'(5x,a52/)')  "****************************************************"    
    write(*,*) ""
    write(*,*) "Please select the tools you want to use:"
    write(*,*) "(1) Create restart file from DSM2-GTM tidefile"
    write(*,*) "(2) Create synthetic tidal time series for dummy tidefile"
    write(*,*) "(3) Print selected cells into a text file"
    
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
        !call create_restart_file("test.txt", "channel_gtm.h5", "01FEB1998 0900")
        !todo:: not working, not sure why. It is working in unit test.
        call test_create_restart
    elseif (tool_no .eq. 2) then
        call print_tidal_to_file
    elseif (tool_no .eq. 3) then
        n_select_cell = 11
        ncell = 1722 !2780 5560 !11120 !22240 
        allocate(arr(n_select_cell))
        arr = (/ 652,764,345,294,280,1319,1304,1285,176,245,84/)
        call print_select_cell(n_select_cell,             &     
                               arr,                       &     
                               ncell,                     &  
                               "cell concentration.txt",  &    
                               "select_cell_conc.txt") 
    elseif (tool_no .eq. 4) then
        call filelist
    
    
    else
        write(*,*) "Please select a tool!"
    end if       
   
    stop

end program
 
    !> Routine to test creating restart text file
    subroutine test_create_restart()
        use gtm_precision
        use create_restart
        implicit none
        integer :: file_unit
        character(len=11) :: restart_filename
        real(gtm_real) :: init(64,2)
        integer :: ncell, nvar
        integer :: i, j
        
        file_unit = 131
        restart_filename = "restart.txt"
        call create_restart_file(restart_filename, "channel_gtm.h5", "01FEB1998 0900")
        open(file_unit, file = restart_filename)
        read(file_unit,*)
        read(file_unit,*)
        read(file_unit,*) nvar
        read(file_unit,*) ncell
        do i = 1, ncell
            read(file_unit,*) (init(i,j),j=1,nvar)
        end do    
        close(file_unit) 
        return
    end subroutine