!<license>
!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!    Department of Water Resources.
!    This file is part of DSM2.
!
!    The Delta Simulation Model 2 (DSM2) is free software: 
!    you can redistribute it and/or modify
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

!> Read input buffer from *.inp 
!>@ingroup gtm_core
module buffer_input

    use common_variables
    integer :: unit_inp = 100                            ! input file unit  
    integer :: max_tbl_name_len = 20
    integer :: max_line_len = 100
    contains
    
    !> Process inp file and store all data in buffer (scalar_t, etc)
    subroutine process_file(inp_file_name)
        implicit none
        integer :: stat
        character(len=*), intent(in) :: inp_file_name   !< gtm.inp file name
        character(len=max_tbl_name_len) :: tbl_name
        open(unit_inp, file = trim(inp_file_name))
        do                
           read(unit_inp,*,iostat=stat) tbl_name
           if (stat .ne. 0) then
               exit
           else
               if (tbl_name(1:6).eq.'ENVVAR') then
                   call process_envvar()
               elseif (tbl_name(1:6).eq.'SCALAR') then
                   !call process_scalar()          !todo: not working
               end if 
           end if            
        enddo
        close(100)
        return
    end subroutine
    
    !> Process ENVVAR table
    subroutine process_envvar()
        implicit none
        character(len=max_line_len) :: line
        character(len=:), allocatable :: param, val
        do 
            read(unit_inp,'(a100)') line
            call split_str(line, " ", param, val)
            if (line(1:3).eq.'END') then
                goto 50
            elseif (line(1:13).eq.'GTMOUTDSSFILE') then
                scalar%gtmoutdssfile = trim(val)
            elseif (line(1:12).eq.'DSM2MODIFIER') then
                scalar%dsm2modifier = trim(val)
            end if
        end do
50      return  
    end subroutine
    
    !> Process SCALAR table
    subroutine process_scalar()
        implicit none
        character(len=max_line_len) :: line
        character(len=:), allocatable :: param, val
        do 
            read(unit_inp,'(a100)') line
            call split_str(line, " ", param, val)
            if (line(1:3).eq.'END') then
                goto 60
            elseif (line(1:5).eq.'title') then
                scalar%title = trim(val)
            elseif (line(1:13).eq.'run_time_step') then
                scalar%run_time_step = trim(val)                
            elseif (line(1:14).eq.'run_start_date') then
                scalar%run_start_date = trim(val)
            elseif (line(1:12).eq.'run_end_date') then
                scalar%run_end_date = trim(val)                
            elseif (line(1:14).eq.'run_start_time') then
                scalar%run_start_time = trim(val)
            elseif (line(1:12).eq.'run_end_time') then
                scalar%run_end_time = trim(val)
            elseif (line(1:8).eq.'temp_dir') then
                scalar%temp_dir = trim(val)
            else
                write(*,*) "Wrong parameter name!!!"                                             
            end if        
        end do
60      return        
    end subroutine                                                                      

   
    !> Split the line by the specified delimiter
    subroutine split_str(line_in, delimiter, param, val)
        character(len=*), intent(in) :: line_in
        character(len=*), intent(in) :: delimiter
        character(len=:), allocatable, intent(out) :: param
        character(len=:), allocatable, intent(out) :: val
        character(len=:), allocatable :: line
        integer :: pos1 = 1, pos2
        integer :: n = 0, i, istate=0
        line = trim(line_in)
        do
            pos2 = index(line(pos1:), delimiter)
            if (pos2.eq.0) then
                n = n + 1
                val = trim(line(pos1:))
                exit
            end if
            n = n + 1
            if (n.eq.1) param = trim(line(pos1:pos1+pos2-2))
            if (n.eq.2) val = trim(line(pos1:pos1+pos2-2))
            pos1 = pos2 + pos1
        end do        
        return
    end subroutine

end module