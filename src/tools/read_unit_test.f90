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

module read_unit_test
    
    use gtm_precision
    
    contains
    
    subroutine filelist()
        implicit none
        character(len=64) :: label(11)
        integer :: start_nx
        integer :: i
        
        !start_nx = 64
        !label(1) = 'se_uniform_advect'
        !label(2) = 'se_uniform_diffuse'
        !label(3) = 'se_uniform_react'
        !label(4) = 'se_uniform_advect_diffuse'
        !label(5) = 'se_uniform_advect_react'
        !label(6) = 'se_uniform_advect_diffuse_react'        
        !do i = 1, 6            
        !    call parse_output(label(i),start_nx) 
        !end do

        start_nx = 128
        label(1) = 'uniform_advect_remote_bc'
        label(2) = 'uniform_diffuse_remote_bc' 
        label(3) = 'uniform_react_remote_bc'  
        label(4) = 'uniform_advect_diffuse_remote_bc'
        label(5) = 'uniform_advect_react_remote_bc' 
        label(6) = 'uniform_advect_diffuse_react_remote_bc'  
        label(7) = 'advection_tidal_gaussian'
        label(8) = 'advection_tidal_sinusoidal'
        label(9) = 'advection_reaction_tidal_sinusoidal'
        label(10) = 'advection_reaction_tidal_gaussian'  
        label(11) = 'se_uniform_advect_remote_bc' 
        do i = 1, 11
            call parse_output(label(i),start_nx) 
        end do
                    
        start_nx = 32                           
        label(1) = 'advection_dispersion_zoppou'
        call parse_output(label(1),start_nx) 

        return
    end subroutine
    
    subroutine parse_output(label,start_nx) 
        implicit none
        character(len=64), intent(in) :: label
        integer, intent(in) :: start_nx
        character(len=64) :: filename, outfile
        real(gtm_real) :: time(512), val(6,512)
        integer :: i, n, nx
        
        write(outfile, "('summary_',a,'.txt')") trim(label)
        open(201,file=trim(outfile)) 
        write(201,*) label
        do n = 1, 3
        nx = start_nx*2**(n-1)
        write(filename, "(a,'_init_',i4.4,'.txt')") trim(label), nx
        open(101,file=trim(filename)) 
        do i=1, nx
            read(101,*) time(i),val(1,i)
        end do    
        write(filename, "(a,'_fourth_',i4.4,'.txt')") trim(label), nx
        open(102,file=trim(filename)) 
        do i=1, nx
            read(102,*) time(i),val(2,i)
        end do  
        write(filename, "(a,'_mid_',i4.4,'.txt')") trim(label), nx
        open(103,file=trim(filename)) 
        do i=1, nx
            read(103,*) time(i),val(3,i)
        end do  
        write(filename, "(a,'_three_fourth_',i4.4,'.txt')") trim(label), nx
        open(104,file=trim(filename)) 
        do i=1, nx
            read(104,*) time(i),val(4,i)
        end do          
        write(filename, "(a,'_end_',i4.4,'.txt')") trim(label), nx
        open(105,file=trim(filename))
        do i=1, nx
            read(105,*) time(i),val(5,i)
        end do             
        write(filename, "(a,'_solution_',i4.4,'.txt')") trim(label), nx
        open(106,file=trim(filename)) 
        do i=1, nx
            read(106,*) time(i),val(6,i)
        end do
        do i=1,nx
            write(201,'(f8.0,6f15.11)') time(i),val(1:6,i)
        end do   
        write(201,*)
        close(101)
        close(102)
        close(103)
        close(104)
        close(105)
        close(106)
        end do
        close(201)     
        return
    end subroutine
    
end module    
