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
!>@ingroup test_process_io

module ut_boundary

    use fruit
    use gtm_precision
    
    contains
    
    subroutine test_find_bound_index()
        use common_dsm2_vars, only: pathinput_t
        !use common_variables, only: boundary_t 
        !use boundary   
        implicit none
        !integer, parameter :: num_boundary = 3
        integer, parameter :: num_inputpaths = 7
        type(pathinput_t) :: pathin(num_inputpaths)
        !type(boundary_t) :: bound(num_boundary)
        integer :: n_bound_ts
        integer, allocatable :: path_index(:)
        !integer, allocatable :: bound_index(:)
        integer :: i
        
        path_index = LARGEINT
        !bound_index = LARGEINT
        !bound(1)%dsm2_node_no = 210
        !bound(1)%cell_no = 841
        !bound(1)%up_down = 1
        !bound(2)%dsm2_node_no = 221
        !bound(2)%cell_no = 881
        !bound(2)%up_down = 1
        !bound(3)%dsm2_node_no = 232
        !bound(3)%cell_no = 921
        !bound(3)%up_down = 0              
        
        do i = 1, num_inputpaths
            pathin(i)%obj_type = 2
        end do    
        pathin(1)%obj_name=' 200 '
        pathin(2)%obj_name=' 202 '
        pathin(3)%obj_name=' 210 '        
        pathin(4)%obj_name=' 220 ' 
        pathin(5)%obj_name=' 221 '              
        pathin(6)%obj_name=' 225 ' 
        pathin(7)%obj_name=' 231 '                    
       
        !call find_boundary_index(n_bound_ts, bound_index, path_index, & 
        !                         num_boundary, bound,                 &
        !                         num_inputpaths, pathin)
        
        !call assertEquals (dble(n_bound_ts), dble(2), weakest_eps,"problem in find_boundary_index n_bound_ts")
        !call assertEquals (dble(bound_index(1)), dble(1), weakest_eps, "problem in find_boundary_index bound_index")
        !call assertEquals (dble(path_index(1)), dble(3), weakest_eps, "problem in find_boundary_index path_index")
        !call assertEquals (dble(bound_index(2)), dble(2), weakest_eps, "problem in find_boundary_index bound_index")
        !call assertEquals (dble(path_index(2)), dble(5), weakest_eps, "problem in find_boundary_index path_index")
        return
    end subroutine
    
end module        