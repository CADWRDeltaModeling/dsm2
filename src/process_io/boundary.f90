 
 
!> 
!>@ingroup process_io
module boundary
 
    contains
    
    !> 
    subroutine find_boundary_index(n_bound_ts, bound_index, path_index, & 
                                   num_boundary, boundary,              &
                                   num_inputpaths, pathin)
        use error_handling
        use gtm_precision
        use common_dsm2_vars, only: pathinput_t
        use common_variables, only: boundary_t
        implicit none
        integer, intent(in) :: num_boundary
        integer, intent(in) :: num_inputpaths
        type(boundary_t), intent(in) :: boundary(num_boundary)
        type(pathinput_t), intent(in) :: pathin(num_inputpaths)
        integer, intent(out) :: n_bound_ts
        integer, allocatable, intent(out) :: bound_index(:)
        integer, allocatable, intent(out) :: path_index(:) 
        integer :: bound_index_tmp(num_boundary)
        integer :: path_index_tmp(num_boundary)       
        integer :: i, j, dsm2_node_num
        
        n_bound_ts = 0
        bound_index_tmp = LARGEINT
        path_index_tmp = LARGEINT
        
        if ((num_boundary .lt. 1).or.(num_inputpaths .lt. 1)) then
            call gtm_fatal("Call this function after determining n_boun and n_inputpaths")
        else
            do i = 1, num_boundary
                do j = 1, num_inputpaths
                    read(pathin(j)%obj_name,*) dsm2_node_num
                    if ((pathin(j)%obj_type==2) .and. (dsm2_node_num==boundary(i)%dsm2_node_no)) then
                        n_bound_ts = n_bound_ts + 1
                        bound_index_tmp(n_bound_ts) = i
                        path_index_tmp(n_bound_ts) = j
                    end if
                end do
            end do 
        end if 
        allocate(bound_index(n_bound_ts)) 
        allocate(path_index(n_bound_ts))    
        call reallocate_arr(bound_index, n_bound_ts, num_boundary, bound_index_tmp)
        call reallocate_arr(path_index, n_bound_ts, num_boundary, path_index_tmp)
        return
    end subroutine     
    
    !> 
    subroutine reallocate_arr(new_array, new_dim, old_dim, old_array)
        implicit none
        integer :: old_dim              !< old_dim > new_dim
        integer :: new_dim              !< old_dim > new_dim
        integer :: old_array(old_dim)   !<
        integer :: new_array(new_dim)   !<
        integer :: i
        do i = 1, new_dim
            new_array(i) = old_array(i)
        end do
        return
    end subroutine    

end module    