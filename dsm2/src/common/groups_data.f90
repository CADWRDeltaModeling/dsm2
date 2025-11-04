module groups_data
    ! use constants
    use group
    implicit none

    integer :: nGroup   ! Actual number of groups calc'd at run time

    type (t_Group), dimension(0:MAX_GROUPS) :: groupArray

end module groups_data
