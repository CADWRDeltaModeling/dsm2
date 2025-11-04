module array_allocator
    !! Module to allocate and deallocate common arrays used in DSM2
    use grid_data, only: int2ext, chan_geom
    implicit none
contains
    subroutine allocate_chan_var(nitem)
        !! Allocate variables related to MaxChannels dynamically
        implicit none
        integer, intent(in) :: nitem

        if (not(allocated(int2ext))) allocate (int2ext(0:nitem))
        if (not(allocated(chan_geom))) allocate (chan_geom(nitem))
        return
    end subroutine

    subroutine deallocate_chan_var()
        !! Deallocate variables related to MaxChannels
        implicit none

        if (allocated(int2ext)) deallocate (int2ext)
        if (allocated(chan_geom)) deallocate (chan_geom)
        return
    end subroutine

    subroutine alloc_reservoir_connections(alloc)
        use grid_data
        use common_tide
        implicit none
        logical :: alloc
        integer i, j, iconnect
        iconnect = 0
        do i = 1, nreser
            do j = 1, res_geom(i)%nnodes
                iconnect = iconnect + 1
            end do
        end do
        nres_connect = iconnect
        if (alloc .and. .not. allocated(qresv)) then
            allocate (qresv(nres_connect))
            qresv = 0.
        end if
        if (alloc .and. .not. allocated(inst_qresv)) then
            allocate (inst_qresv(nres_connect))
            inst_qresv = 0.
        end if
        if (.not. alloc) then
            deallocate (qresv)
            deallocate (inst_qresv)
        end if
        return
    end subroutine

end module array_allocator
