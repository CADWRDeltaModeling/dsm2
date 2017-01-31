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
!>@ingroup process_io
module read_init

    contains
    
    !> read initial condition time series
    subroutine read_init_file(init,              &
                              init_r,            &
                              restart_file_name)
        use gtm_precision
        use error_handling
        use common_variables, only: resv_geom, constituents, n_var, n_cell, n_resv, n_sediment
        implicit none
        character*(*), intent(in) :: restart_file_name   !< Restart file name
        real(gtm_real), intent(out) :: init(n_cell,n_var)  !< Initial concentration for cells
        real(gtm_real), intent(out) :: init_r(n_resv,n_var)!< Initial concentration for reservoirs
        integer :: file_unit
        integer :: nvar_r, nresv_r, ncell_r
        integer :: i, j, k, found, ncol, ncolp, d(n_var)
        real(gtm_real) :: val(n_var)
        character*32 :: name, a, b(n_var)
        logical :: file_exists
        
        init = LARGEREAL
        file_unit = 151
        inquire(file=restart_file_name, exist=file_exists)
        if (file_exists) then
            open(file_unit, file=restart_file_name)
            read(file_unit,*)
            read(file_unit,*)
            read(file_unit,*) nvar_r
            read(file_unit,*) ncell_r
            read(file_unit,*) a, (b(i),i=1,n_var)
            ncol = 0
            do i = 1, n_var                
                do j = 1, n_var
                    if(trim(b(i)).eq.trim(constituents(j)%name)) then
                        ncol = ncol + 1
                        d(ncol) = constituents(j)%conc_no
                    end if
                end do    
            end do
            rewind(file_unit)
            read(file_unit,*)
            read(file_unit,*)
            read(file_unit,*)
            read(file_unit,*)
            read(file_unit,*)                   
            do i = 1, n_var
                if (constituents(i)%simulate) then
                    write(*,*) constituents(i)%name
                end if    
            end do                      
            if (ncell_r .ne. n_cell) then
                call gtm_fatal("Error: number of cells are not consistent in restart file!")
            !elseif (nvar_r .ne. n_var) then
            !    call gtm_fatal("Error: number of constituents are not consistent in restart file!")
            else        
                do i = 1, n_cell
                    read(file_unit,*) k,(init(k,d(j)),j=1,ncol)
                end do
            end if
            read(file_unit,*) nresv_r
            read(file_unit,*)
            if (nresv_r .ne. n_resv) then
                call gtm_fatal("Error: number of reservoirs are not consistent in restart file!")
            else
                do i = 1, n_resv
                    found = 0
                    read(file_unit,*) name, (val(d(j)),j=1,ncol)
                    do k = 1, n_resv
                        if (trim(name).eq.trim(resv_geom(k)%name)) then
                            do j = 1, ncol
                                init_r(resv_geom(k)%resv_no,d(j)) = val(d(j))
                            end do
                            found = 1
                        end if
                    end do
                    if (found.eq.0) call gtm_fatal("Please check the reservoir name:"//name)
                end do
            end if
        else 
            write(*,*) "Please specify a valid file or path for restart file! Otherwise, a constant initial concentration will be used."
        end if    
        close(file_unit)
        return
    end subroutine

    !> check if there are additional constituents in initial condition file
    subroutine check_init_file(nadd,             &
                               name,             &
                               restart_file_name)
        use gtm_precision
        use error_handling
        use common_variables, only: constituents_tmp, n_var
        implicit none
        character*(*), intent(in) :: restart_file_name   !< Restart file name
        character*32, intent(out) :: name(10)            !< additional constituents, maximum 10
        integer, intent(out) :: nadd                     !< number of additional constituents
        integer :: file_unit
        integer :: nvar_r, ncell_r
        integer :: i, j, ncol
        character*32 :: a, b(30)
        logical :: file_exists

        file_unit = 151
        inquire(file=restart_file_name, exist=file_exists)
        if (file_exists) then
            open(file_unit, file=restart_file_name)
            read(file_unit,*)
            read(file_unit,*)
            read(file_unit,*) nvar_r
            read(file_unit,*) ncell_r
            read(file_unit,*) a, (b(i),i=1,nvar_r)
            ncol = 0
            name = ''
            do i = 1, n_var          
                do j = 1, nvar_r
                    if (trim(b(j)).eq.trim(constituents_tmp(i)%name)) ncol = 1
                end do
                if (ncol.eq.0 .and. constituents_tmp(i)%simulate) &
                    call gtm_fatal("Please specify initial condition for "//trim(constituents_tmp(i)%name))
            end do
            nadd = 0
            ncol = 0
            do i = 1, nvar_r              
                do j = 1, n_var
                    if(trim(b(i)).eq.trim(constituents_tmp(j)%name)) ncol = 1
                end do
                if (ncol.eq.0) then
                    write(*,*) trim(b(i))," is simulated without boundary condition given."
                    nadd = nadd + 1
                    name(nadd) = trim(b(i))
                end if
            end do
            rewind(file_unit)
        end if
        return
    end subroutine
    
end module