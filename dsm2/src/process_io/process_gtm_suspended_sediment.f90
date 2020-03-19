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
!>@ingroup process_io
module process_gtm_suspended_sediment

    use gtm_precision

    contains

    subroutine process_suspended_sediment_type(composition)
        use common_variables, only: n_var, constituents, n_sediment, sediment
        implicit none
        character :: composition*16

        call locase(composition)

        n_sediment = n_sediment + 1
        sediment(n_sediment)%composition = composition
        return
    end subroutine


    subroutine process_suspended_sediment_boundary(name,         &   ! boundary location name
                                                   composition,  &   ! composition
                                                   percent)          ! percentage in SSC
      use common_variables, only: n_sediment_bc, sediment_bc
      use io_utilities
      implicit none
      character :: name*32
      character :: composition*16
      real(gtm_real) :: percent

      call locase(name)
      call locase(composition)
      n_sediment_bc = n_sediment_bc + 1
      sediment_bc(n_sediment_bc)%name = name
      sediment_bc(n_sediment_bc)%composition = composition
      sediment_bc(n_sediment_bc)%percent = percent
      return
    end subroutine

end module