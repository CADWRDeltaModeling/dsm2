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

    module select_cell_ts
    
        contains
        
        !> subroutine to print out time series for selected cells into a text file
        subroutine print_select_cell(n_select_cell, &        !< number of cell selected
                                     select_cell,   &        !< selected cell no
                                     ncell,         &        !< total number of cells 
                                     infile_name,   &        !< input file name
                                     outfile_name)           !< output file name
            use gtm_precision
            implicit none            
            integer, intent(in) :: n_select_cell
            integer, intent(in) :: select_cell(n_select_cell)
            integer, intent(in) :: ncell
            character(len=*), intent(in) :: infile_name
            character(len=*), intent(in) :: outfile_name            
            real(gtm_real) :: conc(ncell)                     ! local variable
            integer :: i, j                                   ! local variable
            open(1101, file=infile_name)
            open(1102, file=outfile_name)
            do while (.true.)
                read (1101, *, end=999) (conc(i),i=1,ncell)
                write(1102,'(40f10.2)') (conc(select_cell(j)), j=1,n_select_cell)
            enddo
999         continue            
            close(1101)
            close(1102)
            return
        end subroutine
        
    end module        