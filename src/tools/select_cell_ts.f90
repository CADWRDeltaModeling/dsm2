

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