



program gtm_tools

    use create_restart
    use create_synth_tide
    use select_cell_ts
    implicit none
    integer :: tool_no
    character(len=128) :: restart_file
    character(len=128) :: gtm_tidefile
    character(len=14) :: datetime
    integer :: n_select_cell, ncell
    integer, allocatable :: arr(:)
    
    write(*,'(//5x,a52)') "****************************************************"
    write(*,'(5x,a52)')   "*                                                  *"    
    write(*,'(5x,a52)')   "*                 DSM2-GTM TOOLS                   *"
    write(*,'(5x,a52)')   "*                                                  *"      
    write(*,'(5x,a52/)')  "****************************************************"    
    write(*,*) ""
    write(*,*) "Please select the tools you want to use:"
    write(*,*) "(1) Create restart file from DSM2-GTM tidefile"
    write(*,*) "(2) Create synthetic tidal time series for dummy tidefile"
    write(*,*) "(3) Print selected cells into a text file"
    
    read(*,*) tool_no
    if (tool_no .eq. 1) then
        write(*,*) "You select (1) Create restart file from DSM2-GTM tidefile"
        write(*,*) "Please type in output restart file name:"        
        !read(*,*) restart_file
        write(*,*) "Please type in DSM2-GTM tidefile name:"
        !read(*,*) gtm_tidefile
        write(*,*) "Please type in time to output (format: DDMMMYYYY HHMM):"
        !read(*,'(a14)') datetime
        !call create_restart_file(trim(restart_file), trim(gtm_tidefile), trim(datetime))
        !call create_restart_file("test.txt", "channel_gtm.h5", "01FEB1998 0900")
        !todo:: not working, not sure why. It is working in unit test.
        call test_create_restart
    elseif (tool_no .eq. 2) then
        call print_tidal_to_file
    elseif (tool_no .eq. 3) then
        n_select_cell = 38
        ncell = 2772 !2780 5560 !11120 !22240 
        allocate(arr(n_select_cell))
        arr = (/ 1037,580,583,552,541,508,1443,457,438,381,379,354,2154,2117,2108,2080,1943,1945,1893,290,392,1398,222,141,90,59,128,1364,1820,973,983,781,1617,1159,1767,125,1868,1482 /)
        !arr =(/ 1160,602,608,568,559,530,1545,485,459,378,376,343,2188,2155,2144,2117,2001,2002,1954,265,392,1504,205,119,72,48,102,1480,1892,1100,1112,849,1702,1289,1841,101,1933,1585 /)        
        !arr = (/ 2319,1203,1215,1136,1117,1060,3089,969,918,755,751,685,4376,4309,4287,4233,4002,4003,3907,529,783,3008,409,237,144,95,203,2960,3784,2199,2223,1697,3403,2577,3681,201,3865,3169 /)        
        call print_select_cell(n_select_cell,             &     
                               arr,                       &     
                               ncell,                     &  
                               "cell concentration.txt",  &    
                               "select_cell_conc.txt") 
    else
        write(*,*) "Please select a tool!"
    end if       
   
    stop

end program
 
    !> Routine to test creating restart text file
    subroutine test_create_restart()
        use gtm_precision
        use create_restart
        implicit none
        integer :: file_unit
        character(len=11) :: restart_filename
        real(gtm_real) :: init(64,2)
        integer :: ncell, nvar
        integer :: i, j
        
        file_unit = 131
        restart_filename = "restart.txt"
        call create_restart_file(restart_filename, "channel_gtm.h5", "01FEB1998 0900")
        open(file_unit, file = restart_filename)
        read(file_unit,*)
        read(file_unit,*)
        read(file_unit,*) nvar
        read(file_unit,*) ncell
        do i = 1, ncell
            read(file_unit,*) (init(i,j),j=1,nvar)
        end do    
        close(file_unit) 
        return
    end subroutine