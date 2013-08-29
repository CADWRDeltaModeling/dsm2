
!> This module contains dss related variables and functions
!> to allocate/deallocate arrays.
!>@ingroup process_io
module gtm_dss

    use common_dsm2_vars, only: dataqual_t
    
    !----number of input paths for each time interval
    integer :: npthsin_min15
    integer :: npthsin_hour1
    integer :: npthsin_day1
    integer :: npthsin_week1
    integer :: npthsin_month1
    integer :: npthsin_year1
    integer :: npthsin_irr
    
    
    !----each of the following should be 2 or greater 
    integer, parameter :: mins15 = 4*24*30     ! number of values in a 15MIN interval (30 days worth) 
    integer, parameter :: hrs = 24*30          ! 30 days of hourly values 
    integer, parameter :: dys = 35             ! NOTE: if you change these so that mins15 is no longer 
    integer, parameter :: wks = 5              !       the longest length block, you must resize outdata_arr 
    integer, parameter :: mths = 13            !       in wrt_outpaths.f 
    integer, parameter :: yrs = 3 
    integer, parameter :: irrs = 10 
    integer, parameter :: maxinpsize = max(mins15,hrs,dys,wks,mths,yrs,irrs)
    
    type(dataqual_t), allocatable :: datain_min15(:,:)    ! 15 minute data 
    type(dataqual_t), allocatable :: datain_hour1(:,:)    ! hourly data 
    type(dataqual_t), allocatable :: datain_day1(:,:)     ! daily data 
    type(dataqual_t), allocatable :: datain_week1(:,:)    ! weekly data 
    type(dataqual_t), allocatable :: datain_month1(:,:)   ! monthly data 
    type(dataqual_t), allocatable :: datain_year1(:,:)    ! yearly data 
    type(dataqual_t), allocatable :: datain_irr(:,:)      ! irregular data block
    
    
    !----pointer back to location in global pathinput structure
    integer, allocatable :: ptin_min15(:)
    integer, allocatable :: ptin_hour1(:)
    integer, allocatable :: ptin_day1(:)
    integer, allocatable :: ptin_week1(:)
    integer, allocatable :: ptin_month1(:)
    integer, allocatable :: ptin_year1(:)
    integer, allocatable :: ptin_irr(:)
    
    
    !----runtime variables for dss_readtvd() subroutine
    type(dataqual_t), allocatable :: last_value(:)
    integer, allocatable :: last_ndx_next(:)

    contains
  
    !> Allocate last_value array for the usage in gtm_dss_readtvd module
    subroutine allocate_last_value(npaths)
        implicit none
        integer, intent(in) :: npaths     !< number of input paths 
        allocate( last_value(npaths) )
        allocate( last_ndx_next(npaths) )
        last_ndx_next = 1
        return
    end subroutine    
    
    
    !> Deallocate last_value array for the usage in gtm_dss_readtvd module
    subroutine deallocate_last_value()
        implicit none
        deallocate( last_value )
        deallocate( last_ndx_next )
        return
    end subroutine    
    
    
    !> Allocate datain arrays for the usage in gtm_dss_main module
    subroutine allocate_datain()
        implicit none
        allocate( datain_min15(npthsin_min15, mins15) )
        allocate( datain_hour1(npthsin_hour1, hrs) )
        allocate( datain_day1(npthsin_day1, dys) )
        allocate( datain_week1(npthsin_week1, wks) )
        allocate( datain_month1(npthsin_month1, mths) )
        allocate( datain_year1(npthsin_year1, yrs) )
        allocate( datain_irr(npthsin_irr, irrs) )        
        allocate( ptin_min15(npthsin_min15))
        allocate( ptin_hour1(npthsin_hour1))
        allocate( ptin_day1(npthsin_day1))
        allocate( ptin_week1(npthsin_week1))
        allocate( ptin_month1(npthsin_month1))
        allocate( ptin_year1(npthsin_year1))
        allocate( ptin_irr(npthsin_irr))        
        return 
    end subroutine


    !> Deallocate datain arrays for the usage in gtm_dss_main module
    subroutine deallocate_datain()
        implicit none
        deallocate( datain_min15 )
        deallocate( datain_hour1 )
        deallocate( datain_day1 )
        deallocate( datain_week1 )
        deallocate( datain_month1 )
        deallocate( datain_year1 )
        deallocate( datain_irr )
        deallocate( ptin_min15 )
        deallocate( ptin_hour1 )
        deallocate( ptin_day1 )
        deallocate( ptin_week1 )
        deallocate( ptin_month1 )
        deallocate( ptin_year1 )
        deallocate( ptin_irr )         
        return
    end subroutine
    
    !> Create DSS input pathnames, check for sign change for each path
    subroutine get_dss_each_npath()
    
         use common_dsm2_vars, only: pathinput, n_inputpaths, miss_val_r
         use common_variables, only: unit_error
        
         implicit none
         integer :: p
        
         npthsin_min15 = 0
         npthsin_hour1 = 0
         npthsin_day1 = 0
         npthsin_week1 = 0
         npthsin_month1 = 0
         npthsin_year1 = 0
         npthsin_irr = 0

         do p = 1,n_inputpaths
            if (pathinput(p).no_intervals .eq. 1 .and. pathinput(p).interval .eq. '15min') then 
               npthsin_min15 = npthsin_min15 + 1
               pathinput(p).intvl_path = npthsin_min15
            else if (pathinput(p).no_intervals .eq. 1 .and. pathinput(p).interval(:5) .eq. '1hour') then
               npthsin_hour1=npthsin_hour1+1
               pathinput(p).intvl_path=npthsin_hour1 
            else if (pathinput(p).no_intervals .eq. 1 .and. pathinput(p).interval(:4) .eq. '1day') then
               npthsin_day1=npthsin_day1+1
               pathinput(p).intvl_path=npthsin_day1
            else if (pathinput(p).no_intervals .eq. 1 .and. pathinput(p).interval(:5) .eq. '1week') then
               npthsin_week1=npthsin_week1+1
               pathinput(p).intvl_path=npthsin_week1
            else if (pathinput(p).no_intervals .eq. 1 .and. pathinput(p).interval(:4) .eq. '1mon') then
               npthsin_month1=npthsin_month1+1
               pathinput(p).intvl_path=npthsin_month1
            else if ((pathinput(p).no_intervals .eq. 1 .and. pathinput(p).interval(:5) .eq. '1year') .or. &
                    pathinput(p).constant_value .ne. miss_val_r) then
               pathinput(p).no_intervals = 1
               pathinput(p).interval = 'year'
               npthsin_year1 = npthsin_year1 + 1
               pathinput(p).intvl_path = npthsin_year1
            else if (pathinput(p).interval(:3) .eq. 'ir-') then ! irregular interval
               npthsin_irr = npthsin_irr + 1
               pathinput(p).intvl_path = npthsin_irr
            else                   ! unrecognized interval
               write(unit_error,650) 'input', pathinput(p).no_intervals, trim(pathinput(p).interval)
               write(*,*) "Error in get_dss_each_npath() .intvl_path"
            endif
            call upcase(pathinput(p).path) ! convert to upper case
         end do
      
         call allocate_datain
       
         do p = 1,n_inputpaths
             if (pathinput(p).no_intervals .eq. 1 .and. pathinput(p).interval .eq. '15min') then 
                ptin_min15(npthsin_min15) = p
             else if (pathinput(p).no_intervals .eq. 1 .and. pathinput(p).interval(:5) .eq. '1hour') then
                ptin_hour1(npthsin_hour1)=p
             else if (pathinput(p).no_intervals .eq. 1 .and. pathinput(p).interval(:4) .eq. '1day') then
                ptin_day1(npthsin_day1)=p
             else if (pathinput(p).no_intervals .eq. 1 .and. pathinput(p).interval(:5) .eq. '1week') then
                ptin_week1(npthsin_week1)=p
             else if (pathinput(p).no_intervals .eq. 1 .and. pathinput(p).interval(:4) .eq. '1mon') then
                ptin_month1(npthsin_month1)=p
             else if ((pathinput(p).no_intervals .eq. 1 .and. pathinput(p).interval(:5) .eq. '1year') .or. &
                     pathinput(p).constant_value .ne. miss_val_r) then
                ptin_year1(npthsin_year1) = p
             else if (pathinput(p).interval(:3) .eq. 'ir-') then ! irregular interval
                ptin_irr(npthsin_irr) = p
             else                   ! unrecognized interval
                write(*,*) "Error in get_dss_each_npath() ptin_*"
             endif
         end do
      
 650    format(/'Unrecognized ',a,' data interval: ',i4,1x,a)
 
        return
    end subroutine  
                        
end module
