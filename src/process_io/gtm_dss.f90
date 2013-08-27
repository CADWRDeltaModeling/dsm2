

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
        allocate( datain_min15(mins15, npthsin_min15) )
        allocate( datain_hour1(hrs, npthsin_hour1) )
        allocate( datain_day1(dys, npthsin_day1) )
        allocate( datain_week1(wks, npthsin_week1) )
        allocate( datain_month1(mths, npthsin_month1) )
        allocate( datain_year1(yrs, npthsin_year1) )
        allocate( datain_irr(irrs, npthsin_irr) )        
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
            
            
end module
