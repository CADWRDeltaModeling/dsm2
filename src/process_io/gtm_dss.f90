

module gtm_dss

    use common_dsm2_vars, only: dataqual_t
    
    integer :: npthsin_min15
    integer :: npthsin_hour1
    integer :: npthsin_day1
    integer :: npthsin_week1
    integer :: npthsin_month1
    integer :: npthsin_year1
    integer :: npthsin_irr
      
    integer, parameter :: max_inp_min = 100    ! maximum input paths for 15minute intervals
    integer, parameter :: max_inp_hour = 100   ! maximum input paths for hour intervals
    integer, parameter :: max_inp_day = 150    ! maximum input paths for day intervals
    integer, parameter :: max_inp_week = 5     ! maximum input paths for week intervals
    integer, parameter :: max_inp_month = 1000 ! maximum input paths for month intervals
    integer, parameter :: max_inp_year = 3200 ! maximum input paths for year intervals !@todo was 10, changed because of constant
    integer, parameter :: max_inp_irr = 250    ! maximum input paths for irregular intervals 
    integer, parameter :: max_out_min = 1000   ! maximum output paths for 15minute intervals 
    integer, parameter :: max_out_hour = 200   ! maximum output paths for hour intervals 
    integer, parameter :: max_out_day = 1000   ! maximum output paths for day intervals 
    integer, parameter :: max_out_week = 10    ! maximum output paths for week intervals 
    integer, parameter :: max_out_month = 200  ! maximum output paths for month intervals 
    integer, parameter :: max_out_year = 10    ! maximum output paths for year intervals      
    
    !-----each of the following should be 2 or greater 
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

    
    !--pointer back to location in global pathinput structure
    integer, allocatable :: ptin_min15(:)
    integer, allocatable :: ptin_hour1(:)
    integer, allocatable :: ptin_day1(:)
    integer, allocatable :: ptin_week1(:)
    integer, allocatable :: ptin_month1(:)
    integer, allocatable :: ptin_year1(:)
    integer, allocatable :: ptin_irr(:)

    contains
    
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
        
end module
