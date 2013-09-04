


module ut_gtm_hdf_write

    use fruit 
    
    contains
    
    subroutine test_gtm_hdf_write()
        implicit none
        call test_init_qual_hdf
        call test_write_ts_qual_hdf
        return
    end subroutine    
    
    !> Test for initializing qual tidefile
    subroutine test_init_qual_hdf()
        use common_variables, only: chan_geom, res_geom, constituents
        use gtm_hdf_write
        implicit none
        character*128 :: hdf_name       ! name of qual hdf5 file
        integer :: sim_start                        ! first write time
        integer :: sim_end                          ! last write time
        character*16 :: hdf_interval_char           ! interval
        integer :: nchans
        integer :: nres
        integer :: nconc        
        integer :: error = 0
        hdf_name = "gtm_out_hdf_test.h5"
        nchans = 3
        nres = 1
        nconc = 2
        sim_start = 44100
        sim_end = 44400
        hdf_interval_char = "15min"        
        allocate(chan_geom(nchans))
        allocate(res_geom(nres))
        allocate(constituents(nconc))
        chan_geom(1)%channel_num = 101
        chan_geom(2)%channel_num = 102
        chan_geom(3)%channel_num = 103
        res_geom(1)%name = "res_1"
        constituents(1)%name = "conc_1"
        constituents(2)%name = "conc_2"
        call init_qual_hdf(qual_hdf,          &
                           hdf_name,          &
                           nchans,            &
                           nres,              &
                           nconc,             &
                           sim_start,         &
                           sim_end,           &
                           hdf_interval_char)
        call close_qual_hdf(qual_hdf)          
        deallocate(chan_geom)        
        deallocate(res_geom) 
        deallocate(constituents)                
        return
    end subroutine
 
 
    !> Test for writing time series data to qual tidefile
    subroutine test_write_ts_qual_hdf()
        use common_variables, only: chan_geom, res_geom, constituents
        use gtm_hdf_write
        implicit none
        character*128 :: hdf_name       ! name of qual hdf5 file
        integer :: sim_start                        ! first write time
        integer :: sim_end                          ! last write time
        character*16 :: hdf_interval_char           ! interval
        integer :: nchans
        integer :: nres
        integer :: nconc        
        integer :: julmin
        integer :: error = 0
        hdf_name = "gtm_out_hdf_test.h5"
        nchans = 3
        nres = 1
        nconc = 2
        sim_start = 44100
        sim_end = 44250
        hdf_interval_char = "15min"        
        
        
        allocate(chan_geom(nchans))
        allocate(res_geom(nres))
        allocate(constituents(nconc))
        chan_geom(1)%channel_num = 101
        chan_geom(2)%channel_num = 102
        chan_geom(3)%channel_num = 103
        res_geom(1)%name = "res_1"
        constituents(1)%name = "conc_1"
        constituents(2)%name = "conc_2"
        

        call init_qual_hdf(qual_hdf,          &
                           hdf_name,          &
                           nchans,            &
                           nres,              &
                           nconc,             &
                           sim_start,         &
                           sim_end,           &
                           hdf_interval_char)
                           
        julmin = 44130                           
        call write_qual_hdf(julmin,           &
                            nchans,           &
                            nres,             &
                            nconc)      
        call close_qual_hdf(qual_hdf)          
        deallocate(chan_geom)        
        deallocate(res_geom) 
        deallocate(constituents)                
        return
    end subroutine


 
end module        