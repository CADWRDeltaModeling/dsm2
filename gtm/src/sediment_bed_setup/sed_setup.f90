program sed_setup
    
    use gtm_precision
    use hdf_util
    use sb_hdf_util
    use io_utilities
    use sb_subs
    !use sed_bed_hdf, only : init_bed_geom_hdf !for developing sediment bed input routines
    
implicit none
    integer :: ierror
    call h5open_f(ierror)
    call get_command_args_sb(hydro_hdf5)
    call hdf5_init(hydro_hdf5) 
    !call init_bed_geom_hdf(hydro_hdf5)  !for developing sediment bed input routine
    call get_hydro_attr_sb()
    call dsm2_hdf_geom_sb()
    call allocate_all
    
    call get_elevations()
    call assign_cells()
    call fill_sed_hdf_array()
    call write_hdf_sb(hydro_hdf5)
    call deallocate_all
    call hdf5_close
    
end program
