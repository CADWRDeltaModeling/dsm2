!> Reed Harris Environmental Ltd.
!> ================================================
!> 
!>@ingroup sediment_bed_setup
module sediment_bed_setup

    use gtm_precision
    use input_storage_fortran
    use sed_type_defs
    use sed_internal_vars
    use common_variables , only: n_cell, n_resv, n_chan, group, n_group, obj_channel, obj_reservoir, chan_geom, hydro_hdf5, run_mercury
    use common_dsm2_vars, only: gtm, io_write, io_restart, io_hdf5, io_read, io_files,pathinput_t 
    use sed_bed_hdf
    use common_dsm2_vars, only: dsm2_name, dsm2_modifier,ninpaths,pathinput,n_input_ts,n_inputpaths
    use process_timeseries_input_sed
    implicit none

    !real (gtm_real), allocatable :: group_values_sed(:,:,:) ! (group_id, variable_code, sed_layer)
    
    integer, parameter :: sb_r_ct_labile = 1
    integer, parameter :: sb_q10_ct = 2
    integer, parameter :: sb_tb_ct = 3
    integer, parameter :: sb_inter_frac_base = 4
    integer, parameter :: sb_inter_frac_max  = 5
    integer, parameter :: sb_q10_ct_inter = 6
    integer, parameter :: sb_tb_ct_inter = 7
    integer, parameter :: sb_porosity = 8
    
    integer, parameter :: sb_inter_frac_tb = 9
    
    contains
    
    subroutine set_up_sediment_bed(n_cells, n_chans, init_input_file, sim_start, sim_end, hdf_interval_char, use_gtm_hdf)
        !args
        integer, intent(in) :: n_cells
        integer, intent(in) :: n_chans
        character* (*) ,  intent(in)::  init_input_file
        integer, intent(in) :: sim_start
        integer, intent(in) :: sim_end
        character*16 :: hdf_interval_char
        logical, intent(in):: use_gtm_hdf
        !local
        character(len=130) :: file_name_inp
        call get_sed_io_files( init_input_file, file_name_inp)
        allocate (length(n_cells))
        call init_bed_geom_hdf()
        call init_sed_hdf(n_cells, n_chans, sim_start, sim_end, hdf_interval_char, use_gtm_hdf)
        call setup_sed_internals(n_cells, n_zones, 2,3)
        call sed_get_group_variables()
        !call set_up_interface_parms(n_cells, n_zones)
        call sed_read_init_values(n_cells, n_zones, n_resv, file_name_qrf, 8, 1)
        call sed_solids_initialize(n_cells,n_zones, 2, 3)
       return
    end subroutine set_up_sediment_bed
    
    

    subroutine deallocate_sed_solids()
        deallocate (bed)    
        call deallocate_sed_internals()
        return
    end subroutine deallocate_sed_solids
    
    subroutine get_sed_io_files( init_input_file, io_file_inp)!, io_file_hdf)
        use error_handling
        !args
        character *(*), intent(in) ::init_input_file
        character *(*), intent(out) ::io_file_inp
  
        !character(len=130), intent(out) ::io_file_hdf
        !local
        logical :: file_exists
        integer :: ii
       
        io_file_inp = init_input_file
        ii = index(io_file_inp,".", .true.)
        io_file_inp(ii:ii+8) = "_sed.inp"
        inquire(file=io_file_inp, exist=file_exists)
        if (file_exists==.false.) then 
                call gtm_fatal("sediment bed module - "//trim(io_file_inp)//" does not exist!!")
        endif  
        if (io_files(gtm,io_restart, io_read).use == .true.) then
            file_name_qrf = io_files(gtm,io_restart, io_read).filename
            ii = index(file_name_qrf,".", .true.)
            file_name_qrf(ii:ii+8) = "_sed.qrf" 
            inquire(file=file_name_qrf, exist=file_exists)
            if (file_exists==.false.) then 
                call gtm_fatal("sediment bed module - "//trim(file_name_qrf)//" does not exist!!")
            end if
        else
           file_name_qrf = io_files(gtm,io_hdf5, io_write).filename
           ii = index(file_name_qrf,".", .true.)
            file_name_qrf(ii:ii+8) = "_sed.qrf"
        endif
        file_name_hdf_sed = io_files(gtm,io_hdf5, io_write).filename
        ii = index(file_name_hdf_sed,".", .true.)
        file_name_hdf_sed(ii:ii+8) = "_sed.h5" 
        
        file_name_hdf_sed_flux = io_files(gtm,io_hdf5, io_write).filename
        ii = index(file_name_hdf_sed_flux,".", .true.)
        file_name_hdf_sed_flux(ii:ii+12) = "_sed_flux.h5" 
        
        file_name_hdf_bed = hydro_hdf5
        ii = index(file_name_hdf_bed,".", .true.)
        file_name_hdf_bed(ii:ii+11) = "_bed_zone.h5" 
        
    end subroutine get_sed_io_files

    subroutine sed_read_init_values(n_cells, n_zone, n_resv, restart_file_name_sed, n_var, input_type)
        !args
        integer, intent (in)            :: n_cells
        integer, intent (in)            :: n_zone
        integer, intent (in)            :: n_resv
        character*(*), intent(in) :: restart_file_name_sed
        integer, intent(in) :: n_var
        integer, intent (in)            :: input_type       !1 for solids, 2-Hg
        !local
        
        integer :: ncell_l
        integer :: nvar_l
        integer :: nzone_l
        character*32 :: name, a, b(n_var)
        integer :: file_unit
        integer :: i
        integer :: j
        integer :: count,zonecount
        integer :: cellno, zoneno
        real(gtm_real):: value(n_var)
        !todo: probably needs some error checking above and beyond file exists 
        file_unit = 151
        
        open(file_unit, file=restart_file_name_sed)
        read(file_unit,*)
        read(file_unit,*)
        read(file_unit,*) nvar_l
        read(file_unit,*) ncell_l
        read(file_unit,*) nzone_l
        read(file_unit,*) a, (b(i),i=1,n_var)
            
        if (input_type==1) then                         !sediment bed solids porosity and mass fractions
            do count=1,n_cells
                do zonecount = 1, n_zone
                    read(file_unit,*) cellno, zoneno, (value(i),i=1,n_var)
                    bed(cellno,zoneno,1).thickness = value(1)
                    do j=1,3
                        bed(cellno,zoneno,1).mass_frac(j) = value(j+1)
                    end do
                    bed(cellno,zoneno,2).thickness = value(5)
                    do j=1,3
                        bed(cellno,zoneno,2).mass_frac(j) = value(j+5)
                    end do
                end do
            end do                    
        else
                
        end if
        close(file_unit)
        return
    end subroutine sed_read_init_values
    
    subroutine sed_solids_initialize(ncells, nzones, nlayers, nsolids)
        !args
        integer, intent (in)            :: ncells
        integer, intent (in)            :: nzones
        integer, intent (in)            :: nlayers
        integer, intent (in)            :: nsolids
        !local
        real (gtm_real)     :: mass_total(ncells, nzones, nlayers)
        integer             :: i,j
   
        !convert to g/m3
        do i=1,nsolids
            density(i) = g_cm3_to_g_m3*density(i)
        end do
        bed(:,:,2).area_zone = bed(:,:,1).area_zone
    
        bed(:,:,1).volume = bed(:,:,1).wp_zone*bed(:,:,1).thickness
    
        !todo: maybe make the underlying layer have the same properties
        !normalize mass_fractions
        mass_total(:,:,:) = bed(:,:,:).mass_frac(1)+bed(:,:,:).mass_frac(2)+bed(:,:,:).mass_frac(3)
        bed(:,:,:).mass_frac(1) = bed(:,:,:).mass_frac(1)/mass_total(:,:,:)
        bed(:,:,:).mass_frac(2) = bed(:,:,:).mass_frac(2)/mass_total(:,:,:)
        bed(:,:,:).mass_frac(3) = bed(:,:,:).mass_frac(3)/mass_total(:,:,:)
    
        mass_total(:,:,:) =  bed(:,:,:).wp_zone * bed(:,:,:).thickness * (one - bed(:,:,:).porosity) / (bed(:,:,:).mass_frac(1)/density(1)+bed(:,:,:).mass_frac(2)/density(2)+bed(:,:,:).mass_frac(3)/density(3))
        
        do i=1,nzones
            do j = 1 , nlayers
                mass_total(:,i,j) = mass_total(:,i,j) * length(:) * (ft_to_m **2)
            end do
        end do
        
        do i=1,nsolids
            sedsolids(:,:,:,i,3) = bed(:,:,:).mass_frac(i)* mass_total(:,:,:)
            sedsolids(:,:,:,i,1) = sedsolids(:,:,:,i,3)
        end do
        return
    end subroutine sed_solids_initialize

    subroutine set_up_interface_parms(ncells, nzones )   
        !coefficients for interface decomposition of particle type 1
        !args
        integer, intent (in)            :: ncells
        integer, intent (in)            :: nzones
        !local
        integer             :: i, k
        bed(:,:,:).inter_k = one
        bed(:,:,:).inter_a1 = one
        bed(:,:,:).inter_a2 = one
        do i=1, ncells 
            do  k=1,nzones
                if (bed(i,k,1).inter_frac_tb > zero) then
                    bed(i,k,1).inter_k = bed(i,k,1).inter_frac_tb/(one - bed(i,k,1).inter_frac_tb)
                    bed(i,k,1).inter_a2 = bed(i,k,1).inter_k*bed(i,k,1).Q10_ct_inter**((-two*bed(i,k,1).Tb_ct_inter)/ten)
                    bed(i,k,1).inter_a1 = (one + (ten/(log(bed(i,k,1).Q10_ct_inter)*bed(i,k,1).Tb_ct_inter)) * log(bed(i,k,1).inter_a2/bed(i,k,1).inter_k &
                        + (one/bed(i,k,1).inter_k)* (bed(i,k,1).inter_frac_max/( bed(i,k,1).inter_frac_max - bed(i,k,1).inter_frac_tb)) - (one/bed(i,k,1).inter_k)))
                else
                    bed(i,k,1).inter_frac_max = zero
                end if
            end do
        end do
        return
    end subroutine set_up_interface_parms

    subroutine sed_get_group_variables()
    !local
        character*16 :: init_input_file
        integer :: ierror = 0
        integer :: nitems
        integer :: nitem_input_time_series
        integer :: icount
        integer :: ii, jj, kk
        integer :: start_cell, end_cell
        integer :: chan_number
        integer :: group_id
        integer :: var_id
        character*32 :: group_name
        integer :: sed_layer, sed_zone
        real*8  :: value
        character *16 :: variable
        integer :: no_sed_vars = 7   !for now
        integer :: no_sed_layers = 2
        integer :: min_cell =1000
        integer :: max_cell =0
        integer :: n_sed_cells = 0
        character*128 :: filename
        character*32 :: name
        character*80 :: inpath
        character*8  :: fillin
        type (pathinput_t), allocatable :: path(:)
        character(len=128) :: qrfpath
        call clear_all_buffers(ierror)
        call init_file_reader(ierror)
        call set_initial_context_profile(dsm2_name)
        
        init_input_file = "gtm_sed.inp"
        
        call set_active_profile("envvar",ierror)
        call read_buffer_from_text(init_input_file,ierror)
        nitems = envvar_buffer_size()
        if (nitems>0) then
            call envvar_query_from_buffer(1,       &
                                          name,    &
                                          qrfpath, &
                                          ierror) 
            
        endif
        ierror = 0
        call set_active_profile("all",ierror)
        call read_buffer_from_text(init_input_file,ierror)
        nitems = group_variable_sed_buffer_size()
        
       ! allocate(group_values_sed(nitems,no_sed_vars,no_sed_vars))

        do icount = 1,nitems
            call group_variable_sed_query_from_buffer(icount,   &
                                                      group_name,   &
                                                      sed_zone,     &
                                                      sed_layer,    &
                                                      variable,     &
                                                      value,        &
                                                      ierror) 
            call locase(group_name) 
            call locase(variable)
           
            do ii = 1, n_group
                if (group(ii)%name.eq.group_name) then
                    group_id = group(ii)%id
                end if
            end do           
            call sed_input_variable(variable, var_id)
            !group_values_sed(group_id,var_id,sed_layer) = value

          ! call process_group_variable(group_name,     &
          !                             constituent,    &
          !                             variable,       &
          !                             value) 
            do ii =1, group(group_id)%n_members  
                if (group(group_id)%member_pattern_code(ii) .eq. obj_channel) then
                    read (group(group_id)%member_name(ii), *) chan_number       !get channel no:
                    if (chan_number<=n_chan) then
                        do jj= chan_geom(chan_number)%start_cell,chan_geom(chan_number)%end_cell
                           ! do kk = 1, n_zones
                                call assign_sed_input_variable(jj, sed_zone, sed_layer, var_id, value)
                                n_sed_cells = n_sed_cells + 1
                           ! end do
                        end do
                    end if
                else if (group(group_id)%member_pattern_code(ii) .eq. obj_reservoir) then
                     write(*,*) "???? this is a reservoir"
                else
                     write(*,*) "???? this is neither channel nor reservoir"
                end if
            end do
        end do
        write (*,*) "number of sed input coefficients processed", n_sed_cells
        
        if (run_mercury) then
            nitem_input_time_series = input_time_series_buffer_size() 
            n_input_ts = n_input_ts+nitem_input_time_series
            n_inputpaths = n_inputpaths + nitem_input_time_series
            allocate (path(ninpaths))
            path = pathinput
            deallocate(pathinput)
            allocate (pathinput(ninpaths+nitem_input_time_series))
            do icount = 1,ninpaths
                pathinput(icount)  = path(icount)
            end do
            deallocate (path)
            do icount = 1,nitem_input_time_series
                call input_time_series_query_from_buffer(icount,     &
                                                          name,       &
                                                          variable,   &
                                                          fillin,     &
                                                          filename,   &
                                                          inpath,     &
                                                          ierror)
             !sign = 1
             call process_input_time_series_sed(name,       &
                                           variable,    &
                                           fillin,      &
                                           filename,    &
                                           inpath)
            end do
            print *,"Number of input time series processed: ", nitem_input_time_series
        end if
        
        
        
        
       ! read (group(3)%member_name(1), *) jj 
       ! ii = chan_geom(1)%start_cell
       ! ii = chan_geom(1)%end_cell'
       
        
        !read(*,*)
       ! deallocate(group_values_sed)
        return
    end subroutine sed_get_group_variables
    
    subroutine sed_input_variable(variable, sb_variable_id)
        character (*), intent(in) :: variable
        integer, intent(out) :: sb_variable_id
     
        sb_variable_id = 0
        if (trim(variable).eq.'r_ct_labile') then
            sb_variable_id = sb_r_ct_labile
        else if (variable == "q10_ct") then
            sb_variable_id = sb_q10_ct
        else if (variable == "tb_ct") then
            sb_variable_id = sb_tb_ct
        else if (variable == "inter_frac_base") then
            sb_variable_id = sb_inter_frac_base
        else if (variable == "inter_frac_max") then
            sb_variable_id = sb_inter_frac_max
        else if (variable == "q10_ct_inter") then
            sb_variable_id = sb_q10_ct_inter
        else if (variable == "tb_ct_inter") then
            sb_variable_id =  sb_tb_ct_inter
        else if (variable == "porosity") then
            sb_variable_id = sb_porosity
            
        else if (variable == "inter_frac_tb") then
            sb_variable_id = sb_inter_frac_tb
        end if
        return
    end subroutine
    
     subroutine assign_sed_input_variable(cell_no, sed_zone, sed_layer, sb_variable_id, value)
        integer, intent(in) :: cell_no
        integer, intent(in) :: sed_zone
        integer, intent(in) :: sed_layer
        integer, intent(in) :: sb_variable_id
        real*8, intent(in)   :: value
        
        if ((sed_zone == 0).and.(sed_layer == 0)) then 
            select case  (sb_variable_id)
            case (sb_r_ct_labile)
                bed(cell_no,: , :).r_ct_labile = value
            case (sb_q10_ct)
                bed(cell_no,:, :).q10_ct = value
            case (sb_tb_ct)
                bed(cell_no,:, :).tb_ct = value
            case (sb_inter_frac_base)
                bed(cell_no,:, 1).inter_frac_base = value
            case (sb_inter_frac_max)
                bed(cell_no,:, sed_layer).inter_frac_max = value
            case (sb_q10_ct_inter)
                bed(cell_no,:, :).q10_ct_inter = value
            case (sb_tb_ct_inter)
                bed(cell_no,:, :).tb_ct_inter = value
            case (sb_porosity)
                bed(cell_no,:, :).porosity = value
                
            case (sb_inter_frac_tb)
                bed(cell_no,:, 1).inter_frac_tb = value
            end select
        elseif (sed_zone== 0) then
            select case  (sb_variable_id)
            case (sb_r_ct_labile)
                bed(cell_no,: , sed_layer).r_ct_labile = value
            case (sb_q10_ct)
                bed(cell_no,:, sed_layer).q10_ct = value
            case (sb_tb_ct)
                bed(cell_no,:, sed_layer).tb_ct = value
            case (sb_inter_frac_base)
                bed(cell_no,:, 1).inter_frac_base = value
            case (sb_inter_frac_max)
                bed(cell_no,:, sed_layer).inter_frac_max = value
            case (sb_q10_ct_inter)
                bed(cell_no,:, sed_layer).q10_ct_inter = value
            case (sb_tb_ct_inter)
                bed(cell_no,:, sed_layer).tb_ct_inter = value
            case (sb_porosity)
                bed(cell_no,:, sed_layer).porosity = value 
            case (sb_inter_frac_tb)
                bed(cell_no,:, 1).inter_frac_tb = value
            end select
        elseif (sed_layer == 0) then
            select case  (sb_variable_id)
            case (sb_r_ct_labile)
                bed(cell_no, sed_zone, :).r_ct_labile = value
            case (sb_q10_ct)
                bed(cell_no, sed_zone, :).q10_ct = value
            case (sb_tb_ct)
                bed(cell_no, sed_zone, :).tb_ct = value
            case (sb_inter_frac_base)
                bed(cell_no, sed_zone, 1).inter_frac_base = value
            case (sb_q10_ct_inter)
                bed(cell_no, sed_zone, :).q10_ct_inter = value
            case (sb_tb_ct_inter)
                bed(cell_no, sed_zone, :).tb_ct_inter = value
            case (sb_porosity)
                bed(cell_no, sed_zone, :).porosity = value
            
            case (sb_inter_frac_tb)
                bed(cell_no, sed_zone, :).inter_frac_tb = value
            end select
        else
            select case  (sb_variable_id)
            case (sb_r_ct_labile)
                bed(cell_no, sed_zone, sed_layer).r_ct_labile = value
            case (sb_q10_ct)
                bed(cell_no, sed_zone, sed_layer).q10_ct = value
            case (sb_tb_ct)
                bed(cell_no, sed_zone, sed_layer).tb_ct = value
            case (sb_inter_frac_base)
                if (sed_layer ==1 ) bed(cell_no, sed_zone, sed_layer).inter_frac_base = value
            case (sb_q10_ct_inter)
                bed(cell_no, sed_zone, sed_layer).q10_ct_inter = value
            case (sb_tb_ct_inter)
                bed(cell_no, sed_zone, sed_layer).tb_ct_inter = value
            case (sb_porosity)
                bed(cell_no, sed_zone, sed_layer).porosity = value
            
            case (sb_inter_frac_tb)
                if (sed_layer ==1 ) bed(cell_no, sed_zone, sed_layer).inter_frac_tb = value
            end select
        end if
        return
     end subroutine
     
     subroutine close_sediment_bed()
        call deallocate_sed_solids
        call close_hdf_sed_all
        return
     end subroutine close_sediment_bed
end module