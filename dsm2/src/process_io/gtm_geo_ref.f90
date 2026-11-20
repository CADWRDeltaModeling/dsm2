!>@ingroup process_io
!> Channel-to-station mapping for geo-referenced meteorological inputs.
!> Only used when geo_ref is true: reads the CSV named by geo_ref_input, registers the DSS
!> paths it lists as time-varying inputs, and keeps the channel/variable rows so they can be
!> mapped to cells once the grid is known.
!> Required CSV columns (any order, case-insensitive): channel_num, variable, FILE, PATH.
!> Fields must not be quoted or contain commas. Rows with other variables (e.g. Wind_Direction)
!> are ignored. Only one station per channel and variable is supported; weights are not used.
module gtm_geo_ref
    implicit none

    integer, parameter :: n_geo_var = 6          !< number of geo-referenced met variables
    integer, parameter :: geo_cloud  = 1
    integer, parameter :: geo_dryblb = 2
    integer, parameter :: geo_wetblb = 3
    integer, parameter :: geo_wind   = 4
    integer, parameter :: geo_solar  = 5
    integer, parameter :: geo_atmpr  = 6
    
    character(len=8), parameter :: geo_var_name(n_geo_var) = &
        [character(len=8) :: 'cloud', 'dry_bulb', 'wet_bulb', 'wind', 'solar', 'atm_pressure']

    integer, save :: n_geo_row = 0               !< number of usable rows in the mapping file
    integer, save :: n_geo_path = 0              !< number of unique (file, path) pairs
    integer, save :: geo_path_first = 0          !< pathinput index of the first geo_ref path
    integer, allocatable, save :: geo_row_chan(:)   !< DSM2 channel number of each row
    integer, allocatable, save :: geo_row_ivar(:)   !< met variable (geo_cloud ... geo_solar...geo_atmpr) of each row
    integer, allocatable, save :: geo_row_path(:)   !< unique path number of each row
    character(len=128), allocatable, save :: geo_file(:)   !< unique DSS file names
    character(len=80), allocatable, save :: geo_path(:)    !< unique DSS pathnames
    integer, allocatable, save :: geo_cell_path(:,:)       !< pathinput index for (variable, cell)

contains

    !> Stop with an error message
    subroutine geo_ref_error(message)
        use io_units, only: unit_error
        implicit none
        character(len=*), intent(in) :: message
        write(unit_error, '(/a)') 'geo_ref: ' // trim(message)
        call exit(-1)
    end subroutine geo_ref_error

    function to_lower(s) result(t)
        implicit none
        character(len=*), intent(in) :: s
        character(len=len(s)) :: t
        integer :: i, c
        t = s
        do i = 1, len(s)
            c = iachar(s(i:i))
            if (c >= 65 .and. c <= 90) t(i:i) = achar(c + 32)
        end do
    end function to_lower

    !> Map a variable name in the mapping file to geo_cloud ... geo_solar (0 if not used)
    integer function geo_var_index(name)
        implicit none
        character(len=*), intent(in) :: name
        select case (trim(to_lower(adjustl(name))))
        case ('cloud_cover', 'cloud')
            geo_var_index = geo_cloud
        case ('air_temp', 'dry_bulb')
            geo_var_index = geo_dryblb
        case ('wet_bulb')
            geo_var_index = geo_wetblb
        case ('wind_speed', 'wind')
            geo_var_index = geo_wind
        case ('solar_radiation', 'solar')
            geo_var_index = geo_solar
        case ('atmospheric_pressure', 'atm_pressure')
            geo_var_index = geo_atmpr
        case default
            geo_var_index = 0
        end select
    end function geo_var_index

    !> Split a comma separated line
    subroutine split_csv(line, fields, n)
        implicit none
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: fields(:)
        integer, intent(out) :: n
        integer :: i, start, last
        fields = ' '
        n = 0
        start = 1
        last = len_trim(line)
        do i = 1, last
            if (line(i:i) == ',') then
                n = n + 1
                if (n <= size(fields)) fields(n) = adjustl(line(start:i-1))
                start = i + 1
            end if
        end do
        n = n + 1
        if (n <= size(fields)) fields(n) = adjustl(line(start:last))
    end subroutine split_csv

    !> Read one line, dropping a trailing carriage return
    subroutine read_csv_line(iu, line, ios)
        implicit none
        integer, intent(in) :: iu
        character(len=*), intent(out) :: line
        integer, intent(out) :: ios
        integer :: i
        read(iu, '(a)', iostat=ios) line
        if (ios /= 0) return
        i = index(line, char(13))
        if (i > 0) line(i:) = ' '
    end subroutine read_csv_line

    !> Read the mapping file named by geo_ref_input.
    !> Returns the number of unique DSS paths to register as extra input paths.
    subroutine read_geo_ref_file(n_extra_paths)
        use gtm_vars, only: geo_ref_file
        implicit none
        integer, intent(out) :: n_extra_paths
        integer, parameter :: max_fields = 32
        integer :: iu, ios, nlines, i, j, k, nf, iv, ichan
        integer :: c_chan, c_var, c_file, c_path
        character(len=512) :: line
        character(len=128) :: f(max_fields)
        character(len=128) :: fn

        n_extra_paths = 0
        if (len_trim(geo_ref_file) == 0) call geo_ref_error('geo_ref is true but geo_ref_input is not set')
        open(newunit=iu, file=trim(geo_ref_file), status='old', action='read', iostat=ios)
        if (ios /= 0) call geo_ref_error('cannot open geo_ref_input file ' // trim(geo_ref_file))

        call read_csv_line(iu, line, ios)
        if (ios /= 0) call geo_ref_error('geo_ref_input file is empty: ' // trim(geo_ref_file))
        call split_csv(line, f, nf)
        c_chan = 0; c_var = 0; c_file = 0; c_path = 0
        do i = 1, min(nf, max_fields)
            select case (trim(to_lower(f(i))))
            case ('channel_num'); c_chan = i
            case ('variable');    c_var = i
            case ('file');        c_file = i
            case ('path');        c_path = i
            end select
        end do
        if (c_chan == 0 .or. c_var == 0 .or. c_file == 0 .or. c_path == 0) &
            call geo_ref_error('geo_ref_input header must contain channel_num, variable, FILE and PATH')

        nlines = 0
        do
            call read_csv_line(iu, line, ios)
            if (ios /= 0) exit
            if (len_trim(line) > 0) nlines = nlines + 1
        end do
        rewind(iu)
        call read_csv_line(iu, line, ios)   ! skip header

        if (allocated(geo_row_chan)) deallocate(geo_row_chan, geo_row_ivar, geo_row_path, geo_file, geo_path)
        allocate(geo_row_chan(nlines), geo_row_ivar(nlines), geo_row_path(nlines))
        allocate(geo_file(nlines), geo_path(nlines))
        n_geo_row = 0
        n_geo_path = 0

        do
            call read_csv_line(iu, line, ios)
            if (ios /= 0) exit
            if (len_trim(line) == 0) cycle
            call split_csv(line, f, nf)
            if (nf < max(c_chan, c_var, c_file, c_path)) &
                call geo_ref_error('too few fields in geo_ref_input line: ' // trim(line))
            iv = geo_var_index(f(c_var))
            if (iv == 0) cycle
            read(f(c_chan), *, iostat=ios) ichan
            if (ios /= 0) call geo_ref_error('invalid channel_num in geo_ref_input line: ' // trim(line))
            if (len_trim(f(c_path)) > len(geo_path(1))) &
                call geo_ref_error('PATH is longer than 80 characters: ' // trim(f(c_path)))
            fn = f(c_file)

            k = 0
            do j = 1, n_geo_path
                if (to_lower(geo_file(j)) == to_lower(fn) .and. &
                    to_lower(geo_path(j)) == to_lower(f(c_path))) then
                    k = j
                    exit
                end if
            end do
            if (k == 0) then
                n_geo_path = n_geo_path + 1
                geo_file(n_geo_path) = fn
                geo_path(n_geo_path) = f(c_path)
                k = n_geo_path
            end if

            n_geo_row = n_geo_row + 1
            geo_row_chan(n_geo_row) = ichan
            geo_row_ivar(n_geo_row) = iv
            geo_row_path(n_geo_row) = k
        end do
        close(iu)

        if (n_geo_row == 0) call geo_ref_error('no usable rows in geo_ref_input file ' // trim(geo_ref_file))
        n_extra_paths = n_geo_path
        print *, 'geo_ref: rows read from mapping file: ', n_geo_row, ' unique DSS paths: ', n_geo_path
    end subroutine read_geo_ref_file

    !> Register the DSS paths from the mapping file as time-varying input paths.
    !> Must be called after all other input paths are processed.
    subroutine add_geo_ref_paths()
        use common_gtm_vars, only: ninpaths, n_inputpaths
        use process_gtm_input_climate, only: process_input_climate
        implicit none
        integer :: k
        character :: name*32, param*16, fillin*8, filename*128, inpath*80

        if (ninpaths + n_geo_path /= n_inputpaths) &
            call geo_ref_error('internal error: input path count mismatch')
        geo_path_first = ninpaths + 1
        do k = 1, n_geo_path
            name = 'geo_ref'
            param = 'geo_met'
            fillin = 'linear'
            filename = geo_file(k)
            inpath = geo_path(k)
            call process_input_climate(name, param, fillin, filename, inpath)
        end do
    end subroutine add_geo_ref_paths

end module gtm_geo_ref
