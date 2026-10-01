!! Tests of the real Fortran routines in model_interface.f90 (the routines the C++ oprule binding calls).
!!
!! Purpose: check that the C++ mock used by the oprule tests (oprule/test/support/Dsm2FortranMock.*)
!! describes these routines correctly. Each test says which mock behaviour it backs. Where the real
!! routine behaves in a way that looks like a defect, the test pins the current behaviour and says so
!! (OPRULE_REFERENCE.md B9 / OPRULE_TEST_PLAN.md D-xx).
!!
!! Not tested here: get_device_flow_coef / set_device_flow_coef with direct_to_from_node() (D-13). They
!! call exit(3), which would end the whole test run. The mock throws FortranExit for it instead.
module test_model_interface
    use iso_c_binding
    use testdrive, only: new_unittest, unittest_type, error_type, test_failed
    use constants, only: miss_val_i, miss_val_r, const_data, dss_data, expression_data, &
                         obj_channel, obj_reservoir, hydro, io_hdf5, io_write
    use type_defs, only: datasource_t
    use gates_data, only: GateArray, nGate
    use grid_data, only: qext, nqext, obj2obj, nobj2obj, res_geom, nreser, chan_geom, node_id
    use iopath_data, only: pathinput, ninpaths, tidefile_gate_state, io_files
    use runtime_data, only: julmin
    use logging, only: print_level, oprule_log_level, oprule_log_file, oprule_log_devices, &
                         oprule_log_context, oprule_log_tol_op, oprule_log_tol_dim, &
                       oprule_log_trace_interval, oprule_log_flush_hours, oprule_log_text
    use model_interface
    implicit none
    private

    public :: collect_model_interface

contains

    subroutine collect_model_interface(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("direction_constants", test_direction_constants), &
                    new_unittest("name_lookups", test_name_lookups), &
                    new_unittest("gate_name_must_be_stored_lower_case", test_gate_name_case), &
                    new_unittest("external_and_transfer_flows", test_flows), &
                    new_unittest("flows_are_single_precision", test_single_precision), &
                    new_unittest("gate_install", test_gate_install), &
                    new_unittest("device_op_coefficients", test_device_op_coef), &
                    new_unittest("to_from_op_coefficient_ignores_the_to_node", test_to_from_defect), &
                    new_unittest("device_properties", test_device_properties), &
                    new_unittest("nduplicate_is_rounded_by_the_setter", test_nduplicate), &
                    new_unittest("flow_coefficients", test_flow_coef), &
                    new_unittest("data_source_types", test_datasources), &
                    new_unittest("fetch_data", test_fetch_data), &
                    new_unittest("time_functions", test_time), &
                    new_unittest("reference_minute_of_year", test_reference_minute), &
                    new_unittest("oprule_log_level", test_log_level), &
                    new_unittest("oprule_log_options", test_log_options), &
                    new_unittest("tide_file_options", test_tidefile_options), &
                    new_unittest("gate_tables", test_gate_tables), &
                    new_unittest("gate_state_and_context", test_gate_state), &
                    new_unittest("device_sources", test_device_sources) &
                    ]
    end subroutine collect_model_interface

    ! ------------------------------------------------------------------ helpers

    function itoa(i) result(s)
        integer, intent(in) :: i
        character(len=16) :: s
        write (s, '(i0)') i
    end function itoa

    subroutine expect_i(error, got, want, msg)
        type(error_type), allocatable, intent(inout) :: error
        integer, intent(in) :: got, want
        character(len=*), intent(in) :: msg
        if (allocated(error)) return
        if (got /= want) call test_failed(error, msg//": got "//trim(itoa(got))//", expected "//trim(itoa(want)))
    end subroutine expect_i

    subroutine expect_r(error, got, want, msg, tol)
        type(error_type), allocatable, intent(inout) :: error
        real(8), intent(in) :: got, want
        character(len=*), intent(in) :: msg
        real(8), intent(in), optional :: tol
        real(8) :: t
        character(len=64) :: a, b
        t = 1.0d-12
        if (present(tol)) t = tol
        if (allocated(error)) return
        if (abs(got - want) > t) then
            write (a, '(es22.14)') got
            write (b, '(es22.14)') want
            call test_failed(error, msg//": got "//trim(a)//", expected "//trim(b))
        end if
    end subroutine expect_r

    subroutine expect_true(error, cond, msg)
        type(error_type), allocatable, intent(inout) :: error
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg
        if (allocated(error)) return
        if (.not. cond) call test_failed(error, msg)
    end subroutine expect_true

    ! Two gates; g1 has devices D1 (mixed case, as read from the input) and d2; g2 has one device.
    subroutine setup_gates()
        integer :: g, d
        nGate = 2
        GateArray(1)%name = 'g1'
        GateArray(2)%name = 'g2'
        GateArray(1)%nDevice = 2
        GateArray(2)%nDevice = 1
        GateArray(1)%devices(1)%name = 'D1'
        GateArray(1)%devices(2)%name = 'd2'
        GateArray(2)%devices(1)%name = 'd1'
        do g = 1, 2
            GateArray(g)%free = .false.
            do d = 1, GateArray(g)%nDevice
                GateArray(g)%devices(d)%flow = 0.d0
                GateArray(g)%devices(d)%opCoefToNode = 1.d0
                GateArray(g)%devices(d)%opCoefFromNode = 1.d0
                GateArray(g)%devices(d)%flowCoefToNode = 0.d0
                GateArray(g)%devices(d)%flowCoefFromNode = 0.d0
                GateArray(g)%devices(d)%height = 0.d0
                GateArray(g)%devices(d)%maxWidth = 0.d0
                GateArray(g)%devices(d)%baseElev = 0.d0
                GateArray(g)%devices(d)%nDuplicate = 0.d0
            end do
        end do
    end subroutine setup_gates

    ! ------------------------------------------------------------------ tests

    ! Backs: mock direct_to_node / direct_from_node / direct_to_from_node.
    subroutine test_direction_constants(error)
        type(error_type), allocatable, intent(out) :: error
        call expect_i(error, direct_to_node(), 1, "to node")
        call expect_i(error, direct_from_node(), -1, "from node")
        call expect_i(error, direct_to_from_node(), 0, "to and from node")
    end subroutine test_direction_constants

    ! Backs: mock gate_index, device_index, reservoir_index, reservoir_connect_index, qext_index,
    ! transfer_index, ts_index, value_from_inputpath (the case rules in particular).
    subroutine test_name_lookups(error)
        type(error_type), allocatable, intent(out) :: error
        call setup_gates()
        ! gates: the argument is lower-cased, the stored name is not
        call expect_i(error, gateNdx("g1", 2_c_size_t), 1, "gate g1")
        call expect_i(error, gateNdx("G2", 2_c_size_t), 2, "gate G2 (argument is lower-cased)")
        call expect_i(error, gateNdx("nope", 4_c_size_t), miss_val_i, "unknown gate")
        ! devices: both sides are lower-cased
        call expect_i(error, deviceNdx(1, "d1", 2_c_size_t), 1, "device d1 stored as D1")
        call expect_i(error, deviceNdx(1, "D2", 2_c_size_t), 2, "device D2")
        call expect_i(error, deviceNdx(1, "zz", 2_c_size_t), miss_val_i, "unknown device")

        ! reservoirs: argument lower-cased; the connection index takes the INTERNAL node number
        nreser = 1
        res_geom(1)%name = 'res1'
        res_geom(1)%nnodes = 2
        res_geom(1)%node_no(1) = 10
        res_geom(1)%node_no(2) = 20
        call expect_i(error, resNdx("RES1", 4_c_size_t), 1, "reservoir RES1")
        call expect_i(error, resNdx("nope", 4_c_size_t), miss_val_i, "unknown reservoir")
        call expect_i(error, resConnectNdx(1, 20), 2, "connection to node 20")
        call expect_i(error, resConnectNdx(1, 99), miss_val_i, "no connection to node 99")

        ! external flows and transfers: exact match, the argument is NOT lower-cased
        nqext = 2
        qext(1)%name = 'q1'
        qext(2)%name = 'q2'
        call expect_i(error, qext_index("q2", 2_c_size_t), 2, "qext q2")
        call expect_i(error, qext_index("Q2", 2_c_size_t), miss_val_i, "qext Q2 is not found")
        nobj2obj = 2
        obj2obj(1)%name = 't1'
        obj2obj(2)%name = 't2'
        call expect_i(error, transfer_index("t2", 2_c_size_t), 2, "transfer t2")
        call expect_i(error, transfer_index("T2", 2_c_size_t), miss_val_i, "transfer T2 is not found")

        ! input paths: exact match, absent is -1 (not miss_val_i)
        ninpaths = 2
        pathinput(1)%name = 'ts1'
        pathinput(2)%name = 'shared'
        pathinput(2)%value = 7.5d0
        call expect_i(error, ts_index("ts1", 3_c_size_t), 1, "ts1")
        call expect_i(error, ts_index("TS1", 3_c_size_t), -1, "TS1 is not found")
        call expect_i(error, ts_index("absent", 6_c_size_t), -1, "absent path")
        call expect_r(error, value_from_inputpath(2), 7.5d0, "value of path 2")
    end subroutine test_name_lookups

    ! Backs the mock's lower-case-name assumption: gate_index lower-cases its argument but compares it with
    ! the stored name as is, so a gate stored with capitals can never be found. (process_gate lower-cases
    ! names before they are stored, so this does not arise in a model run.)
    subroutine test_gate_name_case(error)
        type(error_type), allocatable, intent(out) :: error
        call setup_gates()
        GateArray(2)%name = 'MixedGate'
        call expect_i(error, gateNdx("MixedGate", 9_c_size_t), miss_val_i, "capitalised stored name")
        call expect_i(error, gateNdx("mixedgate", 9_c_size_t), miss_val_i, "lower-case argument")
        GateArray(2)%name = 'g2'
    end subroutine test_gate_name_case

    ! Backs: mock get/set_external_flow, get/set_transfer_flow (values that are exact in single precision).
    subroutine test_flows(error)
        type(error_type), allocatable, intent(out) :: error
        nqext = 1
        qext(1)%name = 'q1'
        call set_external_flow(1, -3.5d0)
        call expect_r(error, get_external_flow(1), -3.5d0, "external flow")
        nobj2obj = 1
        obj2obj(1)%name = 't1'
        call set_transfer_flow(1, 12.25d0)
        call expect_r(error, get_transfer_flow(1), 12.25d0, "transfer flow")
    end subroutine test_flows

    ! The model stores external and transfer flows as real*4, so a double written through the interface
    ! is read back rounded to single precision. The mock keeps full double precision.
    ! FORTRAN QUIRK: the oprule value written is not exactly the value read back.
    subroutine test_single_precision(error)
        type(error_type), allocatable, intent(out) :: error
        real(8) :: back
        nqext = 1
        call set_external_flow(1, 0.1d0)
        back = get_external_flow(1)
        call expect_true(error, abs(back - 0.1d0) > 1.0d-12, "0.1 is not exact in single precision")
        call expect_r(error, back, 0.1d0, "single precision rounding error is small", 1.0d-6)
        call expect_r(error, back, real(real(0.1d0, 4), 8), "equals the single precision value", 0.0d0)
        nobj2obj = 1
        call set_transfer_flow(1, 0.1d0)
        call expect_r(error, get_transfer_flow(1), real(real(0.1d0, 4), 8), "transfer flow likewise", 0.0d0)
    end subroutine test_single_precision

    ! Backs: mock set_gate_install, is_gate_install, set_gate_install_datasource.
    subroutine test_gate_install(error)
        type(error_type), allocatable, intent(out) :: error
        call setup_gates()
        call expect_r(error, is_gate_install(1), 1.d0, "installed at the start")
        GateArray(1)%devices(1)%flow = 5.d0
        call set_gate_install(1, 0.d0)
        call expect_r(error, is_gate_install(1), 0.d0, "removed")
        call expect_r(error, GateArray(1)%devices(1)%flow, 0.d0, "device flow is zeroed when the gate becomes free")
        call set_gate_install(1, 1.d0)
        call expect_r(error, is_gate_install(1), 1.d0, "installed again")
        call set_gate_install(1, 0.5d0)               ! anything other than 0 means installed
        call expect_r(error, is_gate_install(1), 1.d0, "any non-zero value installs")
        call set_gate_install_datasource(1, 3, 1.d0, .true.)
        call expect_i(error, GateArray(1)%install_datasource%source_type, expression_data, "install source type")
        call expect_i(error, GateArray(1)%install_datasource%indx_ptr, 3, "install source index")
    end subroutine test_gate_install

    ! Backs: mock get/set_device_op_coef and set_device_op_datasource for to_node, from_node, to_from_node.
    subroutine test_device_op_coef(error)
        type(error_type), allocatable, intent(out) :: error
        call setup_gates()
        call set_device_op_coef(1, 1, direct_to_node(), 0.2d0)
        call expect_r(error, GateArray(1)%devices(1)%opCoefToNode, 0.2d0, "to node set")
        call expect_r(error, GateArray(1)%devices(1)%opCoefFromNode, 1.d0, "from node untouched")
        call set_device_op_coef(1, 1, direct_from_node(), 0.6d0)
        call expect_r(error, get_device_op_coef(1, 1, direct_to_node()), 0.2d0, "get to node")
        call expect_r(error, get_device_op_coef(1, 1, direct_from_node()), 0.6d0, "get from node")
        call set_device_op_coef(1, 2, direct_to_from_node(), 0.3d0)
        call expect_r(error, GateArray(1)%devices(2)%opCoefToNode, 0.3d0, "both: to node")
        call expect_r(error, GateArray(1)%devices(2)%opCoefFromNode, 0.3d0, "both: from node")
        ! an unknown direction: set does nothing, get returns -901
        call set_device_op_coef(1, 2, 99, 0.9d0)
        call expect_r(error, GateArray(1)%devices(2)%opCoefToNode, 0.3d0, "unknown direction: no change")
        call expect_r(error, get_device_op_coef(1, 2, 99), -901.d0, "unknown direction: -901")
        ! data sources follow the direction
        call set_device_op_datasource(1, 1, direct_from_node(), 4, 1.d0, .true.)
        call expect_i(error, GateArray(1)%devices(1)%op_from_node_datasource%source_type, expression_data, "from source")
        call set_device_op_datasource(1, 2, direct_to_from_node(), 5, 2.d0, .false.)
        call expect_i(error, GateArray(1)%devices(2)%op_from_node_datasource%source_type, const_data, "both: from source")
        call expect_i(error, GateArray(1)%devices(2)%op_to_node_datasource%source_type, const_data, "both: to source")
        call expect_i(error, GateArray(1)%devices(2)%op_to_node_datasource%indx_ptr, 5, "both: to index")
    end subroutine test_device_op_coef

    ! PLAN D-04 / REFERENCE B9.4: reading the to_from_node coefficient averages opCoefFromNode with itself,
    ! so the to-node value is ignored. The mock reproduces this.
    subroutine test_to_from_defect(error)
        type(error_type), allocatable, intent(out) :: error
        call setup_gates()
        GateArray(1)%devices(1)%opCoefToNode = 0.2d0
        GateArray(1)%devices(1)%opCoefFromNode = 0.6d0
        call expect_r(error, get_device_op_coef(1, 1, direct_to_from_node()), 0.6d0, &
                      "DEFECT: the average of 0.2 and 0.6 should be 0.4")
    end subroutine test_to_from_defect

    ! Backs: mock get/set_device_height / width / elev (plain assignment, data source per property).
    subroutine test_device_properties(error)
        type(error_type), allocatable, intent(out) :: error
        call setup_gates()
        call set_device_height(1, 2, 3.25d0)
        call set_device_width(1, 2, 40.5d0)
        call set_device_elev(1, 2, -1.5d0)
        call expect_r(error, get_device_height(1, 2), 3.25d0, "height")
        call expect_r(error, get_device_width(1, 2), 40.5d0, "width")
        call expect_r(error, get_device_elev(1, 2), -1.5d0, "elevation")
        call expect_r(error, GateArray(1)%devices(2)%baseElev, -1.5d0, "elevation is stored in baseElev")
        call expect_r(error, GateArray(1)%devices(2)%maxWidth, 40.5d0, "width is stored in maxWidth")
        call expect_r(error, GateArray(1)%devices(1)%height, 0.d0, "other device untouched")
    end subroutine test_device_properties

    ! Backs: mock set_device_nduplicate (rounds to the nearest integer, halves away from zero).
    ! The data source path does not round: see the oprule test study_rule_behaviour (PLAN D-25).
    subroutine test_nduplicate(error)
        type(error_type), allocatable, intent(out) :: error
        call setup_gates()
        call set_device_nduplicate(1, 1, 2.4d0)
        call expect_r(error, get_device_nduplicate(1, 1), 2.d0, "2.4 rounds down")
        call set_device_nduplicate(1, 1, 2.5d0)
        call expect_r(error, get_device_nduplicate(1, 1), 3.d0, "2.5 rounds up")
        call set_device_nduplicate(1, 1, 2.6d0)
        call expect_r(error, get_device_nduplicate(1, 1), 3.d0, "2.6 rounds up")
    end subroutine test_nduplicate

    ! Backs: mock get/set_device_flow_coef for to_node and from_node.
    subroutine test_flow_coef(error)
        type(error_type), allocatable, intent(out) :: error
        call setup_gates()
        call set_device_flow_coef(2, 1, direct_to_node(), 0.8d0)
        call set_device_flow_coef(2, 1, direct_from_node(), 0.9d0)
        call expect_r(error, get_device_flow_coef(2, 1, direct_to_node()), 0.8d0, "to node")
        call expect_r(error, get_device_flow_coef(2, 1, direct_from_node()), 0.9d0, "from node")
        call expect_r(error, GateArray(2)%devices(1)%flowCoefToNode, 0.8d0, "stored in flowCoefToNode")
    end subroutine test_flow_coef

    ! Backs: mock set_datasource (timedep selects expression or constant data; the value and the index are kept).
    ! Called from Fortran, so `timedep` is a Fortran logical. From C++ the argument is a one-byte bool
    ! passed by reference (PLAN D-05); that mismatch cannot be reproduced here.
    subroutine test_datasources(error)
        type(error_type), allocatable, intent(out) :: error
        type(datasource_t) :: src
        call set_datasource(src, 11, 2.5d0, .true.)
        call expect_i(error, src%source_type, expression_data, "time dependent")
        call expect_i(error, src%indx_ptr, 11, "index")
        call expect_r(error, src%value, 2.5d0, "value")
        call set_datasource(src, 12, 3.5d0, .false.)
        call expect_i(error, src%source_type, const_data, "constant")
        call setup_gates()
        call set_device_height_datasource(1, 1, 7, 3.5d0, .true.)
        call expect_i(error, GateArray(1)%devices(1)%height_datasource%source_type, expression_data, "height source")
        call set_device_width_datasource(1, 1, 8, 4.5d0, .false.)
        call expect_i(error, GateArray(1)%devices(1)%width_datasource%source_type, const_data, "width source")
        call set_device_elev_datasource(1, 1, 9, 5.5d0, .true.)
        call expect_i(error, GateArray(1)%devices(1)%elev_datasource%indx_ptr, 9, "elevation source index")
        ! nduplicate takes its logical by value (kind c_bool), unlike the others
        call set_device_nduplicate_datasource(1, 1, 10, 6.5d0, .true._c_bool)
        call expect_i(error, GateArray(1)%devices(1)%nduplicate_datasource%source_type, expression_data, "nduplicate source")
        nqext = 1
        call set_external_flow_datasource(1, 13, 1.d0, .true.)
        call expect_i(error, qext(1)%datasource%source_type, expression_data, "external flow source")
        nobj2obj = 1
        call set_transfer_flow_datasource(1, 14, 1.d0, .false.)
        call expect_i(error, obj2obj(1)%datasource%source_type, const_data, "transfer source")
    end subroutine test_datasources

    ! Backs: mock Model::fetch_data for constant and DSS sources; any other type gives miss_val_r.
    ! (Expression sources call back into C++ and are covered by the oprule tests.)
    subroutine test_fetch_data(error)
        type(error_type), allocatable, intent(out) :: error
        type(datasource_t) :: src
        ninpaths = 2
        pathinput(2)%value = 8.75d0
        src%source_type = const_data
        src%value = 1.5d0
        call expect_r(error, fetch_data(src), 1.5d0, "constant")
        src%source_type = dss_data
        src%indx_ptr = 2
        call expect_r(error, fetch_data(src), 8.75d0, "dss path value")
        src%source_type = 0
        call expect_r(error, fetch_data(src), real(miss_val_r, 8), "unset source")
    end subroutine test_fetch_data

    ! Backs the mock's time assumptions: julmin is minutes with 01JAN1900 00:00 = 1440 (julian day 1),
    ! day of year is 0-based, and the minute of the year is derived from them.
    subroutine test_time(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: iymdjl
        julmin = iymdjl(2018, 9, 20)*1440 + 13*60 + 45
        call expect_i(error, getModelTime(), julmin, "model time is julmin")
        call expect_i(error, getModelTicks(), julmin, "ticks are julmin")
        call expect_i(error, getModelYear(), 2018, "year")
        call expect_i(error, getModelMonth(), 9, "month")
        call expect_i(error, getModelDay(), 20, "day")
        call expect_i(error, getModelHour(), 13, "hour")
        call expect_i(error, getModelMinute(), 45, "minute")
        call expect_i(error, getModelMinuteOfDay(), 13*60 + 45, "minute of day")
        call expect_i(error, getModelDayOfYear(), 262, "day of year (0-based)")
        call expect_i(error, getModelMinuteOfYear(), 262*1440 + 13*60 + 45, "minute of year")
        julmin = iymdjl(1900, 1, 1)*1440
        call expect_i(error, julmin, 1440, "01JAN1900 00:00 is minute 1440")
    end subroutine test_time

    ! Backs: mock get_reference_minute_of_year (uses the model year, so leap years count).
    subroutine test_reference_minute(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: iymdjl
        integer :: mon, day, hour, mnt
        mon = 3; day = 1; hour = 0; mnt = 0
        julmin = iymdjl(2018, 9, 20)*1440
        call expect_i(error, getReferenceMinuteOfYear(mon, day, hour, mnt), (31 + 28)*1440, "01MAR in 2018")
        julmin = iymdjl(2020, 7, 1)*1440
        call expect_i(error, getReferenceMinuteOfYear(mon, day, hour, mnt), (31 + 29)*1440, "01MAR in 2020")
        mon = 1; day = 1
        call expect_i(error, getReferenceMinuteOfYear(mon, day, hour, mnt), 0, "01JAN")
    end subroutine test_reference_minute

    ! The level the C++ log reads (get_oprule_log_level): the oprule_log_level scalar wins; otherwise
    ! print_level 4 and 5 or more give 1 and 2, and lower values (or an unset print_level) give 0.
    subroutine test_log_level(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: saved_print, saved_level
        saved_print = print_level
        saved_level = oprule_log_level

        oprule_log_level = -1
        print_level = miss_val_i
        call expect_i(error, get_oprule_log_level(), 0, "nothing set")
        print_level = 3
        call expect_i(error, get_oprule_log_level(), 0, "print_level 3 (existing behaviour) logs nothing")
        print_level = 4
        call expect_i(error, get_oprule_log_level(), 1, "print_level 4")
        print_level = 5
        call expect_i(error, get_oprule_log_level(), 2, "print_level 5")
        print_level = 6
        call expect_i(error, get_oprule_log_level(), 2, "print_level 6 is capped at 2")
        print_level = 9
        call expect_i(error, get_oprule_log_level(), 2, "print_level above 6 is capped")

        oprule_log_level = 1
        print_level = 6
        call expect_i(error, get_oprule_log_level(), 1, "scalar wins over print_level")
        oprule_log_level = 0
        call expect_i(error, get_oprule_log_level(), 0, "scalar 0 switches the log off")
        oprule_log_level = 7
        call expect_i(error, get_oprule_log_level(), 2, "scalar is capped at 2")

        print_level = saved_print
        oprule_log_level = saved_level
    end subroutine test_log_level

    function cbuf_to_string(buf, n) result(s)
        character(kind=c_char), intent(in) :: buf(*)
        integer, intent(in) :: n
        character(len=max(n, 1)) :: s
        integer :: i
        s = ' '
        do i = 1, n
            s(i:i) = buf(i)
        end do
    end function cbuf_to_string

    ! The options of the oprule log (SCALAR table): the defaults when a scalar is not set, the value when it is.
    ! Backs the option getters the C++ side reads and the mock's defaults (Model::reset).
    subroutine test_log_options(error)
        type(error_type), allocatable, intent(out) :: error
        character(kind=c_char) :: buf(64)
        integer :: n
        character(len=32) :: s_file
        integer :: s_dev, s_ctx, s_trace, s_text
        real(8) :: s_op, s_dim, s_flush
        s_file = oprule_log_file
        s_dev = oprule_log_devices
        s_text = oprule_log_text
        s_ctx = oprule_log_context
        s_trace = oprule_log_trace_interval
        s_op = oprule_log_tol_op
        s_dim = oprule_log_tol_dim
        s_flush = oprule_log_flush_hours

        oprule_log_file = ' '
        oprule_log_devices = -1
        oprule_log_text = -1
        oprule_log_context = -1
        oprule_log_trace_interval = -1
        oprule_log_tol_op = -1.d0
        oprule_log_tol_dim = -1.d0
        oprule_log_flush_hours = -1.d0
        n = get_oprule_log_file(buf, 64)
        call expect_i(error, n, 0, "unset file name is empty")
        call expect_i(error, get_oprule_log_devices(), 1, "devices default")
        call expect_i(error, get_oprule_log_text(), 0, "the text log is off by default")
        call expect_i(error, get_oprule_log_context(), 1, "context default")
        call expect_i(error, get_oprule_log_trace_interval(), 0, "trace default is off")
        call expect_r(error, get_oprule_log_tol_op(), 0.001d0, "op tolerance default")
        call expect_r(error, get_oprule_log_tol_dim(), 0.01d0, "dimension tolerance default")
        call expect_r(error, get_oprule_log_flush_hours(), 24.d0, "flush default")

        oprule_log_file = 'my_log.h5'
        oprule_log_devices = 0
        oprule_log_text = 1
        oprule_log_context = 0
        oprule_log_trace_interval = 3
        oprule_log_tol_op = 0.d0
        oprule_log_tol_dim = 0.5d0
        oprule_log_flush_hours = 6.d0
        n = get_oprule_log_file(buf, 64)
        call expect_i(error, n, 9, "file name length")
        call expect_true(error, cbuf_to_string(buf, n) == 'my_log.h5', "file name")
        call expect_i(error, get_oprule_log_devices(), 0, "devices off")
        call expect_i(error, get_oprule_log_text(), 1, "text log on")
        call expect_i(error, get_oprule_log_context(), 0, "context off")
        call expect_i(error, get_oprule_log_trace_interval(), 3, "trace interval")
        call expect_r(error, get_oprule_log_tol_op(), 0.d0, "a zero op tolerance is a value, not unset")
        call expect_r(error, get_oprule_log_tol_dim(), 0.5d0, "dimension tolerance")
        call expect_r(error, get_oprule_log_flush_hours(), 6.d0, "flush hours")

        n = get_oprule_log_file(buf, 5)                 ! a short buffer is filled and terminated, not overrun
        call expect_i(error, n, 4, "short buffer")
        call expect_true(error, cbuf_to_string(buf, n) == 'my_l', "truncated name")
        call expect_true(error, buf(5) == c_null_char, "terminated")

        oprule_log_file = s_file
        oprule_log_devices = s_dev
        oprule_log_text = s_text
        oprule_log_context = s_ctx
        oprule_log_trace_interval = s_trace
        oprule_log_tol_op = s_op
        oprule_log_tol_dim = s_dim
        oprule_log_flush_hours = s_flush
    end subroutine test_log_options

    ! The tide file options: the gate state default is both (3); the tide file name is only given when the hydro
    ! tide file is in use.
    subroutine test_tidefile_options(error)
        type(error_type), allocatable, intent(out) :: error
        character(kind=c_char) :: buf(160)
        integer :: n, saved_state
        logical :: saved_use
        character(len=130) :: saved_name
        saved_state = tidefile_gate_state
        saved_use = io_files(hydro, io_hdf5, io_write)%use
        saved_name = io_files(hydro, io_hdf5, io_write)%filename

        tidefile_gate_state = -1
        call expect_i(error, get_tidefile_gate_state(), 3, "unset gives both")
        tidefile_gate_state = 0
        call expect_i(error, get_tidefile_gate_state(), 0, "off")
        tidefile_gate_state = 1
        call expect_i(error, get_tidefile_gate_state(), 1, "end")
        tidefile_gate_state = 2
        call expect_i(error, get_tidefile_gate_state(), 2, "mean")

        io_files(hydro, io_hdf5, io_write)%use = .false.
        io_files(hydro, io_hdf5, io_write)%filename = './output/hist.h5'
        n = get_hydro_tidefile_name(buf, 160)
        call expect_i(error, n, 0, "no tide file in use: empty name")
        io_files(hydro, io_hdf5, io_write)%use = .true.
        n = get_hydro_tidefile_name(buf, 160)
        call expect_true(error, cbuf_to_string(buf, n) == './output/hist.h5', "tide file name")

        tidefile_gate_state = saved_state
        io_files(hydro, io_hdf5, io_write)%use = saved_use
        io_files(hydro, io_hdf5, io_write)%filename = saved_name
    end subroutine test_tidefile_options

    ! Backs the mock's gate table accessors (names, device counts, structure types, what the gate is attached to).
    subroutine test_gate_tables(error)
        type(error_type), allocatable, intent(out) :: error
        character(kind=c_char) :: buf(64)
        integer :: n, otype, oid, cp, ncp
        logical :: had_chan_geom
        call setup_gates()
        GateArray(1)%devices(1)%structureType = 2
        GateArray(1)%devices(2)%structureType = 1
        call expect_i(error, get_gate_count(), 2, "gate count")
        call expect_i(error, get_gate_device_count(1), 2, "devices of g1")
        call expect_i(error, get_gate_device_count(2), 1, "devices of g2")
        n = get_gate_name(2, buf, 64)
        call expect_true(error, cbuf_to_string(buf, n) == 'g2', "gate name")
        n = get_device_name(1, 1, buf, 64)
        call expect_true(error, cbuf_to_string(buf, n) == 'D1', "device name keeps its case")
        call expect_i(error, get_device_structure_type(1, 1), 2, "pipe")
        call expect_i(error, get_device_structure_type(1, 2), 1, "weir")
        n = get_gate_name(1, buf, 2)
        call expect_i(error, n, 1, "name is cut to fit a short buffer")

        had_chan_geom = allocated(chan_geom)
        if (.not. had_chan_geom) allocate (chan_geom(3))
        chan_geom(2)%chan_no = 185
        res_geom(1)%name = 'clifton'
        GateArray(1)%objConnectedType = obj_channel
        GateArray(1)%objConnectedID = 2
        GateArray(1)%objCompPoint = 14
        GateArray(1)%nodeCompPoint = 15
        GateArray(2)%objConnectedType = obj_reservoir
        GateArray(2)%objConnectedID = 1
        GateArray(1)%node = 4
        node_id(4) = 77
        n = get_gate_object_name(1, buf, 64)
        call expect_true(error, cbuf_to_string(buf, n) == 'channel 185', "channel gate: external number")
        n = get_gate_object_name(2, buf, 64)
        call expect_true(error, cbuf_to_string(buf, n) == 'reservoir clifton', "reservoir gate: name")
        call expect_i(error, get_gate_node_id(1), 77, "external node number")
        call get_gate_connection(1, otype, oid, cp, ncp)
        call expect_i(error, otype, obj_channel, "connection type")
        call expect_i(error, oid, 2, "internal channel")
        call expect_i(error, cp, 14, "computation point in the channel")
        call expect_i(error, ncp, 15, "computation point at the node")
        if (.not. had_chan_geom) deallocate (chan_geom)
    end subroutine test_gate_tables

    ! The values the sampler reads and the gate flow used for the context.
    subroutine test_gate_state(error)
        type(error_type), allocatable, intent(out) :: error
        call setup_gates()
        GateArray(1)%devices(2)%opCoefToNode = 0.25d0
        GateArray(1)%devices(2)%opCoefFromNode = 0.75d0
        GateArray(1)%devices(2)%height = 6.5d0
        GateArray(1)%devices(2)%baseElev = -2.d0
        GateArray(1)%devices(2)%maxWidth = 20.d0
        GateArray(1)%devices(2)%nDuplicate = 3.d0
        call expect_r(error, get_device_property(1, 2, 1), 0.25d0, "op to node")
        call expect_r(error, get_device_property(1, 2, 2), 0.75d0, "op from node")
        call expect_r(error, get_device_property(1, 2, 3), 6.5d0, "height")
        call expect_r(error, get_device_property(1, 2, 4), -2.d0, "elevation")
        call expect_r(error, get_device_property(1, 2, 5), 20.d0, "width")
        call expect_r(error, get_device_property(1, 2, 6), 3.d0, "nDuplicate")
        call expect_r(error, get_device_property(1, 2, 8), miss_val_r, "an unknown property is the missing value")
        GateArray(1)%free = .false.
        call expect_r(error, get_device_property(1, 0, 7), 1.d0, "installed")
        GateArray(1)%free = .true.
        call expect_r(error, get_device_property(1, 0, 7), 0.d0, "removed (free)")
        GateArray(1)%free = .false.
        GateArray(2)%flow = -14.5d0
        call expect_r(error, get_gate_flow(2), -14.5d0, "gate flow")
    end subroutine test_gate_state

    ! Backs the mock's source report: constant, series (with its name) or expression, for each property.
    subroutine test_device_sources(error)
        type(error_type), allocatable, intent(out) :: error
        character(kind=c_char) :: buf(64)
        integer :: n, saved_n
        character(len=32) :: saved_name
        call setup_gates()
        saved_n = ninpaths
        saved_name = pathinput(2)%name
        ninpaths = max(ninpaths, 2)
        pathinput(2)%name = 'stage_series'
        GateArray(1)%devices(1)%op_to_node_datasource%source_type = const_data
        GateArray(1)%devices(1)%op_from_node_datasource%source_type = dss_data
        GateArray(1)%devices(1)%op_from_node_datasource%indx_ptr = 2
        GateArray(1)%devices(1)%height_datasource%source_type = expression_data
        GateArray(1)%devices(1)%height_datasource%indx_ptr = 9
        GateArray(1)%install_datasource%source_type = const_data
        call expect_i(error, get_device_source(1, 1, 1, buf, 64), 1, "constant")
        call expect_i(error, get_device_source(1, 1, 2, buf, 64), 2, "time series")
        n = 0
        do while (buf(n + 1) /= c_null_char .and. n < 63)
            n = n + 1
        end do
        call expect_true(error, cbuf_to_string(buf, n) == 'stage_series', "series name")
        call expect_i(error, get_device_source(1, 1, 3, buf, 64), 3, "expression")
        n = 0
        do while (buf(n + 1) /= c_null_char .and. n < 63)
            n = n + 1
        end do
        call expect_true(error, cbuf_to_string(buf, n) == 'expression(index=9)', "expression label")
        call expect_i(error, get_device_source(1, 0, 7, buf, 64), 1, "install source")
        call expect_i(error, get_device_source(1, 1, 8, buf, 64), 0, "an unknown property has no source")
        ninpaths = saved_n
        pathinput(2)%name = saved_name
    end subroutine test_device_sources

end module test_model_interface

program tester
    use, intrinsic :: iso_fortran_env, only: error_unit
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_model_interface, only: collect_model_interface

    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
                 new_testsuite("model_interface", collect_model_interface) &
                 ]

    do is = 1, size(testsuites)
        write (error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if

end program tester
