!Interface and procedure pointer for dynamic wave calculation
module dynamicwave_calc
    implicit none

    !define the procedure pointer for dynamic wave calculation
    procedure(dynamic_wave_interface), pointer :: dynamic_wave => null()

    !define the abstract interface for dynamic wave calculation
    abstract interface
        logical function dynamic_wave_interface(Up, Down)
            integer Up, Down
        end function dynamic_wave_interface
    end interface
end module dynamicwave_calc
!==== EOF dynamicwave_calc ==================================================