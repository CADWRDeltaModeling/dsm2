module mod_init_oprules_hydrogtm
    interface
        subroutine init_parser_hydrogtm() bind(C, name="init_parser_hydrogtm")
        end subroutine init_parser_hydrogtm
    end interface
contains
    logical function init_oprules_hydrogtm()
        implicit none

        call init_parser_hydrogtm()

        init_oprules_hydrogtm = .true.

        return
    end function init_oprules_hydrogtm
end module mod_init_oprules_hydrogtm
