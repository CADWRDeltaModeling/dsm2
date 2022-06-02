module pydsm2hydro
contains
    subroutine main(fname_inp) ! bind(c, name='main')
        use fourpt
        implicit none
        character(len=*), intent(in) :: fname_inp

        call run_fourpt(fname_inp)
    end subroutine
end module pydsm2hydro
