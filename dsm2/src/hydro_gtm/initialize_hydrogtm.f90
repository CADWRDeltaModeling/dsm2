module init_hydrogtm
    use dynamicwave_calc
    use floweq1d_ds, only:DynamicWaveEqDS
    implicit none
contains 
    subroutine initialize_hydrogtm()
        dynamic_wave => DynamicWaveEqDS
    end subroutine initialize_hydrogtm
end module init_hydrogtm