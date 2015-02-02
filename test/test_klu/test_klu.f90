
module test_klu

    use klu
    use fruit
    
    contains
    
    subroutine test_klu_exmamples()
        implicit none
        call solve_sample_matrix
        call test_tri2sparse(5)
        return
    end subroutine    
    
    !> Routine to test klu sparse solver
    subroutine solve_sample_matrix()
        !       [   4.0  -1.5  -2.5   0.0 ]
        !       [  -1.5   8.0   0.0  -1.0 ] 
        !   A = [  -2.5   0.0  16.0  -1.0 ]
        !       [   0.0  -2.0  -3.0  11.0 ]
        ! RHS = [   3.0   2.0   8.0  7.0  ]
        use gtm_precision
        implicit none
        real(gtm_real) :: x(4)
        matrix_size = 4
        allocate(ica(matrix_size+1))  !ap
        allocate(jca(12)) !ai
        allocate(coo(12)) !ax
        allocate(b(matrix_size))
        ica = (/ 0, 3, 6, 9, 12/)
        jca = (/ 0, 1, 2, 0, 1, 3, 0, 2, 3, 1, 2, 3/)
        coo = (/ 4.0, -1.5, -2.5, -1.5, 8.0, -2.0, -2.5, 16.0, -3.0, -1.0, -1.0, 11.0/)
        b = (/3.0, 2.0, 8.0, 7.0/)
        
        k_common=klu_fortran_init()
        k_symbolic = klu_fortran_analyze(matrix_size, ica, jca, k_common)
        call klu_fortran_free_numeric(k_numeric, k_common)
        k_numeric = klu_fortran_factor(ica, jca, coo, k_symbolic, k_common)                        
        call klu_fortran_refactor(ica, jca, coo, k_symbolic, k_numeric, k_common)
        call klu_fortran_solve(k_symbolic, k_numeric, matrix_size, 1, b, k_common)
        x = b
        call assertEquals (x(1), dble(1.4900), weakest_eps, "problem in test_klu")
        call assertEquals (x(2), dble(0.6508), weakest_eps, "problem in test_klu")
        call assertEquals (x(3), dble(0.7935), weakest_eps, "problem in test_klu")
        call assertEquals (x(4), dble(0.9711), weakest_eps, "problem in test_klu")
        call klu_fortran_free(k_symbolic, k_numeric, k_common)
        deallocate(ica, jca, coo, b)
        return
    end subroutine
    
    
    subroutine test_tri2sparse(ncell)
        use gtm_precision
        implicit none
        integer, intent(in) :: ncell
        real(gtm_real) :: up_diag(ncell)
        real(gtm_real) :: down_diag(ncell)
        real(gtm_real) :: center_diag(ncell)
        real(gtm_real) :: ax(4+3*(ncell-2))
        integer :: ap(ncell+1)
        integer :: ai(4+3*(ncell-2))
        center_diag = one
        up_diag = two
        down_diag = three
        call tri2sparse(ap, ai, ax, up_diag, center_diag, down_diag, ncell)      
        return
    end subroutine    
    
    

    
end module    