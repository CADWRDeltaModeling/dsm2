!> Routines to test buffer input module
!>@ingroup test_gtm_core
module ut_buffer_input

    use fruit
   
    contains
    
    subroutine test_buffer_input(inp_file_name)
        use buffer_input
        implicit none
        character(len=*), intent(in) :: inp_file_name
        call process_file(inp_file_name)
    
    end subroutine
    
end module