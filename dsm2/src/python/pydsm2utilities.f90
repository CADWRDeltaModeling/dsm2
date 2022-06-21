module pydsm2utilities
    use iso_c_binding
    implicit none

contains
    subroutine string_copy_f_c(f_string, c_string)
        character(len=*), intent(in)   :: f_string
        character(kind=c_char), intent(out) :: c_string(:)

        !local variables
        integer :: indx

        do indx = 1, len_trim(f_string)
            c_string(indx) = f_string(indx:indx)
        end do
        if (len(f_string) .gt. len_trim(f_string)) c_string(len_trim(f_string) + 1) = c_null_char
    end subroutine string_copy_f_c

    subroutine string_copy_c_f(c_string, f_string)
        character(kind=c_char), intent(in) :: c_string(:)
        character(len=*), intent(out) :: f_string

        !local variables
        integer :: indx

        do indx = 1, size(c_string)
            if (c_string(indx) .eq. c_null_char) then
                f_string(indx:) = ' '
                return
            else
                f_string(indx:indx) = c_string(indx)
            end if
        end do
    end subroutine string_copy_c_f
end module pydsm2utilities
