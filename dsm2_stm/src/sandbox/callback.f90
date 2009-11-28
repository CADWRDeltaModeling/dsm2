

program callback_example
implicit none
include 'callback.fi'
integer :: intarg = 1
external callback1
external callback2
procedure(callback),pointer :: callback_ptr1 
callback_ptr1 => callback1
call sub1(intarg,callback_ptr1)
callback_ptr1 => callback2
call sub1(intarg,callback_ptr1)
end program

subroutine callback1(intarg)
implicit none
integer,intent(in) :: intarg
print*,"Here is the argument",intarg
end subroutine

subroutine callback2(intarg)
implicit none
integer,intent(in) :: intarg
print*,"Here is the argument times 2",2*intarg
end subroutine

subroutine sub1(intarg,callback)
implicit none
include 'callback.fi'
integer,intent(in) :: intarg
call callback(intarg)
end subroutine



