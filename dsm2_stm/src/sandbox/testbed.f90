

program testbed
implicit none
real(kind=8) :: t0
real(kind=8) :: t1
real(kind=8) :: time_loop
real(kind=8) :: time_vector
integer, parameter :: nx = 100000
real(kind=8) :: x0(nx)
real(kind=8) :: x1(nx)
real(kind=8) :: x2(nx)
real(kind=8) :: x3(nx)
real(kind=8) :: idbl
real(kind=8) :: xmax
real(kind=8),external:: diff
integer i,icount
integer, parameter :: nrep = 200
do i = 1, nx
  call random_number(x0(i))
end do

x1=0.
x2=0.
x3=0.
print*,"Initialized"
call cpu_time(t0)
do icount = 1,nrep
  idbl = dble(icount)
  x1(2:(nx-1)) = (x0(3:nx) - x0(1:(nx-2))) !+idbl
  x2(1:(nx-1)) = (x0(2:nx) - x0(1:(nx-1))) !+idbl
  x3(2:nx)     = (x0(2:nx) - x0(1:(nx-1))) !+idbl
  x3(nx) = (x0(nx) - x0(nx-1))
  x2(1) = (x0(2) - x0(1))
end do
call cpu_time(t1)
print*,"********************************"
print*, x1(1),x1(2),x1(nx-1),x1(nx),&
        x2(1),x2(2),x2(nx-1),x2(nx),&
        x3(1),x2(2),x3(nx-1),x3(nx)
print*,"** Time: **"
print*, (t1-t0), t1, t0
x1=0.
x2=0.
x3=0.
call cpu_time(t0)
do icount = 1,nrep
  idbl = dble(icount)
  do i = 2,nx-1
    x1(i) = (x0(i+1) - x0(i-1)) !+idbl
    x2(i) = (x0(i+1) - x0(i))   !+idbl
    !x3(i) = (x0(i) - x0(i-1))   !+idbl
    !x3(i) = diff(x0(i),x0(i-1))
    x3(i) = huge(x3)
    x3(i) = (x0(i) - x0(i-1))
  end do
  x3(nx) = (x0(nx) - x0(nx-1))
  x2(1) = (x0(2) - x0(1))
end do
call cpu_time(t1)
print*,"********************************"
print*, x1(1),x1(2),x1(nx-1),x1(nx),&
        x2(1),x2(2),x2(nx-1),x2(nx),&
        x3(1),x2(2),x3(nx-1),x3(nx)
print*,"** Time: **"
print*, (t1-t0), t1, t0
pause
end program testbed

real(kind=8) function diff(x,y)
diff=x-y
return
end function


