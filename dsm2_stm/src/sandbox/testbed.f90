

program testbed
implicit none
real(kind=8) :: t0
real(kind=8) :: t1
real(kind=8) :: time_loop = 0.D0
real(kind=8) :: time_vector = 0.D0
integer, parameter :: nx = 2000000
real(kind=8) :: x0(nx)
real(kind=8) :: x1(nx)
real(kind=8) :: x2(nx)
real(kind=8) :: x3(nx)

real(kind=8) :: idbl
!real(kind=8),parameter :: idbl = 7.D0
integer i,icount
integer, parameter :: nrep = 20000
real(kind=8) :: y(nrep)=0

do i = 1, nx
  call random_number(x0(i))
end do

x1=0.
x2=0.
x3=0.
print*,"Initialized"
do icount = 1,nrep
  idbl = dble(icount)
  call cpu_time(t0)
  x1(2:(nx-1)) = (x0(3:nx) - x0(1:(nx-2))) *idbl
  x2(1:(nx-1)) = (x0(2:nx) - x0(1:(nx-1))) *idbl
  x3(2:nx)     = (x0(2:nx) - x0(1:(nx-1))) *idbl
  !x1 = x1*idbl
  !x2 = x2*idbl
  !x3 = x3*idbl
  call cpu_time(t1)
  time_vector = time_vector + (t1-t0)
  x1=0.
  x2=0.
  x3=0.
  call cpu_time(t0)
  do i = 2,nx-1
    x1(i) = (x0(i+1) - x0(i-1)) !*idbl
    x2(i) = (x0(i+1) - x0(i))   !*idbl
    x3(i) = (x0(i) - x0(i-1))   !*idbl
  end do
  x3(nx) = (x0(nx) - x0(nx-1))*idbl
  x2(1) = (x0(2) - x0(1))*idbl
  call cpu_time(t1)
  time_loop = time_loop + (t1-t0)
  !if (x3(5) .gt. 170000000.D0)then
  !   print*,"exiting"
  !   !goto 10
  !end if
  !y(icount)=x2(5)+x3(17)+x1(33)
  x2(8)=x2(5)+x3(17)+x1(33)
end do
10 continue

print*,"Vector: ", time_vector," Loop: ",time_loop
pause
end program testbed