module exactsolution
use stm_precision

contains
subroutine exact_sol(error,conc_exact &  
                                    ,ntime,ncell,i_case,dx,dt,pi,conc_result)

integer :: i_case
integer :: ntime
integer :: ncell
integer :: ivar,jvar,kvar

integer,parameter:: stm_real=8


real(stm_real) :: conc_exact(ncell,ntime)
real(stm_real) :: error(ncell,ntime)
real(stm_real) :: conc_result(ncell,ntime)
real(stm_real) ::pi,dx,dt


if (i_case==2) then
do ivar=1,ncell
    do jvar=1,ntime
        conc_exact(ivar,jvar)=two*((ivar-1)*dx+1.0d-1)+two*two*cos(half*pi*((ivar-1)*dx+1.0d-1))*exp(-(jvar)*dt*(pi**2.0d0)/4.0d0)
    end do
end do
else 
 print *, 'there is no exact solution for this case' 
end if

do ivar2=1,ncell
 do jvar2=1,ntime
    error(ivar2,jvar2)= abs(conc_exact(ivar2,jvar2)-conc_result(ivar2,jvar2))/conc_exact(ivar2,jvar2)
 end do
end do

!open(4,file='error.dat',status='unknown')
open (100,file= 'errorcomp.dat',status= 'unknown')
    write (100,*) ' Number of volumes are    ' , ncell-1
    write (100,*) '    X     -', '      error percentage    -   ' , '         abs (Uexact-Unum) error'
do kvar=1,ntime
!    write (4,*) 'step ',  kvar
!    write (4,*) 
!    write (4,*) error (:,kvar)
    
    write (100,*) kvar,'time'
    write (100,*) kvar*dt
    do ivar2=1,ncell
    write (100,*)  ((ivar2-1)*dx+1.0d-1), error (ivar2,kvar), error (ivar2,kvar)*conc_exact(ivar2,jvar2)
   end do
    
end do

return
end subroutine



endmodule exactsolution