!!<license>
!!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!!    Department of Water Resources.
!!    This file is part of DSM2.

!!    The Delta Simulation Model 2 (DSM2) is free software:
!!    you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License as published by
!!    the Free Software Foundation, either version 3 of the License, or
!!    (at your option) any later version.

!!    DSM2 is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License for more details.

!!    You should have received a copy of the GNU General Public License
!!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!!</license>
!!

!========= BOF reservoirs.f
!
!          routines for retrieving and calculating reservoir flow
!
module reservoir_geometry

    implicit none
contains
    !======================================================================
    !     Calculate reservoir area, vol
    !======================================================================
    subroutine calculateReservoirGeometry(resno, z, reser_area, reser_vol)
        use IO_Units
        use grid_data
        implicit none
        !   Arguments:
        integer resno
        real*8 z, reser_area, reser_vol
        !   Argument definitions:
        !     Z      - Elevation based!

        !----local variables
        real*8 z1,z2,A1,A2,V1,V2,dz,dV,A,factor,F
        real*8 Small
        parameter (Small = 1.00e-6)
        integer nn

        nn=res_geom(resno).nelevs
        if ( nn.eq.0) then   !constant area reservoir
           if ( z < res_geom(resno).botelv) then  !lower than the bottom
              WRITE(UNIT_ERROR,923)res_geom(resno).name,z
 923          FORMAT(' ERROR ... RESERVOIR: ',a,'HAS NEGATIVE DEPTH',/, &
                ' Water Surface Elevation =', 1PE12.5)
               call exit(2)
           endif
           reser_area=res_geom(resno).toparea
           reser_vol=reser_area*(z-res_geom(resno).botelv)
           return
        endif
        
        if ( z > res_geom(resno).elev(nn)) then  !higher than the highest layer, assume constant area
           z1=res_geom(resno).elev(nn)
           V1=res_geom(resno).vol(nn)
           reser_area=res_geom(resno).area(nn)
           reser_vol=reser_area*(z-z1)+V1
           return
        endif  
        
        if ( z < res_geom(resno).elev(1)) then  !lower than the bottom
           WRITE(UNIT_ERROR,923)res_geom(resno).name,z
           call exit(2)
        endif        
        
        nn=nn-1
        do while (res_geom(resno).elev(nn) > z)
        nn=nn-1
        enddo
        if ( nn < 1) then
            write(unit_error,*) 'Reservoir bug: elevation cannot be located!'
            WRITE(UNIT_ERROR,925)res_geom(resno).name,z
 925        FORMAT(' ERROR ... RESERVOIR: ',a,'Elevation =', 1PE12.5)
            call exit(2)
        end if
        
        z1=res_geom(resno).elev(nn)
        z2=res_geom(resno).elev(nn+1)
        A1=res_geom(resno).area(nn)
        A2=res_geom(resno).area(nn+1)        
        V1=res_geom(resno).vol(nn)
        V2=res_geom(resno).vol(nn+1)
        dz=z2-z1
        if ( abs(dz) <= Small) then
            write(unit_error,*) 'Reservoir division by zero! Two layers having the same elevation.'
            WRITE(UNIT_ERROR,926)res_geom(resno).name,z1
 926        FORMAT(' ERROR ... RESERVOIR: ',a,'Elevation =', 1PE12.5)
            call exit(2)
        end if
        dV=0.5*(A1+A2)*(z2-z1)
        factor=(V2-V1)/dV
        if (abs(factor-1) > 0.0001) then
            WRITE(unit_error,1926)res_geom(resno).name,z,factor,V1,V2
1926        FORMAT('resno ',a,'@Z', 2PE12.5,' Error was expecting Reservoir Factor (should be 1):',&
                   2PE12.5,' V1', 2PE12.5,' V2', 2PE12.5)
            call exit(2)
        end if 
        ! A=A1+(A2-A1)/dz*(z-z1)
        F=(A2-A1)/(z2-z1)
        A=A1+F*(z-z1)
        reser_area= A
        ! reser_vol=V1+factor*0.5*(A1+A)*(z-z1)
        reser_vol=0.5*F*(z-z1)**2+A1*(z-z1)+V1
        return
    end subroutine

end module
