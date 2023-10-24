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

!     Subroutine to obtain diagnostic information about a row that failed
!     to converge. The information provided includes the time, the row that
!     failed, the interpretation (equation type such as 'mass conservation'
!     of the row and the associated channel or reservoir.
!     This routine currently prints this information to a hard-wired file
!     called diagnostics.txt


subroutine solver_diagnostics(row)
    use IO_Units
    use Gates, only: NGate, GateArray, deviceTypeString
    use grid_data
    use network
    use chconnec
    use solver
    use chnlcomp
    implicit none


    integer,external :: UpstreamPointer,DownstreamPointer
    integer,parameter :: DF=2
    integer row,i,j,mid
    integer ihigh,ilow
    integer resno,rescon
    integer gateno,devno



    character*32 gatestr,devstr
    character*32  devtypestr

    !---- treat channel or reservoir separately
    if ( row <= TotalChanRows ) then
        !        !channel - binary search
        ilow=              1        ! low comparison value
        ihigh=nchans                ! high comparison value
        do while (ilow <= ihigh)
            mid = (ilow+ihigh)/2;            ! integer divide, rounds down
            if (row < (UpCompPointer(mid)*DF-1)) then
                ihigh=mid-1
            else if (row > DownCompPointer(mid)*DF) then
                ilow = mid+1
            else            !       (row .eq. mid)
                ilow=mid
                ihigh=mid
                exit
            end if
        end do
        write(unit_screen,"('Row: ',i5,1x,'Channel: ',i6,1x,'Up/down code: ', &
          i6,',',i6,1x,'Up equation:',i5,/, &
         'Up/down boundary row',i5,', ',i5,4x,'Row scale:',f10.5)"        )row, &
            chan_geom(mid)%chan_no,UpBoundaryCode(mid),DownBoundaryCode(mid), &
            UpCompPointer(mid)*DF-1,UpConstraintEq(mid),DownConstraintEq(mid), &
            RowScale(row)
    else if( row > TotalChanRows .and.  &
        row <= TotalChanResRows) then
        ! reservoir, find by direct search
        do i=1,nreser
            if (  &
                (ResEqRow(i) <= row) .and.  &
                ( (i == nreser) .or. (ResEqRow(i+1) > row) ) &
                ) then
                resno=i
                rescon=(row-ResEqRow(resno))
            end if
        end do
        if (rescon > res_geom(resno)%nconnect) then
            gatestr='(gate)'
        else
            gatestr=''
        end if
        if (rescon /= 0) then
            write(unit_screen,"('Row',i5,'Reservoir: ',a,'(',i2,')', &
             3x,'Connection no.: ',i5,4x,' to node: ',i6, &
             1x,a,4x,'Row Scale: ',f10.5,4x,'QRes=',f14.5)"            ) &
                row,trim(res_geom(resno)%name),resno, &
                rescon,node_geom(res_geom(resno)%node_no(rescon))%node_ID, &
                trim(gatestr),RowScale(row),QRes(resno,rescon)
        else
            write(unit_screen,"('Row',i5,'Reservoir: ',a,'(',i2,')')") &
                row,trim(res_geom(resno)%name),resno
        end if
    else if ( row > TotalChanResRows) then
        ! gate device equation
        do i=1,NGate
            do j=1,gateArray(i)%nDevice
                if (gateArray(i)%Devices(j)%calcRow == row) then
                    gateno=i
                    devno=j
                    gatestr=gateArray(i)%name
                    call deviceTypeString(devtypestr, &
                        gateArray(gateNo)%devices(devno)%structureType)
                    devstr=gateArray(i)%Devices(j)%name
                    goto 101 ! escape loop
                end if
            end do
        end do
101     write(unit_screen,"('Row',i5,1x,'Gate: ',a,'(',i3,')', &
             3x,a,': ',a,'(',i3,')',//,'Row Scale: ',f10.5)"        ) &
            row,trim(gatestr),gateno, &
            devtypestr,trim(devstr),devno,RowScale(row)
    end if

    return

end subroutine













