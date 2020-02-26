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

!==== BOF tidefile =====================================================
module tidefile
    use network
    use channel_xsect_tbl, only: CxArea
    use channel_schematic, only: OpenChannel, CloseChannel, &
        NumberOfChannels
    use chstatus, only: GlobalStreamSurfaceElevation,GlobalStreamFlow
    use channel_schematic, only: StreamEndNode
    use netcntrl, only: NetworkQuadPts, NetworkQuadPtWt
    implicit none
!**********contains routines for processing state variables
!-----(computations of average area in channel,velocity, etc.)
!-----and for writing to the tidefile
contains
    !== Public (ChannelVelocity) ===================================

    real*8 function ChannelVelocity(ChannNum,XX)
        use grid_data
        use network
        use chconnec
        implicit none

        !   Purpose:  Compute velocity of flow in the channels.

        !   Arguments:
        integer ChannNum
        real*8  XX

        !   Argument definitions:
        !     ChannNum  - ChannelNumber
        !     XX - distance in the channel


        !   Module data:
        !  INCLUDE 'chcxrec1.inc'


        !   Local Variables:
        real*8  Area,ZZ,Q
        logical OK

        integer &
            intchan &            ! internal channel numbers &
            ,nodeup,nodedown     ! Hydro upstream and downstream 'node' number &
         

        !   Routines by module:

        integer node1, node2    !up and down global comp. node
        real*8 &
            val_x &              ! interpolated value statement function &
            ,val_up,val_down &    ! value at upstream and downstream end of chan &
            ,reach_dist &          ! distance in a reach (not channel) &
            ,reach_len            ! reach length

        !-----statement function to interpolate value along channel
        val_x(val_up,val_down,reach_dist,reach_len)=val_up-(val_up &
            -val_down)*(reach_dist/reach_len)


        !   Programmed by: Parviz Nader
        !   Date:          March  1993
        !   Modified by: Lianwu Liu
        !   Last modified: October 2011

        !-----Implementation -----------------------------------------------------

        OK = OpenChannel(ChannNum)
               
        intchan = ChannNum
        nodedown=-StreamEndNode(-intchan)
        nodeup=StreamEndNode(intchan)

        node1=int(dfloat(nodeup)+ XX/ &
            dfloat(chan_geom(intchan)%length)*(dfloat(nodedown)- &
            dfloat(nodeup)))
        if(node1==nodedown) node1 = node1 - 1
        node2 = node1 + 1
        reach_len = dfloat(chan_geom(intchan)%length)/(dfloat(nodedown)- &
            dfloat(nodeup))
        reach_dist = XX - reach_len*(node1-nodeup)
                    
        ZZ=val_x( &
            globalStreamSurfaceElevation(node1), &
            globalStreamSurfaceElevation(node2), &
            reach_dist, &
            reach_len)
        Q=val_x( &
            globalStreamFlow(node1), &
            globalStreamFlow(node2), &
            reach_dist, &
            reach_len)

        Area=CxArea(XX,ZZ)

        ChannelVelocity=Q/Area
        OK = CloseChannel()

        return
    end function

    !== Public (DetermineFirstTidefileInterval =================
    subroutine DetermineFirstTidefileInterval
        use runtime_data
        use common_tide
        use iopath_data
        use chconnec
        implicit none
        !   Purpose:  Calculate the first interval that will be in the
        !             (output) hydro tidefile. Due to averaging and
        !             alignment to calendar time boundaries, this may be delayed.

        integer, external :: incr_intvl
        integer :: first_hydro_interval
        character*14,external :: jmin2cdt
        !-----figure out the first interval to start tidefiles; if the model run
        !-----didn't start on an even time boundary, this will be delayed


        first_hydro_interval=incr_intvl(tf_start_julmin, &
            io_files(hydro,io_hdf5,io_write)%interval, NEAREST_BOUNDARY)


        if (first_hydro_interval /= start_julmin) then
            first_hydro_interval=first_hydro_interval+TidefileWriteInterval
        endif
        next_hydro_interval = first_hydro_interval
        tf_start_julmin = first_hydro_interval
        return
    end subroutine

    !== Public (AverageFlow) ===================================

    logical function AverageFlow()
        use common_tide
        use runtime_data
        use grid_data
        use network
        use chnlcomp
        use chconnec
        use chstatus
        use netcntrl           
        use channel_xsect_tbl, only: cxarea
        use netcntrl
        implicit none

        !   Purpose:  To calculate the average flow in the channels and
        !             reservoirs, so it can be stored in a hydro file
        !             for later use in a transport model.

        !   Arguments:

        !   Argument definitions:
        !     TideFileWriteInterval - Time interval for hydro file
        !     Nsample   - Number of time steps within a unit time in hydro file


        !   Local Variables:
        integer i,j,Up, Down, iconnect, icp

        !   Routines by module:


        !   Programmed by: Parviz Nader
        !   Date:          October 1994

        !-----Implementation -----------------------------------------------------

        AverageFlow = .false.

        if (julmin-time_step >= next_hydro_interval) then
            !--------Initialize
            next_hydro_interval=next_hydro_interval+TideFileWriteInterval

            do Branch=1,NumberofChannels()
                QChan(1,Branch)=0.  !QChan is period average, not inst. value
                QChan(2,Branch)=0.
            enddo

            iconnect = 0
            do i=1,Nreser
                do j=1,res_geom(i)%nnodes
                    iconnect = iconnect + 1
                    QResv(iconnect)=0.
                    inst_QResv(iconnect)=0.
                enddo
            enddo

            do i=1,max_qext
                qext(i)%avg=0.
                inst_qext(i)=0.
            enddo

            do i=1,nobj2obj
                obj2obj(i)%flow_avg=0.
                inst_obj2obj(i)=0.
            enddo
            
            do icp=1,TotalCompLocations
                Qcp(icp)=0.
            enddo            
            
        endif

        do Branch=1,NumberofChannels()
            Up=UpCompPointer(Branch)
            Down=DownCompPointer(Branch)
            QChan(1,Branch)=QChan(1,Branch)+ &
                (theta*Q(Up)+(1.-theta)*QOld(Up))
            QChan(2,Branch)=QChan(2,Branch)+ &
                (theta*Q(Down)+(1.-theta)*QOld(Down))
        enddo

        do i=1,nqext
            qext(i)%avg=qext(i)%avg+theta*qext(i)%flow + &                     
                (1.-theta)*qext(i)%prev_flow
            inst_qext(i)=inst_qext(i)+qext(i)%flow
        enddo

        iconnect = 0
        do i=1,Nreser
            do j=1,res_geom(i)%nnodes
                iconnect = iconnect + 1
                QResv(iconnect)=QResv(iconnect)+ theta*Qres(i,j)+(1.-theta)* &  
                    QresOld(i,j)
                inst_QResv(iconnect)=inst_QResv(iconnect)+Qres(i,j)
            enddo
        enddo

        do i=1,nobj2obj
            obj2obj(i)%flow_avg=obj2obj(i)%flow_avg + &                         
                theta*obj2obj(i)%flow + (1.-theta)*obj2obj(i)%prev_flow
            inst_obj2obj(i)=inst_obj2obj(i)+obj2obj(i)%flow
        enddo
        
        do icp=1,TotalCompLocations
            Qcp(icp)=Q(icp)               !output instantaneous flow to computational point, not theta average
            Zcp(icp)=WS(icp)              !was H(icp), using water surface instead
            Branch=ChannelNo(icp)            
        enddo
        
        if (julmin >= next_hydro_interval) then
            do Branch=1,NumberofChannels()
                Up=UpCompPointer(Branch)
                Down=DownCompPointer(Branch)
                QChan(1,Branch)=QChan(1,Branch)/dble(NSample)
                QChan(2,Branch)=QChan(2,Branch)/dble(NSample)
            enddo

            do icp=1,TotalCompLocations
                Qcp(icp)=Qcp(icp)/dble(NSample)
                Zcp(icp)=WS(icp)                
                Branch=ChannelNo(icp)
            enddo
            
            do i=1,nqext
                qext(i)%avg=qext(i)%avg/dble(NSample)
                inst_qext(i)=inst_qext(i)/dble(NSample)
            enddo

         
            do iconnect=1,nres_connect
                QResv(iconnect)=QResv(iconnect)/dble(NSample)
                inst_QResv(iconnect)=inst_QResv(iconnect)/dble(NSample)
            enddo

            do i=1,nobj2obj
                obj2obj(i)%flow_avg=obj2obj(i)%flow_avg/dble(NSample)
                inst_obj2obj(i)=inst_obj2obj(i)/dble(NSample)
            enddo

        endif
        AverageFlow = .true.

        Branch=0
        return
    end function

    !== Public (WriteHydroFile) ===================================

    logical function InitHydroTidefile()

        use hdfvars
        use io_units, only : unit_hydro
        use common_tide
        use iopath_data
        use grid_data
        use chconnec
        use dss
        implicit none

        !   Purpose:  To write information in a hydro file
        !             For later use in a transport model

        !   Module data:


        !   Local Variables:

        !   Routines by module:

        !**** Channel flow status:


        !   Programmed by: Parviz Nader
        !   Date:          October 1994
        !   Modified by: Ralph Finch
        !   Date: November 1997
        !   Modified by: Tawnly Pranger
        !   Date: May 2003
        !   Addition of HDF5 file writing
        !     Notes: For HDF5, make sure you are using the VS version of the binaries
        !            Also requires that the zlib library be available (get version from NSCA)
        !-----Implementation -------------------------------------------------

        InitHydroTidefile = .false.


        if (io_files(hydro,io_hdf5,io_write)%use) then
            hdf5_hydrofile=io_files(hydro,io_hdf5,io_write)%filename
            call InitHDF5File()        ! HDF5 - open space and create file
        endif


        InitHydroTidefile = .true.

        return
    end function

    !== Public (WriteHydroFile) ===================================

    logical function WriteHydroToTidefile(InitialCall)

        use HDFVARS
        use io_units, only : unit_hydro
        use common_tide
        use runtime_data
        use iopath_data
        use grid_data
        use chconnec
        use dss
        implicit none

        !   Purpose:  To write information in a hydro file
        !             For later use in a transport model

        !   Local Variables:
        integer i

        logical ok
        logical InitialCall
        !   Routines by module:

        !**** Channel flow status:

        !   Programmed by: Parviz Nader
        !   Date:          October 1994
        !   Modified by: Ralph Finch
        !   Date: November 1997
        !   Modified by: Tawnly Pranger
        !   Date: May 2003
        !   Addition of HDF5 file writing
        !     Notes: For HDF5, make sure you are using the VS version of the binaries
        !            Also requires that the zlib library be available (get version from NSCA)
        !-----Implementation -------------------------------------------------

        WriteHydroToTidefile = .false.

        OK=Compute_ChArea(InitialCall)

                                  ! Why do we have EResv????
        do i=1,NReser
            EResv(i)=YRes(i)
        end do

      
        if (julmin >= next_hydro_interval) then
            !--------Figure out how many external flows have changed
            !--------Save only the values which have changed
	         
            !This call needs to be before the assignment of prev_avg
            if (io_files(hydro,io_hdf5,io_write)%use) then
                call SetHDF5ToTime(julmin)
                call WriteQExtChangedToHDF5()
                call WriteChannelFlowToHDF5()
                call WriteReservoirFlowToHDF5()
                call WriteTransferFlowToHDF5()
                call WriteChannelAreaToHDF5()
                call WriteReservoirHeightToHDF5()
                if (output_inst) then
                    call WriteCompPointToHDF5()
                end if                 
            end if
        end if
        WriteHydroToTidefile = .true.

        return
    end function


    !== Public (Compute_ChArea) ===================================

    logical function Compute_ChArea(InitialCall)
        use common_tide
        use network
        use chconnec
        use chnlcomp
        use chstatus, only: ws
        use channel_xsect_tbl, only: cxarea
        use channel_schematic, only: numberofchannels
        implicit none

        !   Purpose:  Compute channel area at the two ends of a channel and the average value
        !             for all the channels

        !      INCLUDE 'chcxtbl.inc'  !not necessary, disabled to avoid potential conflict 9/23/2011
        !   Local Variables:
        integer Up, Down, j, I
        real*8 XX, ZZ
        logical InitialCall
      
        real*8 delx,aavg
        real*8 Area1, X1, X2, Z1, Z2, Wt1, N1,N2
        integer QuadPts
        real*8 QuadWt, QuadPt

        !   Programmed by: Parviz Nader
        !   Date:          December 97
        !   Modified:      Lianwu Liu
        !   Date:          October 2011 to use general quadpts

        !-----Implementation -------------------------------------------------

        Compute_ChArea = .false.
        do Branch=1,NumberofChannels()
            Up=UpCompPointer(Branch)
            Down=DownCompPointer(Branch)
            HChan(1,Branch)=WS(Up)-chan_geom(Branch)%bottomelev(1)
            ZChan(1,Branch)=WS(Up)
            HChan(2,Branch)=WS(Down)-chan_geom(Branch)%bottomelev(2) !hardwired
            ZChan(2,Branch)=WS(Down)

            AChan_Avg(Branch)=0.
            aavg = 0.D0
            QuadPts = NetworkQuadPts()

            if (InitialCall) then
                AChan(1,Branch)=CxArea(Dble(0.),Dble(ZChan(1,Branch)))
                AChan(2,Branch)=CxArea(dble(chan_geom(Branch)%length),Dble(ZChan(2,Branch)))
            else
                AChan(1,Branch) = AreaChannelComp(Branch,1)
                AChan(2,Branch) = AreaChannelComp(Branch, (QuadPts-1)*(Down-Up)+1)
            endif
            delx=dble(chan_geom(Branch)%length)/dble(Down-Up)

            do j=Up, Down-1

                !----generalize with QuadPts
                X1 = (dble(j-Up))*delx
                Z1 = ws(j)
                X2 = (dble(j-Up)+1.D0)*delx
                Z2 = ws(j+1)
            

                do 200 I=1,QuadPts

                    !--------Estimate quadrature-point values.

                    call NetworkQuadPtWt( I, QuadPt, QuadWt )

                    !--------Interpolation functions. To avoid potential problems, N(1),N(2) are replaced with local variable N1,N2
                    N1 = 1.0 - QuadPt
                    N2 = QuadPt

                    !--------Location of quadrature point.
                    XX = N1 * X1 + N2 * X2

                    !--------Dependent variables.
                    ZZ = N1 * Z1 + N2 * Z2

                    !--------not use Frustum Formula
                    if (InitialCall) then
                        Area1 = CxArea( XX, ZZ )
                    else
                        Area1 = AreaChannelComp(Branch, (j-Up)*(QuadPts-1)+i)
                    endif
                    Wt1 = QuadWt
                    aavg = aavg + Area1 * Wt1
               
200             continue
                enddo
                AChan_Avg(Branch)=(aavg/dble(Down-Up))
            enddo
            Branch=0

            Compute_ChArea=.true.

            return
        end function
    end module
