C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    The Delta Simulation Model 2 (DSM2) is free software: 
C!    you can redistribute it and/or modify
C!    it under the terms of the GNU General Public License as published by
C!    the Free Software Foundation, either version 3 of the License, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public License for more details.

C!    You should have received a copy of the GNU General Public License
C!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
C!</license>

*==== BOF tidefile =====================================================

c**********contains routines for processing state variables 
c-----(computations of average area in channel,velocity, etc.)
c-----and for writing to the tidefile

*== Public (ChannelVelocity) ===================================

      REAL*8 FUNCTION ChannelVelocity(ChannNum,x)
      use grid_data
      IMPLICIT NONE

*   Purpose:  Compute velocity of flow in the channels.

*   Arguments:
      INTEGER ChannNum

*   Argument definitions:
*     ChannNum  - ChannelNumber
*     Extremity - index indicating
*                   [+1] upstream end of current channel.
*                   [-1] downstream end of current channel.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'
      INCLUDE 'chcxrec1.inc'


*   Local Variables:
      REAL*8    y,x,xx,h,Area,Q
      LOGICAL OK

*   Routines by module:

      INTEGER  NumberOfStreamLocations
      REAL*8     BtmElev,CxArea
      LOGICAL  OpenChannel,CloseChannel
      REAL*8     GlobalStreamSurfaceElevation,GlobalStreamFlow
      INTEGER  StreamEndNode,nodeup,nodedown,closest_node

      EXTERNAL NumberOfStreamLocations
      EXTERNAL BtmElev,CxArea
      EXTERNAL OpenChannel,CloseChannel
      EXTERNAL GlobalStreamSurfaceElevation,GlobalStreamFlow,StreamEndNode

*   Programmed by: Parviz Nader
*   Date:          March  1993
*   Modified by:
*   Last modified:

*-----Implementation -----------------------------------------------------

      OK = OpenChannel(ChannNum)
      nodedown=-StreamEndNode(-ChannNum)
      nodeup=StreamEndNode(ChannNum)
      closest_node=int(dble(nodeup)+x/dble(chan_geom(ChannNum).length)*
     &     (dble(nodedown)-dble(nodeup))+0.5)

      xx=dble(closest_node-nodeup)/dble(nodedown-nodeup)*
     &     dble(chan_geom(ChannNum).length)

      y=globalStreamSurfaceElevation(closest_node)
      h=y-BtmElev(xx)
      Area=CxArea(xx,y)
      Q=globalStreamFlow(closest_node)
      ChannelVelocity=Q/Area
      OK = CloseChannel()

      RETURN
      END

*== Public (DetermineFirstTidefileInterval =================
      subroutine DetermineFirstTidefileInterval
      use runtime_data
      use common_tide
      use iopath_data
      implicit none
*   Purpose:  Calculate the first interval that will be in the 
*             (output) hydro tidefile. Due to averaging and 
*             alignment to calendar time boundaries, this may be delayed.
      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'
      integer, external :: incr_intvl
	integer :: first_hydro_interval
	character*14,external :: jmin2cdt
c-----figure out the first interval to start tidefiles; if the model run
c-----didn't start on an even time boundary, this will be delayed


      first_hydro_interval=incr_intvl(tf_start_julmin,
     &        io_files(hydro,io_hdf5,io_write).interval, NEAREST_BOUNDARY)


      if (first_hydro_interval .ne. start_julmin) then
         first_hydro_interval=first_hydro_interval+TidefileWriteInterval
      endif
	next_hydro_interval = first_hydro_interval
	tf_start_julmin = first_hydro_interval
      return
      end subroutine

*== Public (AverageFlow) ===================================

      LOGICAL FUNCTION AverageFlow()
      use common_tide
      use runtime_data
      use grid_data
      IMPLICIT NONE

*   Purpose:  To calculate the average flow in the channels and
*             reservoirs, so it can be stored in a hydro file
*             for later use in a transport model.

*   Arguments:

*   Argument definitions:
*     TideFileWriteInterval - Time interval for hydro file
*     Nsample   - Number of time steps within a unit time in hydro file

*   Module data:


      INCLUDE 'network.inc'

      INCLUDE 'chnlcomp.inc'
      INCLUDE 'chconnec.inc'
      INCLUDE 'chstatus.inc'
      INCLUDE 'netcntrl.inc'

*   Local Variables:
      INTEGER i,j,Up, Down, iconnect

*   Routines by module:

***** Channel flow status:
      INTEGER  NumberofChannels
      EXTERNAL NumberofChannels

      LOGICAL  CalculateReservoirFlow
      EXTERNAL CalculateReservoirFlow

*   Programmed by: Parviz Nader
*   Date:          October 1994

*-----Implementation -----------------------------------------------------

      AverageFlow = .FALSE.

      IF (julmin-time_step .ge. next_hydro_interval) THEN
C--------Initialize
         next_hydro_interval=next_hydro_interval+TideFileWriteInterval

         DO Branch=1,NumberofChannels()
            QChan(1,Branch)=0.  !QChan is period average, not inst. value
            QChan(2,Branch)=0.
         ENDDO

         iconnect = 0
         DO i=1,Nreser
            DO j=1,res_geom(i).nnodes
               iconnect = iconnect + 1
               QResv(iconnect)=0.
            ENDDO
         ENDDO

         DO i=1,max_qext
            qext(i).avg=0.
         ENDDO

         do i=1,nobj2obj
            obj2obj(i).flow_avg=0.
         enddo
      ENDIF

      DO Branch=1,NumberofChannels()
         Up=UpCompPointer(Branch)
         Down=DownCompPointer(Branch)
         QChan(1,Branch)=QChan(1,Branch)+
     &        (theta*Q(Up)+(1.-theta)*QOld(Up))
         QChan(2,Branch)=QChan(2,Branch)+
     &        (theta*Q(Down)+(1.-theta)*QOld(Down))
      ENDDO

      DO i=1,nqext
         qext(i).avg=qext(i).avg+theta*qext(i).flow +
     &        (1.-theta)*qext(i).prev_flow
      ENDDO

      iconnect = 0
      DO i=1,Nreser
         DO j=1,res_geom(i).nnodes
            iconnect = iconnect + 1
            QResv(iconnect)=QResv(iconnect)+ theta*Qres(i,j)+(1.-theta)*
     &           QresOld(i,j)   
         ENDDO
      ENDDO

      DO i=1,nobj2obj
         obj2obj(i).flow_avg=obj2obj(i).flow_avg +
     &        theta*obj2obj(i).flow + (1.-theta)*obj2obj(i).prev_flow
      ENDDO

      IF (julmin .ge. next_hydro_interval) THEN
         DO Branch=1,NumberofChannels()
            Up=UpCompPointer(Branch)
            Down=DownCompPointer(Branch)
            QChan(1,Branch)=QChan(1,Branch)/dble(NSample)
            QChan(2,Branch)=QChan(2,Branch)/dble(NSample)
         ENDDO

         DO i=1,nqext
            qext(i).avg=qext(i).avg/dble(NSample)
         ENDDO

         
         DO iconnect=1,nres_connect
               QResv(iconnect)=QResv(iconnect)/dble(NSample)
         ENDDO

         do i=1,nobj2obj
            obj2obj(i).flow_avg=obj2obj(i).flow_avg/dble(NSample)
         enddo

      ENDIF
      AverageFlow = .true.

      Branch=0
      RETURN
      END

*== Public (WriteHydroFile) ===================================

      LOGICAL FUNCTION InitHydroTidefile()

      use hdfvars
      use io_units, only : unit_hydro
      use common_tide
      use iopath_data
      use grid_data
      IMPLICIT NONE

*   Purpose:  To write information in a hydro file
*             For later use in a transport model

*   Module data:

      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'
      include '../timevar/dss.inc'

*   Local Variables:
      INTEGER i

*   Routines by module:

***** Channel flow status:
      INTEGER  NumberofChannels
      EXTERNAL NumberofChannels

      logical ok,Compute_ChArea,AverageFlow

*   Programmed by: Parviz Nader
*   Date:          October 1994
*   Modified by: Ralph Finch
*   Date: November 1997
*   Modified by: Tawnly Pranger
*   Date: May 2003
*   Addition of HDF5 file writing
*     Notes: For HDF5, make sure you are using the VS version of the binaries
*            Also requires that the zlib library be available (get version from NSCA)
*-----Implementation -------------------------------------------------

      InitHydroTidefile = .FALSE.


      if (io_files(hydro,io_hdf5,io_write).use) then
         hdf5_hydrofile=io_files(hydro,io_hdf5,io_write).filename
         call InitHDF5File()        ! HDF5 - open space and create file
      endif

      !Initialization of some values
      OK = Compute_ChArea()
      OK = AverageFlow()
                                ! Why do we have EResv????
      DO i=1,NReser
         EResv(i)=YRes(i)
      End Do

      InitHydroTidefile = .TRUE.

      RETURN
      END

*== Public (WriteHydroFile) ===================================

      LOGICAL FUNCTION WriteHydroToTidefile()

      use HDFVARS
      use io_units, only : unit_hydro
      use common_tide
      use runtime_data
      use iopath_data
      use grid_data
      IMPLICIT NONE

*   Purpose:  To write information in a hydro file
*             For later use in a transport model

*   Module data:

      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'
      include '../timevar/dss.inc'

*   Local Variables:
      INTEGER i

      logical ok, Compute_ChArea, AverageFlow
*   Routines by module:

***** Channel flow status:
      INTEGER  NumberofChannels
      EXTERNAL NumberofChannels

*   Programmed by: Parviz Nader
*   Date:          October 1994
*   Modified by: Ralph Finch
*   Date: November 1997
*   Modified by: Tawnly Pranger
*   Date: May 2003
*   Addition of HDF5 file writing
*     Notes: For HDF5, make sure you are using the VS version of the binaries
*            Also requires that the zlib library be available (get version from NSCA)
*-----Implementation -------------------------------------------------

      WriteHydroToTidefile = .FALSE.

      OK=Compute_ChArea()
      OK=AverageFlow()

                                ! Why do we have EResv????
      DO i=1,NReser
         EResv(i)=YRes(i)
      End Do

      
      IF (julmin .ge. next_hydro_interval) THEN
C--------Figure out how many external flows have changed
C--------Save only the values which have changed
	         
      !This call needs to be before the assignment of prev_avg 
         if (io_files(hydro,io_hdf5,io_write).use) then
	     call SetHDF5ToTime(julmin)
           call WriteQExtChangedToHDF5()
           call WriteChannelFlowToHDF5()
           call WriteReservoirFlowToHDF5()
           call WriteTransferFlowToHDF5()
           call WriteChannelAreaToHDF5()
           call WriteReservoirHeightToHDF5()
	    end if
      end if
      WriteHydroToTidefile = .TRUE.

      RETURN
      END


*== Public (Compute_ChArea) ===================================

      LOGICAL FUNCTION Compute_ChArea()
      use common_tide
      IMPLICIT NONE

*   Purpose:  Compute channel area at the two ends of a channel and the average value
*             for all the channels
*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'chconnec.inc'
      INCLUDE 'chnlcomp.inc'
      INCLUDE 'chstatus.inc'
!      INCLUDE 'chcxtbl.inc'  !not necessary, disabled to avoid potential conflict 9/23/2011
*   Local Variables:
      INTEGER Up, Down, j, I
      real*8 XX, ZZ

*   Routines by module:

*****-Channel flow status:
      INTEGER  NumberofChannels
      EXTERNAL NumberofChannels

      REAL*8     CxArea, ChannelWidth, BtmElev
      EXTERNAL CxArea, ChannelWidth, BtmElev
      
      INTEGER  NetworkQuadPts
      EXTERNAL NetworkQuadPts, NetworkQuadPtWt
      
      real*8 delx,aavg
      real*8 Area1, Area2, X1, X2, Z1, Z2, Wt1, Wt2, N1,N2
      INTEGER QuadPts
      real*8 QuadWt, QuadPt

*   Programmed by: Parviz Nader
*   Date:          December 97
*   Modified 9/23/2011 L. Liu to use general quadpts, and frustum formula 

*-----Implementation -------------------------------------------------

      Compute_ChArea = .FALSE.
      DO Branch=1,NumberofChannels()
         Up=UpCompPointer(Branch)
         Down=DownCompPointer(Branch)
         HChan(1,Branch)=WS(Up)-chan_geom(Branch).bottomelev(1)
         ZChan(1,Branch)=WS(Up)
         HChan(2,Branch)=WS(Down)-chan_geom(Branch).bottomelev(2) !hardwired
         ZChan(2,Branch)=WS(Down)

         AChan_Avg(Branch)=0.
	   aavg = 0.D0
         AChan(1,Branch)=CxArea(Dble(0.),Dble(ZChan(1,Branch)))
         AChan(2,Branch)=CxArea(dble(chan_geom(Branch).length),Dble(ZChan(2,Branch)))
         delx=dble(chan_geom(Branch).length)/dble(Down-Up)
         DO j=Up, Down-1
c            xx=(dble(j-Up)+0.5)*delx

           ! old version of this routine (never used, but may be better)
c-----------zavg=(ws(j)+ws(j+1))/2.
c-----------zz=zavg-BtmElev(xx)
c-----------AChan_Avg(Branch)=AChan_Avg(Branch)+CxArea(xx,zz)
c            zz=(H(j)+H(j+1))/2.
c            aavg=aavg+CxArea(xx,zz)
C----it is bad (using average depth),change to average area, Lianwu Liu 8/15/2011
!            xx=(dble(j-Up))*delx
!            zz=ws(j)
!            aavg=aavg+0.5*CxArea(xx,zz)
!            xx=(dble(j-Up)+1.D0)*delx
!            zz=ws(j+1)
!            aavg=aavg+0.5*CxArea(xx,zz)
C----use frustum formula            
!            xx=(dble(j-Up))*delx
!            zz=ws(j)
!            area1=CxArea(xx,zz)
!            xx=(dble(j-Up)+1.D0)*delx
!            zz=ws(j+1)
!            area2=CxArea(xx,zz)
!            aavg=aavg+(area1+area2+sqrt(area1*area2))/3.0

C----generalize with QuadPts            
            X1 = (dble(j-Up))*delx
            Z1 = ws(j)
            X2 = (dble(j-Up)+1.D0)*delx
            Z2 = ws(j+1)
            
            QuadPts = NetworkQuadPts()

            DO 200 I=1,QuadPts

*--------Estimate quadrature-point values.

               CALL NetworkQuadPtWt( I, QuadPt, QuadWt )

*--------Interpolation functions. To avoid potential problems, N(1),N(2) are replaced with local variable N1,N2
               N1 = 1.0 - QuadPt
               N2 = QuadPt

*--------Location of quadrature point.
               XX = N1 * X1 + N2 * X2

*--------Dependent variables.
               ZZ = N1 * Z1 + N2 * Z2

*--------do not use Frustum Formula                   
               Area1 = CxArea( XX, ZZ )
               Wt1 = QuadWt               
               aavg = aavg + Area1 * Wt1
               
*--------if use Frustum formula
!               if (I.eq.1) then
!                  Area1 = CxArea( XX, ZZ )
!                  Wt1 = QuadWt
!                  
!                  if(QuadPts.eq.1) then
!                  aavg = aavg + Area1
!                  endif
!                  
!               else if (I.eq.QuadPts) then
!                  Area2 = CxArea( XX, ZZ )
!                  Wt2 = QuadWt
!                  aavg=aavg+(Area1+Area2+ sqrt(Area1*Area2))/3.0 * (Wt1+Wt2)
!               else
!                  Area2 = CxArea( XX, ZZ )
!                  Wt2 = QuadWt/2.D0
!                  aavg=aavg+(Area1+Area2+ sqrt(Area1*Area2))/3.0 * (Wt1+Wt2)
!                  Wt1 = Wt2
!                  Area1 = Area2
!               endif
 200        CONTINUE               
         ENDDO
         AChan_Avg(Branch)=(aavg/dble(Down-Up))
      ENDDO
      Branch=0

      Compute_ChArea=.true.

      return
      end
