C!    Copyright (C) 1996-1999 State of California,
C!    Department of Water Resources.
C!    
C!    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
C!    numerical model.  No protection claimed in original FOURPT and
C!    Branched Lagrangian Transport Model (BLTM) code written by the
C!    United States Geological Survey.  Protection claimed in the
C!    routines and files listed in the accompanying file "Protect.txt".
C!    If you did not receive a copy of this file contact Dr. Paul
C!    Hutton, below.
C!    
C!    This program is licensed to you under the terms of the GNU General
C!    Public License, version 2, as published by the Free Software
C!    Foundation.
C!    
C!    You should have received a copy of the GNU General Public License
C!    along with this program; if not, contact Dr. Paul Hutton, below,
C!    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
C!    02139, USA.
C!    
C!    DSM2 - SPARSE LIBRARY INTERFACE COPYRIGHT
C!    AdjustReservoir() Copyright (C) 1998-1999 Eli Ateljevich
C!    
C!    Note that the routines below which contain part of an interface to
C!    the SPARSE matrix library were created by Eli Ateljevich.
C!    
C!    The SPARSE matrix library was created by Kenneth S. Kundert and
C!    the University of California for which copyright information is
C!    given below.

C!    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
C!    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
C!    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
C!    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
C!    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
C!    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
C!    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
C!    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
C!    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
C!    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
C!    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
C!    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
C!    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
C!    DAMAGE.
C!    
C!    For more information about DSM2, contact:
C!    
C!    Dr. Paul Hutton
C!    California Dept. of Water Resources
C!    Division of Planning, Delta Modeling Section
C!    1416 Ninth Street
C!    Sacramento, CA  95814
C!    916-653-5601
C!    hutton@water.ca.gov
C!    
C!    or see our home page: http://wwwdelmod.water.ca.gov/
C!    
C!    For information about the solver routines, contact:
C!    Eli Ateljevich
C!    
C!    

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
      Area=CxArea(xx,h)
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
      INTEGER i,j,Up, Down

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
            QChan(Branch,1)=0.  !QChan is period average, not inst. value
            QChan(Branch,2)=0.
         ENDDO

         DO i=1,Nreser
            DO j=1,res_geom(i).nnodes
               QResv(i,j)=0.
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
         QChan(Branch,1)=QChan(Branch,1)+
     &        (theta*Q(Up)+(1.-theta)*QOld(Up))
         QChan(Branch,2)=QChan(Branch,2)+
     &        (theta*Q(Down)+(1.-theta)*QOld(Down))
      ENDDO

      DO i=1,nqext
         qext(i).avg=qext(i).avg+theta*qext(i).flow +
     &        (1.-theta)*qext(i).prev_flow
      ENDDO

      DO i=1,Nreser
         DO j=1,res_geom(i).nnodes
            QResv(i,j)=QResv(i,j)+ theta*Qres(i,j)+(1.-theta)*
     &           QresOld(i,j)   !QResv is a time-ave version of QRes used in qual
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
            QChan(Branch,1)=QChan(Branch,1)/dble(NSample)
            QChan(Branch,2)=QChan(Branch,2)/dble(NSample)
         ENDDO

         DO i=1,nqext
            qext(i).avg=qext(i).avg/dble(NSample)
         ENDDO

         DO i=1,Nreser
            DO j=1,res_geom(i).nnodes
               QResv(i,j)=QResv(i,j)/dble(NSample)
            ENDDO
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

      OK = Compute_ChArea()
      OK = AverageFlow()

                                ! Why do we have EResv????
      DO i=1,NReser
         EResv(i)=YRes(i)
      End Do


      if (io_files(hydro,io_hdf5,io_write).use) then
         hdf5_hydrofile=io_files(hydro,io_hdf5,io_write).filename
         call InitHDF5File()        ! HDF5 - open space and create file
      endif

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
      INCLUDE 'chcxtbl.inc'
*   Local Variables:
      INTEGER Up, Down, nn, j
      real*8 xx,zz

*   Routines by module:

*****-Channel flow status:
      INTEGER  NumberofChannels
      EXTERNAL NumberofChannels

      REAL*8     CxArea, ChannelWidth, BtmElev
      EXTERNAL CxArea, ChannelWidth, BtmElev

      real*8 delx,aavg

*   Programmed by: Parviz Nader
*   Date:          December 97

*-----Implementation -------------------------------------------------

      Compute_ChArea = .FALSE.
      DO Branch=1,NumberofChannels()
         Up=UpCompPointer(Branch)
         Down=DownCompPointer(Branch)
         YChan(Branch,1)=WS(Up)-chan_geom(Branch).bottomelev(1)
         nn= 2                  ! fixme: this is hardwired chan_geom(Branch).nxsect ! last X-Section (i.e. downstream)
         YChan(Branch,2)=WS(Down)-chan_geom(Branch).bottomelev(nn)
         AChan_Avg(Branch)=0.
	   aavg = 0.D0
         AChan(Branch,1)=CxArea(Dble(0.),Dble(YChan(Branch,1)))
         AChan(Branch,2)=CxArea(dble(chan_geom(Branch).length),Dble(YChan(Branch,2)))
         delx=dble(chan_geom(Branch).length)/dble(Down-Up)
         DO j=Up, Down-1
            xx=(dble(j-Up)+0.5)*delx

           ! old version of this routine (never used, but may be better)
c-----------zavg=(ws(j)+ws(j+1))/2.
c-----------zz=zavg-BtmElev(xx)
c-----------AChan_Avg(Branch)=AChan_Avg(Branch)+CxArea(xx,zz)
            zz=(H(j)+H(j+1))/2.
            aavg=aavg+CxArea(xx,zz)
         ENDDO
         AChan_Avg(Branch)=(aavg/dble(Down-Up))
      ENDDO
      Branch=0

      Compute_ChArea=.true.

      return
      end
