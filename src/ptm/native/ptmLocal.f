<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public License as published by
C!    the Free Software Foundation, either version 3 of the License, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public License for more details.

C!    You should have received a copy of the GNU General Public License
C!    along with DSM2.  If not, see <http://www.gnu.org/licenses/>.
</license>

c-----convert a character interval to minutes
      subroutine CharIntvl2Mins(interval, minutes)
      use constants
      implicit none

      integer*4 incr_intvl,minutes
      character interval*80

      minutes=incr_intvl(0,interval,IGNORE_BOUNDARY)
      return
      end

c-----$Id: ptmLocal.f,v 1.6.6.6 2007/07/31 18:30:41 eli2 Exp $
      subroutine convert2stringDates(julianMin, date, time)
      integer julianMin
      character date*9, time*4
      character dateTime*14, jmin2cdt*14

      dateTime = jmin2cdt(julianMin)
      date = dateTime(1:9)
      time = dateTime(11:14)
      return
      end

      real*8 function get_output(ptr)
      use io_units
      use ptm_local
      use iopath_data
      implicit none

      integer
     &     ptr

c-----global variables

c-----local variables
      integer
     &     i
     &     , fluxNumber
     &     , groupNumber

      if (pathoutput(ptr).meas_type .eq. 'ptm_flux') then
         if (pathoutput(ptr).b_part .ne. ' ') then
            fluxNumber=pathoutput(ptr).flux_group_ndx
            get_output = flux(fluxNumber).fluxOut
         endif
      else if(pathoutput(ptr).meas_type .eq. 'ptm_group')then
         if (pathoutput(ptr).b_part .ne. ' ') then
            groupNumber=pathoutput(ptr).flux_group_ndx
            get_output = groupOut(groupNumber).value
         endif
      else
	   write(unit_error,*)"Unrecognized PTM output type for output: ",
     &      pathoutput(ptr).b_part
      endif
      end

c-----++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine process_tide(new_tidefile, 
     &     first_used_tidefile, curr_tidefile)

      use common_tide
      use ptm_local
C-----Processes tide file input
      implicit none
C-----This subroutine is called from read_mult_tide after reading in the tide
C-----information
      include '../../hydrolib/network.inc'

c-----argumnents
      logical
     &     new_tidefile         ! true if new tidefile
      integer
     &     first_used_tidefile  ! first used tidefile number (INPUT)
     &     ,curr_tidefile       ! current tidefile number
     &     ,old_tide_block_no   ! old tide block number
     &     ,tide_block_no       ! tide block number within tidefile
      logical 
     &     recycle_tidefile    ! true if tidefile should be recycled (rewound)
      
c-----local variables
      integer    k!,numchangedflows
!      integer nodeIndex, reservoirNumber
     &     , new_tide           ! new tide block being used

      save old_tide_block_no

      new_tide= new_tidefile


      if (new_tide) then
         do k=1,max_reservoirs
            reservoirVolume(k)=
     &           (eresv(k)-res_geom(k).botelv)*res_geom(k).area
         enddo
      endif
c----- update all waterbody flows
      call updateWBHydroInfo()
!     update stage boundary flows ?? 
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c-----sets the model julmin time just before calling read_mult_tide
      subroutine set_tidefile_time(modelTime)
      use runtime_data
      implicit none
      integer*4 modelTime
      character*14 jmin2cdt
      julmin = modelTime
      current_date=jmin2cdt(julmin)
      return
      end

c-----++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c-----Check Hydro tidefile for size compatibility with PTM.
      subroutine check_tidefile(dim_res,dim_chan,n_res,n_chan
     &     ,tidefile)
      implicit none


! c-----argumnents
      integer
     &     dim_res,dim_chan     ! reservoir and channel array dimensions
     &     ,n_res,n_chan        ! reservoir and channels used in tidefile

      character*(*) tidefile    ! tidefile name

      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine read_ptm(init_input_file)
	use IO_Units
	use dsm2_database
      use iopath_data
	use groups,only:ConvertGroupPatternsToMembers,PrintGroupMembers
      use common_ptm
	implicit none
      integer istat
      character
     &     init_input_file*130  ! initial input file on command line [optional]
c-----get optional starting input file from command line,
c-----then from environment variables,
c-----then default

      if (len_trim(init_input_file) .eq. 0 ) then ! no command line arg
         call getenv('PTMINPUT',init_input_file)
         if (init_input_file .eq. ' ') then
            call getenv('DSM2INPUT',init_input_file)
            if (init_input_file .eq. ' ') then
               init_input_file='dsm2.inp'
            endif
         endif
      endif
c-----initialize all arrays and logical variables.
      call dsm2_init()
      call init_ptm()
c-----read input file(s)
      database_name='DSM2Input'


      if (init_input_file .ne. ' ') then
         call read_fixed(init_input_file,.true.,istat) !First pass is for envvars only
         if (istat .ne. 0) then
            write(unit_error, *)'Error in loading fixed data from text files; run stopped.'
            call exit(1)
         endif
         if (model_name .eq. miss_val_c)then
            write(unit_error, *)
     &           'Model name not loaded at command line or in text input file; run stopped.'
            call exit(1)            
         end if   
      end if
      call init_database(istat)
      if (istat .ne. 0) then
         write(unit_error, *) 'Error initializing database; run stopped.'
         call exit(1)
      endif

c-----read input for grid
      if (model_name .ne. miss_val_c) then
         call read_sql(istat)
         if (istat .ne. 0) then
            write(unit_error, *) 'Error in loading fixed data from RDMS; run stopped.'
            call exit(1)
         endif
      endif

      if (init_input_file .ne. ' ') then ! Second pass gives text input priority
         call read_fixed(init_input_file,.false.,istat)

         if (istat .ne. 0) then
            write(unit_error, *)'Error in loading fixed data from text files; run stopped.'
            call exit(1)
         endif
      end if

c-----read irregular geometry x-section data
!      call readirreg            ! read the irregular x-section data

      call check_fixed(istat)
      if (istat .ne. 0) then
         write(unit_error, *)
     &        'Error in checking fixed data; run stopped.'
         call exit(1)
      endif


c-----initialize grouping output

c      call init_group(istat)
c      if (istat .ne. 0) then
c         write(unit_error, *)
c     &        'Error in init_group.f; run stopped.'
c         call exit(1)
c      endif
c-----shift this later to check_fixed_ptm..

 !todo: eli to compile
 !revisit: ignore qual binary for now
c      call init_qual_bin()

c      call ConvertGroupPatternsToMembers

c      call PrintGroupMembers

      call check_fixed_ptm(istat)
      if (istat .ne. 0) then
         write(unit_error, *)
     &        'Error in checking fixed ptm data; run stopped.'
         call exit(1)
      endif
      call InitHDF5MemoryDims()
      call echoversion
      return
      end

c-----++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine init_flux_output()
      use runtime_data
      use constants
      use common_ptm      
      implicit none

c-----global variables

      integer*4 next_output_flush,incr_intvl
      common /local/ next_output_flush
      character
     &     jmin2cdt*14         ! convert from julian minute to char date/time

      integer istat
      istat = 0

      time_step = ptm_time_step

      prev_julmin=0
      julmin = start_julmin
      current_date = jmin2cdt(julmin)
      next_output_flush=incr_intvl(start_julmin,flush_intvl,TO_BOUNDARY)

      call update_intervals

      call init_store_outpaths(istat)

      prev_julmin = julmin
      julmin=julmin+time_step
      current_date=jmin2cdt(julmin)
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine write_flux_output()
      use runtime_data
      use constants
      implicit none

c-----global variables

      character
     &     jmin2cdt*14          ! convert from julian minute to char date/time
      integer*4 next_output_flush,incr_intvl
      common /local/ next_output_flush
      if(julmin .le. end_julmin) then

         call update_intervals

         if (julmin .ge. next_output_flush) then
            next_output_flush=incr_intvl(next_output_flush,
     &           flush_intvl,TO_BOUNDARY)
            call store_outpaths(.true.)
         else
            call store_outpaths(.false.)
         endif
         prev_julmin=julmin
         julmin=julmin+time_step
         current_date=jmin2cdt(julmin)
      endif
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine set_flux(fluxId, fluxValue)
      use ptm_local
      implicit none
c-----global variables
      integer fluxId
      real fluxValue
      flux(fluxId).fluxOut = fluxValue
      return
      end

c-----++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine set_group(groupId, groupValue)
      use ptm_local
      use common_ptm      
      implicit none
c-----global variables
      integer groupId, tmpval
      real groupValue
      groupOut(groupId).value = groupValue
      return
      end

c-----++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine close_flux_output()
	implicit none
      call store_outpaths(.true.)
      call wrt_outpaths
      return
      end

c-----++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine echoversion()
	use io_units
	use runtime_data
	use groups, only:WriteGroupMembers2File
	use common_ptm
	implicit none
	integer i
c-----copyright notices
      write(unit_screen, 805)
 805  format(/
     &     /'Copyright (C) 1996 State of California, Department of Water'
     &     /'Resources.'
     &     /
     &     /'Delta Simulation Model 2 (DSM2): A River, Estuary, and Land'
     &     /'numerical model.  No protection claimed in original FOURPT and'
     &     /'Branched Lagrangian Transport Model (BLTM) code written by the'
     &     /'United States Geological Survey.  Protection claimed in the'
     &     /'routines and files listed in the accompanying file "Protect.txt".'
     &     /'If you did not receive a copy of this file contact Dr. Paul'
     &     /'Hutton, below.'
     &     /
     &     /'This program is licensed to you under the terms of the GNU General'
     &     /'Public License, version 2, as published by the Free Software'
     &     /'Foundation.'
     &     /
     &     /'You should have received a copy of the GNU General Public License'
     &     /'along with this program; if not, contact Dr. Paul Hutton, below,'
     &     /'or the Free Software Foundation, 675 Mass Ave, Cambridge, MA'
     &     /'02139, USA.'
     &     /
     &     /'THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA'
     &     /'DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY'
     &     /'EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE'
     &     /'IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR'
     &     /'PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA'
     &     /'DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR'
     &     /'ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR'
     &     /'CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT'
     &     /'OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR'
     &     /'BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF'
     &     /'LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT'
     &     /'(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE'
     &     /'USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH'
     &     /'DAMAGE.'
     &     /
     &     )

      write(unit_screen, 808) dsm2_name, dsm2_version
      write(unit_output, 808) dsm2_name, dsm2_version
 808  format(/'DSM2-',a,' Version ',a/)
      call upcase(run_start_date)
      call upcase(run_end_date)
      write(unit_screen,920) run_start_date,run_end_date,ptm_time_step
      write(unit_output,920) run_start_date,run_end_date,ptm_time_step
 920  format(' Model  run:   Date   Time'/
     &     ' Start date: ',A14/
     &     ' End   date: ',A14/
     &     ' Time  step:',I5,' MINUTES')
      if ( npartno .gt. 0) then
         write(unit_output,1000) npartno
 1000    format(//,1x,'Number of particle injections',i6)
         write(unit_output,1010) 'Node', 'NParts'
     &        , 'Start date & time', 'End date & time'
 1010    format(//1x,'Particle Insertions'/
     &        '----------------------'///
     &        a6,1x,a6,1x,a30,1x,a30/
     &        '------ ------ ------------------------------ ',
     &        '------------------------------')
         do i=1,npartno
            call upcase(part_injection(i).start_date)
            call upcase(part_injection(i).end_date)
            write(unit_output,1020) part_injection(i).node
     &           , part_injection(i).nparts
     &           , part_injection(i).start_date
     &           , part_injection(i).end_date
         enddo

 1020    format(i6,1x,i6,1x,a30,1x,a30)

c--------output scalar parameters
 1100    format(/a/)
 1110    format(/a,f10.5/)
 1120    format(/a,i6/)
         if (ptm_ivert .eq. .FALSE.) write(unit_output,1100) 'Vertical profile disabled'
         if (ptm_itrans .eq. .FALSE.) write(unit_output,1100) 'Transverse profile disabled'
         if (ptm_iez .eq. .FALSE.) write(unit_output,1100) 'Vertical movement disabled'
         if (ptm_iey .eq. .FALSE.) write(unit_output,1100) 'Transverse movement disabled'
         if (ptm_random_seed .gt. 0) write(unit_output,1120) 'Random seed initialization: ', ptm_random_seed
         write(unit_output,1110) 'Transverse mixing constant: '
     &        , ptm_trans_constant
         write(unit_output,1110) 'Vertical mixing constant: '
     &        , ptm_vert_constant
         write(unit_output,1110) 'Transverse velocity A coef: '
     &        , ptm_trans_a_coef
         write(unit_output,1110) 'Transverse velocity B coef: '
     &        , ptm_trans_b_coef
         write(unit_output,1110) 'Transverse velocity C coef: '
     &        , ptm_trans_c_coef

c     an output of imported group memebers Jon 4/5/06
	call WriteGroupMembers2File(unit_output)

         write(unit_screen, 9000)
         write(unit_output, 9000)
 9000    format(//,1x,'Starting run...'/)
      endif

      return
      end
