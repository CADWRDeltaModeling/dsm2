C!    Copyright (C) 1996, 1997, 1998 State of California,
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

      subroutine dsm2_init
      use Groups, only: InitGroupAll
	use rate_coeff_assignment,only:initialize_rate_coefficient
      use IO_Units, only: unit_output
      use Gates, only: GATE_OPEN, GATE_FREE
      USE DFLIB                 !! <NT>
      
c-----initialize variables for DSM2

      implicit none

      include 'common.f'
      include 'common_ptm.inc'
      include 'common_qual.inc'
      include '../hdf_tidefile/common_tide.f'
      include '../timevar/dss.inc'
      include '../timevar/writedss.inc'
      include '../timevar/readdss.inc'
      include '../hdf_tidefile/tide.inc'
      include '../hydrolib/network.inc'
      
c-----local variables

      character ctemp1*20,ctemp2*20,ctmpl*250 ! temporary
      integer
     &     istat                ! status variable (returned)
     &     ,i,iu,k,j
     &     ,itmp1,itmp2         ! temp variables
     &     ,getpid              ! unix fortran system call to get process ID
     &     ,ihr,imin,isec,ihundredth
      data
     &     run_start_date /' '/
     &     ,run_end_date /' '/
     &     ,run_length /' '/
     &     ,tf_start_date /' '/
     &     ,flush_intvl /'5DAY'/
     &     ,display_intvl /' '/
     &     ,temp_dir /' '/
     &     ,print_start_date /max_print_dates*' '/
     &     ,print_end_date /max_print_dates*' '/
     &     ,time_step_intvl_hydro /' '/
     &     ,time_step_intvl_qual /' '/
     &     ,time_step_intvl_ptm /' '/
     &     ,tide_cycle_length /' '/
     &     ,dss_direct /.TRUE./
     &     ,binary_output /.FALSE./
     &     ,need_tmp_outfiles /.FALSE./

c-----filled in check_fixed
      data
     &     nonconserve_list / max_constituent * ' ' /

      print_level=miss_val_i


      per_type_names(per_type_per_aver)='PER-AVER'
      per_type_names(per_type_per_cum)='PER-CUM'
      per_type_names(per_type_per_min)='PER-MIN'
      per_type_names(per_type_per_max)='PER-MAX'
      per_type_names(per_type_inst_val)='INST-VAL'
      per_type_names(per_type_inst_cum)='INST-CUM'
      per_type_names(per_type_null)=' '

      obj_names(obj_channel)='channel'
      obj_names(obj_node)='node'
      obj_names(obj_reservoir)='reservoir'
      obj_names(obj_gate)='gate'
      obj_names(obj_qext)='qext'
      obj_names(obj_obj2obj)='o2o'
      obj_names(obj_flux)='flux'
      obj_names(obj_stage)='stage'
      obj_names(obj_null)=miss_val_c

      do i=0,max_nodes
         node_geom(i).boundary_type=miss_val_i
	   node_geom(i).qual_int=.false.
      enddo

      do i=1,max_reservoirs
         res_geom(i).inUse=.false.
         res_geom(i).id=0
         res_geom(i).name=' '
         res_geom(i).nnodes=0
         coeff_res_name(i)=' '
      enddo

      do i=0,max_inputpaths
         pathinput(i).use=.false.
         pathinput(i).name=' '
         pathinput(i).filename=' '
         pathinput(i).object_name=' '
         pathinput(i).path=' '
         pathinput(i).a_part=' '
         pathinput(i).b_part=' '
         pathinput(i).c_part=' '
         pathinput(i).e_part=' '
         pathinput(i).f_part=' '
         pathinput(i).start_date=' '
         pathinput(i).interval=' '
         pathinput(i).ID=' '
         pathinput(i).per_type=miss_val_i
         pathinput(i).sign=' '
         pathinput(i).object=miss_val_i
         pathinput(i).object_no=miss_val_i
         pathinput(i).constant_value=miss_val_r
         pathinput(i).value=miss_val_r
         pathinput(i).mass_frac=1.0 ! default, take 100% of mass in a sink
         pathinput(i).priority=0 ! no priority default
         pathinput(i).fillin=miss_val_i
         pathinput(i).use_flag=miss_val_i
      enddo

      do i=1,max_dssinfiles
         infilenames(i)=' '
      enddo

      do i=1,max_outputpaths
         pathoutput(i).use=.false.
         pathoutput(i).filename=' '
         pathoutput(i).path=' '
         pathoutput(i).name=' '
         pathoutput(i).object_name=' '
         pathoutput(i).meas_type=' '
         pathoutput(i).interval=' '
         pathoutput(i).modifier=' '
         pathoutput(i).a_part=' '
         pathoutput(i).b_part=' '
         pathoutput(i).c_part=' '
         pathoutput(i).e_part=' '
         pathoutput(i).f_part=' '
         pathoutput(i).object=miss_val_i
         pathoutput(i).object_no=miss_val_i
         pathoutput(i).flux_from_ndx=miss_val_i
         pathoutput(i).flux_to_ndx=miss_val_i
         pathoutput(i).flux_from_type=miss_val_i
         pathoutput(i).flux_to_type=miss_val_i
         pathoutput(i).source_group_ndx=miss_val_i
      enddo

      need_tmpfile_min15=.false.
      need_tmpfile_hour1=.false.
      need_tmpfile_day1=.false.
      need_tmpfile_week1=.false.
      need_tmpfile_month1=.false.
      need_tmpfile_year1=.false.

      do i=1,max_dssoutfiles
         outfilenames(i)=' '
      enddo

      output_filename=' '
      iu=unit_output+1
      do i=1,max_iogroups
         do j=1,max_file_types
            do k=1,2
               io_files(i,j,k).use=.false.
               io_files(i,j,k).filename=' '
               io_files(i,j,k).interval=' '
               io_files(i,j,k).unit=iu ! fill in file unit numbers
               iu=iu+1
            enddo
         enddo
      enddo

c-----groups. initialize the "wildcard" grou
      call InitGroupAll()


c------non-converative constitute rate coefficients initialization Jon 4/12/06
      call initialize_rate_coefficient
      
c-----default checkpoint intervals
      io_files(hydro,io_restart,io_write).interval='1HOUR'
      io_files(qual,io_restart,io_write).interval='1HOUR'
      io_files(ptm,io_restart,io_write).interval='1HOUR'
c-----default binary output intervals
      io_files(hydro,io_tide,io_write).interval='15MIN'
      io_files(qual,io_tide,io_write).interval='15MIN'
      io_files(ptm,io_tide,io_write).interval='15MIN'
c-----default HDF5 output intervals
      io_files(hydro,io_hdf5,io_write).interval='15MIN'
      io_files(qual,io_hdf5,io_write).interval='15MIN'
      io_files(ptm,io_hdf5,io_write).interval='15MIN'

      do i=1,max_xsects
         xsect_geom(i).init_stage=miss_val_r
         xsect_geom(i).init_flow=miss_val_r
      enddo

      current_tidefile=miss_val_i
      do i=1,max_tide_files
         tide_files(i).start_date=' '
         tide_files(i).end_date=' '
         tide_files(i).filename=' '
      enddo

      do i=1,max_constituent
         constituents(i).name=' '
         constituents(i).group_ndx=miss_val_i
         constituents(i).object=miss_val_i
         constituents(i).object_no=miss_val_i
      enddo

      do i=1,max_injection
         part_injection(i).slength=' '
         part_injection(i).length=' '
         part_injection(i).start_date=' '
         part_injection(i).end_date=' '
         part_injection(i).type=' '
      enddo

      do i=1,max_qext
         qext(i).obj_name=' '
         qext(i).attach.obj_name=' '
         qext(i).mass_frac=1.0  ! default, take 100% of mass in a sink
      enddo

      do i=1,max_obj2obj
         obj2obj(i).use=.false.
         obj2obj(i).name=' '
         obj2obj(i).from.mass_frac=1.0 ! default, take 100% of mass in a sink
         obj2obj(i).to.mass_frac=1.0 ! default, take 100% of mass in a sink
      enddo

c-----set non-conservative constituent names for DSS C part
      nonconserve_list(ncc_do)='do'
      nonconserve_list(ncc_organic_n)='organic_n'
      nonconserve_list(ncc_nh3)='nh3'
      nonconserve_list(ncc_no2)='no2'
      nonconserve_list(ncc_no3)='no3'
      nonconserve_list(ncc_organic_p)='organic_p'
      nonconserve_list(ncc_po4)='po4'
      nonconserve_list(ncc_algae)='algae'
      nonconserve_list(ncc_bod)='bod'
      nonconserve_list(ncc_temp)='temp'

      ntitles=0

C-----Qual default values

      mass_tracking=.false.
      init_conc=0.0
      dispersion=.true.

c-----irregular xsects preparation
      call prep_irreg

c-----set runtime ID; can be either the process ID
c-----(multi-tasking OS) or random number (other OS)
      crid=' '
      irid=abs(getpid())        ! Sun Unix and NT
C!OTHER  irid=int(rand(0)*1000000)  ! others
      write(crid,'(i6.6)') irid
c-----prepend custom ID to run ID?
      ctemp1=''
      istat=getenvqq('CID', ctemp1) !! <NT>
      if (istat .gt. 0) then    ! custom ID
         read(ctemp1,'(i1)') itmp1
         crid=trim(ctemp1) // '_' // trim(crid)
      endif
c-----date of run
      ctemp1=''
      call cdate(ctemp1)
      call datjul(ctemp1, itmp1, istat)
      crdt14=' '
      call juldat(itmp1, 104, crdt14(1:9), itmp2) ! DDMMMYYYY
      call juldat(itmp1, -11, ctemp1, itmp2)
      ctemp1=ctemp1(:itmp2)
      crdt10=' '
      crdt10(1:2)=ctemp1(7:8)   ! YYMMDD (easy to sort on)
      crdt10(3:4)=ctemp1(1:2)
      crdt10(5:6)=ctemp1(4:5)
c-----time of run
      ctemp1=' '
c      call ctime(ctemp1)
      call gettim(ihr,imin,isec,ihundredth)
      write (ctemp1,'(2I2)') ihr,imin
      crdt14(11:12)=ctemp1(1:2) ! hhmm
      crdt14(13:14)=ctemp1(3:4)
      crdt10(7:8)=ctemp1(1:2)   ! hhmm
      crdt10(9:10)=ctemp1(3:4)
	
	return
      end
