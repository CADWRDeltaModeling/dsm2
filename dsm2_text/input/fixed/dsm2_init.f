C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.
C!
C!    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
C!    numerical model.  No protection claimed in original FOURPT and
C!    Branched Lagrangian Transport Model (BLTM) code written by the
C!    United States Geological Survey.  Protection claimed in the
C!    routines and files listed in the accompanying file "Protect.txt".
C!    If you did not receive a copy of this file contact Tara Smith,
C!    below.
C!
C!    This program is licensed to you under the terms of the GNU General
C!    Public License, version 2, as published by the Free Software
C!    Foundation.
C!
C!    You should have received a copy of the GNU General Public License
C!    along with this program; if not, contact Tara Smith, below,
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
C!    Tara Smith
C!    California Dept. of Water Resources
C!    Division of Planning, Delta Modeling Section
C!    1416 Ninth Street
C!    Sacramento, CA  95814
C!    916-653-9885
C!    tara@water.ca.gov
C!
C!    or see our home page: http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/


      subroutine dsm2_init

c-----initialize variables for DSM2

      implicit none

      include 'common.f'
      include 'common_ptm.inc'
      include '../time-varying/common_tide.f'
      include '../time-varying/dss.inc'
      include '../time-varying/readdss.inc'
      include '../time-varying/tide.inc'
      include '../../hydro/network.inc'
      
c-----local variables

      integer
     &     i,iu,k,j

      data
     &     run_start_dt /' '/
     &     ,run_end_dt /' '/
     &     ,run_length /' '/
     &     ,flush_intvl /' '/
     &     ,display_intvl /' '/
     &     ,temp_dir /' '/
     &     ,print_start_dt /max_print_dates*' '/
     &     ,print_end_dt /max_print_dates*' '/
     &     ,time_step_intvl_hydro /' '/
     &     ,time_step_intvl_qual /' '/
     &     ,time_step_intvl_ptm /' '/
     &     ,tide_cycle_length /' '/

c-----filled in check_fixed
      data
     &     acct_names / max_acct_names*miss_val_c /
     &     ,nonconserve_list / max_constituent * ' ' /

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

      value_codes(1).code='gate_open'
      value_codes(1).value=float(GATE_OPEN)
      value_codes(2).code='gate_close'
      value_codes(2).value=float(GATE_CLOSE)
      value_codes(3).code='gate_free'
      value_codes(3).value=float(GATE_FREE)
      value_codes(4).code=' '
      value_codes(4).value=miss_val_r
      
      do i=0,max_nodes
         node_geom(i).boundary_type=0
      enddo

      do i=1,max_reservoirs
         res_geom(i).name=' '
         res_geom(i).nnodes=0
         coeff_res_name(i)=' '
         res_geom(i).maxstage=miss_val_r
         do j=1,maxresnodes
            res_geom(i).maxq2res(j)=miss_val_r
         enddo
      enddo

      do i=0,max_gates
         gate_geom(i).name=' '
         gate_geom(i).loc=' '
         gate_geom(i).oper='ign '
         gate_geom(i).lapse=30  ! 30 minute default gate lapse time
         gate_geom(i).ngates=1  ! default 1 gate per structure
         gate_geom(i).widthfree=miss_val_r
         gate_geom(i).crestfree=miss_val_r
      enddo

      do i=0,max_inputpaths
         pathinput(i).filename=' '
         pathinput(i).label=' '
         pathinput(i).path=' '
         pathinput(i).a_part=' '
         pathinput(i).b_part=' '
         pathinput(i).c_part=' '
         pathinput(i).e_part=' '
         pathinput(i).f_part=' '
         pathinput(i).start_dt=' '
         pathinput(i).acct_name=' '
         pathinput(i).interval=' '
         pathinput(i).ID=' '
         pathinput(i).sign=' '
         pathinput(i).constant_value=miss_val_r
         pathinput(i).mass_frac=1.0 ! default, take 100% of mass in a sink
         pathinput(i).priority=0 ! no priority default
         pathinput(i).fillin=miss_val_i
         pathinput(i).use_flag=miss_val_i
      enddo

      do i=1,max_dssinfiles
         infilenames(i)=' '
      enddo

      do i=1,max_outputpaths
         pathoutput(i).filename=' '
         pathoutput(i).path=' '
         pathoutput(i).name=' '
         pathoutput(i).meas_type=' '
         pathoutput(i).interval=' '
         pathoutput(i).modifier=' '
         pathoutput(i).a_part=' '
         pathoutput(i).b_part=' '
         pathoutput(i).c_part=' '
         pathoutput(i).e_part=' '
         pathoutput(i).f_part=' '
         do j=1,max_ft_flux
            pathoutput(i).flux_from(j).obj_name=' '
            pathoutput(i).flux_to(j).obj_name=' '
         enddo
         pathoutput(i).source.loc_name=' '
         pathoutput(i).source.obj_name=' '
         pathoutput(i).source.acct_name=' '
      enddo

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

c-----default checkpoint intervals
      io_files(hydro,io_restart,io_write).interval='1HOUR'
      io_files(qual,io_restart,io_write).interval='1HOUR'
      io_files(ptm,io_restart,io_write).interval='1HOUR'
c-----default binary output intervals
      io_files(hydro,io_tide,io_write).interval='15MIN'
      io_files(qual,io_tide,io_write).interval='15MIN'
      io_files(ptm,io_tide,io_write).interval='15MIN'

      do i=0,max_translations
         translations(i).from_name=' '
         translations(i).obj_name=' '
         translations(i).constituent=' '
      enddo

      do i=1,max_types
         type_spec(i).string=' '
         type_spec(i).part=' '
         type_spec(i).match=' '
         type_spec(i).sign=' '
         type_spec(i).acct_name=' '
         type_spec(i).value_flag=miss_val_i
         type_spec(i).mass_frac=miss_val_r
         type_spec(i).value_in=miss_val_r
      enddo

      do i=1,max_xsects
         xsect_geom(i).init_stage=miss_val_r
         xsect_geom(i).init_flow=miss_val_r
      enddo

      do i=1,max_tide_files
         tide_files(i).start_dt=' '
         tide_files(i).end_dt=' '
         tide_files(i).filename=' '
      enddo

      do i=1,max_constituent
         constituents(i).constituent=' '
         constituents(i).loc_name=' '
      enddo

      do i=1,max_injection
         part_injection(i).slength=' '
         part_injection(i).length=' '
         part_injection(i).start_dt=' '
         part_injection(i).end_dt=' '
         part_injection(i).type=' '
      enddo

      do i=1,max_qext
         qext(i).obj_name=' '
         qext(i).attach.obj_name=' '
         qext(i).acct_name=' '
         qext(i).mass_frac=1.0  ! default, take 100% of mass in a sink
      enddo

      do i=1,max_obj2obj
         obj2obj(i).obj_name=' '
         obj2obj(i).from.acct_name=' '
         obj2obj(i).to.acct_name=' '
         obj2obj(i).label=' '
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

      return
      end
