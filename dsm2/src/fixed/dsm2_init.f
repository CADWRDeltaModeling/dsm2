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

      subroutine dsm2_init
      use Groups, only: InitGroupAll
	use rate_coeff_assignment,only:initialize_rate_coefficient
      use IO_Units, only: unit_output
      use Gates, only: GATE_OPEN, GATE_FREE
      use ifport, only: getpid,getenvqq, rand              !! <INTEL>
      use constants
      use logging
      use runtime_data
      use iopath_data
      use grid_data
      use common_qual
      use common_ptm
      
      use common_tide
      use network
      use dss
      use mod_readdss
      use mod_writedss
c-----initialize variables for DSM2

      implicit none
c-----local variables

      character ctemp1*20,ctemp2*20,ctmpl*250 ! temporary
      integer
     &     istat                ! status variable (returned)
     &     ,i,iu,k,j
     &     ,itmp1,itmp2         ! temp variables
     &     ,ihr,imin,isec,ihundredth


c-----filled in check_fixed
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

      need_tmpfile_min15=.false.
      need_tmpfile_hour1=.false.
      need_tmpfile_day1=.false.
      need_tmpfile_week1=.false.
      need_tmpfile_month1=.false.
      need_tmpfile_year1=.false.
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

c-----groups. initialize the "wildcard" group
      call InitGroupAll()


c------non-converative constitute rate coefficients initialization Jon 4/12/06
      call initialize_rate_coefficient
      
c-----default checkpoint intervals
      io_files(hydro,io_restart,io_write).interval='1HOUR'
      io_files(qual,io_restart,io_write).interval='1HOUR'
      io_files(ptm,io_restart,io_write).interval='1HOUR'
c-----default binary output intervals
      io_files(hydro,io_echo,io_write).interval=miss_val_c
      io_files(qual,io_echo,io_write).interval=miss_val_c
      io_files(ptm,io_echo,io_write).interval=miss_val_c
c-----default HDF5 output intervals
      io_files(hydro,io_hdf5,io_write).interval='15MIN'
      io_files(qual,io_hdf5,io_write).interval='15MIN'
      io_files(ptm,io_hdf5,io_write).interval='15MIN'



      current_tidefile=miss_val_i
      do i=1,max_tide_files
         tide_files(i).start_date=' '
         tide_files(i).end_date=' '
         tide_files(i).filename=' '
      enddo


      do i=1,max_injection
         part_injection(i).slength=' '
         part_injection(i).length=' '
         part_injection(i).start_date=' '
         part_injection(i).end_date=' '
         part_injection(i).type=' '
      enddo

      do i=1,max_filter
         part_filter(i).type_f=miss_val_c
         part_filter(i).name=miss_val_c
         part_filter(i).ndx=miss_val_i
         part_filter(i).node=miss_val_i
         part_filter(i).resname=miss_val_c         
         part_filter(i).at_wb=miss_val_c
         part_filter(i).at_wb_type=miss_val_i         
         part_filter(i).at_wb_ndx=miss_val_i
         part_filter(i).at_wb_id=miss_val_i
         part_filter(i).op=0.0
         part_filter(i).fillin=miss_val_c
         part_filter(i).filename=miss_val_c
         part_filter(i).path=miss_val_c
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
      ! todo: is this a good idea?
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
         read(ctemp1,'(i)') itmp1
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
