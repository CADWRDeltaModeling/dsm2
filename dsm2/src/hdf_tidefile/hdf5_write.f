
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

c**********contains routines for writing data to an HDF5 file

*   Programmed by: Tawnly Pranger
*   Date:          October 2003
*   Revised:       Eli Ateljevich, October 2005
***********************************************************************
***********************************************************************

      subroutine WriteAttributesToHDF5()

      use HDF5
      use hdfvars
      use inclvars
      use runtime_data
      use grid_data
      use common_tide

      implicit none

      integer(HID_T) :: attr_id ! Attribute identifier
      integer(HID_T) :: aspace_id ! Attribute Dataspace identifier
      integer(HID_T) :: atype_id ! Attribute Dataspace identifier
      integer(HSIZE_T), dimension(1) :: adims = (/1/) ! Attribute dimension
      integer     :: arank = 1  ! Attribute rank
      integer     :: error      ! HDF5 Error flag
      integer(HSIZE_T), dimension(1) :: a_data_dims
      real(kind=4), dimension(MaxChannels) :: bottom_el1,bottom_el2
      real(kind=4), dimension(max_nodes) :: inodeidx,enodeidx
      real(kind=4), dimension(max_reservoirs) :: iresidx,eresidx
      integer :: i,j
      integer calcHDF5NumberOfTimeIntervals

      integer(HID_T) :: in_dspace_id ! Dataspace identifier
      integer     ::    in_rank = 1 ! Dataset rank
      integer(HSIZE_T), dimension(7) :: in_data_dims
c      integer(HSIZE_T), dimension(1) :: in_dims = (/0/) ! Dataset dimensions

      integer(HID_T) :: cg_dspace_id ! Dataspace identifier
      integer     ::    cg_rank = 2 ! Dataset rank
      integer(HSIZE_T), dimension(7) :: cg_data_dims

      integer(HID_T) :: ng_dspace_id ! Dataspace identifier
      integer     ::    ng_rank = 2 ! Dataset rank
      integer(HSIZE_T), dimension(7) :: ng_data_dims
      integer, dimension(max_nodes) :: node_obj

      integer(HID_T) :: rg_dspace_id ! Dataspace identifier
      integer     ::    rg_rank = 2 ! Dataset rank
      integer(HSIZE_T), dimension(7) :: rg_data_dims
      integer, dimension(max_reservoirs) :: res_obj

      integer(HID_T) :: bname_dspace_id ! Dataspace identifier
      integer(HID_T) :: bnode_dspace_id ! Dataspace identifier
      integer     ::    boundary_rank = 1 ! Dataset rank
      integer(HSIZE_T), dimension(7) :: bname_data_dims
      integer(HSIZE_T), dimension(7) :: bnode_data_dims
      character*32, dimension(max_stgbnd) :: bname_obj
      integer, dimension(max_stgbnd) :: bnode_obj
      integer(SIZE_T) :: typesize
      integer(HID_T) :: dtc32_id ! Memory datatype identifier

      integer(HID_T) :: cparms  !dataset creatation property identifier
      integer(HSIZE_T), dimension(2) :: h_offset
      integer(HID_T) :: filespace ! Dataspace identifier
      integer(HID_T) :: memspace ! memspace identifier

      integer, parameter :: label_len = 12
	integer, parameter :: name_len=32
      character(LEN=label_len),dimension(2) :: chan_location 
     &                                    = (/"upstream","downstream"/)
      character(LEN=name_len),dimension(:), allocatable :: names


      integer :: tempstart,tempend,nlen

      integer cdt2jmin
      EXTERNAL cdt2jmin

      a_data_dims(1) = 1
                                ! Creating attributes for HDF5
      call h5screate_simple_f(arank, adims, aspace_id, error)

      call h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, error)

      nlen = len('Hydro Version ' // trim(dsm2_version))
      call h5tset_size_f(atype_id, nlen, error)
      call h5acreate_f(hydro_id, "Hydro Version",
     &     atype_id, aspace_id, attr_id, error)
      call h5awrite_f(attr_id, atype_id, 'Hydro Version ' // trim(dsm2_version),
     &     a_data_dims, error)
      nlen = len(tf_start_date)
      call h5tset_size_f(atype_id, nlen, error)
      call h5acreate_f(hydro_id, "Start time string",
     &     atype_id, aspace_id, attr_id, error)
      call h5awrite_f(attr_id, atype_id, tf_start_date, a_data_dims, error)
	call h5tclose_f(atype_id,error)

      call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)
      call h5acreate_f(hydro_id, "Maximum number of reservoirs",
     &     atype_id, aspace_id, attr_id, error)
      call h5awrite_f(attr_id, atype_id, MaxNres, a_data_dims, error)
      call h5acreate_f(hydro_id, "Maximum number of channels",
     &     atype_id, aspace_id, attr_id, error)
      call h5awrite_f(attr_id, atype_id, MaxChannels, a_data_dims, error)
      call h5acreate_f(hydro_id, "Number of reservoirs",
     &     atype_id, aspace_id, attr_id, error)
      call h5awrite_f(attr_id, atype_id, Nreser, a_data_dims, error)
      call h5acreate_f(hydro_id, "Number of channels",
     &     atype_id, aspace_id, attr_id, error)
      call h5awrite_f(attr_id, atype_id, nchans, a_data_dims, error)
      call h5acreate_f(hydro_id, "Number of stage boundaries",
     &     atype_id, aspace_id, attr_id, error)
      call h5awrite_f(attr_id, atype_id, nstgbnd, a_data_dims, error)

      call h5acreate_f(hydro_id, "Start time",
     &     atype_id, aspace_id, attr_id, error)
      call h5awrite_f(attr_id, atype_id, tf_start_julmin, a_data_dims, error)
      call h5acreate_f(hydro_id, "Time interval",
     &     atype_id, aspace_id, attr_id, error)
      call h5awrite_f(attr_id, atype_id, TideFileWriteInterval,
     &                a_data_dims, error)
      call h5acreate_f(hydro_id, "Number of intervals",
     &     atype_id, aspace_id, attr_id, error)
      call h5awrite_f(attr_id, atype_id, calcHDF5NumberOfTimeIntervals(), 
     &                a_data_dims, error)
      call h5tclose_f(atype_id,error)

                 ! Write out channel geometry
      call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)


      in_dims(1) = MaxChannels
      ! Write out external channel numbers int2ext
      call h5screate_simple_f(in_rank, in_dims, in_dspace_id, error)
      call h5dcreate_f(geom_id, "channel_number", H5T_NATIVE_INTEGER,
     &     in_dspace_id, in_dset_id, error, cparms)
      call h5dwrite_f(in_dset_id,H5T_NATIVE_INTEGER, 
     &     int2ext(1), in_dims, error)
      call h5sclose_f (in_dspace_id, error)
      call h5dclose_f(in_dset_id,error)

      ! Write channel up/down labels
	in_dims(1) = 2
	call Write1DStringArray(geom_id,"channel_location",
     &                        chan_location,label_len,2)
      
	! Write reservoir names
	in_dims(1)=max_reservoirs
      allocate(names(max_reservoirs))
	names=' '
	do i=1,max_reservoirs
	   names(i)=res_geom(i).name 
      end do
	call Write1DStringArray(geom_id,"reservoir_names",names,
     &                      name_len,max_reservoirs)
      deallocate(names)     


	! Write transfer names
	in_dims(1)=max_obj2obj
      allocate(names(max_obj2obj))
	names=' '
	do i=1,max_obj2obj
	   names(i)=obj2obj(i).name 
      end do
	call Write1DStringArray(geom_id,"transfer_names",names,
     &                      name_len,max_obj2obj)
      deallocate(names) 


      ! Write external flow names
	in_dims(1)=max_qext
      allocate(names(max_qext))
	names=' '
	do i=1,max_qext
	   names(i)=qext(i).name 
      end do
	call Write1DStringArray(geom_id,"external_flow_names",names,
     &                      name_len,max_qext)
      deallocate(names) 



      ! Write out bottom_el
      cg_dims(1) = MaxChannels
      cg_dims(2) = 2            ! bottom_el:2
      cg_data_dims(1) = cg_dims(1)
      cg_data_dims(2) =  1
      call h5screate_simple_f(cg_rank, cg_dims, cg_dspace_id, error)
      call h5dcreate_f(geom_id, "channel geometry", H5T_NATIVE_REAL,
     &     cg_dspace_id, cg_dset_id, error, cparms)
      Do i = 1,MaxChannels
         bottom_el1(i) = chan_geom(i).bottomelev(1)
         bottom_el2(i) = chan_geom(i).bottomelev(2)
      end do
      h_offset(1) = 0
      h_offset(2) = bottom_elIdx
      call h5sselect_hyperslab_f(cg_dspace_id, H5S_SELECT_SET_F,
     &     h_offset, cg_data_dims, error)
      call h5screate_simple_f(cg_rank, cg_data_dims, memspace, error)
      call h5dwrite_f(cg_dset_id,H5T_NATIVE_REAL, bottom_el1, cg_data_dims,
     &     error, mem_space_id=memspace, file_space_id=cg_dspace_id)
      h_offset(2) = bottom_elIdx + 1
      call h5sselect_hyperslab_f(cg_dspace_id, H5S_SELECT_SET_F,
     &     h_offset, cg_data_dims, error)
      call h5screate_simple_f(cg_rank, cg_data_dims, memspace, error)
      call h5dwrite_f(cg_dset_id,H5T_NATIVE_REAL, bottom_el2, cg_data_dims,
     &     error, mem_space_id=memspace, file_space_id=cg_dspace_id)

                                ! Write out node geometry
      call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)

      ng_dims(1) = max_nodes
      ng_dims(2) = max_qobj * 2

      call h5screate_simple_f(ng_rank, ng_dims, ng_dspace_id, error)
      call h5dcreate_f(geom_id, "node geometry", H5T_NATIVE_INTEGER,
     &     ng_dspace_id, ng_dset_id, error, cparms)

      ng_data_dims(1) = ng_dims(1)
      ng_data_dims(2) = 1

                                ! Creation of hyperslab

      h_offset(1) = 0

                                ! Write out node_geom.qint
      do i = 1,max_qobj
         do j = 1,max_nodes
            node_obj(j) = node_geom(j).qint(i)
         end do
         h_offset(2) = i - 1
         call h5dget_space_f(ng_dset_id, filespace, error)
         call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F,
     &        h_offset, ng_data_dims, error)
         call h5screate_simple_f(ng_rank, ng_data_dims, memspace, error)
         call h5dwrite_f(ng_dset_id,H5T_NATIVE_INTEGER, node_obj, ng_data_dims,
     &        error, mem_space_id=memspace, file_space_id=filespace)
         call h5sclose_f (filespace, error)
      end do
                                ! Write out node_geom.qext
      do i = 1,max_qobj
         do j = 1,max_nodes
            node_obj(j) = node_geom(j).qext(i)
         end do
         h_offset(2) = max_qobj + i - 1
         call h5dget_space_f(ng_dset_id, filespace, error)
         call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F,
     &        h_offset, ng_data_dims, error)
         call h5screate_simple_f(ng_rank, ng_data_dims, memspace, error)
         call h5dwrite_f(ng_dset_id,H5T_NATIVE_INTEGER, node_obj, ng_data_dims,
     &        error, mem_space_id=memspace, file_space_id=filespace)
         call h5sclose_f (filespace, error)
      end do

                                ! Write out reservoir geometry
      call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)

      rg_dims(1) = max_reservoirs
      rg_dims(2) = max_qobj * 2

      call h5screate_simple_f(rg_rank, rg_dims, rg_dspace_id, error)
      call h5dcreate_f(geom_id, "reservoir geometry", H5T_NATIVE_INTEGER,
     &     rg_dspace_id, rg_dset_id, error, cparms)

      rg_data_dims(1) = rg_dims(1)
      rg_data_dims(2) = 1

                                ! Creation of hyperslab
      call h5dget_space_f(rg_dset_id, filespace, error)
      h_offset(1) = 0

                                ! Write out res_geom.qint
      do i = 1,max_qobj
         do j = 1,max_reservoirs
            res_obj(j) = res_geom(j).qint(i)
         end do
         call h5dget_space_f(rg_dset_id, filespace, error)
         h_offset(2) = i - 1
         call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F,
     &        h_offset, rg_data_dims, error)
         call h5screate_simple_f(rg_rank, rg_data_dims, memspace, error)
         call h5dwrite_f(rg_dset_id,H5T_NATIVE_INTEGER, res_obj, rg_data_dims,
     &        error, mem_space_id=memspace, file_space_id=filespace)
         call h5sclose_f (filespace, error)
      end do
                                ! Write out res_geom.qext
      do i = 1,max_qobj
         do j = 1,max_reservoirs
            res_obj(j) = res_geom(j).qext(i)
         end do
         call h5dget_space_f(rg_dset_id, filespace, error)
         h_offset(2) = max_qobj + i - 1
         call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F,
     &        h_offset, rg_data_dims, error)
         call h5screate_simple_f(rg_rank, rg_data_dims, memspace, error)
         call h5dwrite_f(rg_dset_id,H5T_NATIVE_INTEGER, res_obj, rg_data_dims,
     &        error, mem_space_id=memspace, file_space_id=filespace)
         call h5sclose_f (filespace, error)
      end do

      call InitObj2ObjType()
      if (nobj2obj .gt. 0) then
         call WriteObj2ObjAttribute()
      endif

      call InitQExtType()
      call WriteQExt()

                                ! Write out Stage Boundaries
      call h5pcreate_f(H5P_DATASET_CREATE_F, cparms, error)

      bname_dims(1) = max_stgbnd
      bnode_dims(1) = max_stgbnd

      call h5tcopy_f(H5T_NATIVE_CHARACTER, dtc32_id, error)
      typesize = 32
      call h5tset_size_f(dtc32_id, typesize, error)

      call h5screate_simple_f(boundary_rank, bname_dims, bname_dspace_id, error)
      call h5dcreate_f(geom_id, "stage boundary names", dtc32_id,
     &     bname_dspace_id, bname_dset_id, error, cparms)
      call h5screate_simple_f(boundary_rank, bnode_dims, bnode_dspace_id, error)
      call h5dcreate_f(geom_id, "stage boundary nodes", H5T_NATIVE_INTEGER,
     &     bnode_dspace_id, bnode_dset_id, error, cparms)

      bname_data_dims(1) = bname_dims(1)
      bnode_data_dims(1) = bnode_dims(1)

      do i = 1,max_stgbnd
         bname_obj(i) = stgbnd(i).name
         bnode_obj(i) = stgbnd(i).node
      end do

      call h5screate_simple_f(boundary_rank, bname_data_dims, memspace, error)
      call h5dwrite_f(bname_dset_id,dtc32_id, bname_obj, bname_data_dims, error)
      call h5screate_simple_f(boundary_rank, bnode_data_dims, memspace, error)
      call h5dwrite_f(bnode_dset_id,H5T_NATIVE_INTEGER, bnode_obj, bnode_data_dims, error)

      return
      end

***********************************************************************
***********************************************************************

      subroutine Write1DStringArray(dest_id,name,arr,strlen,nstr)

      use HDF5
      use hdfvars
      use inclvars

      implicit none
      integer(HID_T),intent(in) :: dest_id ! Destination group identifier      
      character*(*) :: name       ! Name of array
	integer       :: strlen     ! Length of string element
	integer       :: nstr       ! Dimension of string array
	character(LEN=strlen),dimension(nstr) :: arr

      integer(HID_T) :: dspace_id ! Dataspace identifier
      integer(HID_T) :: dtype_id  ! Dataspace identifier
      integer(HID_T) :: dset_id  ! Dataspace identifier
      integer,parameter  ::    drank = 1 ! Dataset rank


      integer,parameter  :: arank = 1  ! Attribute rank
      integer     :: error      ! HDF5 Error flag
      integer(HSIZE_T), dimension(arank) :: data_dims

	data_dims(1)=nstr
      call h5tcopy_f(H5T_NATIVE_CHARACTER, dtype_id, error)
      call h5tset_size_f(dtype_id, strlen, error)
	call h5screate_simple_f(drank, data_dims, dspace_id, error)
	call h5dcreate_f(dest_id,trim(name),dtype_id,
     &     dspace_id, dset_id, error)
	call h5dwrite_f(dset_id,dtype_id, 
     &     arr(1), data_dims, error)
	call h5tclose_f(dtype_id,error)
	call h5sclose_f(dspace_id,error)
	call h5dclose_f(dset_id,error)
      return
	end subroutine




***********************************************************************
***********************************************************************

      subroutine WriteChannelAreaToHDF5()

      use HDF5                  ! HDF5 This module contains all necessary modules
      use hdfvars
      use inclvars
      use common_tide

      implicit none

      integer(HSIZE_T), dimension(3) :: h_offset = (/0,0,0/)
      integer        :: error   ! HDF5 Error flag
      integer :: flag
      if (mod(hdf5point,24*10) .eq. 1) call h5garbage_collect_f(error)


          ! Write out channel stage
      h_offset(1) = 0
      h_offset(2) = 0
      h_offset(3) = hdf5point
  

      call h5dget_space_f (chan_z_dset_id, chan_z_fspace_id, error)
      call h5sselect_hyperslab_f(chan_z_fspace_id, H5S_SELECT_SET_F,
     &     h_offset, chan_z_fsubset_dims, error)
      call h5dwrite_f(chan_z_dset_id,H5T_NATIVE_REAL, YChan, chan_z_mdata_dims,
     &     error, chan_z_memspace, chan_z_fspace_id)
	call VerifyHDF5(error,"Channel stage write")
      call h5sclose_f (chan_z_fspace_id, error)
   

          ! Write out AChan
      call h5dget_space_f (chan_a_dset_id, chan_a_fspace_id, error)
      call h5sselect_hyperslab_f(chan_a_fspace_id, H5S_SELECT_SET_F,
     &     h_offset, chan_a_fsubset_dims, error)
      call h5dwrite_f(chan_a_dset_id,H5T_NATIVE_REAL, AChan, chan_a_mdata_dims,
     &     error, chan_a_memspace, chan_a_fspace_id)
	call VerifyHDF5(error,"Channel area write")
      call h5sclose_f (chan_a_fspace_id, error)


      h_offset(1) = 0
      h_offset(3) = 0
      h_offset(2) = hdf5point
          ! Write out AChan avg
      call h5dget_space_f (chan_aa_dset_id, chan_aa_fspace_id, error)
      call h5sselect_hyperslab_f(chan_aa_fspace_id, H5S_SELECT_SET_F,
     &     h_offset, chan_aa_fsubset_dims, error)
      call h5dwrite_f(chan_aa_dset_id,H5T_NATIVE_REAL, AChan_Avg, chan_aa_mdata_dims,
     &     error, chan_aa_memspace, chan_aa_fspace_id)
	call VerifyHDF5(error,"Channel avg area write")
      call h5sclose_f (chan_aa_fspace_id, error)   

      return
      end

***********************************************************************
***********************************************************************

      subroutine WriteChannelFlowToHDF5()

      use HDF5                  ! HDF5 This module contains all necessary modules
      use hdfvars
      use inclvars
      use common_tide

      implicit none

      integer(HSIZE_T), dimension(3) :: h_offset

      integer        :: error   ! HDF5 Error flag

      h_offset(1) = 0
      h_offset(2) = 0
      h_offset(3) = hdf5point   ! represents the time index, which can be
	                          ! is increased by one for each write

      call h5dget_space_f (chan_q_dset_id, chan_q_fspace_id, error)
      call h5sselect_hyperslab_f(chan_q_fspace_id, H5S_SELECT_SET_F,
     &     h_offset, chan_q_fsubset_dims, error)
      call h5dwrite_f(chan_q_dset_id,H5T_NATIVE_REAL, QChan, chan_q_mdata_dims,
     &     error, chan_q_memspace, chan_q_fspace_id)
	call VerifyHDF5(error,"Channel flow write")
      call h5sclose_f (chan_q_fspace_id, error)

      return
      end

***********************************************************************
***********************************************************************

      subroutine WriteReservoirFlowToHDF5()

      use HDF5                  ! HDF5 This module contains all necessary modules 
      use hdfvars
      use inclvars
      use common_tide

      implicit none

      integer(HSIZE_T), dimension(3) :: h_offset

      integer        :: error   ! HDF5 Error flag
      integer :: i



      h_offset(1) = 0
      h_offset(2) = 0
      h_offset(3) = hdf5point

      call h5dget_space_f (res_q_dset_id, res_q_fspace_id, error)
      call h5sselect_hyperslab_f(res_q_fspace_id, H5S_SELECT_SET_F, 
     &     h_offset, res_q_fsubset_dims, error)
                                !fixme: this may have to be inverted
      call h5dwrite_f(res_q_dset_id,H5T_NATIVE_REAL, 
     &     QResv, res_q_mdata_dims, 
     &     error, res_q_memspace, res_q_fspace_id)
	call VerifyHDF5(error,"Reservoir flow write")
      call h5sclose_f (res_q_fspace_id, error)      

      return
      end

***********************************************************************
***********************************************************************

      subroutine WriteReservoirHeightToHDF5()

      use HDF5                  ! HDF5 This module contains all necessary modules 
      use hdfvars
      use inclvars
      use common_tide

      implicit none

      integer(HSIZE_T), dimension(3) :: h_offset
      integer        :: error   ! HDF5 Error flag

      h_offset(1) = 0
      h_offset(2) = hdf5point

      call h5dget_space_f (res_h_dset_id, res_h_fspace_id, error)
      call h5sselect_hyperslab_f(res_h_fspace_id, H5S_SELECT_SET_F, 
     &     h_offset, res_h_fsubset_dims, error) 
      call h5dwrite_f(res_h_dset_id,H5T_NATIVE_REAL, EResv, res_h_mdata_dims, 
     &     error, res_h_memspace, res_h_fspace_id)
	call VerifyHDF5(error,"Reservoir height write")
      call h5sclose_f (res_h_fspace_id, error)      



      return
      end

***********************************************************************
***********************************************************************

      subroutine WriteTransferFlowToHDF5()

      use HDF5                  ! HDF5 This module contains all necessary modules 
      use hdfvars
      use inclvars
      use common_tide

      implicit none

      integer(HSIZE_T), dimension(2) :: h_offset

      integer        :: error   ! HDF5 Error flag
      real(kind=4), dimension(max_qext) :: objavg = 0.

      integer::i

      do i=1,nobj2obj
         objavg(i) = obj2obj(i).flow_avg
      end do

      h_offset(1) = 0
      h_offset(2) = hdf5point
      call h5dget_space_f (transfer_dset_id, transfer_fspace_id, error)
      call h5sselect_hyperslab_f(transfer_fspace_id, H5S_SELECT_SET_F, 
     &     h_offset, transfer_fsubset_dims, error) 
      call h5dwrite_f(transfer_dset_id,H5T_NATIVE_REAL, objavg, transfer_mdata_dims, 
     &     error, transfer_memspace, transfer_fspace_id)
	call VerifyHDF5(error,"Transfer flow write")
      call h5sclose_f (transfer_fspace_id, error)

      return
      end

***********************************************************************
***********************************************************************

      subroutine WriteQExtChangedToHDF5()

      use HDF5                  ! HDF5 This module contains all necessary modules 
      use hdfvars
      use inclvars
      use common_tide

      implicit none

      integer(HSIZE_T), dimension(2) :: h_offset

      integer        :: error   ! HDF5 Error flag
      real(kind=4), dimension(max_qext) :: qextavg = 0.

      integer::i

c-----Preprocess the values to changes
! fixme: should this be done here?
      !if (hdf5point .eq. 0) then
      !   do i=1,max_qext
      !      qextavg(i) = qext(i).avg
      !   end do
      !else
         do i=1,max_qext
            qextavg(i) = qext(i).avg !- qext(i).prev_avg 
         end do
      !endif
      do i=1,max_qext
         qext(i).prev_avg = qext(i).avg
      end do

                                ! Creation of hyperslab
      h_offset(1) = 0
      h_offset(2) = hdf5point
      call h5dget_space_f (qext_change_dset_id, qext_fspace_id, error)
      call h5sselect_hyperslab_f(qext_fspace_id, H5S_SELECT_SET_F, 
     &     h_offset, qext_fsubset_dims, error) 
      call h5dwrite_f(qext_change_dset_id,H5T_NATIVE_REAL, qextavg, qext_mdata_dims, 
     &     error, qext_memspace, qext_fspace_id)
	call VerifyHDF5(error,"Qextchanged write")
      call h5sclose_f (qext_fspace_id, error)

      return
      end

***********************************************************************
***********************************************************************

      subroutine WriteObj2ObjAttribute()

      use HDF5
      use hdfvars
      use objvars
      use inclvars
      use grid_data

      implicit none

      integer ::   error        ! Error flag
      integer(HSIZE_T), dimension(1) :: data_dims 
      integer(HSIZE_T), dimension(1) :: h_data_dims 
      integer(HSIZE_T), dimension(1) :: h_offset
      integer(HID_T) :: filespace ! Dataspace identifier 
      integer(HID_T) :: memspace ! memspace identifier 
      integer     ::    rank = 1 ! Dataset rank

      integer::i

      data_dims(1) = max_obj2obj
      h_data_dims(1) = 1

      do i = 1,max_obj2obj

         h_offset(1) = i - 1
         call h5dget_space_f(obj2obj_dset_id, filespace, error)
         call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, 
     &        h_offset, h_data_dims, error) 
         call h5screate_simple_f(rank, h_data_dims, memspace, error)
         call h5dwrite_f(obj2obj_dset_id, o_name_tid, obj2obj(i).name, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, fo_id_tid, obj2obj(i).from.object, data_dims, error,
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, fo_name_tid, obj2obj(i).from.obj_name, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, fo_num_tid, obj2obj(i).from.object_no, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, fo_hychan_tid, obj2obj(i).from.hydrochan, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, fo_massfrac_tid, obj2obj(i).from.mass_frac, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, fo_coeff_tid, obj2obj(i).from.coeff, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, to_id_tid, obj2obj(i).to.object, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, to_name_tid, obj2obj(i).to.obj_name, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, to_num_tid, obj2obj(i).to.object_no, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, to_hychan_tid, obj2obj(i).to.hydrochan, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, to_massfrac_tid, obj2obj(i).to.mass_frac, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, to_coeff_tid, obj2obj(i).to.coeff, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, constval_tid, obj2obj(i).constant_value, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, datasrc_type_tid,obj2obj(i).datasource.source_type, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, datasrc_idx_tid, obj2obj(i).datasource.indx_ptr, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, datasrc_val_tid, obj2obj(i).datasource.value, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, curQ_tid, obj2obj(i).flow, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, prevQ_tid, obj2obj(i).prev_flow, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, avgQ_tid, obj2obj(i).flow_avg, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(obj2obj_dset_id, conc_tid, obj2obj(i).constituent_conc, data_dims, error, 
     &        xfer_prp = obj_plist_id, mem_space_id=memspace, file_space_id=filespace)
      end do

      return
      end

***********************************************************************
***********************************************************************

      subroutine WriteQExt()

      use HDF5
      use hdfvars
      use qextvars
      use inclvars
      use grid_data

      implicit none

      integer ::   error        ! Error flag
      integer(HSIZE_T), dimension(7) :: data_dims
      integer(HSIZE_T), dimension(7) :: h_data_dims
      integer(HSIZE_T), dimension(1) :: h_offset
      integer(HID_T) :: filespace ! Dataspace identifier
      integer(HID_T) :: memspace ! memspace identifier
      integer     ::    rank = 1 ! Dataset rank

      integer::i

      data_dims(1) = max_qext
      h_data_dims(1) = 1

      if (rank .eq. 1) then
!         return
      endif

                                !! TODO Add nqext as an attribute  !! done in init??

      do i = 1,max_qext

         h_offset(1) = i - 1
         call h5dget_space_f(qext_dset_id, filespace, error)
         call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F,
     &        h_offset, h_data_dims, error)
         call h5screate_simple_f(rank, h_data_dims, memspace, error)

                                ! Select Hyperslab
         call h5dwrite_f(qext_dset_id, q_name_tid, qext(i).name, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_flow_tid, qext(i).flow, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_prev_flow_tid, qext(i).prev_flow, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_avg_tid, qext(i).avg, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_prev_flow_tid, qext(i).prev_avg, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_dsrc_type_tid, qext(i).datasource.source_type, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_dsrc_idx_tid, qext(i).datasource.indx_ptr, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_dsrc_val_tid, qext(i).datasource.value, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_chng_idx_tid, qext(i).changed_ndx, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_obj_name_tid, qext(i).obj_name, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_attach_id_tid, qext(i).attach.object, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_attach_name_tid, qext(i).attach.obj_name, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_attach_num_tid, qext(i).attach.object_no, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_grp_idx_tid, qext(i).group_ndx, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
         call h5dwrite_f(qext_dset_id, q_mass_frac_tid, qext(i).mass_frac, data_dims, error,
     &        xfer_prp = qext_plist_id, mem_space_id=memspace, file_space_id=filespace)
      end do

      return
      end

***********************************************************************
***********************************************************************
