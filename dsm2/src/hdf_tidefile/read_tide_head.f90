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

subroutine read_tide_head(filenm, check_headers)

!-----read in header info from DSM2-Hydro binary tidefile
!-----or HDF5 tidefile.
!-----if check_headers is true, also check headers
      use IO_Units
      use Groups, only: ngroup
      use hdfvars
      use common_tide
      use runtime_data
      use iopath_data
      use grid_data
      implicit none


!-----local variables

      integer unit_tide, &        ! binary tide file unit number
          lnblnk, &              ! intrisic last non blank function
          i,j,n,p               ! loop indices

      logical check_headers, &    ! [INPUT]
          file_exists		      ! TRUE if file exists

      character(LEN=*) :: filenm ! [INPUT]

      integer objType            ! type of object, channel, reservoir,...
      integer getReservoirId, getStageId, getExternalId, getInternalId
      integer getGroupIndex
      integer aIndex
      character*130 ctmp         ! temporary storage for string
!      character*21 chead        ! header (same length as 'hydro version '+dsm2_version)
      logical updated_pathoutput, &
          constituent_found     ! true if this constituent found for qext
      data updated_pathoutput /.false./
      common /tide_l/ updated_pathoutput

 810  format(/'Note: Blank accounting name in ', &
          a, ' flow (from tidefile)' &
          /' for ',a,' ',a)

 815  format(/'Warning: multiple concentrations assigned to '/ &
          a, ' ',a,' external flow (from tidefile), last one used.')

 820  format(/'Error: could not find ', a, ' flow (from tidefile)' &
          /' accounting name ',a,' in input account names.')

 825  format(/'Too many individual constituents, increase size of' &
          /' max_conpth:',i2)

      tidefile_version=' '



!-----open tidefile and check version

      hdf5_hydrofile=trim(filenm)
      inquire (file=hdf5_hydrofile, exist=file_exists)
      if (.not. file_exists) goto 900
      ! Opens the file and groups for DSM2
      call OpenHDF5()
      ! Initialize memory spaces for reading/writing
      call hdf5_read_attributes()
      if (chead(:5) .ne. 'Hydro') then
          tidefile_version=' '
      else
          tidefile_version=chead(15:)
      endif


      do i=1,nqext
!--------fix incorrect mass_frac value from old tidefiles
         if (qext(i).mass_frac .eq. miss_val_r) then
            qext(i).mass_frac=1.0
         endif
      enddo



!-----Assign input path constituents to external flows for qual.
!-----An input path of the same object, object number, and group
!-----is considered to belong to the external flow.
!-----Trap multiple concentrations to same flow
      if (dsm2_module .eq. qual .and. check_headers) then
         do i=1,nqext
            n_conqext(i)=0
            do p=1,ninpaths
               if (pathinput(p).name .eq. qext(i).name) then
!-----------------matched external flow and this input path
                  constituent_found=.false.
                  do n=1,n_conqext(i)
!--------------------check for same constituent multiple times to same flow
!--------------------compare constituent type
                     if (pathinput(p).variable .eq. &
                         pathinput(const_qext(i,n)).variable) then ! same constituent
                        constituent_found=.true.
                        write(unit_error,815) &
                            trim(obj_names(qext(i).attach_obj_type)), &
                            trim(qext(i).attach_obj_name)
                        const_qext(i,n)=p
                     endif
                  enddo
                  if (.not. constituent_found) then
!--------------------first input path of this constituent to this flow
                     n_conqext(i)=n_conqext(i)+1
                     if (n_conqext(i) .gt. max_conqext) then
                        write(unit_error,825) max_conqext
                        call exit(2)
                     endif
                     const_qext(i,n_conqext(i))=p
                  endif
               endif
            enddo
         enddo
      endif                     ! qual module

      if (check_headers) then
!--------check for compatible tidefile
         call check_tidefile(dim_res_tf,dim_chan_tf,n_res_tf,n_chan_tf, &
             filenm(:lnblnk(filenm)))

      endif

      return

 900  continue                  ! error on opening tide file
      write(unit_error,920) filenm(:lnblnk(filenm))
 920  format(/'Could not open input tide file:' &
          /a)
      call exit(2)

end

