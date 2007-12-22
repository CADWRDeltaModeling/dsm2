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

      subroutine read_tide_head(filenm, check_headers)

c-----read in header info from DSM2-Hydro binary tidefile
c-----or HDF5 tidefile.
c-----if check_headers is true, also check headers
      use IO_Units
      use Groups, only: ngroup
      use hdfvars
      use common_tide
      use runtime_data
      use iopath_data
      use grid_data      
      implicit none


c-----local variables

      integer unit_tide         ! binary tide file unit number
     &     ,lnblnk              ! intrisic last non blank function
     &     ,i,j,n,p             ! loop indices

      logical check_headers     ! [INPUT]
     &     ,version_fn          ! function to compare version numbers
     &     ,pcunix
     &     ,binarytf            ! true if tidefile is binary, false if HDF5
     &     ,binarytf_fn         ! determine if tidefile is HDF5 or binary fortran
     &     ,file_exists		  ! TRUE if file exists

      character*150 filenm      ! [INPUT]

      integer objType           ! type of object, channel, reservoir,...
      integer getReservoirId, getStageId, getExternalId, getInternalId
      integer getGroupIndex
      integer aIndex
      character*130 ctmp        ! temporary storage for string
!      character*21 chead        ! header (same length as 'hydro version '+dsm2_version)
      logical updated_pathoutput
     &     ,constituent_found   ! true if this constituent found for qext
      data updated_pathoutput /.false./
      common /tide_l/ updated_pathoutput

 810  format(/'Note: Blank accounting name in ',
     &     a, ' flow (from tidefile)'
     &     /' for ',a,' ',a)

 815  format(/'Warning: multiple concentrations assigned to '/
     &     a, ' ',a,' external flow (from tidefile), last one used.')

 820  format(/'Error: could not find ', a, ' flow (from tidefile)'
     &     /' accounting name ',a,' in input account names.')

 825  format(/'Too many individual constituents, increase size of'
     &     /' max_conpth:',i2)

      tidefile_version=' '
      
      binarytf=binarytf_fn(filenm)

c-----open tidefile and check version


      if (.not. binarytf) then
         hdf5_hydrofile=filenm
	   inquire (file=hdf5_hydrofile, exist=file_exists)
	   if (.not. file_exists) goto 900
         ! Opens the file and groups for DSM2
	   call OpenHDF5()
	   ! Initialize memory spaces for reading/writing
         call ReadAttributesFromHDF5()
         if (chead(:5) .ne. 'Hydro') then
            tidefile_version=' '
         else
            tidefile_version=chead(15:)
         endif
      endif




      do i=1,nqext
c--------fix incorrect mass_frac value from old tidefiles
         if (qext(i).mass_frac .eq. miss_val_r) then
            qext(i).mass_frac=1.0
         endif
      enddo



c-----Assign input path constituents to external flows for qual.
c-----An input path of the same object, object number, and group
c-----is considered to belong to the external flow.
c-----Trap multiple concentrations to same flow
      if (dsm2_module .eq. qual .and. check_headers) then
         do i=1,nqext
            n_conqext(i)=0
            do p=1,ninpaths
               if (pathinput(p).name .eq. qext(i).name) then
c-----------------matched external flow and this input path
                  constituent_found=.false.
                  do n=1,n_conqext(i)
c--------------------check for same constituent multiple times to same flow
c--------------------compare constituent type
                     if (pathinput(p).c_part .eq.
     &                    pathinput(const_qext(i,n)).c_part) then ! same constituent
                        constituent_found=.true.
                        write(unit_error,815)
     &                       trim(obj_names(qext(i).attach.object)),
     &                       trim(qext(i).attach.obj_name)
                        const_qext(i,n)=p
                     endif
                  enddo
                  if (.not. constituent_found) then
c--------------------first input path of this constituent to this flow
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
c--------check for compatible tidefile
         call check_tidefile(dim_res_tf,dim_chan_tf,n_res_tf,n_chan_tf,
     &        filenm(:lnblnk(filenm)))

      endif

      return

 900  continue                  ! error on opening tide file
      write(unit_error,920) filenm(:lnblnk(filenm))
 920  format(/'Could not open input tide file:'
     &     /a)
      call exit(2)

      end

