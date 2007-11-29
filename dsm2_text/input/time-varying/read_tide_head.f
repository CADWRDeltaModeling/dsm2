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


      subroutine read_tide_head(filenm, check_headers)

c-----read in header info from DSM2-Hydro tide (binary) file.
c-----if check_headers is true, also check headers

      implicit none

      include '../fixed/common.f'
      include 'common_tide.f'
      include 'tide.inc'

c-----local variables

      integer unit_tide         ! binary tide file unit number
     &     ,lnblnk              ! intrisic last non blank function
     &     ,i,j,n,p             ! loop indices

      logical check_headers     ! [INPUT]
     &     ,version_fn          ! function to compare version numbers
     &     ,pcunix
      character*150 filenm      ! [INPUT]

      integer objType           ! type of object, channel, reservoir,...
      integer getReservoirId, getStageId, getExternalId, getInternalId
      integer getAccountingIndex
      integer aIndex
      character*130 ctmp        ! temporary storage for string
      character*21 chead        ! header (same length as 'hydro version '+dsm2_version)
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

c-------open tidefile and check version
      unit_tide=io_files(qual,io_tide,io_read).unit
      close(unit_tide)
      open(unit_tide
     &     ,file=filenm
     &     ,status='old'
     &     ,form='unformatted'
c  
     &     ,convert='big_endian'          !! <NT> Comment this line for UNIX executable
     &     ,err=900)

      tidefile_version=' '
      read(unit_tide) chead

      if (chead(:5) .ne. 'Hydro') then
         rewind(unit_tide)
         tidefile_version=' '
      else
         tidefile_version=chead(15:)
      endif
c-----cannot read tidefiles earlier than 4.17
      if (.not. version_fn(tidefile_version, '4.17')) then
         write(unit_error,805)
 805     format(/'Cannot read a tidefile written before Hydro version 4.17')
         call exit(2)
      endif

      
      read(unit_tide) dim_res_tf,dim_chan_tf
      read(unit_tide) n_res_tf,n_chan_tf
      read(unit_tide) repeating_tidefile
      read(unit_tide) int2ext,ext2int

      read(unit_tide) (chan_geom(i).bottomelev(1),
     &     chan_geom(i).bottomelev(2),i=1,max_channels)

      read(unit_tide) (node_geom(i).qint,node_geom(i).qext,
     &     i=1,max_nodes)
      read(unit_tide) (res_geom(i).qint,res_geom(i).qext,
     &     i=1,max_reservoirs)


c----- two ways of reading structures. One is backward compatible with 4.18 - 6.0 tidefiles
c      the other (newer) is required for PC-UNIX compatible files.
         pcunix=version_fn(tidefile_version, '6.1')



c--------object-to-object (internal) flow info
         read(unit_tide) nobj2obj
      if (.not. pcunix) then
         read(unit_tide)obj2obj
      else
         read(unit_tide) (
     &          obj2obj(i).obj_name,
     &          obj2obj(i).from.object,
     &          obj2obj(i).from.obj_name,
     &          obj2obj(i).from.object_no,
     &          obj2obj(i).from.hydrochan,
     &          obj2obj(i).from.acct_name,
     &          obj2obj(i).from.acct_ndx,
     &          obj2obj(i).from.mass_frac,
     &          obj2obj(i).from.coeff,
     &          obj2obj(i).to.object,
     &          obj2obj(i).to.obj_name,
     &          obj2obj(i).to.object_no,
     &          obj2obj(i).to.hydrochan,
     &          obj2obj(i).to.acct_name,
     &          obj2obj(i).to.acct_ndx,
     &          obj2obj(i).to.mass_frac,
     &          obj2obj(i).to.coeff,
     &          obj2obj(i).constant_value,
     &          obj2obj(i).label,
     &          obj2obj(i).in_no,
     &          obj2obj(i).flow,
     &          obj2obj(i).prev_flow,
     &          obj2obj(i).flow_avg,
     &          obj2obj(i).constituent_conc,
     &          i=1,max_obj2obj)
         end if

c--------external flow info
         read(unit_tide) nqext
      if (.not. pcunix) then
         read(unit_tide)qext
      else
         read(unit_tide) (
     &          qext(i).flow,
     &          qext(i).prev_flow,
     &          qext(i).avg,
     &          qext(i).prev_avg,
     &          qext(i).in_no,
     &          qext(i).changed_ndx,
     &          qext(i).obj_name,
     &          qext(i).attach.object,
     &          qext(i).attach.obj_name,
     &          qext(i).attach.object_no,
     &          qext(i).acct_name,
     &          qext(i).acct_ndx,
     &          qext(i).mass_frac,
     &          i=1,max_qext)
      end if



c--------stage boundaries; write out node number and name
         read(unit_tide) nstgbnd
      if (.not. pcunix) then
         read(unit_tide)stgbnd
      else        
         read(unit_tide) (stgbnd(i).name,stgbnd(i).node,i=1,max_stgbnd)
      end if

      if (nacct_names .gt. 0) then ! don't bother with accounting names if none assigned yet
c--------check and assign accounting name indices

c--------object-to-object flows first
         do i=1,nobj2obj
c-----------from object
            if (obj2obj(i).from.acct_name .eq. ' ') then
               write(unit_error,810) 'internal-from',
     &              obj_names(obj2obj(i).from.object)
     &              (:lnblnk(obj_names(obj2obj(i).from.object))),
     &              obj2obj(i).from.obj_name(:lnblnk(obj2obj(i).from.obj_name))
               obj2obj(i).from.acct_ndx=nacct_names ! null name
            else
               obj2obj(i).from.acct_ndx=0
               j=1
               do while (acct_names(j) .ne. obj2obj(i).from.acct_name
     &              .and. j .le. nacct_names)
                  j=j+1
               enddo
               if (j .le. nacct_names) obj2obj(i).from.acct_ndx=j

               if (obj2obj(i).from.acct_ndx .eq. 0) then
                  write(unit_error,820) 'internal-from',
     &                 obj2obj(i).from.acct_name
     &                 (:lnblnk(obj2obj(i).from.acct_name))
                  call exit(2)
               endif
            endif

c-----------to object
            obj2obj(i).to.acct_ndx=0
            if (obj2obj(i).to.acct_name .eq. ' ') then
               write(unit_error,810) 'internal-to',
     &              obj_names(obj2obj(i).to.object)
     &              (:lnblnk(obj_names(obj2obj(i).to.object))),
     &              obj2obj(i).to.obj_name(:lnblnk(obj2obj(i).to.obj_name))
            else
               j=1
               do while (acct_names(j) .ne. obj2obj(i).to.acct_name .and.
     &              j .le. nacct_names)
                  j=j+1
               enddo
               if (j .le. nacct_names) obj2obj(i).to.acct_ndx=j

               if (obj2obj(i).to.acct_ndx .eq. 0) then
                  write(unit_error,820) 'internal-to',
     &                 obj2obj(i).to.acct_name
     &                 (:lnblnk(obj2obj(i).to.acct_name))
                  call exit(2)
               endif
            endif
         enddo

         do i=1,nqext
c-----------fix incorrect mass_frac value from old tidefiles
            if (qext(i).mass_frac .eq. miss_val_r) then
               qext(i).mass_frac=1.0
            endif

            qext(i).acct_ndx=0
            if (qext(i).acct_name .eq. ' ') then
               write(unit_error,810) 'external',
     &              obj_names(qext(i).attach.object)
     &              (:lnblnk(obj_names(qext(i).attach.object))),
     &              qext(i).attach.obj_name(:lnblnk(qext(i).attach.obj_name))
            else
               j=1
               do while (acct_names(j) .ne. qext(i).acct_name .and.
     &              j .le. nacct_names)
                  j=j+1
               enddo
               if (j .le. nacct_names) qext(i).acct_ndx=j

               if (qext(i).acct_name .eq. ' ') then
                  write(unit_error,820) 'external',
     &                 qext(i).acct_name(:lnblnk(qext(i).acct_name))
                  call exit(2)
               endif
            endif
         enddo

c--------fill in object numbers in pathoutput structure for from/to objects
         if ( (dsm2_module .eq. ptm .or. dsm2_module .eq. qual)
     &        .and. .not. updated_pathoutput) then
            updated_pathoutput = .true.
            do i=1, noutpaths
               j=1
               do while( pathoutput(i).flux_from(j).object .ne. 0 )
                  objType = pathoutput(i).flux_from(j).object
c-----------------handle reservoir, external, internal flow ids to number changes
                  if ( objType .eq. obj_reservoir ) then
                     ctmp = pathoutput(i).flux_from(j).obj_name
                     aIndex = getAccountingIndex(ctmp)
                     if ( aIndex .gt. 0 ) then
                        pathoutput(i).flux_from(j).acct_ndx = aIndex
                     else if ( ctmp(1:lnblnk(ctmp)) .eq. 'all') then
                        pathoutput(i).flux_from(j).acct_ndx = 0
                     else
                        pathoutput(i).flux_from(j).object_no =
     &                       getReservoirId(ctmp)
                     endif
                  else if ( objType .eq. obj_stage ) then
                     ctmp = pathoutput(i).flux_from(j).acct_name
                     aIndex = getAccountingIndex(ctmp)
                     if ( aIndex .gt. 0 ) then
                        pathoutput(i).flux_from(j).acct_ndx = aIndex
                     else if ( ctmp(1:lnblnk(ctmp)) .eq. 'all') then
                        pathoutput(i).flux_from(j).acct_ndx = 0
                     else
                        pathoutput(i).flux_from(j).obj_name =
     &                       pathoutput(i).flux_from(j).acct_name
                        pathoutput(i).flux_from(j).acct_name = ""
                        pathoutput(i).flux_from(j).object_no =
     &                       getStageId(ctmp)
                     endif
                  else if (objType .eq. obj_qext) then
                     ctmp = pathoutput(i).flux_from(j).acct_name
                     aIndex = getAccountingIndex(ctmp)
                     if ( aIndex .gt. 0 ) then
                        pathoutput(i).flux_from(j).acct_ndx = aIndex
                     else if ( ctmp(1:lnblnk(ctmp)) .eq. 'all') then
                        pathoutput(i).flux_from(j).acct_ndx = 0
                     else
                        pathoutput(i).flux_from(j).obj_name =
     &                       pathoutput(i).flux_from(j).acct_name
                        pathoutput(i).flux_from(j).acct_name = ""
                        pathoutput(i).flux_from(j).object_no =
     &                       getExternalId(ctmp)
                     endif
                  else if ( objType .eq. obj_obj2obj ) then
                     ctmp = pathoutput(i).flux_from(j).acct_name
                     aIndex = getAccountingIndex(ctmp)
                     if ( aIndex .gt. 0 ) then
                        pathoutput(i).flux_from(j).acct_ndx = aIndex
                     else if ( ctmp(1:lnblnk(ctmp)) .eq. 'all') then
                        pathoutput(i).flux_from(j).acct_ndx = 0
                     else
                        pathoutput(i).flux_from(j).obj_name =
     &                       pathoutput(i).flux_from(j).acct_name
                        pathoutput(i).flux_from(j).acct_name = ""
                        pathoutput(i).flux_from(j).object_no =
     &                       getInternalId(ctmp)
                     endif
                  endif
                  j=j+1
               enddo
               j=1
               do while( pathoutput(i).flux_to(j).object .ne. 0 )
                  objType = pathoutput(i).flux_to(j).object
c-----------------handle reservoir, external, internal flow ids to number changes
                  if ( objType .eq. obj_reservoir ) then
                     ctmp = pathoutput(i).flux_to(j).obj_name
                     aIndex = getAccountingIndex(ctmp)
                     if ( aIndex .gt. 0 ) then
                        pathoutput(i).flux_to(j).acct_ndx = aIndex
                     else if ( ctmp(1:lnblnk(ctmp)) .eq. 'all') then
                        pathoutput(i).flux_to(j).acct_ndx = 0
                     else
                        pathoutput(i).flux_to(j).object_no =
     &                       getReservoirId(ctmp)
                     endif
                  else if ( objType .eq. obj_stage ) then
                     ctmp = pathoutput(i).flux_to(j).acct_name
                     aIndex = getAccountingIndex(ctmp)
                     if ( aIndex .gt. 0 ) then
                        pathoutput(i).flux_to(j).acct_ndx = aIndex
                     else if ( ctmp(1:lnblnk(ctmp)) .eq. 'all') then
                        pathoutput(i).flux_to(j).acct_ndx = 0
                     else
                        pathoutput(i).flux_to(j).obj_name =
     &                       pathoutput(i).flux_to(j).acct_name
                        pathoutput(i).flux_to(j).acct_name = ""
                        pathoutput(i).flux_to(j).object_no =
     &                       getStageId(ctmp)
                     endif
                  else if (objType .eq. obj_qext) then
                     ctmp = pathoutput(i).flux_to(j).acct_name
                     aIndex = getAccountingIndex(ctmp)
                     if ( aIndex .gt. 0 ) then
                        pathoutput(i).flux_to(j).acct_ndx = aIndex
                     else if ( ctmp(1:lnblnk(ctmp)) .eq. 'all') then
                        pathoutput(i).flux_to(j).acct_ndx = 0
                     else
                        pathoutput(i).flux_to(j).obj_name =
     &                       pathoutput(i).flux_to(j).acct_name
                        pathoutput(i).flux_to(j).acct_name = ""
                        pathoutput(i).flux_to(j).object_no =
     &                       getExternalId(ctmp)
                     endif
                  else if ( objType .eq. obj_obj2obj ) then
                     ctmp = pathoutput(i).flux_to(j).acct_name
                     aIndex = getAccountingIndex(ctmp)
                     if ( aIndex .gt. 0 ) then
                        pathoutput(i).flux_to(j).acct_ndx = aIndex
                     else if ( ctmp(1:lnblnk(ctmp)) .eq. 'all') then
                        pathoutput(i).flux_to(j).acct_ndx = 0
                     else
                        pathoutput(i).flux_to(j).obj_name =
     &                       pathoutput(i).flux_to(j).acct_name
                        pathoutput(i).flux_to(j).acct_name = ""
                        pathoutput(i).flux_to(j).object_no =
     &                       getInternalId(ctmp)
                     endif
                  endif
                  j=j+1
               enddo
            enddo
         endif
c--------end of fill in pathoutput structure

c--------Assign input path constituents to external flows for qual.
c--------An input path of the same object, object number, and accounting
c--------name, is considered to belong to the external flow.
c--------Trap multiple concentrations to same flow if not prioritized.
         if (dsm2_module .eq. qual .and. check_headers) then
            do i=1,nqext
               n_conqext(i)=0
               do p=1,ninpaths
                  if (pathinput(p).object .eq. qext(i).attach.object .and.
     &                 pathinput(p).object_no .eq. qext(i).attach.object_no .and.
     &                 pathinput(p).acct_ndx .eq. qext(i).acct_ndx) then
c--------------------same object type, object number, and accounting name
c--------------------between external flow and this input path
                     constituent_found=.false.
                     do n=1,n_conqext(i)
c-----------------------check for same constituent multiple times to same flow
c-----------------------compare constituent type and then priority
                        if (pathinput(p).c_part .eq.
     &                       pathinput(const_qext(i,n)).c_part) then ! same constituent
                           constituent_found=.true.
                           if (pathinput(p).priority .eq.
     &                          pathinput(const_qext(i,n)).priority) then ! same priority
                              write(unit_error,815)
     &                             obj_names(qext(i).attach.object)
     &                             (:lnblnk(obj_names(qext(i).attach.object))),
     &                             qext(i).attach.obj_name(:lnblnk(qext(i).attach.obj_name))
                              const_qext(i,n)=p
                           else ! different priorities, use highest priority (lowest number)
c-----------------------------subr store_values will figure out which path to use
                              if (pathinput(p).priority .lt.
     &                             pathinput(const_qext(i,n)).priority) then
                                 const_qext(i,n)=p
                              endif
                           endif
                        endif
                     enddo
                     if (.not. constituent_found) then ! first input path of this constituent to this flow
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
         endif                  ! qual module
      endif

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
c-----+++++++++++++++++++++++++++++++++++++++++++++++++
      function getReservoirId(name)
      implicit none
      include '../fixed/common.f'
      integer getReservoirId
      character*130 name
      logical found
      integer i, lnblnk, multiplier, sindex

      found = .false.
      i = 1
      if (name(1:1) .eq. '-') then
         multiplier = -1
         sindex = 2
      else
         multiplier = 1
         sindex = 1
      endif

      do while( i .le. max_reservoirs .and. .not. found)
         if ( res_geom(i).name(:lnblnk(res_geom(i).name)) .eq.
     &        name(sindex:lnblnk(name))) then
            found = .true.
            getReservoirId = i
         endif
         i=i+1
      enddo
      if ( .not. found ) then
         getReservoirId = 0
         write(*,*) 'Name ', name, ' not found in reservoir names'
      endif
      getReservoirId = multiplier*getReservoirId
      return
      end
c-----+++++++++++++++++++++++++++++++++++++++++++++++++
      function getStageId(name)
      implicit none
      include '../fixed/common.f'
      integer getStageId
      character*130 name
      logical found
      integer i, lnblnk, multiplier, sindex

      if (name(1:1) .eq. '-') then
         multiplier = -1
         sindex = 2
      else
         multiplier = 1
         sindex = 1
      endif
      found = .false.
c-----check in object names
      if ( .not. found ) then
         i = 1
         do while( i .le. max_stgbnd .and. .not. found)
            if ( stgbnd(i).name(:lnblnk(stgbnd(i).name)) .eq.
     &           name(sindex:lnblnk(name))) then
               found = .true.
               getStageId = stgbnd(i).node
            endif
            i=i+1
         enddo
      endif

      if ( .not. found ) then
         getStageId = 0
         write(*,*) 'Name: ', name, ' not found in stage ids'
      endif
      getStageId = multiplier*getStageId
      return
      end
c-----+++++++++++++++++++++++++++++++++++++++++++++++++
      function getExternalId(name)
      implicit none
      include '../fixed/common.f'
      include 'tide.inc'
      integer getExternalId
      character*130 name
      logical found
      integer i, lnblnk, multiplier, sindex

      if (name(1:1) .eq. '-') then
         multiplier = -1
         sindex = 2
      else
         multiplier = 1
         sindex = 1
      endif
      found = .false.
c-----check in object names
      if ( .not. found ) then
         i = 1
         do while( i .le. max_qext .and. .not. found)
            if ( qext(i).obj_name(:lnblnk(qext(i).obj_name)) .eq.
     &           name(sindex:lnblnk(name))) then
               found = .true.
               getExternalId = i
            endif
            i=i+1
         enddo
      endif

      if ( .not. found ) then
         getExternalId = 0
         write(*,*) 'Name: ', name, ' not found in externals'
      endif
      getExternalId = multiplier*getExternalId
      return
      end
c-----+++++++++++++++++++++++++++++++++++++++++++++++++
      function getInternalId(name)
      implicit none
      include '../fixed/common.f'
      integer getInternalId
      character*130 name
      logical found
      integer i, lnblnk, multiplier, sindex

      if (name(1:1) .eq. '-') then
         multiplier = -1
         sindex = 2
      else
         multiplier = 1
         sindex = 1
      endif
      found = .false.
      i = 1
c-----check in object names
      if ( .not. found ) then
         i = 1
         do while( i .le. max_obj2obj .and. .not. found)
            if ( obj2obj(i).obj_name(:lnblnk(obj2obj(i).obj_name)) .eq.
     &           name(sindex:lnblnk(name))) then
               found = .true.
               getInternalId = i
            endif
            i=i+1
         enddo
      endif

      if ( .not. found ) then
         getInternalId = 0
         write(*,*) 'Name: ', name, ' not found in internals'
      endif

      getInternalId = multiplier*getInternalId
      return
      end
c-----+++++++++++++++++++++++++++++++++++++++++++++++
c-----check in accounting names
      function getAccountingIndex(name)
      implicit none
      include '../fixed/common.f'
      include 'tide.inc'
      integer getAccountingIndex
      character*130 name, ctmp
      logical found
      integer i, lnblnk

      found = .false.
      i = 1
      do while( i .le. nacct_names .and. .not. found)
         ctmp = acct_names(i)
         if ( ctmp(:lnblnk(ctmp)) .eq. name(:lnblnk(name)) ) then
            found = .true.
            getAccountingIndex = i
         endif
         i=i+1
      enddo
      if ( .not. found ) getAccountingIndex = 0
      return
      end
