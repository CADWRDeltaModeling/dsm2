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


      subroutine input_channels(field_names, mxflds, nfields, nflds,
     &     ifld, rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for channel info

      implicit none

      include 'common.f'

      logical
     &     ldefault             ! true if values are for defaults
      common /read_fix_l/ ldefault

c-----local variables

      integer
     &     mxflds               ! maximum number of fields
     &     ,nfields             ! number of fields in data line (input)
     &     ,nflds               ! number of fields in headers (input)
     &     ,ifld(mxflds)        ! ifld(i)=order header keyword i occurs in file (input)
     &     ,rifld(mxflds)       ! reverse ifld
     &     ,ibegf(mxflds)       ! beginning position of each field in line (input)
     &     ,ilenf(mxflds)       ! length of each field in line (input)
     &     ,istat               ! conversion status of this line (output)

      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)

      integer
     &     channo               ! channel number
     &     ,i,j,k               ! indices

      character
     &     cstring*15           ! string field
      logical xsect_defined,dist_defined

 610  format(/a)
 620  format(/a
     &     /'Input string is: ',a)
 630  format(/a,i5)
 640  format('Error ....duplicate xsect info supplied'/
     &     '          channel:',i5/
     &     '  Duplicate xsect:',i5)
 650  format('Error ....duplicate dist info supplied'/
     &     '          channel:',i6/
     &     '   Duplicate dist:',i6)
 660  format(
     &     ' Error... Either the channel header information needs to be'/
     &     '          extended or the header is incorrect'/
     &     '          Channel Number: ',i5)
 670  format(/'Error: ',a,' node number: ',i5,
     &     ' out of range for channel: ',i4
     &     /'Valid range is 1:',i3)

c-----channel number is required for each line, except for default
      if (.not. ldefault .and. ifld(chan_no) .eq. 0) then
         write(unit_error, 610)
     &        'No channel number given.'
         istat=-1
         goto 900
      endif

      if (ldefault) then
         channo=0
      else
         cstring=line(ibegf(ifld(chan_no)):ibegf(ifld(chan_no))+
     &        ilenf(ifld(chan_no))-1)
         read(cstring,'(i5)',err=810) channo

c--------valid channel number?
         if (channo .lt. 1 .or. channo .gt. max_channels) then
            write(unit_error, 630)
     &           'Channel number out of bounds:',channo
            istat=-1
            goto 900
         endif
      endif

      k=1                       ! header field index
      i=1                       ! data field index
      do while (i .le. nfields)
         if (rifld(k) .eq. chan_no) goto 100 ! channel number already processed
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
         if (rifld(k) .eq. length) then
            read(cstring,'(i10)', err=810) chan_geom(channo).length
         else if (rifld(k) .eq. manning) then
            read(cstring,'(f10.0)', err=810) chan_geom(channo).manning
         else if (rifld(k) .eq. upnode) then
            read(cstring,'(i5)', err=810) chan_geom(channo).upnode
c-----------check for valid range
            if (chan_geom(channo).upnode .lt. 1 .or.
     &           chan_geom(channo).upnode .gt. max_nodes) then
               write(unit_error,670) 'Upstream',chan_geom(channo).upnode,
     &              channo,max_nodes
               istat=-1
               goto 900
            endif
         else if (rifld(k) .eq. downnode) then
            read(cstring,'(i5)', err=810) chan_geom(channo).downnode
            if (chan_geom(channo).downnode .lt. 1 .or.
     &           chan_geom(channo).downnode .gt. max_nodes) then
               write(unit_error,670) 'Downstream',chan_geom(channo).downnode,
     &              channo,max_nodes
               istat=-1
               goto 900
            endif
         else if (rifld(k) .eq. disp) then
            read(cstring,'(f10.0)', err=810) chan_geom(channo).disp
         else if (rifld(k) .eq. xsect) then ! loop over (possible) multiple args
            j=1                 ! cross-section index
            chan_geom(channo).nxsect=0
            xsect_defined=.false.
            dist_defined=.false.
            do while (
     &           cstring .ne. delimiter .and.
     &           i .le. nfields .and.
     &           j .le. max_xsects)
               if (rifld(i).eq.xsect) then
                  read(cstring,'(i5)', err=810) chan_geom(channo)
     &                 .xsect(j)
                  if (xsect_defined) then
                     write(unit_error,640)channo,chan_geom(channo)
     &                    .xsect(j)
                     istat=-1
                     goto 900
                  endif
                  xsect_defined=.true.
               elseif (rifld(i).eq.dist) then
                  read(cstring,'(i10)', err=810) chan_geom(channo)
     &                 .dist(j)
                  if (dist_defined) then
                     write(unit_error,650)channo,chan_geom(channo)
     &                    .dist(j)
                     istat=-1
                     goto 900
                  endif
                  dist_defined=.true.
               else
                  write(unit_error,660)channo
                  istat=-1
                  goto 900
               endif
               i=i+1
               if(dist_defined.and.xsect_defined)then
                  j=j+1
                  chan_geom(channo).nxsect=chan_geom(channo).nxsect+1
                  xsect_defined=.false.
                  dist_defined=.false.
               endif
               cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
            enddo
         endif
 100     continue
         k=k+1
         i=i+1
      enddo

      return

c-----char-to-value conversion errors

 810  continue
      write(unit_error, 620) 'Conversion error on field ' //
     &     field_names(rifld(i)), cstring

      istat=-2

 900  continue                  ! fatal error

      return
      end

      subroutine input_xsects(field_names, mxflds, nfields, nflds, ifld,
     &     rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for rectangular
c-----cross section info

      implicit none

      include 'common.f'

      logical
     &     ldefault             ! true if values are for defaults
      common /read_fix_l/ ldefault

c-----local variables

      integer
     &     mxflds               ! maximum number of fields
     &     ,nfields             ! number of fields in data line (input)
     &     ,nflds               ! number of fields in headers (input)
     &     ,ifld(mxflds)        ! ifld(i)=order header keyword i occurs in file (input)
     &     ,rifld(mxflds)       ! reverse ifld
     &     ,ibegf(mxflds)       ! beginning position of each field in line (input)
     &     ,ilenf(mxflds)       ! length of each field in line (input)
     &     ,istat               ! conversion status of this line (output)

      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)

      integer
     &     xno                  ! cross section number
     &     ,i                   ! indices

      character
     &     cstring*15           ! string field

 610  format(/a)
 620  format(/a
     &     /'Input string is: ',a)
 630  format(/a,i5)

c-----cross section number is required for each line except for default
      if (.not. ldefault .and. ifld(x_no) .eq. 0) then
         write(unit_error, 610)
     &        'No cross section number given.'
         istat=-1
         goto 900
      endif

      if (ldefault) then
         xno=0
      else
         cstring=line(ibegf(ifld(x_no)):ibegf(ifld(x_no))+
     &        ilenf(ifld(x_no))-1)
         read(cstring,'(i5)',err=810) xno

c--------valid xsect number?
         if (xno .lt. 1 .or. xno .gt. max_xsects_tot) then
            write(unit_error, 630)
     &           'Cross section number out of bounds:',xno
            istat=-1
            goto 900
         endif
      endif

      i=1                       ! header and data field index
      do while (i .le. nfields)
         if (rifld(i) .eq. x_no) goto 100 ! Cross section number already processed
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
         if (rifld(i) .eq. x_width) then
            read(cstring,'(f10.0)', err=810) xsect_geom(xno).width
         else if (rifld(i) .eq. x_botelev) then
            read(cstring,'(f10.0)', err=810) xsect_geom(xno).botelv
         else if (rifld(i) .eq. x_init_stage) then
            if (cstring .ne. ' ')
     &           read(cstring,'(f10.0)', err=810) xsect_geom(xno).init_stage
         else if (rifld(i) .eq. x_init_flow) then
            read(cstring,'(f10.0)', err=810) xsect_geom(xno).init_flow
         endif
 100     continue
         i=i+1
      enddo

      return

c-----char-to-value conversion errors

 810  continue
      write(unit_error, 620) 'Conversion error on field ' //
     &     field_names(rifld(i)), cstring

      istat=-2

 900  continue                  ! fatal error

      return
      end

      subroutine input_irreg_geom(field_names, mxflds, nfields, nflds,
     &     ifld, rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for
c-----irregular geometry info

      implicit none

      include 'common.f'
      include 'common_irreg_geom.f'

c-----local variables

      integer
     &     mxflds               ! maximum number of fields
     &     ,nfields             ! number of fields in data line (input)
     &     ,nflds               ! number of fields in headers (input)
     &     ,ifld(mxflds)        ! ifld(i)=order header keyword i occurs in file (input)
     &     ,rifld(mxflds)       ! reverse ifld
     &     ,ibegf(mxflds)       ! beginning position of each field in line (input)
     &     ,ilenf(mxflds)       ! length of each field in line (input)
     &     ,istat               ! conversion status of this line (output)

      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)

      integer
     &     i                    ! index
     &     ,irgno               ! index
     &     ,channo              ! channel number
      real*8
     &     distance             ! normalized distance along channel (0.0->1.0)

      character
     &     cstring*80           ! string field

      character
     &     input_line*250       ! raw input line
      common /input_lines/ input_line

      data nirg /0/

 610  format(/a)
 620  format(/a
     &     /'Input string is: ',a)
 630  format(/a,i5)

c-----channel number, distance, and filename are required for each line
      i=ifld(irg_chan)
      if (i .eq. 0) then
         write(unit_error, 610) 'No channel number given for irregular xsect.'
         istat=-1
         goto 900
      else
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
         read(cstring,'(i5)',err=810) channo
         if (channo .lt. 1 .or.
     &        channo .gt. max_channels) then
            write(unit_error, 630)
     &           'Channel number in irregular cross sections out of bounds:',
     &           channo
            istat=-1
            goto 900
         endif
      endif

      i=ifld(irg_dist)
      if (i .eq. 0) then
         write(unit_error, 610) 'No distance given for irregular xsect.'
         istat=-1
         goto 900
      else
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
         if (index(cstring,'len') .gt. 0) then
            distance=1.0
         else if (index(cstring,'mid') .gt. 0) then
            distance=0.5
         else
            read(cstring,'(f10.0)',err=810) distance
c-----------check for range error
            if (distance .gt. 1.0 .or. distance .lt. 0.0) then
               write(unit_error, 610)
     &              'Cross-section distance must be between 0.0 and 1.0.'
               istat=-1
               goto 900
            endif
         endif
      endif

c-----check if channel and distance combo already used, if so, overwrite,
c-----else, new irregular cross section
      irgno=0
      do i=1,nirg
         if (irreg_geom(i).chan_no .eq. channo .and.
     &        abs(irreg_geom(i).dist_ratio-distance) .lt. 1.0e-4) then
            irgno=i
            goto 100
         endif
      enddo

 100  continue

      if (irgno .eq. 0) then
         nirg=nirg+1
         if (nirg .gt. max_irr_xsects) then
            write(unit_error,630)
     &           'Too many irregular cross-sections specified; max allowed is:'
     &           ,max_irr_xsects
            istat=-1
            goto 900
         endif
         irgno=nirg
      endif

      i=ifld(irg_fn)
      if (i .eq. 0) then
         write(unit_error, 610) 'No filename given for irregular xsect.'
         istat=-1
         goto 900
      else
         irreg_geom(irgno).chan_no=channo
         irreg_geom(irgno).dist_ratio=distance
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
         irreg_geom(irgno).filename=
     &        input_line(ibegf(i):ibegf(i)+ilenf(i)-1) ! use raw input to preserve case
      endif

      return

c-----char-to-value conversion errors

 810  continue
      write(unit_error, 620) 'Conversion error on field ' //
     &     field_names(rifld(i)), cstring

      istat=-2

 900  continue                  ! fatal error

      return
      end

      subroutine input_junctions(field_names, mxflds, nfields, nflds,
     &     ifld, rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for junction info

      implicit none

      include 'common.f'

      logical
     &     ldefault             ! true if values are for defaults
      common /read_fix_l/ ldefault

c-----local variables

      integer
     &     mxflds               ! maximum number of fields
     &     ,nfields             ! number of fields in data line (input)
     &     ,nflds               ! number of fields in headers (input)
     &     ,ifld(mxflds)        ! ifld(i)=order header keyword i occurs in file (input)
     &     ,rifld(mxflds)       ! reverse ifld
     &     ,ibegf(mxflds)       ! beginning position of each field in line (input)
     &     ,ilenf(mxflds)       ! length of each field in line (input)
     &     ,istat               ! conversion status of this line (output)

      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)

      integer
     &     nodeno               ! node number
     &     ,i,k                 ! indices

      character
     &     cstring*15           ! string field

 610  format(/a)
 620  format(/a
     &     /'Input string is: ',a)
 630  format(/a,i5)
 640  format(/'Incorrect boundary type given: ',a
     &     /'Must be either "stage" or "flow"')

c-----node number is required for each line except for default
      if (.not. ldefault .and. ifld(node_no) .eq. 0) then
         write(unit_error, 610)
     &        'No node number given.'
         istat=-1
         goto 900
      endif

      if (ldefault) then
         nodeno=0
      else
         i=ifld(node_no)
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
         read(cstring,'(i5)',err=810) nodeno

c--------valid node number?
         if (nodeno .lt. 1 .or. nodeno .gt. max_nodes) then
            write(unit_error, 630)
     &           'Node number out of bounds:',nodeno
            istat=-1
            goto 900
         endif
      endif

      k=1                       ! header field index
      i=1                       ! data field index
      do while (i .le. nfields .and. k .le. max_fields)
         node_id(i)=nodeno
         if (rifld(k) .eq. 0) goto 101 ! allow for non-consecutive field numbers
         if (rifld(k) .eq. node_no) goto 100 ! node number already processed
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
         if (rifld(k) .eq. boundary_type) then
            if (cstring .eq. 'flow') then
               node_geom(nodeno).boundary_type=flow_type
            else if(cstring .eq. 'stage') then
               node_geom(nodeno).boundary_type=stage_type
            else
               write(unit_error, 640) cstring(1:5)
               istat=-2
               goto 900
            endif
         endif
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
 100     continue
         i=i+1
 101     continue
         k=k+1
      enddo

      return

c-----char-to-value conversion errors

 810  continue
      write(unit_error, 620) 'Conversion error on field ' //
     &     field_names(rifld(i)), cstring

      istat=-2

 900  continue                  ! fatal error

      return
      end

      subroutine input_reservoirs(field_names, mxflds, nfields, nflds,
     &     ifld, rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for reservoir info

      implicit none

      include 'common.f'

      logical
     &     ldefault             ! true if values are for defaults
      common /read_fix_l/ ldefault

c-----local variables

      integer
     &     mxflds               ! maximum number of fields
     &     ,nfields             ! number of fields in data line (input)
     &     ,nflds               ! number of fields in headers (input)
     &     ,ifld(mxflds)        ! ifld(i)=order header keyword i occurs in file (input)
     &     ,rifld(mxflds)       ! reverse ifld
     &     ,ibegf(mxflds)       ! beginning position of each field in line (input)
     &     ,ilenf(mxflds)       ! length of each field in line (input)
     &     ,istat               ! conversion status of this line (output)

      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)

      integer
     &     resno                ! reservoir number
     &     ,i                   ! loop index
     &     ,nn                  ! node number
     &     ,ndno                ! temporary node number

      character
     &     cstring*15           ! string field

      data nreser /0/

 610  format(/a)
 615  format(/'Warning: more reservoir data fields (',i2,
     &     ') than keywords (',i2,'),'
     &     /'not using extra data fields.')
 620  format(/a
     &     /'Input string is: ',a)
 630  format(/a,i5)
 640  format(/'Error: reservoir node number out of range: ',i10
     &     /'Valid range is 1:',i3)

c-----reservoir name is required for each line except for default
      if (.not. ldefault .and. ifld(res_name) .eq. 0) then
         write(unit_error, 610)
     &        'No reservoir name given.'
         istat=-1
         goto 900
      endif

      if (ldefault) then        ! this data line sets default values
         resno=0
      else
c--------individual reservoir
c--------check for more data fields than header fields
         if (nfields .gt. nflds) then
            write(unit_error, 615)
     &           nfields,nflds
            istat=1
         endif
c--------check for duplicate, if so, overwrite
         i=ifld(res_name)
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1) ! reservoir name
         resno=1
         if (nreser .eq. 0) then
            nreser=1
         else
            do while (resno .le. nreser .and.
     &           res_geom(resno).name .ne. cstring)
               resno=resno+1
            enddo
            nreser=max(nreser,resno)
            if (nreser .gt. max_reservoirs) then
               write(unit_error,630)
     &              'Too many reservoirs specified; max allowed is:'
     &              ,max_reservoirs
               istat=-1
               goto 900
            endif
         endif
      endif

      i=1                       ! header field index
      nn=0
      do while (i .le. nfields)
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
         if (rifld(i) .eq. res_name) then
            res_geom(resno).name=cstring
         else if (rifld(i) .eq. res_area) then
            read(cstring,'(f10.0)', err=810) res_geom(resno).area
         else if (rifld(i) .eq. res_stage) then
            read(cstring,'(f10.0)', err=810) res_geom(resno).stage
         else if (rifld(i) .eq. res_botelv) then
            read(cstring,'(f10.0)', err=810) res_geom(resno).botelv
         else if (rifld(i) .eq. res_node) then
            read(cstring,'(i5)', err=810) ndno
c-----------check for out of range node number
            if (ndno .lt. 1 .or. ndno .gt. max_nodes) then
               write(unit_error,640) ndno,max_nodes
               istat=-1
               goto 900
            endif
c-----------check for same node number, if so, overwrite
            do nn=1,res_geom(resno).nnodes
               if (res_geom(resno).node_no(nn) .eq. ndno) goto 100
            enddo
 100        continue
            if (nn .gt. res_geom(resno).nnodes) then ! not a duplicate node
               res_geom(resno).nnodes=res_geom(resno).nnodes+1
               nn=res_geom(resno).nnodes
               res_geom(resno).node_no(nn)=ndno
            endif
c-----------the following fields all require the node number
c-----------be specified first
         else if (rifld(i) .eq. res_coeff2res) then
            if (nn .eq. 0) goto 820
            read(cstring,'(f10.0)', err=810)
     &           res_geom(resno).coeff2res(nn)
         else if (rifld(i) .eq. res_coeff2chan) then
            if (nn .eq. 0) goto 820
            read(cstring,'(f10.0)', err=810)
     &           res_geom(resno).coeff2chan(nn)
         else if (rifld(i) .eq. res_maxq2res) then
            if (nn .eq. 0) goto 820
            read(cstring,'(f10.0)', err=810)
     &           res_geom(resno).maxq2res(nn)
            res_geom(resno).maxq2res(nn)=
     &           abs(res_geom(resno).maxq2res(nn))
         else if (rifld(i) .eq. res_maxstage) then
            read(cstring,'(f10.0)', err=810)
     &           res_geom(resno).maxstage
         endif
         i=i+1
      enddo

      return

c-----char-to-value conversion errors

 810  continue
      write(unit_error, 620) 'Conversion error on field ' //
     &     field_names(rifld(i)), cstring

      istat=-2
      return

 820  continue
      write(unit_error, 620)
     &     'This field must have the node specified first: ' //
     &     field_names(rifld(i)), cstring

      istat=-2
      return

 900  continue                  ! fatal error

      return
      end

      subroutine input_obj2obj(field_names, mxflds, nfields, nflds,
     &     ifld, rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for object-to-
c-----object connection info

      implicit none

      include 'common.f'

      logical
     &     ldefault             ! true if values are for defaults
      common /read_fix_l/ ldefault

c-----local variables

      integer
     &     mxflds               ! maximum number of fields
     &     ,nfields             ! number of fields in data line (input)
     &     ,nflds               ! number of fields in headers (input)
     &     ,ifld(mxflds)        ! ifld(i)=order header keyword i occurs in file (input)
     &     ,rifld(mxflds)       ! reverse ifld
     &     ,ibegf(mxflds)       ! beginning position of each field in line (input)
     &     ,ilenf(mxflds)       ! length of each field in line (input)
     &     ,istat               ! conversion status of this line (output)
     &     ,lnblnk              ! intrinsic

      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)

      integer
     &     i                    ! index

      character
     &     cstring*15           ! string field

      data nobj2obj /0/

 605  format(/'Object-to-object type must be either node or reservoir;'
     &     /a,' not recognized.')
 610  format(/a)
 620  format(/a
     &     /'Input string is: ',a)
 630  format(/a,i5)

c-----currently no overwriting is allowed

      nobj2obj=nobj2obj+1
      if (nobj2obj .gt. max_obj2obj) then
         write(unit_error,630)
     &        'Too many object connections specified; max allowed is:'
     &        ,max_obj2obj
         istat=-1
         goto 900
      endif

      i=ifld(obj2obj_from_objtype)
      if (i .eq. 0) then
         write(unit_error,610) 'No from object type given.'
         istat=-2
         goto 900
      else
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
         if (cstring(1:4) .eq. 'node') then
            obj2obj(nobj2obj).from.object=obj_node
         else if (cstring(1:3) .eq. 'res') then
            obj2obj(nobj2obj).from.object=obj_reservoir
         else
            write(unit_error,605) cstring(:lnblnk(cstring))
            istat=-2
            goto 900
         endif
      endif

      i=ifld(obj2obj_from_objname)
      if (i .eq. 0) then
         write(unit_error,610) 'No from object name or number given.'
         istat=-2
         goto 900
      else
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
         if (obj2obj(nobj2obj).from.object .eq. obj_node) then
            read(cstring,'(i10)',err=710) obj2obj(nobj2obj).from.object_no
            goto 711
 710        continue            ! error reading node number
c-----------might be a translation label, store to name and resolve later
            obj2obj(nobj2obj).from.obj_name=cstring
            obj2obj(nobj2obj).from.object_no=0
 711        continue
         else
            obj2obj(nobj2obj).from.obj_name=cstring ! reservoir name
         endif
      endif

      i=ifld(obj2obj_to_objtype)
      if (i .eq. 0) then
         write(unit_error,610) 'No to object type given.'
         istat=-2
         goto 900
      else
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
         if (cstring(1:4) .eq. 'node') then
            obj2obj(nobj2obj).to.object=obj_node
         else if (cstring(1:3) .eq. 'res') then
            obj2obj(nobj2obj).to.object=obj_reservoir
         else
            write(unit_error,605) cstring(:lnblnk(cstring))
            istat=-2
            goto 900
         endif
      endif

      i=ifld(obj2obj_to_objname)
      if (i .eq. 0) then
         write(unit_error,610) 'No to object name or number given.'
         istat=-2
         goto 900
      else
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
         if (obj2obj(nobj2obj).to.object .eq. obj_node) then
            read(cstring,'(i10)',err=720) obj2obj(nobj2obj).to.object_no
            goto 721
 720        continue            ! error reading node number
c-----------might be a translation label, store to name and resolve later
            obj2obj(nobj2obj).to.obj_name=cstring
            obj2obj(nobj2obj).to.object_no=0
 721        continue
         else
            obj2obj(nobj2obj).to.obj_name=cstring ! reservoir name
         endif
      endif

      i=ifld(obj2obj_flow)
      if (i .ne. 0) then
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
         if (cstring(1:2) .eq. 'st' .or.
     &        cstring(1:2) .eq. 'he') then ! stage head diff
            obj2obj(nobj2obj).constant_value=head_diff
         else                   ! fixed flow value
            read(cstring,'(1f10.0)',err=810) obj2obj(nobj2obj).constant_value
         endif
         obj2obj(nobj2obj).label=' ' ! can't have both label and value
      endif

      i=ifld(obj2obj_poscoeff)
      if (i .ne. 0) then
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
         read(cstring,'(l2)',err=810) obj2obj(nobj2obj).from.coeff
      endif

      i=ifld(obj2obj_negcoeff)
      if (i .ne. 0) then
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
         read(cstring,'(l2)',err=810) obj2obj(nobj2obj).to.coeff
      endif

      i=ifld(obj2obj_acctname)
      if (i .ne. 0) then
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
c--------symmetrical accounting names for now
         obj2obj(nobj2obj).from.acct_name=cstring
         obj2obj(nobj2obj).to.acct_name=cstring
      endif

      i=ifld(obj2obj_objname)
      if (i .ne. 0) then
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
         obj2obj(nobj2obj).obj_name=cstring
      endif

      i=ifld(obj2obj_pathinput_label)
      if (i .ne. 0) then
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
         obj2obj(nobj2obj).label=cstring
         obj2obj(nobj2obj).constant_value=miss_val_r ! can't have both label and value
      endif

      return

c-----char-to-value conversion errors

 810  continue
      write(unit_error, 620) 'Conversion error on field ' //
     &     field_names(rifld(i)), cstring

      istat=-2

 900  continue                  ! fatal error

      return
      end

      subroutine input_gates(field_names, mxflds, nfields, nflds, ifld,
     &     rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for gate info

      implicit none

      include 'common.f'

      logical
     &     ldefault             ! true if values are for defaults
      common /read_fix_l/ ldefault

c-----local variables

      integer
     &     mxflds               ! maximum number of fields
     &     ,nfields             ! number of fields in data line (input)
     &     ,nflds               ! number of fields in headers (input)
     &     ,ifld(mxflds)        ! ifld(i)=order header keyword i occurs in file (input)
     &     ,rifld(mxflds)       ! reverse ifld
     &     ,ibegf(mxflds)       ! beginning position of each field in line (input)
     &     ,ilenf(mxflds)       ! length of each field in line (input)
     &     ,istat               ! conversion status of this line (output)

      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)

      character
     &     cstring*15           ! string field

      integer
     &     gateno               ! gate number
     &     ,i                   ! index

      data ngates /0/

 610  format(/a)
 620  format(/a
     &     /'Input string is: ',a)
 625  format(/a/a)
 630  format(/a,i5)

c-----gate name is required for each line except for default
      if (.not. ldefault .and. ifld(gate_name) .eq. 0) then
         write(unit_error, 610)
     &        'No gate name given.'
         istat=-1
         goto 900
      endif

      if (ldefault) then        ! this data line sets default values
         gateno=0
      else
c--------individual gate; check for duplicate, if so, overwrite
         i=ifld(gate_name)
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1) ! gate name
         gateno=1
         if (ngates .eq. 0) then
            ngates=1
         else
            do while (gateno .le. ngates .and.
     &           gate_geom(gateno).name .ne. cstring)
               gateno=gateno+1
            enddo
            ngates=max(ngates,gateno)
            if (ngates .gt. max_gates) then
               write(unit_error,630)
     &              'Too many gates specified; max allowed is:' ,max_gates
               istat=-1
               goto 900
            endif
         endif
      endif

      i=1                       ! header field index
      do while (i .le. nfields)
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)

         if (rifld(i) .eq. gate_name) then
            gate_geom(gateno).name=cstring
         else if (rifld(i) .eq. gate_chan) then
            read(cstring,'(i5)',err=810) gate_geom(gateno).chan_no
         else if (rifld(i) .eq. gate_node) then
            read(cstring,'(i5)',err=810) gate_geom(gateno).node_no
         else if (rifld(i) .eq. gate_loc) then
            if (cstring(1:2) .eq. 'up' .or.
     &           cstring(1:4) .eq. 'down') then
               gate_geom(gateno).loc=cstring
            else
               write(unit_error, 625)
     &              'Invalid value for type of gate location: ' // cstring,
     &              'Valid values are UP or DOWN.'
               istat=-1
               goto 900
            endif
            gate_geom(gateno).loc=cstring
         else if (rifld(i) .eq. gate_lapse) then
            read(cstring,'(i5)',err=810) gate_geom(gateno).lapse
         else if (rifld(i) .eq. gate_ngates) then
            read(cstring,'(i5)',err=810) gate_geom(gateno).ngates
         else if (rifld(i) .eq. gate_width_up) then
            read(cstring,'(f10.0)',err=810) gate_geom(gateno).widthup
         else if (rifld(i) .eq. gate_width_down) then
            read(cstring,'(f10.0)',err=810) gate_geom(gateno).widthdown
         else if (rifld(i) .eq. gate_width_free) then
            read(cstring,'(f10.0)',err=810) gate_geom(gateno).widthfree
         else if (rifld(i) .eq. gate_crest_elev) then
            read(cstring,'(f10.0)',err=810) gate_geom(gateno).crestelev
         else if (rifld(i) .eq. gate_crest_free) then
            read(cstring,'(f10.0)',err=810) gate_geom(gateno).crestfree
         else if (rifld(i) .eq. gate_coeff_weir_down) then
            read(cstring,'(f10.0)',err=810) gate_geom(gateno).coeffweirdown
         else if (rifld(i) .eq. gate_coeff_weir_up) then
            read(cstring,'(f10.0)',err=810) gate_geom(gateno).coeffweirup
         else if (rifld(i) .eq. gate_npipes) then
            read(cstring,'(i5)',err=810) gate_geom(gateno).npipes
         else if (rifld(i) .eq. gate_pipe_rad) then
            read(cstring,'(f10.0)',err=810) gate_geom(gateno).piperad
         else if (rifld(i) .eq. gate_pipe_elev) then
            read(cstring,'(f10.0)',err=810) gate_geom(gateno).pipeelev
         else if (rifld(i) .eq. gate_coeff_pipe_down) then
            read(cstring,'(f10.0)',err=810) gate_geom(gateno).coeffpipedown
         else if (rifld(i) .eq. gate_coeff_pipe_up) then
            read(cstring,'(f10.0)',err=810) gate_geom(gateno).coeffpipeup
         else if (rifld(i) .eq. gate_oper) then
            if (cstring(1:4) .eq. 'open' .or. ! connected, open position
     &           cstring(1:2) .eq. 'cl' .or. ! connected, closed position
     &           cstring(1:3) .eq. 'ign' .or. ! not connected, ignore this gate
     &           cstring(1:4) .eq. 'free' .or. ! connected, all coeffs=1.0
     &           cstring(1:4) .eq. 'time' .or. ! connected, open/close from dss
     &           cstring(1:4) .eq. 'calc') then ! connected, open/close calculated
               gate_geom(gateno).oper=cstring
            else
               write(unit_error, 625)
     &              'Invalid value for type of gate operation: ' // cstring,
     &              'Valid values are OPEN, CLOSE, IGNORE, FREE, TIME, or CALC.'
               istat=-1
               goto 900
            endif
         else if (rifld(i) .eq. gate_dhopen) then
            read(cstring,'(f10.0)',err=810) gate_geom(gateno).dhopen
         else if (rifld(i) .eq. gate_velclose) then
            read(cstring,'(f10.0)',err=810) gate_geom(gateno).velclose
         endif
         i=i+1
      enddo

      return

c-----char-to-value conversion errors

 810  continue
      write(unit_error, 620) 'Conversion error on field ' //
     &     field_names(rifld(i)), cstring

      istat=-2

 900  continue                  ! fatal error

      return
      end

      subroutine input_inputpath(field_names, mxflds, nfields, nflds,
     &     ifld, rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for
c-----pathnames info: data paths for time-varying input

      implicit none

      include 'common.f'

      logical
     &     ldefault             ! true if values are for defaults
      common /read_fix_l/ ldefault

c-----local variables

      integer
     &     mxflds               ! maximum number of fields
     &     ,nfields             ! number of fields in data line (input)
     &     ,nflds               ! number of fields in headers (input)
     &     ,ifld(mxflds)        ! ifld(i)=order header keyword i occurs in file (input)
     &     ,rifld(mxflds)       ! reverse ifld
     &     ,ibegf(mxflds)       ! beginning position of each field in line (input)
     &     ,ilenf(mxflds)       ! length of each field in line (input)
     &     ,istat               ! conversion status of this line (output)

      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)

      integer
     &     i                    ! index
     &     ,itmp                ! scratch variable
     &     ,loccarr             ! locate string in char array function

      character
     &     cstring*80           ! string field
     &     ,ctmp*80             ! temporary char variable

      character
     &     input_line*250       ! raw input line
      common /input_lines/ input_line
      data ninpaths /1/

 610  format(/a)
 620  format(/a
     &     /'Input string is: ',a)
 630  format(/a,i5)

c-----name or channel/distance

      if (ldefault) then
         ninpaths=0
      else
         if (ninpaths .eq. 0) ninpaths=1
      endif

      i=1
      do while (i .le. nfields)
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)

         if (rifld(i) .eq. inpath_label) then
            pathinput(ninpaths).label=cstring
         else if (rifld(i) .eq. inpath_node) then
            read(cstring,'(i5)',err=810) pathinput(ninpaths).object_no
            pathinput(ninpaths).object=obj_node
         else if (rifld(i) .eq. inpath_a_part) then
            pathinput(ninpaths).a_part=cstring
         else if (rifld(i) .eq. inpath_b_part) then
            pathinput(ninpaths).b_part=cstring
         else if (rifld(i) .eq. inpath_c_part .or.
     &           rifld(i) .eq. inpath_meas_type) then
            pathinput(ninpaths).c_part=cstring
         else if (rifld(i) .eq. inpath_e_part .or.
     &           rifld(i) .eq. inpath_interval) then
            call split_epart(cstring,itmp,ctmp)
            if (itmp .ne. miss_val_i) then ! valid interval, parse it
               pathinput(ninpaths).e_part=cstring
               pathinput(ninpaths).no_intervals=itmp
               pathinput(ninpaths).interval=ctmp
            else
               write(unit_error,610)
     &              'Unknown input E part or interval: ' // cstring
               istat=-1
               goto 900
            endif
         else if (rifld(i) .eq. inpath_f_part) then
            pathinput(ninpaths).f_part=cstring
         else if (rifld(i) .eq. inpath_ID) then
            if (cstring(1:4) .eq. 'none') then
               pathinput(ninpaths).f_part=' '
            else
               pathinput(ninpaths).f_part=cstring
            endif
         else if (rifld(i) .eq. inpath_sdate) then
            if (cstring(1:3) .eq. 'gen') then
               pathinput(ninpaths).start_dt=generic_dt
            else if (cstring(1:3) .eq. 'syn') then
               pathinput(ninpaths).start_dt=generic_dt
               pathinput(ninpaths).sync=.true.
            else
               pathinput(ninpaths).start_dt(1:9)=cstring(1:9)
               if (index(pathinput(ninpaths).start_dt(1:9), 'none') .gt. 0)
     &              pathinput(ninpaths).start_dt(1:9)=' '
            endif
         else if (rifld(i) .eq. inpath_stime) then
            if (cstring(1:3) .eq. 'gen') then
               pathinput(ninpaths).start_dt=generic_dt
            else if (cstring(1:3) .eq. 'syn') then
               pathinput(ninpaths).start_dt=generic_dt
               pathinput(ninpaths).sync=.true.
            else
               pathinput(ninpaths).start_dt(11:14)=cstring(1:4)
               if (index(pathinput(ninpaths).start_dt(11:14), 'none') .gt. 0)
     &              pathinput(ninpaths).start_dt(11:14)=' '
            endif
         else if (rifld(i) .eq. inpath_filename) then
            pathinput(ninpaths).filename=
     &           input_line(ibegf(i):ibegf(i)+ilenf(i)-1) ! use raw input to preserve case
c-----------accumulate unique dss input filenames
            itmp=loccarr(pathinput(ninpaths).filename,infilenames
     &           ,max_dssinfiles, EXACT_MATCH)
            if (itmp .lt. 0) then
               if (abs(itmp) .le. max_dssinfiles) then
                  infilenames(abs(itmp))=pathinput(ninpaths).filename
                  pathinput(ninpaths).ndx_file=abs(itmp)
               else
                  write(unit_error,610)
     &                 'Maximum number of unique DSS input files exceeded'
                  goto 900
               endif
            else
               pathinput(ninpaths).ndx_file=itmp
            endif
         else if (rifld(i) .eq. inpath_value) then
            read(cstring,'(f10.0)',err=810) pathinput(ninpaths).constant_value
         else if (rifld(i) .eq. inpath_fillin) then
            if (index(cstring, 'first') .gt. 0) then
               pathinput(ninpaths).fillin=fill_first
            else if (index(cstring, 'interp') .gt. 0) then
               pathinput(ninpaths).fillin=fill_interp
            else if (index(cstring, 'last') .gt. 0) then
               pathinput(ninpaths).fillin=fill_last
            else if (index(cstring, 'data') .gt. 0) then
               pathinput(ninpaths).fillin=fill_bydata
            else
               write(unit_error,610)
     &              'Unknown data fill method: ' // cstring
               istat=-1
               goto 900
            endif
         else if (rifld(i) .eq. inpath_priority) then
            read(cstring,'(i5)',err=810) pathinput(ninpaths).priority
         endif
         i=i+1
      enddo

      ninpaths=ninpaths+1
      if (ninpaths .gt. max_inputpaths) then
         write(unit_error,630)
     &        'Too many input paths specified; max allowed is:'
     &        ,max_inputpaths
         istat=-1
         goto 900
      endif

      return

c-----char-to-value conversion errors

 810  continue
      write(unit_error, 620) 'Conversion error on field ' /
     &     /field_names(rifld(i)), cstring

      istat=-2

 900  continue                  ! fatal error

      return
      end

      subroutine input_tidefile(field_names, mxflds, nfields, nflds,
     &     ifld, rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for
c-----tide file info.

      implicit none

      include 'common.f'
      include '../time-varying/common_tide.f'

      logical
     &     ldefault             ! true if values are for defaults
      common /read_fix_l/ ldefault

c-----local variables

      integer
     &     mxflds               ! maximum number of fields
     &     ,nfields             ! number of fields in data line (input)
     &     ,nflds               ! number of fields in headers (input)
     &     ,ifld(mxflds)        ! ifld(i)=order header keyword i occurs in file (input)
     &     ,rifld(mxflds)       ! reverse ifld
     &     ,ibegf(mxflds)       ! beginning position of each field in line (input)
     &     ,ilenf(mxflds)       ! length of each field in line (input)
     &     ,istat               ! conversion status of this line (output)

      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)

      integer
     &     i                    ! index

      integer*4
     &     incr_intvl           ! increment julian minute by interval function

      character
     &     cstring*80           ! string field

      character
     &     input_line*250       ! raw input line
      common /input_lines/ input_line
      data nintides /1/

c! The optional starting and ending datetimes specify when to use
c! each tidefile; they override the timestamp in the tidefile
c! itself.  If not given, the timestamp in the tidefile
c! will be used for the start datetime, and it will be used to
c! the end of the tidefile or model run.  However, if the tidefile
c! length is tide_cycle_length_mins long (minutes), then Qual or PTM will
c! assume that the tidefile should be recycled an integer number of
c! times until the next tidefile is to be used.

c! Keywords used for the starting and ending datetimes can be used to
c! simplify chaining together tidefiles.

c! Start datetime keyword explanation:
c! generic:	use generic datetime for tidefile (i.e. ignore tidefile
c!      	time, just start using first tideblock, etc.)
c! runtime:	try to find run start time in tidefiles; if not succesful
c!      	exit with error (same as if no start time given)
c! previous:	use this tidefile right when the previous tidefile ends
c! last:	same as 'previous'
c! none:	field placeholder (doesn't do anything; same as if field
c!    		not given)

c! End datetime keywords:
c! length:	use all of tidefile, to its end
c! none:	see above

      if (ldefault) then
         nintides=0
      else
         if (nintides .eq. 0) nintides=1
      endif

      i=1
      tide_files(nintides).start_dt=' '
      tide_files(nintides).end_dt=' '
      do while (i .le. nfields)
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)

         if (rifld(i) .eq. tide_fname) then
            tide_files(nintides).filename=
     &           input_line(ibegf(i):ibegf(i)+ilenf(i)-1) ! use raw input to preserve case
         else if (rifld(i) .eq. tide_sdate) then ! starting date for this tidefile
            if (index(cstring,'gen') .gt. 0) then
               tide_files(nintides).start_dt=generic_dt
            else if (index(cstring,'run') .gt. 0) then
               tide_files(nintides).start_dt=' '
            else if ( (index(cstring,'prev') .gt. 0) .or.
     &              (index(cstring,'last') .gt. 0) ) then
               if (nintides .ne. 1) then
                  tide_files(nintides).start_dt='last'
               else             ! can't have 'last' for first tide file
                  write(unit_error, '(a)')
     &                 'Cannot use "last" or "prev" keyword for first tidefile.'
                  istat=-1
                  goto 900
               endif
            else
               if (index(cstring, 'none') .le. 0)
     &              tide_files(nintides).start_dt(1:9)=cstring(1:9)
            endif
         else if ( (rifld(i) .eq. tide_stime)) then ! starting time
            if (index(cstring,'gen') .gt. 0) then
               tide_files(nintides).start_dt=generic_dt
            elseif ( (index(cstring,'prev') .gt. 0) .or.
     &              (index(cstring,'last') .gt. 0) ) then
               if (nintides .ne. 1) then
                  tide_files(nintides).start_dt='last'
               else             ! can't have 'last' for first tide file
                  write(unit_error, '(a)')
     &                 'Cannot use "last" keyword for first tidefile.'
                  istat=-1
                  goto 900
               endif
            else
               if (index(cstring, 'none') .le. 0)
     &              tide_files(nintides).start_dt(11:14)=cstring(1:4)
            endif
         else if (rifld(i) .eq. tide_edate) then ! ending date for this tidefile
            if (index(cstring,'len') .gt. 0) then
               tide_files(nintides).end_dt='length'
            else if (index(cstring,'run') .gt. 0) then
               tide_files(nintides).start_dt=' '
            else if (index(cstring, 'none') .le. 0) then
               if (incr_intvl(0,cstring,IGNORE_BOUNDARY) .eq. miss_val_i) then ! not a time length
                  tide_files(nintides).end_dt(1:9)=cstring(1:9)
               else             ! is a time length
                  tide_files(nintides).end_dt=cstring ! take the full string
               endif
            endif
            if (tide_files(nintides).end_dt .eq. ' ') then
               tide_files(nintides).end_dt='length'
            endif
         else if (rifld(i) .eq. tide_etime) then ! ending time
            if (index(cstring,'len') .gt. 0) then
               tide_files(nintides).end_dt='length'
            else if (index(cstring, 'none') .le. 0) then
               if (incr_intvl(0,cstring,IGNORE_BOUNDARY) .eq. miss_val_i) then ! not a time length
                  tide_files(nintides).end_dt(11:14)=cstring(1:4)
               else             ! is a time length
                  tide_files(nintides).end_dt=cstring ! take the full string
               endif
            endif
            if (tide_files(nintides).end_dt .eq. ' ') then
               tide_files(nintides).end_dt='length'
            endif
         endif
         i=i+1
      enddo

      nintides=nintides+1
      if (nintides .gt. max_tide_files) then
         write(unit_error,630)
     &        'Too many tidefiles specified; max allowed is:'
     &        ,max_tide_files
 630     format(/a,i5)
         istat=-1
         goto 900
      endif

      return

 900  continue
      return

      end

      subroutine input_qualbin(field_names, mxflds, nfields, nflds,
     &     ifld, rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for
c-----tide file info.

      implicit none

      include 'common.f'
      include '../time-varying/common_qual_bin.inc'

      logical
     &     ldefault             ! true if values are for defaults
      common /read_fix_l/ ldefault

c-----local variables

      integer
     &     mxflds               ! maximum number of fields
     &     ,nfields             ! number of fields in data line (input)
     &     ,nflds               ! number of fields in headers (input)
     &     ,ifld(mxflds)        ! ifld(i)=order header keyword i occurs in file (input)
     &     ,rifld(mxflds)       ! reverse ifld
     &     ,ibegf(mxflds)       ! beginning position of each field in line (input)
     &     ,ilenf(mxflds)       ! length of each field in line (input)
     &     ,istat               ! conversion status of this line (output)

      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)

      integer
     &     i                    ! index

      integer*4
     &     incr_intvl           ! increment julian minute by interval function

      character
     &     cstring*80           ! string field

      character
     &     input_line*250       ! raw input line
      common /input_lines/ input_line

      i=1
      do while (i .le. nfields)
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)

         if (rifld(i) .eq. binary_fname) then
            qual_bin_file.filename=
     &           input_line(ibegf(i):ibegf(i)+ilenf(i)-1) ! use raw input to preserve case
         endif
         i=i+1
      enddo

      return

      end


      subroutine input_outputpath(field_names, mxflds, nfields, nflds,
     &     ifld, rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for
c-----print out info: names and type of data to print

      implicit none

      include 'common.f'

      logical
     &     ldefault             ! true if values are for defaults
      common /read_fix_l/ ldefault

c-----local variables

      integer
     &     mxflds               ! maximum number of fields
     &     ,nfields             ! number of fields in data line (input)
     &     ,nflds               ! number of fields in headers (input)
     &     ,ifld(mxflds)        ! ifld(i)=order header keyword i occurs in file (input)
     &     ,rifld(mxflds)       ! reverse ifld
     &     ,ibegf(mxflds)       ! beginning position of each field in line (input)
     &     ,ilenf(mxflds)       ! length of each field in line (input)
     &     ,istat               ! conversion status of this line (output)
     &     ,loccarr             ! function to return array location of string

      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)

      integer
     &     itmp                 ! index
     &     ,i                   ! index

      character
     &     cstring*80           ! string field
     &     ,ctmp*80             ! temporary char variable

      character
     &     input_line*250       ! raw input line
      common /input_lines/ input_line

      data noutpaths /1/

 610  format(/a)
 620  format(/a
     &     /'Input string is: ',a)
 630  format(/a,i5)

      if (ldefault) then
         noutpaths=0
      else
         if (noutpaths .eq. 0) noutpaths=1
      endif

      i=1
      do while (i .le. nfields)
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)

         if (rifld(i) .eq. outpath_name) then
            pathoutput(noutpaths).name=cstring
         else if (rifld(i) .eq. outpath_filename) then
            pathoutput(noutpaths).filename=
     &           input_line(ibegf(i):ibegf(i)+ilenf(i)-1) ! use raw input to preserve case
            if (index(pathoutput(noutpaths).filename, '.dss') .gt. 0) then
c--------------accumulate unique dss output filenames
               itmp=loccarr(pathoutput(noutpaths).filename,outfilenames
     &              ,max_dssoutfiles, EXACT_MATCH)
               if (itmp .lt. 0) then
                  if (abs(itmp) .le. max_dssoutfiles) then
                     outfilenames(abs(itmp))=pathoutput(noutpaths).filename
                     pathoutput(noutpaths).ndx_file=abs(itmp)
                  else
                     write(unit_error,610)
     &                    'Maximum number of unique DSS output files exceeded'
                     goto 900
                  endif
               else
                  pathoutput(noutpaths).ndx_file=itmp
               endif
            endif
         else if (rifld(i) .eq. outpath_chan) then
            read(cstring,'(i5)',err=810) pathoutput(noutpaths).object_no
            pathoutput(noutpaths).object=obj_channel
            if (pathoutput(noutpaths).object_no .le. 0) then
               write(unit_error, 630)
     &              'Invalid output channel number given:',
     &              pathoutput(noutpaths).object_no
               istat=-1
               goto 900
            endif
         else if (rifld(i) .eq. outpath_dist) then
            pathoutput(noutpaths).object=obj_channel
            if (index(cstring,'len') .gt. 0) then
               pathoutput(noutpaths).chan_dist=chan_length
            else
               read(cstring,'(i10)',err=810) pathoutput(noutpaths).chan_dist
            endif
         else if (rifld(i) .eq. outpath_node) then
            if (ifld(outpath_res_name) .gt. 0 .and.
     &           ifld(outpath_res_node) .eq. 0) then
c--------------user probably meant reservoir_node: reservoir field keyword
c--------------is given and reservoir_node is not given
               pathoutput(noutpaths).object=obj_reservoir
               if (cstring .ne. 'none') then
                  read(cstring,'(i5)',err=810) pathoutput(noutpaths).reservoir.node_no
               else
                  pathoutput(noutpaths).reservoir.node_no=0
               endif
            else                ! not a reservoir-related node
               read(cstring,'(i5)',err=810) pathoutput(noutpaths).object_no
               pathoutput(noutpaths).object=obj_node
            endif
         else if (rifld(i) .eq. outpath_res_name) then
            pathoutput(noutpaths).object=obj_reservoir
            pathoutput(noutpaths).name=cstring
         else if (rifld(i) .eq. outpath_res_node) then
            pathoutput(noutpaths).object=obj_reservoir
            if (cstring .ne. 'none') then
               read(cstring,'(i5)',err=810) pathoutput(noutpaths).reservoir.node_no
            else
               pathoutput(noutpaths).reservoir.node_no=0
            endif
         else if (rifld(i) .eq. outpath_type) then
            pathoutput(noutpaths).meas_type=cstring
            if (index(cstring, 'flow') .gt. 0 .or.
     &           index(cstring, 'pump') .gt. 0) then
               pathoutput(noutpaths).units='cfs'
            else if (cstring(1:3) .eq. 'vel') then
               pathoutput(noutpaths).meas_type='vel'
               pathoutput(noutpaths).units='ft/s'
            else if (cstring .eq. 'stage') then
               pathoutput(noutpaths).units='feet'
            else if (cstring .eq. 'tds') then
               pathoutput(noutpaths).units='ppm'
            else if (cstring .eq. 'ec') then
               pathoutput(noutpaths).units='umhos/cm'
            else if (cstring .eq. 'do') then
               pathoutput(noutpaths).units='mg/l'
            else if (cstring .eq. 'nh3-n') then
               pathoutput(noutpaths).units='mg/l'
            else if (cstring .eq. 'org-n') then
               pathoutput(noutpaths).units='mg/l'
            else if (cstring .eq. 'no2-n') then
               pathoutput(noutpaths).units='mg/l'
            else if (cstring .eq. 'no3-n') then
               pathoutput(noutpaths).units='mg/l'
            else if (cstring .eq. 'bod') then
               pathoutput(noutpaths).units='mg/l'
            else if (cstring .eq. 'org-p') then
               pathoutput(noutpaths).units='mg/l'
            else if (cstring .eq. 'po4-p') then
               pathoutput(noutpaths).units='mg/l'
            else if (cstring .eq. 'algae') then
               pathoutput(noutpaths).units='mg/l'
            else if (cstring .eq. 'temp') then
               pathoutput(noutpaths).units='deg c'
            else                ! unidentified output type; default part per million
               pathoutput(noutpaths).units='ppm'
            endif
         else if (rifld(i) .eq. outpath_from_name) then
            pathoutput(noutpaths).source.loc_name=cstring
         else if (rifld(i) .eq. outpath_from_node) then
            read(cstring,'(i5)',err=810) pathoutput(noutpaths).source.object_no
            pathoutput(noutpaths).source.object=obj_node
         else if (rifld(i) .eq. outpath_from_type) then
               pathoutput(noutpaths).source.acct_name=cstring
         else if (rifld(i) .eq. outpath_interval) then
            call split_epart(cstring,itmp,ctmp)
            if (itmp .ne. miss_val_i) then ! valid interval, parse it
               pathoutput(noutpaths).no_intervals=itmp
               pathoutput(noutpaths).interval=ctmp
            else
               write(unit_error,610)
     &              'Unknown input interval: ' // cstring
               istat=-1
               goto 900
            endif
         else if (rifld(i) .eq. outpath_period) then
            pathoutput(noutpaths).per_type=per_type_inst_val ! assume instantaneous
            if (cstring(1:2) .eq. 'av')
     &           pathoutput(noutpaths).per_type=per_type_per_aver
            if (cstring(1:3) .eq. 'min')
     &           pathoutput(noutpaths).per_type=per_type_per_min
            if (cstring(1:3) .eq. 'max')
     &           pathoutput(noutpaths).per_type=per_type_per_max
         else if (rifld(i) .eq. outpath_modifier) then
            if (cstring(1:4) .eq. 'none') then
               pathoutput(noutpaths).modifier=' '
            else
               pathoutput(noutpaths).modifier=cstring
            endif
         endif
         i=i+1
      enddo

      noutpaths=noutpaths+1
      if (noutpaths .gt. max_outputpaths) then
         write(unit_error,630)
     &        'Too many pathoutput paths specified; max allowed is:'
     &        ,max_outputpaths
         istat=-1
         goto 900
      endif

      return

c-----char-to-value conversion errors

 810  continue
      write(unit_error, 620) 'Conversion error on field ' //
     &     field_names(rifld(i)), cstring

      istat=-2

 900  continue                  ! fatal error

      return
      end

      subroutine input_iofiles(field_names, mxflds, nfields, nflds, ifld,
     &     rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for
c-----output file names

      implicit none

      include 'common.f'
      include 'common_ptm.inc'
c-----local variables

      integer
     &     mxflds               ! maximum number of fields
     &     ,nfields             ! number of fields in data line (input)
     &     ,nflds               ! number of fields in headers (input)
     &     ,ifld(mxflds)        ! ifld(i)=order header keyword i occurs in file (input)
     &     ,rifld(mxflds)       ! reverse ifld
     &     ,ibegf(mxflds)       ! beginning position of each field in line (input)
     &     ,ilenf(mxflds)       ! length of each field in line (input)
     &     ,istat               ! conversion status of this line (output)

      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)

      integer
     &     i,i1,i2,i3           ! indices

      character*10
     &     cstring1             ! string for model
     &     ,cstring2            ! string for type
     &     ,cstring3            ! string for io
     &     ,cstring4            ! string for interval
      character*80
     &     cstring5             ! string for filename

      character
     &     input_line*250       ! raw input line
      common /input_lines/ input_line

 610  format(/a)
 620  format(/'Invalid value given in ',a,' field: ',a)

c-----model, type, and io are required for each line
      if (ifld(io_model) .eq. 0) then
         write(unit_error, 610)
     &        'No model given.'
         istat=-1
         goto 900
      endif

      if (ifld(io_type) .eq. 0) then
         write(unit_error, 610)
     &        'No type given.'
         istat=-1
         goto 900
      endif

      if (ifld(io_io) .eq. 0) then
         write(unit_error, 610)
     &        'No io method given.'
         istat=-1
         goto 900
      endif

      i=ifld(io_model)
      cstring1=' '
      cstring1=line(ibegf(i):ibegf(i)+ilenf(i)-1)

      i=ifld(io_type)
      cstring2=' '
      cstring2=line(ibegf(i):ibegf(i)+ilenf(i)-1)

      i=ifld(io_io)
      cstring3=' '
      cstring3=line(ibegf(i):ibegf(i)+ilenf(i)-1)

      cstring4=' '
      if (ifld(io_interval) .gt. 0) then
         i=ifld(io_interval)
         cstring4=line(ibegf(i):ibegf(i)+ilenf(i)-1)
      endif

      cstring5=' '
      if (ifld(io_filename) .gt. 0) then
         i=ifld(io_filename)
         cstring5=input_line(ibegf(i):ibegf(i)+ilenf(i)-1) ! use raw input to preserve case
      endif

c-----fill in structure

      if (cstring1(1:3) .eq. 'out') then
         output_filename=cstring5
         return
      else if (cstring1(1:3) .eq. 'hyd') then
         i1=hydro
      else if (cstring1(1:3) .eq. 'qua') then
         i1=qual
      else if (cstring1(1:3) .eq. 'ptm') then
         i1=ptm
      else
         write(unit_error, 620) 'model', cstring1
         istat=-1
         goto 900
      endif

      if (cstring2(1:3) .eq. 'res') then
         i2=io_restart
      else if (cstring2(1:3) .eq. 'bin' .or.
     &        cstring2(1:3) .eq. 'tid') then
         i2=io_tide
      else if (cstring2(1:3) .eq. 'ani') then
         i2=io_animation
      else if (cstring2(1:3) .eq. 'tra') then
         i2=io_trace
      else if (cstring2(1:3) .eq. 'beh') then
         i2=io_behavior
      else if (cstring2(1:3) .eq. 'gro') then
         i2=io_group
         ptm_igroup_int=1
         ptm_igroup=.true.
      else
         write(unit_error, 620) 'type', cstring2
         istat=-1
         goto 900
      endif

      if (cstring3(1:2) .eq. 'in') then
         i3=io_read
      else if (cstring3(1:3) .eq. 'out') then
         i3=io_write
      else
         write(unit_error, 620) 'io', cstring3
         istat=-1
         goto 900
      endif

      if (cstring4 .ne. ' ' .and.
     &     cstring4(1:4) .ne. 'none') then
         io_files(i1,i2,i3).interval=cstring4
      endif

      io_files(i1,i2,i3).use=.true.
      io_files(i1,i2,i3).filename=cstring5

      return

 900  continue

      return

      end

      subroutine input_translations(field_names, mxflds, nfields, nflds,
     &     ifld, rifld,line, ibegf, ilenf, istat)

c-----process a character line into data arrays for
c-----place name translation info: name to chan/dist, node,
c-----reservoir, or path part

      implicit none

      include 'common.f'

      logical
     &     ldefault             ! true if values are for defaults
      common /read_fix_l/ ldefault

c-----local variables

      integer
     &     mxflds               ! maximum number of fields
     &     ,nfields             ! number of fields in data line (input)
     &     ,nflds               ! number of fields in headers (input)
     &     ,ifld(mxflds)        ! ifld(i)=order header keyword i occurs in file (input)
     &     ,rifld(mxflds)       ! reverse ifld
     &     ,ibegf(mxflds)       ! beginning position of each field in line (input)
     &     ,ilenf(mxflds)       ! length of each field in line (input)
     &     ,istat               ! conversion status of this line (output)
     &     ,i

      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)

      integer
     &     tranno               ! translation number

      character
     &     cstring*32           ! string field

      data ntrans /0/

 610  format(/a)
 620  format(/a
     &     /'Input string is: ',a)
 630  format(/a,i5)

c-----translation from name is required for each line; no default
      if (ifld(trans_name) .eq. 0) then
         write(unit_error, 610)
     &        'No translation from-name given.'
         istat=-1
         goto 900
      endif

c-----individual translation; check for duplicate, if so, overwrite
      i=ifld(trans_name)
      cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1) ! tranlation name
      tranno=1
      if (ntrans .eq. 0) then
         ntrans=1
      else
         do while (tranno .le. ntrans .and.
     &        translations(tranno).from_name .ne. cstring)
            tranno=tranno+1
         enddo
         ntrans=max(ntrans,tranno)
         if (ntrans .gt. max_translations) then
            write(unit_error,630)
     &           'Too many translations specified; max allowed is:'
     &           ,max_translations
            istat=-1
            goto 900
         endif
      endif

c-----can translate to either channel/node, or reservoir, or gate,
c-----or a path part, but not multiples
      i=1                       ! data field index
      translations(tranno).object_no=0
      translations(tranno).chan_dist=0
      translations(tranno).obj_name=' '
      translations(tranno).constituent=' '
      do while (i .le. nfields)
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
         if (rifld(i) .eq. trans_name) then
            translations(tranno).from_name=cstring
         else if (rifld(i) .eq. trans_chan) then
            read(cstring,'(i5)',err=810) translations(tranno).object_no
            translations(tranno).object=obj_channel
         else if (rifld(i) .eq. trans_dist) then
            if (index(cstring,'len') .gt. 0) then
               translations(tranno).chan_dist=chan_length
            else
               read(cstring,'(i10)',err=810) translations(tranno).chan_dist
            endif
         else if (rifld(i) .eq. trans_node) then
            read(cstring,'(i5)',err=810) translations(tranno).object_no
            translations(tranno).object=obj_node
         else if (rifld(i) .eq. trans_res) then
            translations(tranno).obj_name=cstring
            translations(tranno).object=obj_reservoir
         else if (rifld(i) .eq. trans_gate) then
            translations(tranno).obj_name=cstring
            translations(tranno).object=obj_gate
         else if (rifld(i) .eq. trans_const) then
            translations(tranno).constituent=cstring
         endif
         i=i+1
      enddo
      return

c-----char-to-value conversion errors

 810  continue
      write(unit_error, 620) 'Conversion error on field ' //
     &     field_names(rifld(i)), cstring

      istat=-2

 900  continue                  ! fatal error

      return
      end

      subroutine input_type(field_names, mxflds, nfields, nflds,
     &     ifld, rifld,line, ibegf, ilenf, istat)

c-----process a character line into data arrays for
c-----assigning types to input paths
c-----e.g. specify which paths should have sign changed on
c-----input values; or specify if an outgoing flow is a diversion,
c-----evaporation, seepage, etc.; or change incoming values (usually
c-----used for gate codes).

      implicit none

      include 'common.f'

      logical
     &     ldefault             ! true if values are for defaults
      common /read_fix_l/ ldefault

c-----local variables

      integer
     &     mxflds               ! maximum number of fields
     &     ,nfields             ! number of fields in data line (input)
     &     ,nflds               ! number of fields in headers (input)
     &     ,ifld(mxflds)        ! ifld(i)=order header keyword i occurs in file (input)
     &     ,rifld(mxflds)       ! reverse ifld
     &     ,ibegf(mxflds)       ! beginning position of each field in line (input)
     &     ,ilenf(mxflds)       ! length of each field in line (input)
     &     ,istat               ! conversion status of this line (output)
     &     ,i,j                 ! array index numbers
     &     ,lnblnk              ! intrinsic

      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)

      character
     &     cstring*15           ! string field

      data ntypes /0/

 610  format(/a)
 611  format(/a,i5)

      ntypes=ntypes+1
      if (ntypes .gt. max_translations) then
         write(unit_error,611)
     &        'Too many types specified; max allowed is:'
     &        ,max_types
         istat=-1
         goto 900
      endif

      i=1
      do while (i .le. nfields)
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
         if (rifld(i) .eq. type_string) then
            type_spec(ntypes).string=cstring
         else if (rifld(i) .eq. type_part) then
 620        format(/'Type part spec must be A, B, C, E, or F.')
            if (ilenf(i) .ne. 1) then
               write(unit_error, 620) cstring
               goto 900
            else if (
     &              cstring .ne. 'a' .and.
     &              cstring .ne. 'b' .and.
     &              cstring .ne. 'c' .and.
     &              cstring .ne. 'e' .and.
     &              cstring .ne. 'f' .and.
     &              cstring .ne. 'p' .and.
     &              cstring .ne. 'l'
     &              ) then
               write(unit_error, 620) cstring
               goto 900
            endif
            type_spec(ntypes).part=cstring
         else if (rifld(i) .eq. type_match) then
 630        format(/'Unrecognized field input, must be either EXACT'
     &           ' or SUBSTRING: ',a)
            if (index(cstring,'ex') .gt. 0) then ! exact match
               type_spec(ntypes).match='e'
            else if (index(cstring,'sub') .gt. 0) then ! substring match
               type_spec(ntypes).match='s'
            else                ! field error
               write(unit_error, 630) cstring
               goto 900
            endif
         else if (rifld(i) .eq. type_sign) then
 640        format(/'Unrecognized field input, must be either +, -, or none: ',
     &           a)
            if (cstring .eq. '-' .or.
     &           index(cstring, 'neg') .gt. 0 .or.
     &           index(cstring, 'minus') .gt. 0) then ! change sign to minus
               type_spec(ntypes).sign='-'
            else if (cstring .eq. '+' .or.
     &              index(cstring, 'pos') .gt. 0 .or.
     &              index(cstring, 'plus') .gt. 0) then ! change sign to plus
               type_spec(ntypes).sign='+'
            else if (cstring .eq. 'none') then ! reset sign to nothing
               type_spec(ntypes).sign=' '
            else                ! field input error
               write(unit_error, 640) cstring
               goto 900
            endif
         else if (rifld(i) .eq. type_acctname) then
c-----------check for 'none' first
            if (cstring .eq. 'none') then
               type_spec(ntypes).acct_name=' '
               type_spec(ntypes).acct_ndx=0
            else
               type_spec(ntypes).acct_name=cstring
c--------------fill acct_ndx in check_fixed
            endif
         else if (rifld(i) .eq. type_massfrac) then
            read(cstring,'(f10.0)',err=810) type_spec(ntypes).mass_frac
         else if (rifld(i) .eq. type_value_in) then
            read(cstring,'(f10.0)',err=810) type_spec(ntypes).value_in
         else if (rifld(i) .eq. type_value_out) then
            read(cstring,'(f10.0)',err=710) type_spec(ntypes).value_out
 710        continue            ! conversion error, check value codes
            j=1
            do while (value_codes(j).code .ne. cstring .and.
     &           value_codes(j).code .ne. ' ')
               j=j+1
            enddo
            if (value_codes(j).code .ne. ' ') then
               type_spec(ntypes).value_out=value_codes(j).value
            else                ! couldn't find code
               write(unit_error,650) cstring(:lnblnk(cstring))
 650           format(/'Could not find value_out code ''',a,'''')
               goto 900
            endif
         else if (rifld(i) .eq. type_value_flag) then
            if (cstring(1:3) .eq. 'scr') then
               type_spec(ntypes).value_flag=SCREENED_BIT
            else if (cstring(1:4) .eq. 'good') then
               type_spec(ntypes).value_flag=GOOD_BIT
            else if (cstring(1:3) .eq. 'que') then
               type_spec(ntypes).value_flag=QUESTION_BIT
            else if (cstring(1:4) .eq. 'miss') then
               type_spec(ntypes).value_flag=MISSING_BIT
            else if (cstring(1:3) .eq. 'rej') then
               type_spec(ntypes).value_flag=REJECT_BIT
            else
               write(unit_error,652) cstring(:lnblnk(cstring))
 652           format(/'Invalid data quality flag code, should be:'
     &              /'Screen, Good, Question, Miss, Reject')
               goto 900
            endif
         endif
         i=i+1
      enddo

      return

 810  continue
      write(unit_error, 620) 'Conversion error on field ' //
     &     field_names(rifld(i)), cstring

 900  continue                  ! fatal error
      istat=-2

      return
      end

      subroutine input_quadrature(field_names, mxflds, nfields, nflds,
     &     ifld, rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for
c-----quadrature integration info

      implicit none

      include 'common.f'

      logical
     &     ldefault             ! true if values are for defaults
      common /read_fix_l/ ldefault

      include '../../hydro/network.inc'
      include '../../hydro/netcntrl.inc'

c-----local variables

      integer
     &     mxflds               ! maximum number of fields
     &     ,nfields             ! number of fields in data line (input)
     &     ,nflds               ! number of fields in headers (input)
     &     ,ifld(mxflds)        ! ifld(i)=order header keyword i occurs in file (input)
     &     ,rifld(mxflds)       ! reverse ifld
     &     ,ibegf(mxflds)       ! beginning position of each field in line (input)
     &     ,ilenf(mxflds)       ! length of each field in line (input)
     &     ,istat               ! conversion status of this line (output)
     &     ,i                   ! array index

      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)

      character
     &     cstring*15           ! string field

      data nquadpts /1/

 620  format(/a
     &     /'Input string is: ',a)
 630  format(/a,f10.2)

      if (ldefault) then
         nquadpts=0
      else
         if (nquadpts .eq. 0) nquadpts=1
      endif

      i=q_pt
      cstring=line(ibegf(ifld(i)):ibegf(ifld(i)) +
     &     ilenf(ifld(i))-1)
      read(cstring,'(f10.0)',err=810) quadpt(nquadpts)
      i=q_wt
      cstring=line(ibegf(ifld(i)):ibegf(ifld(i)) +
     &     ilenf(ifld(i))-1)
      read(cstring,'(f10.0)',err=810) quadwt(nquadpts)

      if (
     &     quadpt(nquadpts) .gt. 1.0 .or.
     &     quadpt(nquadpts) .lt. 0.0) then
         write(unit_error,630)
     &        'Quad Point out of bounds:',quadpt(nquadpts)
         goto 900
      endif

      nquadpts=nquadpts+1
      if (nquadpts .gt. maxquadpts) then
         write(unit_error,630)
     &        'Too many quadpts specified; max allowed is:'
     &        ,maxquadpts
         istat=-1
         goto 900
      endif

      return

c-----char-to-value conversion errors

 810  continue
      write(unit_error, 620) 'Conversion error on field ' //
     &     field_names(ifld(i)), cstring

 900  continue

      istat=-2

      return
      end

      subroutine input_envvar(field_names, mxflds, nfields, nflds,
     &     ifld, rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for
c-----pseudo environment variable info

      implicit none

      include 'common.f'

      character
     &     input_line*250       ! raw input line
      common /input_lines/ input_line

c-----local variables

      integer
     &     mxflds               ! maximum number of fields
     &     ,nfields             ! number of fields in data line (input)
     &     ,nflds               ! number of fields in headers (input)
     &     ,ifld(mxflds)        ! ifld(i)=order header keyword i occurs in file (input)
     &     ,rifld(mxflds)       ! reverse ifld
     &     ,ibegf(mxflds)       ! beginning position of each field in line (input)
     &     ,ilenf(mxflds)       ! length of each field in line (input)
     &     ,istat               ! conversion status of this line (output)
     &     ,i                   ! array index
     &     ,nenvvars            ! number of env vars

      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)

      save nenvvars
      data nenvvars /1/

 610  format(/a)
 630  format(/a,i3)

c-----name required for each line; empty value indicates erase it
      if (ifld(envvar_name) .eq. 0) then
         write(unit_error, 610) 'No environment variable name given.'
         istat=-1
         goto 900
      endif
      i=ifld(envvar_name)
      envvars(nenvvars).name=input_line(ibegf(ifld(i)):ibegf(ifld(i)) +
     &     ilenf(ifld(i))-1)
      
      if (ifld(envvar_value) .eq. 0) then ! no value
         envvars(nenvvars).value=' '
      else
         i=ifld(envvar_value)
         envvars(nenvvars).value=input_line(ibegf(ifld(i)):ibegf(ifld(i)) + 
     &        ilenf(ifld(i))-1)
      endif
      nenvvars=nenvvars+1
      if (nenvvars .gt. max_envvars) then
         write(unit_error,630)
     &        'Too many envvars specified; max allowed is:'
     &        ,max_envvars
         istat=-1
         goto 900
      endif

      return

 900  continue

      istat=-2

      return
      end

      subroutine input_scalar(field_names, mxflds, nfields, nflds, ifld,
     &     rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for scalar info

      implicit none

      include '../../hydro/network.inc'
      include '../../hydro/netcntrl.inc'
      include '../../hydro/chconnec.inc'

      include 'common.f'
      include 'common_ptm.inc'
      logical
     &     ldefault             ! true if values are for defaults
      common /read_fix_l/ ldefault

c-----local variables

      integer
     &     mxflds               ! maximum number of fields
     &     ,nfields             ! number of fields in data line (input)
     &     ,nflds               ! number of fields in headers (input)
     &     ,ifld(mxflds)        ! ifld(i)=order header keyword i occurs in file (input)
     &     ,rifld(mxflds)       ! reverse ifld
     &     ,ibegf(mxflds)       ! beginning position of each field in line (input)
     &     ,ilenf(mxflds)       ! length of each field in line (input)
     &     ,istat               ! conversion status of this line (output)

      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)

      integer
     &     lnblnk               ! last non-blank function

      character
     &     cstring1*20          ! string field for keyword or value
     &     ,cstring2*20         ! string field for keyword or value
     &     ,ctmp*20             ! scratch character variable

      data nprints /1/

c-----defaults

      data
     &     variabledensity /.false./
     &     variablesinuosity /.false./
     &     theta /0.6/
     &     maxiterations /50/
     &     luinc /1/
     &     toleranceq /0.0005/
     &     tolerancez /0.0005/

 610  format(/'Unrecognized line in SCALAR section:'
     &     /a)
 615  format(/'Theta must be between 0.5 and 1.0:',f5.2)
 620  format(/a,' ',a/a)
 630  format(/a)

      cstring1=line(ibegf(1):ibegf(1)+ilenf(1)-1)
      cstring2=line(ibegf(2):ibegf(2)+ilenf(2)-1)

c-----run start date and time can be a DSS date (e.g. 01jan1994 0100),
c-----or 'restart' (use date from restart file), or
c-----'tide' (use date from tidefile)
      if (
     &     cstring1 .eq. 'run_start_date' .or.
     &     cstring2 .eq. 'run_start_date') then
         if (cstring1 .eq. 'run_start_date') then
            run_start_dt(1:9)=cstring2(1:9)
         else
            run_start_dt(1:9)=cstring1(1:9)
         endif

      else if (
     &        cstring1 .eq. 'run_start_time' .or.
     &        cstring2 .eq. 'run_start_time') then
         if (cstring1 .eq. 'run_start_time') then
            run_start_dt(11:14)=cstring2(1:4)
         else
            run_start_dt(11:14)=cstring1(1:4)
         endif

      else if (
     &        cstring1 .eq. 'run_end_date' .or.
     &        cstring2 .eq. 'run_end_date') then
         if (cstring1 .eq. 'run_end_date') then
            run_end_dt(1:9)=cstring2(1:9)
         else
            run_end_dt(1:9)=cstring1(1:9)
         endif

      else if (
     &        cstring1 .eq. 'run_end_time' .or.
     &        cstring2 .eq. 'run_end_time') then
         if (cstring1 .eq. 'run_end_time') then
            run_end_dt(11:14)=cstring2(1:4)
         else
            run_end_dt(11:14)=cstring1(1:4)
         endif

      else if (
     &        cstring1 .eq. 'run_length' .or.
     &        cstring2 .eq. 'run_length') then
         if (cstring1 .eq. 'run_length') then
            run_length=cstring2
         else
            run_length=cstring1
         endif

      else if (
     &        cstring1 .eq. 'print_start_date' .or.
     &        cstring2 .eq. 'print_start_date') then
         if (ldefault) then
            nprints=0
         else
            if (nprints .eq. 0) nprints=1
         endif
         if (cstring1 .eq. 'print_start_date') then
            print_start_dt(nprints)(1:9)=cstring2(1:9)
         else
            print_start_dt(nprints)(1:9)=cstring1(1:9)
         endif
         nprints=nprints+1
      else if (
     &        cstring1 .eq. 'print_start_time' .or.
     &        cstring2 .eq. 'print_start_time') then
         if (ldefault) then
            nprints=0
         else
            if (nprints .eq. 0) nprints=1
         endif
         if (cstring1 .eq. 'print_start_time') then
            print_start_dt(nprints)(11:14)=cstring2(1:4)
         else
            print_start_dt(nprints)(11:14)=cstring1(1:4)
         endif

      else if (
     &        cstring1 .eq. 'flush_output' .or.
     &        cstring2 .eq. 'flush_output') then
         if (cstring1 .eq. 'flush_output') then
            flush_intvl=cstring2
         else
            flush_intvl=cstring1
         endif

      else if (
     &        cstring1 .eq. 'hydro_time_step' .or.
     &        cstring2 .eq. 'hydro_time_step') then
         if (cstring1 .eq. 'hydro_time_step') then
            time_step_intvl_hydro=cstring2
         else
            time_step_intvl_hydro=cstring1
         endif

      else if (
     &        cstring1 .eq. 'qual_time_step' .or.
     &        cstring2 .eq. 'qual_time_step') then
         if (cstring1 .eq. 'qual_time_step') then
            time_step_intvl_qual=cstring2
         else
            time_step_intvl_qual=cstring1
         endif

      else if (
     &        cstring1 .eq. 'ptm_time_step' .or.
     &        cstring2 .eq. 'ptm_time_step') then
         ptm_time_step_int=1
         if (cstring1 .eq. 'ptm_time_step') then
            time_step_intvl_ptm=cstring2
         else
            time_step_intvl_ptm=cstring1
         endif

      else if (
     &        cstring1 .eq. 'mass_tracking' .or.
     &        cstring2 .eq. 'mass_tracking') then
         if (cstring1.eq.'mass_tracking') then
            field_names(1)=cstring1
            read(cstring2,'(l2)', err=810) mass_tracking
         elseif(cstring2.eq.'mass_tracking')then
            field_names(1)=cstring2
            read(cstring1,'(l2)', err=810) mass_tracking
         endif

      else if (
     &        cstring1 .eq. 'init_conc' .or.
     &        cstring2 .eq. 'init_conc') then
         if(cstring1.eq.'init_conc')then
            field_names(1)=cstring1
            read(cstring2,'(f10.0)', err=810) init_conc
         elseif(cstring2.eq.'init_conc')then
            field_names(1)=cstring2
            read(cstring1,'(f10.0)', err=810) init_conc
         endif

      else if (
     &        cstring1 .eq. 'dispersion' .or.
     &        cstring2 .eq. 'dispersion') then
         if(cstring1.eq.'dispersion')then
            field_names(1)=cstring1
            read(cstring2,'(l2)', err=810) dispersion
         elseif(cstring2.eq.'dispersion')then
            field_names(1)=cstring2
            read(cstring1,'(l2)', err=810) dispersion
         endif
c--------global rates for non-conserative const.

      else if (
     &        cstring1 .eq. 'algaefract_n' .or.
     &        cstring2 .eq. 'algaefract_n') then
         if(cstring1.eq.'algaefract_n')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) algaefract_n
         elseif(cstring2.eq.'algaefract_n')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) algaefract_n
         endif

      else if (
     &        cstring1 .eq. 'algaefract_p' .or.
     &        cstring2 .eq. 'algaefract_p') then
         if(cstring1.eq.'algaefract_p')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) algaefract_p
         elseif(cstring2.eq.'algaefract_p')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) algaefract_p
         endif

      else if (
     &        cstring1 .eq. 'oxy_photo' .or.
     &        cstring2 .eq. 'oxy_photo') then
         if(cstring1.eq.'oxy_photo')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) oxy_photo
         elseif(cstring2.eq.'oxy_photo')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) oxy_photo
         endif

      else if (
     &        cstring1 .eq. 'oxy_resp' .or.
     &        cstring2 .eq. 'oxy_resp') then
         if(cstring1.eq.'oxy_resp')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) oxy_resp
         elseif(cstring2.eq.'oxy_resp')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) oxy_resp
         endif

      else if (
     &        cstring1 .eq. 'oxy_nh3' .or.
     &        cstring2 .eq. 'oxy_nh3') then
         if(cstring1.eq.'oxy_nh3')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) oxy_nh3
         elseif(cstring2.eq.'oxy_nh3')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) oxy_nh3
         endif

      else if (
     &        cstring1 .eq. 'oxy_no2' .or.
     &        cstring2 .eq. 'oxy_no2') then
         if(cstring1.eq.'oxy_no2')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) oxy_no2
         elseif(cstring2.eq.'oxy_no2')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) oxy_no2
         endif

      else if (
     &        cstring1 .eq. 'alg_chl_ratio' .or.
     &        cstring2 .eq. 'alg_chl_ratio') then
         if(cstring1.eq.'alg_chl_ratio')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) alg_chl_ratio
         elseif(cstring2.eq.'alg_chl_ratio')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) alg_chl_ratio
         endif

      else if (
     &        cstring1 .eq. 'pref_factor' .or.
     &        cstring2 .eq. 'pref_factor') then
         if(cstring1.eq.'pref_factor')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) pref_factor
         elseif(cstring2.eq.'pref_factor')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) pref_factor
         endif

      else if (
     &        cstring1 .eq. 'klight_half' .or.
     &        cstring2 .eq. 'klight_half') then
         if(cstring1.eq.'klight_half')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) klight_half
         elseif(cstring2.eq.'klight_half')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) klight_half
         endif
      else if (
     &        cstring1 .eq. 'knit_half' .or.
     &        cstring2 .eq. 'knit_half') then
         if(cstring1.eq.'knit_half')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) knit_half
         elseif(cstring2.eq.'knit_half')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) knit_half
         endif

      else if (
     &        cstring1 .eq. 'kpho_half' .or.
     &        cstring2 .eq. 'kpho_half') then
         if(cstring1.eq.'kpho_half')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) kpho_half
         elseif(cstring2.eq.'kpho_half')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) kpho_half
         endif

      else if (
     &        cstring1 .eq. 'lambda0' .or.
     &        cstring2 .eq. 'lambda0') then
         if(cstring1.eq.'lambda0')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) lambda0
         elseif(cstring2.eq.'lambda0')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) lambda0
         endif

      else if (
     &        cstring1 .eq. 'lambda1' .or.
     &        cstring2 .eq. 'lambda1') then
         if(cstring1.eq.'lambda1')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) lambda1
         elseif(cstring2.eq.'lambda1')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) lambda1
         endif

      else if (
     &        cstring1 .eq. 'lambda2' .or.
     &        cstring2 .eq. 'lambda2') then
         if(cstring1.eq.'lambda2')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) lambda2
         elseif(cstring2.eq.'lambda2')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) lambda2
         endif

      else if (
     &        cstring1 .eq. 'alg_bod' .or.
     &        cstring2 .eq. 'alg_bod') then
         if(cstring1.eq.'alg_bod')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) alg_bod
         elseif(cstring2.eq.'alg_bod')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) alg_bod
         endif
c--------heat and temperature related parameters

      else if (
     &        cstring1 .eq. 'elev' .or.
     &        cstring2 .eq. 'elev') then
         if(cstring1.eq.'elev')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) elev
         elseif(cstring2.eq.'elev')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) elev
         endif

      else if (
     &        cstring1 .eq. 'lat' .or.
     &        cstring2 .eq. 'lat') then
         if(cstring1.eq.'lat')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) lat
         elseif(cstring2.eq.'lat')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) lat
         endif

      else if (
     &        cstring1 .eq. 'long' .or.
     &        cstring2 .eq. 'long') then
         if(cstring1.eq.'long')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) long
         elseif(cstring2.eq.'long')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) long
         endif

      else if (
     &        cstring1 .eq. 'long_std_merid' .or.
     &        cstring2 .eq. 'long_std_merid') then
         if(cstring1.eq.'long_std_merid')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) long_std_merid
         elseif(cstring2.eq.'long_std_merid')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) long_std_merid
         endif

      else if (
     &        cstring1 .eq. 'dust_attcoeff' .or.
     &        cstring2 .eq. 'dust_attcoeff') then
         if(cstring1.eq.'dust_attcoeff')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) dust_attcoeff
         elseif(cstring2.eq.'dust_attcoeff')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) dust_attcoeff
         endif

      else if (
     &        cstring1 .eq. 'evapcoeff_a' .or.
     &        cstring2 .eq. 'evapcoeff_a') then
         if(cstring1.eq.'evapcoeff_a')then
            field_names(1)=cstring1
            read(cstring2,'(f10.0)', err=810) evapcoeff_a
         elseif(cstring2.eq.'evapcoeff_a')then
            field_names(1)=cstring2
            read(cstring1,'(f10.0)', err=810) evapcoeff_a
         endif

      else if (
     &        cstring1 .eq. 'evapcoeff_b' .or.
     &        cstring2 .eq. 'evapcoeff_b') then
         if(cstring1.eq.'evapcoeff_b')then
            field_names(1)=cstring1
            read(cstring2,'(f10.0)', err=810) evapcoeff_b
         elseif(cstring2.eq.'evapcoeff_b')then
            field_names(1)=cstring2
            read(cstring1,'(f10.0)', err=810) evapcoeff_b
         endif

      else if (
     &        cstring1 .eq. 'temp_bod_decay' .or.
     &        cstring2 .eq. 'temp_bod_decay') then
         if(cstring1.eq.'temp_bod_decay')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) thet(temp_bod_decay)
         elseif(cstring2.eq.'temp_bod_decay')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) thet(temp_bod_decay)
         endif

      else if (
     &        cstring1 .eq. 'temp_bod_set' .or.
     &        cstring2 .eq. 'temp_bod_set') then
         if(cstring1.eq.'temp_bod_set')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) thet(temp_bod_set)
         elseif(cstring2.eq.'temp_bod_set')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) thet(temp_bod_set)
         endif

      else if (
     &        cstring1 .eq. 'temp_reaer' .or.
     &        cstring2 .eq. 'temp_reaer') then
         if(cstring1.eq.'temp_reaer')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) thet(temp_reaer)
         elseif(cstring2.eq.'temp_reaer')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) thet(temp_reaer)
         endif

      else if (
     &        cstring1 .eq. 'temp_do_ben' .or.
     &        cstring2 .eq. 'temp_do_ben') then
         if(cstring1.eq.'temp_do_ben')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) thet(temp_do_ben)
         elseif(cstring2.eq.'temp_do_ben')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) thet(temp_do_ben)
         endif

      else if (
     &        cstring1 .eq. 'temp_orgn_decay' .or.
     &        cstring2 .eq. 'temp_orgn_decay') then
         if(cstring1.eq.'temp_orgn_decay')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) thet(temp_orgn_decay)
         elseif(cstring2.eq.'temp_orgn_decay')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) thet(temp_orgn_decay)
         endif

      else if (
     &        cstring1 .eq. 'temp_orgn_set' .or.
     &        cstring2 .eq. 'temp_orgn_set') then
         if(cstring1.eq.'temp_orgn_set')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) thet(temp_orgn_set)
         elseif(cstring2.eq.'temp_orgn_set')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) thet(temp_orgn_set)
         endif

      else if (
     &        cstring1 .eq. 'temp_nh3_decay' .or.
     &        cstring2 .eq. 'temp_nh3_decay') then
         if(cstring1.eq.'temp_nh3_decay')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) thet(temp_nh3_decay)
         elseif(cstring2.eq.'temp_nh3_decay')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) thet(temp_nh3_decay)
         endif

      else if (
     &        cstring1 .eq. 'temp_nh3_ben' .or.
     &        cstring2 .eq. 'temp_nh3_ben') then
         if(cstring1.eq.'temp_nh3_ben')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) thet(temp_nh3_ben)
         elseif(cstring2.eq.'temp_nh3_ben')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) thet(temp_nh3_ben)
         endif

      else if (
     &        cstring1 .eq. 'temp_no2_decay' .or.
     &        cstring2 .eq. 'temp_no2_decay') then
         if(cstring1.eq.'temp_no2_decay')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) thet(temp_no2_decay)
         elseif(cstring2.eq.'temp_no2_decay')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) thet(temp_no2_decay)
         endif

      else if (
     &        cstring1 .eq. 'temp_orgp_decay' .or.
     &        cstring2 .eq. 'temp_orgp_decay') then
         if(cstring1.eq.'temp_orgp_decay')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) thet(temp_orgp_decay)
         elseif(cstring2.eq.'temp_orgp_decay')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) thet(temp_orgp_decay)
         endif

      else if (
     &        cstring1 .eq. 'temp_orgp_set' .or.
     &        cstring2 .eq. 'temp_orgp_set') then
         if(cstring1.eq.'temp_orgp_set')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) thet(temp_orgp_set)
         elseif(cstring2.eq.'temp_orgp_set')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) thet(temp_orgp_set)
         endif

      else if (
     &        cstring1 .eq. 'temp_po4_ben' .or.
     &        cstring2 .eq. 'temp_po4_ben') then
         if(cstring1.eq.'temp_po4_ben')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) thet(temp_po4_ben)
         elseif(cstring2.eq.'temp_po4_ben')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) thet(temp_po4_ben)
         endif

      else if (
     &        cstring1 .eq. 'temp_alg_grow' .or.
     &        cstring2 .eq. 'temp_alg_grow') then
         if(cstring1.eq.'temp_alg_grow')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) thet(temp_alg_grow)
         elseif(cstring2.eq.'temp_alg_grow')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) thet(temp_alg_grow)
         endif

      else if (
     &        cstring1 .eq. 'temp_alg_resp' .or.
     &        cstring2 .eq. 'temp_alg_resp') then
         if(cstring1.eq.'temp_alg_resp')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) thet(temp_alg_resp)
         elseif(cstring2.eq.'temp_alg_resp')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) thet(temp_alg_resp)
         endif

      else if (
     &        cstring1 .eq. 'temp_alg_set' .or.
     &        cstring2 .eq. 'temp_alg_set') then
         if(cstring1.eq.'temp_alg_set')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) thet(temp_alg_set)
         elseif(cstring2.eq.'temp_alg_set')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) thet(temp_alg_set)
         endif

      else if (
     &        cstring1 .eq. 'temp_alg_die' .or.
     &        cstring2 .eq. 'temp_alg_die') then
         if(cstring1.eq.'temp_alg_die')then
            field_names(1)=cstring1
            read(cstring2,'(f8.0)', err=810) thet(temp_alg_die)
         elseif(cstring2.eq.'temp_alg_die')then
            field_names(1)=cstring2
            read(cstring1,'(f8.0)', err=810) thet(temp_alg_die)
         endif

      else if (
     &        cstring1 .eq. 'display_intvl' .or.
     &        cstring2 .eq. 'display_intvl') then
         if (cstring1 .eq. 'display_intvl') then
            display_intvl=cstring2
         else
            display_intvl=cstring1
         endif

      else if (
     &        cstring1 .eq. 'deltax' .or.
     &        cstring2 .eq. 'deltax') then
c--------keyword 'length' means use channel length for each delta x
         if (cstring1 .eq. 'deltax') then
            if (index(cstring2, 'len') .eq. 0) then
               field_names(1)=cstring1
               read(cstring2,'(f10.0)', err=810) deltax_requested
            else
               deltax_requested=0.0
            endif
         else
            if (index(cstring1, 'len') .eq. 0) then
               field_names(1)=cstring2
               read(cstring1,'(f10.0)', err=810) deltax_requested
            else
               deltax_requested=0.0
            endif
         endif

      else if (
     &        cstring1 .eq. 'levee_slope' .or.
     &        cstring2 .eq. 'levee_slope') then
         if (cstring1 .eq. 'levee_slope') then
            field_names(1)=cstring1
            read(cstring2,'(f10.0)', err=810) levee_slope
         else
            field_names(1)=cstring2
            read(cstring1,'(f10.0)', err=810) levee_slope
         endif

      else if (
     &        cstring1 .eq. 'theta' .or.
     &        cstring2 .eq. 'theta') then
         if (cstring1 .eq. 'theta') then
            field_names(1)=cstring1
            read(cstring2,'(f10.0)', err=810) theta
         else
            field_names(1)=cstring2
            read(cstring1,'(f10.0)', err=810) theta
         endif

         if (
     &        theta .lt. 0.5 .or.
     &        theta .gt. 1.0
     &        ) then
            write(unit_error, 615) theta
            istat=-1
            goto 900
         endif

      else if (
     &        cstring1 .eq. 'terms' .or.
     &        cstring2 .eq. 'terms') then
         if (cstring1 .eq. 'terms') then
            ctmp=cstring2
         else
            ctmp=cstring1
         endif
         terms=0
         if (ctmp(1:3) .eq. 'dyn') then
            terms=1             ! dynamic wave
         else if (ctmp(1:3) .eq. 'dif') then
            terms=2             ! diffusion wave
         else if (ctmp(1:3) .eq. 'kin') then
            terms=3             ! kinematic wave
         else
            write(unit_error, 620)
     &           'Unrecognized value for solution method:',
     &           ctmp(:lnblnk(ctmp)),
     &           'Should be dynamic, diffusion, or kinematic.'
            istat=-1
            goto 900
         endif
      else if (
     &        cstring1 .eq. 'vardensity' .or.
     &        cstring2 .eq. 'vardensity') then
         if (cstring1 .eq. 'vardensity') then
            field_names(1)=cstring1
            read(cstring2,'(l2)', err=810) variabledensity
         else
            field_names(1)=cstring2
            read(cstring1,'(l2)', err=810) variabledensity
         endif
         if (variabledensity .and. terms .ne. 1) then
            variabledensity=.false.
            write(unit_error, 630)
     &           'Warning: Variable Density allowed only with dynamic wave.'
         endif
      else if (
     &        cstring1 .eq. 'varsinuosity' .or.
     &        cstring2 .eq. 'varsinuosity') then
         if (cstring1 .eq. 'varsinuosity') then
            field_names(1)=cstring1
            read(cstring2,'(l2)', err=810) variablesinuosity
         else
            field_names(1)=cstring2
            read(cstring1,'(l2)', err=810) variablesinuosity
         endif
         if (variablesinuosity .and. terms .eq. 3) then
            variablesinuosity=.false.
            write(unit_error, 630)
     &           'Warning: variable sinuosity not allowed with kinematic wave.'
         endif
      else if (
     &        cstring1 .eq. 'gravity' .or.
     &        cstring2 .eq. 'gravity') then
         if (cstring1 .eq. 'gravity') then
            field_names(1)=cstring1
            read(cstring2,'(f10.0)', err=810) gravity
         else
            field_names(1)=cstring2
            read(cstring1,'(f10.0)', err=810) gravity
         endif
      else if (
     &        cstring1 .eq. 'toleranceq' .or.
     &        cstring2 .eq. 'toleranceq') then
         if (cstring1 .eq. 'toleranceq') then
            field_names(1)=cstring1
            read(cstring2,'(f10.0)', err=810) toleranceq
         else
            field_names(1)=cstring2
            read(cstring1,'(f10.0)', err=810) toleranceq
         endif
      else if (
     &        cstring1 .eq. 'tolerancez' .or.
     &        cstring2 .eq. 'tolerancez') then
         if (cstring1 .eq. 'tolerancez') then
            field_names(1)=cstring1
            read(cstring2,'(f10.0)', err=810) tolerancez
         else
            field_names(1)=cstring2
            read(cstring1,'(f10.0)', err=810) tolerancez
         endif
      else if (
     &        cstring1 .eq. 'maxiter' .or.
     &        cstring2 .eq. 'maxiter') then
         if (cstring1 .eq. 'maxiter') then
            field_names(1)=cstring1
            read(cstring2,'(i5)', err=810) maxiterations
         else
            field_names(1)=cstring2
            read(cstring1,'(i5)', err=810) maxiterations
         endif
      else if (
     &        cstring1 .eq. 'luinc' .or.
     &        cstring2 .eq. 'luinc') then
         if (cstring1 .eq. 'luinc') then
            field_names(1)=cstring1
            read(cstring2,'(i5)', err=810) luinc
         else
            field_names(1)=cstring2
            read(cstring1,'(i5)', err=810) luinc
         endif
      else if (
     &        cstring1 .eq. 'repeating_tide' .or.
     &        cstring2 .eq. 'repeating_tide') then
         if (cstring1 .eq. 'repeating_tide') then
            field_names(1)=cstring1
            read(cstring2,'(l2)', err=810) repeating_tide
         else
            field_names(1)=cstring2
            read(cstring1,'(l2)', err=810) repeating_tide
         endif
      else if (
     &        cstring1 .eq. 'warmup_run' .or.
     &        cstring2 .eq. 'warmup_run') then
         if (cstring1 .eq. 'warmup_run') then
            field_names(1)=cstring1
            read(cstring2,'(l2)', err=810) warmup_run
         else
            field_names(1)=cstring2
            read(cstring1,'(l2)', err=810) warmup_run
         endif
      else if (
     &        cstring1 .eq. 'max_tides' .or.
     &        cstring2 .eq. 'max_tides') then
         if (cstring1 .eq. 'max_tides') then
            field_names(1)=cstring1
            read(cstring2,'(i5)', err=810) max_tides
         else
            field_names(1)=cstring2
            read(cstring1,'(i5)', err=810) max_tides
         endif
      else if (
     &        cstring1 .eq. 'tide_length' .or.
     &        cstring2 .eq. 'tide_length') then
         if (cstring1 .eq. 'tide_length') then
            tide_cycle_length=cstring2
         else
            tide_cycle_length=cstring1
         endif
      else if (
     &        cstring1 .eq. 'toler_stage' .or.
     &        cstring2 .eq. 'toler_stage') then
         if (cstring1 .eq. 'toler_stage') then
            field_names(1)=cstring1
            read(cstring2,'(f10.0)', err=810) repeat_stage_tol
         else
            field_names(1)=cstring2
            read(cstring1,'(f10.0)', err=810) repeat_stage_tol
         endif
      else if (
     &        cstring1 .eq. 'toler_flow' .or.
     &        cstring2 .eq. 'toler_flow') then
         if (cstring1 .eq. 'toler_flow') then
            field_names(1)=cstring1
            read(cstring2,'(f10.0)', err=810) repeat_flow_tol
         else
            field_names(1)=cstring2
            read(cstring1,'(f10.0)', err=810) repeat_flow_tol
         endif
      else if (
     &        cstring1 .eq. 'printlevel' .or.
     &        cstring2 .eq. 'printlevel') then
         if (cstring1 .eq. 'printlevel') then
            field_names(1)=cstring1
            read(cstring2,'(i5)', err=810) print_level
         else
            field_names(1)=cstring2
            read(cstring1,'(i5)', err=810) print_level
         endif
      else if (
     &        cstring1 .eq. 'temp_dir' .or.
     &        cstring2 .eq. 'temp_dir') then
         if (cstring1 .eq. 'temp_dir') then
            temp_dir=cstring2
         else
            temp_dir=cstring1
         endif
      else if (
     &        cstring1 .eq. 'checkdata' .or.
     &        cstring2 .eq. 'checkdata') then
         if (cstring1 .eq. 'checkdata') then
            field_names(1)=cstring1
            read(cstring2,'(l2)', err=810) check_input_data
         else
            field_names(1)=cstring2
            read(cstring1,'(l2)', err=810) check_input_data
         endif
      else if (
     &        cstring1 .eq. 'cont_missing' .or.
     &        cstring2 .eq. 'cont_missing') then
         if (cstring1 .eq. 'cont_missing') then
            field_names(1)=cstring1
            read(cstring2,'(l2)', err=810) cont_missing
         else
            field_names(1)=cstring2
            read(cstring1,'(l2)', err=810) cont_missing
         endif
      else if (
     &        cstring1 .eq. 'cont_unchecked' .or.
     &        cstring2 .eq. 'cont_unchecked') then
         if (cstring1 .eq. 'cont_unchecked') then
            field_names(1)=cstring1
            read(cstring2,'(l2)', err=810) cont_unchecked
         else
            field_names(1)=cstring2
            read(cstring1,'(l2)', err=810) cont_unchecked
         endif
      else if (
     &        cstring1 .eq. 'cont_question' .or.
     &        cstring2 .eq. 'cont_question') then
         if (cstring1 .eq. 'cont_question') then
            field_names(1)=cstring1
            read(cstring2,'(l2)', err=810) cont_question
         else
            field_names(1)=cstring2
            read(cstring1,'(l2)', err=810) cont_question
         endif
      else if (
     &        cstring1 .eq. 'cont_bad' .or.
     &        cstring2 .eq. 'cont_bad') then
         if (cstring1 .eq. 'cont_bad') then
            field_names(1)=cstring1
            read(cstring2,'(l2)', err=810) cont_bad
         else
            field_names(1)=cstring2
            read(cstring1,'(l2)', err=810) cont_bad
         endif
      else if (
     &        cstring1 .eq. 'warn_missing' .or.
     &        cstring2 .eq. 'warn_missing') then
         if (cstring1 .eq. 'warn_missing') then
            field_names(1)=cstring1
            read(cstring2,'(l2)', err=810) warn_missing
         else
            field_names(1)=cstring2
            read(cstring1,'(l2)', err=810) warn_missing
         endif
      else if (
     &        cstring1 .eq. 'warn_unchecked' .or.
     &        cstring2 .eq. 'warn_unchecked') then
         if (cstring1 .eq. 'warn_unchecked') then
            field_names(1)=cstring1
            read(cstring2,'(l2)', err=810) warn_unchecked
         else
            field_names(1)=cstring2
            read(cstring1,'(l2)', err=810) warn_unchecked
         endif
      else if (
     &        cstring1 .eq. 'warn_question' .or.
     &        cstring2 .eq. 'warn_question') then
         if (cstring1 .eq. 'warn_question') then
            field_names(1)=cstring1
            read(cstring2,'(l2)', err=810) warn_question
         else
            field_names(1)=cstring2
            read(cstring1,'(l2)', err=810) warn_question
         endif
      else if (
     &        cstring1 .eq. 'warn_bad' .or.
     &        cstring2 .eq. 'warn_bad') then
         if (cstring1 .eq. 'warn_bad') then
            field_names(1)=cstring1
            read(cstring2,'(l2)', err=810) warn_bad
         else
            field_names(1)=cstring2
            read(cstring1,'(l2)', err=810) warn_bad
         endif
      else if (
     &        cstring1 .eq. 'ptm_ivert' .or.
     &        cstring2 .eq. 'ptm_ivert') then
         ptm_ivert_int=1
         if (cstring1 .eq. 'ptm_ivert') then
            field_names(1)=cstring1
            read(cstring2,'(l2)', err=810) ptm_ivert
         else
            field_names(1)=cstring2
            read(cstring1,'(l2)', err=810) ptm_ivert
         endif
      else if (
     &        cstring1 .eq. 'ptm_itrans' .or.
     &        cstring2 .eq. 'ptm_itrans') then
         ptm_itrans_int=1
         if (cstring1 .eq. 'ptm_itrans') then
            field_names(1)=cstring1
            read(cstring2,'(l2)', err=810) ptm_itrans
         else
            field_names(1)=cstring2
            read(cstring1,'(l2)', err=810) ptm_itrans
         endif
      else if (
     &        cstring1 .eq. 'ptm_iey' .or.
     &        cstring2 .eq. 'ptm_iey') then
         ptm_iey_int=1
         if (cstring1 .eq. 'ptm_iey') then
            field_names(1)=cstring1
            read(cstring2,'(l2)', err=810) ptm_iey
         else
            field_names(1)=cstring2
            read(cstring1,'(l2)', err=810) ptm_iey
         endif
      else if (
     &        cstring1 .eq. 'ptm_iez' .or.
     &        cstring2 .eq. 'ptm_iez') then
         ptm_iez_int=1
         if (cstring1 .eq. 'ptm_iez') then
            field_names(1)=cstring1
            read(cstring2,'(l2)', err=810) ptm_iez
         else
            field_names(1)=cstring2
            read(cstring1,'(l2)', err=810) ptm_iez
         endif
      else if (
     &        cstring1 .eq. 'ptm_flux_percent' .or.
     &        cstring2 .eq. 'ptm_flux_percent') then
         ptm_flux_percent_int=1
         if (cstring1 .eq. 'ptm_flux_percent') then
            field_names(1)=cstring1
            read(cstring2,'(l2)', err=810) ptm_flux_percent
         else
            field_names(1)=cstring2
            read(cstring1,'(l2)', err=810) ptm_flux_percent
         endif
      else if (
     &        cstring1 .eq. 'ptm_group_percent' .or.
     &        cstring2 .eq. 'ptm_group_percent') then
         ptm_group_percent_int=1
         if (cstring1 .eq. 'ptm_group_percent') then
            field_names(1)=cstring1
            read(cstring2,'(l2)', err=810) ptm_group_percent
         else
            field_names(1)=cstring2
            read(cstring1,'(l2)', err=810) ptm_group_percent
         endif
      else if (
     &        cstring1 .eq. 'ptm_flux_cumulative' .or.
     &        cstring2 .eq. 'ptm_flux_cumulative') then
         ptm_flux_cumulative_int=1
         if (cstring1 .eq. 'ptm_flux_cumulative') then
            field_names(1)=cstring1
            read(cstring2,'(l2)', err=810) ptm_flux_cumulative
         else
            field_names(1)=cstring2
            read(cstring1,'(l2)', err=810) ptm_flux_cumulative
         endif
      else if (
     &        cstring1 .eq. 'ptm_random_seed' .or.
     &        cstring2 .eq. 'ptm_random_seed') then
         ptm_random_seed_int=1
         if (cstring1 .eq. 'ptm_random_seed') then
            field_names(1)=cstring1
            read(cstring2,'(i5)', err=810) ptm_random_seed
         else
            field_names(1)=cstring2
            read(cstring1,'(i5)', err=810) ptm_random_seed
         endif
      else if (
     &        cstring1 .eq. 'ptm_no_animated' .or.
     &        cstring2 .eq. 'ptm_no_animated') then
         ptm_no_animated_int=1
         if (cstring1 .eq. 'ptm_no_animated') then
            field_names(1)=cstring1
            read(cstring2,'(i5)', err=810) ptm_no_animated
         else
            field_names(1)=cstring2
            read(cstring1,'(i5)', err=810) ptm_no_animated
         endif
      else if (
     &        cstring1 .eq. 'ptm_trans_constant' .or.
     &        cstring2 .eq. 'ptm_trans_constant') then
         ptm_trans_constant_int=1
         if (cstring1 .eq. 'ptm_trans_constant') then
            field_names(1)=cstring1
            read(cstring2,'(f7.4)', err=810) ptm_trans_constant
         else
            field_names(1)=cstring2
            read(cstring1,'(f7.4)', err=810) ptm_trans_constant
         endif
      else if (
     &        cstring1 .eq. 'ptm_vert_constant' .or.
     &        cstring2 .eq. 'ptm_vert_constant') then
         ptm_vert_constant_int=1
         if (cstring1 .eq. 'ptm_vert_constant') then
            field_names(1)=cstring1
            read(cstring2,'(f7.4)', err=810) ptm_vert_constant
         else
            field_names(1)=cstring2
            read(cstring1,'(f7.4)', err=810) ptm_vert_constant
         endif
      else if (
     &        cstring1 .eq. 'ptm_iprof' .or.
     &        cstring2 .eq. 'ptm_iprof') then
         ptm_iprof_int=1
         if (cstring1 .eq. 'ptm_iprof') then
            field_names(1)=cstring1
            read(cstring2,'(l2)', err=810) ptm_iprof
         else
            field_names(1)=cstring2
            read(cstring1,'(l2)', err=810) ptm_iprof
         endif
      else if (
     &        cstring1 .eq. 'ptm_trans_a_coef' .or.
     &        cstring2 .eq. 'ptm_trans_a_coef') then
         ptm_trans_a_coef_int=1
         if (cstring1 .eq. 'ptm_trans_a_coef') then
            field_names(1)=cstring1
            read(cstring2,'(f7.4)', err=810) ptm_trans_a_coef
         else
            field_names(1)=cstring2
            read(cstring1,'(f7.4)', err=810) ptm_trans_a_coef
         endif
      else if (
     &        cstring1 .eq. 'ptm_trans_b_coef' .or.
     &        cstring2 .eq. 'ptm_trans_b_coef') then
         ptm_trans_b_coef_int=1
         if (cstring1 .eq. 'ptm_trans_b_coef') then
            field_names(1)=cstring1
            read(cstring2,'(f7.4)', err=810) ptm_trans_b_coef
         else
            field_names(1)=cstring2
            read(cstring1,'(f7.4)', err=810) ptm_trans_b_coef
         endif
      else if (
     &        cstring1 .eq. 'ptm_trans_c_coef' .or.
     &        cstring2 .eq. 'ptm_trans_c_coef') then
         ptm_trans_c_coef_int=1
         if (cstring1 .eq. 'ptm_trans_c_coef') then
            field_names(1)=cstring1
            read(cstring2,'(f7.4)', err=810) ptm_trans_c_coef
         else
            field_names(1)=cstring2
            read(cstring1,'(f7.4)', err=810) ptm_trans_c_coef
         endif
      else
         print*,cstring1,cstring2
         write(unit_error, 610) line
         istat=-1
         goto 900
      endif

      return

c-----char-to-value conversion errors

 810  continue
      write(unit_error, 620) 'Conversion error on field ' //
     &     field_names(1), cstring1 // '  ' // cstring2

      istat=-2

 900  continue                  ! fatal error

      return
      end

      subroutine input_particle_flux(field_names, mxflds, nfields, nflds,
     &     ifld, rifld, line, ibegf, ilenf, idelmt, istat)

c-----process a character line into data arrays for particle flux counting

      implicit none

      include 'common.f'
      include 'common_ptm.inc'

      logical
     &     ldefault             ! true if values are for defaults
      common /read_fix_l/ ldefault

c-----local variables

      logical
     &     new_object           ! true if a new waterbody object type is being processed

      integer
     &     objtype              ! type of waterbody object
     &     ,mxflds              ! maximum number of fields
     &     ,nfields             ! number of fields in data line (input)
     &     ,nflds               ! number of fields in headers (input)
     &     ,ifld(mxflds)        ! ifld(i)=order header keyword i occurs in file (input)
     &     ,rifld(mxflds)       ! reverse ifld
     &     ,ibegf(mxflds)       ! beginning position of each field in line (input)
     &     ,ilenf(mxflds)       ! length of each field in line (input)
     &     ,idelmt(mxflds)      ! type of delimiter for each field
     &     ,istat               ! conversion status of this line (output)
     &     ,lfldndx             ! array index for line fields
     &     ,objndx              ! array index for object IDs
     &     ,kfldndx             ! array index for field keywords
     &     ,i                   ! array index
     &     ,loc                 ! array location number
     &     ,loccarr             ! function to return array location of string
     &     ,lnblnk              ! intrinsic last non blank function
     &     ,itmp                ! index

      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)

      character
     &     cstring*15           ! string field
     &     ,ctmp*80             ! temporary char variable

      character
     &     input_line*250       ! raw input line
      common /input_lines/ input_line

 610  format(/a)

 620  format(/a
     &     /'Input string is: ',a)
 630  format(/a,i5)

      if (ldefault) then
         noutpaths=0
      else
         if (noutpaths .eq. 0) noutpaths=1
      endif

      lfldndx=1
      kfldndx=1
      pathoutput(noutpaths).object=obj_flux

      do while (lfldndx .le. nfields)
         cstring=' '
         cstring=line(ibegf(lfldndx):ibegf(lfldndx)+ilenf(lfldndx)-1)

         if (rifld(kfldndx) .eq. ptm_interval) then
            call split_epart(cstring,itmp,ctmp)
            if (itmp .ne. miss_val_i) then ! valid interval, parse it
               pathoutput(noutpaths).no_intervals=itmp
               pathoutput(noutpaths).interval=ctmp
            else
               write(unit_error,610)
     &              'Unknown input interval: ' // cstring
               istat=-1
               goto 900
            endif
         else if (rifld(kfldndx) .eq. ptm_filename) then
            pathoutput(noutpaths).filename=
     &           input_line(ibegf(lfldndx):ibegf(lfldndx)+ilenf(lfldndx)-1) ! use raw input to preserve case
            if (index(pathoutput(noutpaths).filename, '.dss') .gt. 0) then
c--------------accumulate unique dss output filenames
               itmp=loccarr(pathoutput(noutpaths).filename,outfilenames
     &              ,max_dssoutfiles, EXACT_MATCH)
               if (itmp .lt. 0) then
                  if (abs(itmp) .le. max_dssoutfiles) then
                     outfilenames(abs(itmp))=pathoutput(noutpaths).filename
                     pathoutput(noutpaths).ndx_file=abs(itmp)
                  else
                     write(unit_error,610)
     &                    'Maximum number of unique DSS output files exceeded'
                     goto 900
                  endif
               else
                  pathoutput(noutpaths).ndx_file=itmp
               endif
            endif
         else if (rifld(kfldndx) .eq. ptm_modifier) then
            if (cstring(1:4) .eq. 'none') then
               pathoutput(noutpaths).modifier=' '
            else
               pathoutput(noutpaths).modifier=cstring
            endif
         else if (rifld(kfldndx) .eq. b_part) then
            pathoutput(noutpaths).b_part=cstring
c-----------the fields ptm_from_wb and ptm_to_wb must be delimited
c-----------with 'delimiter'; also, object IDs for each object type
c-----------are separated with commas, while different objects are
c-----------separated with spaces--the delimiter type array tells us which
         else if (rifld(kfldndx) .eq. ptm_from_wb) then
            objndx=1
            new_object=.true.
            do while (
     &           cstring .ne. delimiter .and.
     &           lfldndx .le. nfields)
               if (new_object) then ! get object type
c-----------------first field is object type (e.g. chan, reservoir, qext, ...)
                  loc=loccarr(cstring,obj_names(1),obj_null,SUBSTR_MATCH)
                  if (loc .gt. 0) then
                     pathoutput(noutpaths).flux_from(objndx).object=loc
                     objtype=loc ! remember object for case of multiple IDs per object
                  else
 650                 format(/'Unrecognized object name: ',a
     &                    /'Must be one of:'/8(a,' '))
                     write(unit_error, 650) cstring(:lnblnk(cstring)),
     &                    (obj_names(i)(:lnblnk(obj_names(i))),
     &                    i=1,obj_null-1)
                     istat=-1
                     goto 900
                  endif
               else             ! field is not a new object--get object ID or account name
c-----------------subsequent fields for the object (to either the delimiter or
c-----------------space separator), are object IDs (name or number), or account name
c-----------------if object type is channel or node, then parse to number;
c-----------------a reservoir, to name; external or internal flow or stage boundary,
c-----------------use account label and later determine whether object name or account name
                  pathoutput(noutpaths).flux_from(objndx).object=objtype
                  if (objtype .eq. obj_channel .or. objtype .eq. obj_node) then
                     read(cstring,'(i5)', err=810)
     &                    pathoutput(noutpaths).flux_from(objndx).object_no
                  else if (objtype .eq. obj_reservoir) then
                     pathoutput(noutpaths).flux_from(objndx).obj_name=cstring
                  else if (objtype .eq. obj_qext .or.
     &                    objtype .eq. obj_obj2obj .or.
     &                    objtype .eq. obj_stage) then
                     pathoutput(noutpaths).flux_from(objndx).acct_name=cstring
                  endif
               endif
c--------------if last processing was a new object, don't bump the object index
               if (.not. new_object) then
                  objndx=objndx+1
                  if (objndx .gt. max_ft_flux) then
                     write(unit_error,630)
     &                    'Too many particle_flux waterbody objects specified; max allowed is:'
     &                    ,max_ft_flux
                     istat=-1
                     return
                  endif
               endif
               if (idelmt(lfldndx) .eq. 2) then
c-----------------blank type delimiter denotes end of object IDs, and thus
c-----------------a new object type
                  new_object=.true.
               else
                  new_object=.false.
               endif
               lfldndx=lfldndx+1
               cstring=' '
               cstring=line(ibegf(lfldndx):ibegf(lfldndx)+ilenf(lfldndx)-1)
            enddo
         else if (rifld(kfldndx) .eq. ptm_to_wb) then
            objndx=1
            new_object=.true.
            do while (
     &           cstring .ne. delimiter .and.
     &           lfldndx .le. nfields)
               if (new_object) then ! get object type
c-----------------first field is object type (e.g. chan, reservoir, qext, ...)
                  loc=loccarr(cstring,obj_names(1),obj_null,SUBSTR_MATCH)
                  if (loc .gt. 0) then
                     pathoutput(noutpaths).flux_to(objndx).object=loc
                     objtype=loc ! remember object for case of multiple IDs per object
                  else
                     write(unit_error, 650) cstring(:lnblnk(cstring)),
     &                    (obj_names(i)(:lnblnk(obj_names(i))),
     &                    i=1,obj_null-1)
                     istat=-1
                     goto 900
                  endif
               else             ! field is not a new object--get object ID or account name
c-----------------subsequent fields for the object (to either the delimiter or
c-----------------space separator), are object IDs (name or number), or account name
c-----------------if object type is channel or node, then parse to number;
c-----------------a reservoir, to name; external or internal flow or stage boundary,
c-----------------use account label and later determine whether object name or account name
                  pathoutput(noutpaths).flux_to(objndx).object=objtype
                  if (objtype .eq. obj_channel .or. objtype .eq. obj_node) then
                     read(cstring,'(i5)', err=810)
     &                    pathoutput(noutpaths).flux_to(objndx).object_no
                  else if (objtype .eq. obj_reservoir) then
                     pathoutput(noutpaths).flux_to(objndx).obj_name=cstring
                  else if (objtype .eq. obj_qext .or.
     &                    objtype .eq. obj_obj2obj .or.
     &                    objtype .eq. obj_stage) then
                     pathoutput(noutpaths).flux_to(objndx).acct_name=cstring
                  endif
               endif
c--------------if last processing was a new object, don't bump the object index
               if (.not. new_object) then
                  objndx=objndx+1
                  if (objndx .gt. max_ft_flux) then
                     write(unit_error,630)
     &                    'Too many particle_flux waterbody objects specified; max allowed is:'
     &                    ,max_ft_flux
                     istat=-1
                     return
                  endif
               endif
               if (idelmt(lfldndx) .eq. 2) then
c-----------------blank type delimiter denotes end of object IDs, and thus
c-----------------a new object type
                  new_object=.true.
               else
                  new_object=.false.
               endif
               lfldndx=lfldndx+1
               cstring=' '
               cstring=line(ibegf(lfldndx):ibegf(lfldndx)+ilenf(lfldndx)-1)
            enddo
         endif
         lfldndx=lfldndx+1
         kfldndx=kfldndx+1
      enddo

      pathoutput(noutpaths).meas_type='ptm_flux'
      pathoutput(noutpaths).units='percent'
      pathoutput(noutpaths).per_type=per_type_inst_cum

      noutpaths=noutpaths+1
      if (noutpaths .gt. max_outputpaths) then
         write(unit_error,630)
     &        'Too many particle_flux paths specified; max allowed is:'
     &        ,max_outputpaths
         istat=-1
      endif

      return

 810  continue
      write(unit_error, 620) 'Conversion error on field ' //
     &     field_names(rifld(kfldndx)), cstring

      istat=-2

 900  continue

      return
      end

      subroutine input_partno(field_names, mxflds, nfields, nflds, ifld,
     &     rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for
c-----particle injection over time periods

      implicit none

      include 'common.f'
      include 'common_ptm.inc'

      logical
     &     ldefault             ! true if values are for defaults
      common /read_fix_l/ ldefault

c-----local variables

      integer
     &     mxflds               ! maximum number of fields
     &     ,nfields             ! number of fields in data line (input)
     &     ,nflds               ! number of fields in headers (input)
     &     ,ifld(mxflds)        ! ifld(i)=order header keyword i occurs in file (input)
     &     ,rifld(mxflds)       ! reverse ifld
     &     ,ibegf(mxflds)       ! beginning position of each field in line (input)
     &     ,ilenf(mxflds)       ! length of each field in line (input)
     &     ,istat               ! conversion status of this line (output)

      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)

      integer
     &     i                    ! index

      character
     &     cstring*80           ! string field

      data npartno /1/

 610  format(/a)
 620  format(/a
     &     /'Input string is: ',a)
 630  format(/a,i5)

      if (ldefault) then
         npartno=0
      else
         if (npartno .eq. 0) npartno=1
      endif

      i=1
      do while (i .le. nfields)
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
         if (rifld(i) .eq. partno_node) then
            read(cstring,'(i5)', err=810) part_injection(npartno).node
         else if (rifld(i) .eq. partno_nparts) then
            read(cstring,'(i6)', err=810) part_injection(npartno).nparts
         else if (rifld(i) .eq. partno_slength) then
            part_injection(npartno).slength=cstring
         else if (rifld(i) .eq. partno_length) then
            part_injection(npartno).length=cstring
         else if (rifld(i) .eq. partno_sdate) then
            if (index(cstring,'gen') .gt. 0) then
               part_injection(npartno).start_dt=generic_dt
            else
               part_injection(npartno).start_dt(1:9)=cstring(1:9)
            endif
         else if (rifld(i) .eq. partno_stime) then
            if (index(cstring,'gen') .gt. 0) then
               part_injection(npartno).start_dt=generic_dt
            else
               part_injection(npartno).start_dt(11:14)=cstring(1:4)
            endif
         else if (rifld(i) .eq. partno_edate) then
            if (index(cstring,'gen') .gt. 0) then
               part_injection(npartno).end_dt=generic_dt
            else
               part_injection(npartno).end_dt(1:9)=cstring(1:9)
            endif
         else if (rifld(i) .eq. partno_etime) then
            if (index(cstring,'gen') .gt. 0) then
               part_injection(npartno).end_dt=generic_dt
            else
               part_injection(npartno).end_dt(11:14)=cstring(1:4)
            endif
         else if (rifld(i) .eq. partno_type) then
            part_injection(npartno).type=cstring
         endif
         i=i+1
      enddo

      npartno=npartno+1
      if (npartno .gt. max_injection) then
         write(unit_error,630)
     &        'Too many input paths specified; max allowed is:'
     &        ,max_injection
         istat=-1
         goto 900
      endif

      return

c-----char-to-value conversion errors

 810  continue
      write(unit_error, 620) 'Conversion error on field ' //
     &     field_names(rifld(i)), cstring

      istat=-2

 900  continue                  ! fatal error

      return
      end

      subroutine input_group(field_names, mxflds, nfields, nflds, ifld,
     &     rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for
c-----channnels and open water areas contained in groups

      implicit none

      include 'common.f'
      include 'common_ptm.inc'

      logical
     &     ldefault             ! true if values are for defaults
      common /read_fix_l/ ldefault

c-----local variables

      integer
     &     mxflds               ! maximum number of fields
     &     ,nfields             ! number of fields in data line (input)
     &     ,nflds               ! number of fields in headers (input)
     &     ,ifld(mxflds)        ! ifld(i)=order header keyword i occurs in file (input)
     &     ,rifld(mxflds)       ! reverse ifld
     &     ,ibegf(mxflds)       ! beginning position of each field in line (input)
     &     ,ilenf(mxflds)       ! length of each field in line (input)
     &     ,istat               ! conversion status of this line (output)

      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)

      integer
     &     i                    ! index

      character
     &     cstring*80           ! string field

      data nchanres /1/

 610  format(/a)
 620  format(/a
     &     /'Input string is: ',a)
 630  format(/a,i5)

      if (ldefault) then
         nchanres=0
      else
         if (nchanres .eq. 0) nchanres=1
      endif

      i=1
      do while (i .le. nfields)
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)

         if (rifld(i) .eq. group_object) then
            if (
     &           cstring .eq. 'chan' .or.
     &           cstring .eq. 'res'
     &           ) then
               group_areas(nchanres).object=cstring
            else
               write(unit_error,610)
     &              'Unknown group object: ' // cstring
               istat=-1
               goto 900
            endif

         else if (rifld(i) .eq. group_chnlno) then
            read(cstring,'(i5)', err=810) group_areas(nchanres).number
         else if (rifld(i) .eq. group_num) then
            read(cstring,'(i5)', err=810) group_areas(nchanres).group
         endif
         i=i+1
      enddo
      nchanres=nchanres+1
c-----npartno=npartno+1
      if (npartno .gt. max_chanres) then
         write(unit_error,630)
     &        'Too many groups specified; max allowed is:'
     &        ,max_chanres
         istat=-1
         goto 900
      endif

      return

c-----char-to-value conversion errors

 810  continue
      write(unit_error, 620) 'Conversion error on field ' //
     &     field_names(rifld(i)), cstring

      istat=-2

 900  continue                  ! fatal error

      return
      end

      subroutine list_channels(field_names, mxflds, nfields, nflds,
     &     ifld, rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for channel
c-----number sequencing

      implicit none

      include 'common.f'

c-----local variables

      integer
     &     mxflds               ! maximum number of fields
     &     ,nfields             ! number of fields in data line (input)
     &     ,nflds               ! number of fields in headers (input)
     &     ,ifld(mxflds)        ! ifld(i)=order header keyword i occurs in file (input)
     &     ,rifld(mxflds)       ! reverse ifld
     &     ,ibegf(mxflds)       ! beginning position of each field in line (input)
     &     ,ilenf(mxflds)       ! length of each field in line (input)
     &     ,istat               ! conversion status of this line (output)

      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)

      character
     &     cstring*15           ! string field

      data nchan_list /1/

 620  format(/a
     &     /'Input string is: ',a)
 630  format(/a,i5)

      if (nfields .ne. 1) then
         write(unit_error, 630) 'Too many input fields.',nfields
         istat=-1
         goto 900
      endif

      cstring=' '
      cstring=line(ibegf(1):ibegf(1)+ilenf(1)-1)

      read(cstring,'(i5)', err=810) int2ext(nchan_list) ! external to internal chan no.
      ext2int(int2ext(nchan_list))=nchan_list

      nchan_list=nchan_list+1
      if (nchan_list .gt. max_channels) then
         write(unit_error,630)
     &        'Too many channels specified; max allowed is:'
     &        ,max_channels
         istat=-1
         goto 900
      endif

      return

c-----char-to-value conversion errors

 810  continue
      write(unit_error, 620) 'Conversion error on field ' //
     &     field_names(1), cstring

      istat=-2

 900  continue                  ! fatal error

      return
      end

      subroutine input_rate_coeffs(field_names, mxflds, nfields, nflds,
     &     ifld, rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for
c-----channel coefficient info

      implicit none

      include 'common.f'
      include 'common_irreg_geom.f'

c-----local variables

      integer
     &     mxflds               ! maximum number of fields
     &     ,nfields             ! number of fields in data line (input)
     &     ,nflds               ! number of fields in headers (input)
     &     ,ifld(mxflds)        ! ifld(i)=order header keyword i occurs in file (input)
     &     ,rifld(mxflds)       ! reverse ifld
     &     ,ibegf(mxflds)       ! beginning position of each field in line (input)
     &     ,ilenf(mxflds)       ! length of each field in line (input)
     &     ,istat               ! conversion status of this line (output)
     &     ,loccarr             ! function to return array location of string

      character line*(*)        ! line from file (input)
     &     ,field_names(mxflds)*15 ! copy of hdr_form.fld(*)
     &     ,get_substring*200   ! get substring function
     &     ,cnext*128           ! next channel name
     &     ,next_res*128        ! next reservoir name
     &     ,cchan*128           ! channel start and end numbers

      integer
     &     i                    ! index
     &     ,j                   ! index

c-----channel coefficients

      integer
     &     type                 ! coefficient type codes
     &     ,ncc                 ! non-conservative constituent index
     &     ,chan_start
     &     ,chan_end
     &     ,res_num             ! reservoir numbering order in rate coeff. input

      real*8
     &     value

      character
     &     cstring*80           ! string field

      character
     &     input_line*250       ! raw input line
      common /input_lines/ input_line

 610  format(/a)
 620  format(/a
     &     /'Input string is: ',a)
 630  format(/a,i5)

      if(num_res.lt.0) num_res=0

c-----type, constituent, and value fields required for each line;
c-----and either channel or reservoir field, or both

      if (ifld(coeff_type) .eq. 0) then
         write(unit_error, 610) 'No rate type given.'
         istat=-1
         goto 900
      else
         i=ifld(coeff_type)
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
         if (cstring(1:3) .eq. 'dec') then ! decay
            type=decay
         else if (cstring(1:3) .eq. 'set') then ! settling
            type=settle
         else if (cstring(1:3) .eq. 'ben') then ! benthic
            type=benthic
         else if (index(cstring,'gro') .gt. 0) then ! algal growth
            type=alg_grow
         else if (index(cstring,'res') .gt. 0) then ! algal respiration
            type=alg_resp
         else if (index(cstring,'die') .gt. 0) then ! algal mortality
            type=alg_die
         else
            write(unit_error,610)
     &           'Unknown rate coefficient type: ' // cstring
            istat=-1
            goto 900
         endif
      endif

      if (ifld(coeff_const) .eq. 0) then
         write(unit_error, 610) 'No rate constituent given.'
         istat=-1
         goto 900
      else
         i=ifld(coeff_const)
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
         ncc=loccarr(cstring,nonconserve_list,max_constituent,EXACT_MATCH)
         if (ncc .le. 0) then
            write(unit_error,610)
     &           'Unknown constituent type: ' // cstring
            istat=-1
            goto 900
         endif
      endif

      if (ifld(coeff_value) .eq. 0) then
         write(unit_error, 610) 'No rate value given.'
         istat=-1
         goto 900
      else
         i=ifld(coeff_value)
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
         read(cstring,'(f10.0)',err=810) value
      endif

c-----channel and/or reservoir input?

      if (ifld(coeff_chan) .ne. 0) then ! channel input
         i=ifld(coeff_chan)
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
c--------parse for channel numbers of the form: 123-456,789
         cnext=get_substring(cstring,',')
c--------cnext will be either a group (123-456), or a single channel (789)
         do while (cnext .ne. ' ')
            cchan=get_substring(cnext,'-') ! starting channel of group
            read(cchan,'(i5)',err=810) chan_start
c-----------valid channel number?
            if (chan_start .lt. 1 .or. chan_start .gt. max_channels) then
               write(unit_error, 630)
     &              'Channel number in rate coeff. section out of bounds:',chan_start
               istat=-1
               goto 900
            endif
            cchan=get_substring(cnext,'-') ! ending channel of group
            if (cchan .ne. ' ') then ! true group, check validity
               read(cchan,'(i5)',err=810) chan_end
c--------------valid channel number?
               if (chan_end .lt. 1 .or. chan_end .gt. max_channels) then
                  write(unit_error, 630)
     &                 'Channel number in rate coeff. section out of bounds:',chan_end
                  istat=-1
                  goto 900
               endif
            else                ! wasn't a second channel number for group
               chan_end=chan_start
            endif

            if (chan_start .gt. chan_end) then
               write(unit_error,610)
     &              'Channel start number is greater than channel end number.'
               istat=-1
               goto 900
            endif

            do j = chan_start, chan_end
               rcoef_chan(ncc,type,j)=value
            enddo

            cnext=get_substring(cstring,',')
         enddo
      endif

      if (ifld(coeff_res) .ne. 0) then ! reservoir input
         i=ifld(coeff_res)
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
c--------parse for comma-separated reservoir names
         next_res=get_substring(cstring,',')
         do while (next_res .ne. ' ')
            if (next_res .ne. 'none') then ! "none" - no reservoir
c--------------see if information for this reservoir has been given previously
               res_num=loccarr(next_res,coeff_res_name,max_reservoirs,EXACT_MATCH)
               if (res_num .le. 0) then
c-----------------No match was found. i.e. this is a new reservoir.
                  num_res=num_res+1
                  res_num=num_res
               endif
               coeff_res_name(res_num)=next_res
               rcoef_res_temp(ncc,type,res_num)=value
            endif
            next_res=get_substring(cstring,',')
         enddo
      endif

      return

c-----char-to-value conversion errors

 810  continue
      write(unit_error, 620) 'Conversion error on field ' //
     &     field_names(rifld(i)), cstring

      istat=-2

 900  continue                  ! fatal error

      return
      end

