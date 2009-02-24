C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public !<license as published by
C!    the Free Software Foundation, either version 3 of the !<license, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public !<license for more details.

C!    You should have received a copy of the GNU General Public !<license
C!    along with DSM2.  If not, see <http://www.gnu.org/!<licenses/>.
C!</license>

      subroutine input_tidefile(field_names, mxflds, nfields, nflds,
     &     ifld, rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for
c-----tide file info.
      use io_units
      use constants
      use common_tide
      implicit none


      include '../hydrolib/network.inc'

      logical
     &     ldefault             ! true if values are for defaults
      common /read_fix_l/ ldefault

c-----local variables

      logical
     &     binarytf_fn          ! determine if tidefile is HDF5 or binary fortran
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

      return !todo: disabled
c! The optional starting and ending datetimes specify when to use
c! each tidefile; they override the timestamp in the tidefile
c! itself.  If not given, the timestamp in the tidefile
c! will be used for the start datetime, and it will be used to
c! the end of the tidefile or model run. 

c! Keywords used for the starting and ending datetimes can be used to
c! simplify chaining together tidefiles.

c! Start datetime keyword explanation:
c! runtime: start time in tidefiles; if not succesful
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
      tide_files(nintides).start_date=' '
      tide_files(nintides).end_date=' '
      do while (i .le. nfields)
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)

         if (rifld(i) .eq. tide_fname) then
            tide_files(nintides).filename=
     &           input_line(ibegf(i):ibegf(i)+ilenf(i)-1) ! use raw input to preserve case
c-----------determine if binary or HDF5 tidefile
            tide_files(nintides).binarytf=
     &           binarytf_fn(tide_files(nintides).filename)
         else if (rifld(i) .eq. tide_sdate) then ! starting date for this tidefile
            if (index(cstring,'gen') .gt. 0) then
               tide_files(nintides).start_date=generic_date
            else if (index(cstring,'runtime') .gt. 0) then
               tide_files(nintides).start_date=' '
            else if ( (index(cstring,'prev') .gt. 0) .or.
     &              (index(cstring,'last') .gt. 0) ) then
               if (nintides .ne. 1) then
                  tide_files(nintides).start_date='last'
               else             ! can't have 'last' for first tide file
                  write(unit_error, '(a)')
     &                 'Cannot use "last" or "prev" keyword for first tidefile.'
                  istat=-1
                  goto 900
               endif
            else
               if (index(cstring, 'none') .le. 0)
     &              tide_files(nintides).start_date(1:9)=cstring(1:9)
            endif
         else if ( (rifld(i) .eq. tide_stime)) then ! starting time
            if (index(cstring,'gen') .gt. 0) then
               tide_files(nintides).start_date=generic_date
            elseif ( (index(cstring,'prev') .gt. 0) .or.
     &              (index(cstring,'last') .gt. 0) ) then
               if (nintides .ne. 1) then
                  tide_files(nintides).start_date='last'
               else             ! can't have 'last' for first tide file
                  write(unit_error, '(a)')
     &                 'Cannot use "last" keyword for first tidefile.'
                  istat=-1
                  goto 900
               endif
            else
               if (index(cstring, 'none') .le. 0)
     &              tide_files(nintides).start_date(11:14)=cstring(1:4)
            endif
         else if (rifld(i) .eq. tide_edate) then ! ending date for this tidefile
            if (index(cstring,'len') .gt. 0) then
               tide_files(nintides).end_date='length'
            else if (index(cstring,'run') .gt. 0) then
               tide_files(nintides).start_date=' '
            else if (index(cstring, 'none') .le. 0) then
               if (incr_intvl(0,cstring,IGNORE_BOUNDARY) .eq. miss_val_i) then ! not a time length
                  tide_files(nintides).end_date(1:9)=cstring(1:9)
               else             ! is a time length
                  tide_files(nintides).end_date=cstring ! take the full string
               endif
            endif
            if (tide_files(nintides).end_date .eq. ' ') then
               tide_files(nintides).end_date='length'
            endif
         else if (rifld(i) .eq. tide_etime) then ! ending time
            if (index(cstring,'len') .gt. 0) then
               tide_files(nintides).end_date='length'
            else if (index(cstring, 'none') .le. 0) then
               if (incr_intvl(0,cstring,IGNORE_BOUNDARY) .eq. miss_val_i) then ! not a time length
                  tide_files(nintides).end_date(11:14)=cstring(1:4)
               else             ! is a time length
                  tide_files(nintides).end_date=cstring ! take the full string
               endif
            endif
            if (tide_files(nintides).end_date .eq. ' ') then
               tide_files(nintides).end_date='length'
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


      subroutine input_outputpath(field_names, mxflds, nfields, nflds,
     &     ifld, rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for
c-----print out info: names and type of data to print
      Use IO_Units
	Use Groups, only : GROUP_ALL
	use iopath_data
	use gates, only : gateArray,deviceIndex
      implicit none
      
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
     &     ,ext2int             ! function converting ext chan number to internal
     &     ,ext2intnode         ! function converting ext node number to internal
     &     ,name_to_objno       ! function converting an object name to object number
     &     ,gateno,devno

      
      character
     &     cstring*80           ! string field
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
      endif

      noutpaths=noutpaths+1
      if (noutpaths .gt. max_outputpaths) then
         write(unit_error,630)
     &        'Too many pathoutput paths specified; max allowed is:'
     &        ,max_outputpaths
         istat=-1
         goto 900
      endif
c-----default source group is from all sources
      pathoutput(noutpaths).source_group_ndx=GROUP_ALL
      gateno=-901
      i=1
      do while (i .le. nfields)
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)

         if (rifld(i) .eq. outpath_name) then
            pathoutput(noutpaths).name=cstring
            pathoutput(noutpaths).obj_name=cstring
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
            read(cstring,'(i5)',err=810) pathoutput(noutpaths).obj_no
            pathoutput(noutpaths).obj_no=ext2int(pathoutput(noutpaths).obj_no)
            pathoutput(noutpaths).obj_type=obj_channel
            if (pathoutput(noutpaths).obj_no .le. 0) then
               write(unit_error, 630)
     &              'Invalid output channel number given:',
     &              pathoutput(noutpaths).obj_no
               istat=-1
               goto 900
            endif
         else if (rifld(i) .eq. outpath_dist) then
            pathoutput(noutpaths).obj_type=obj_channel
            if (index(cstring,'len') .gt. 0) then
               pathoutput(noutpaths).chan_dist=chan_length
            else
               read(cstring,'(i10)',err=810) pathoutput(noutpaths).chan_dist
            endif
         else if (rifld(i) .eq. outpath_node) then
            read(cstring,'(i5)',err=810) pathoutput(noutpaths).obj_no
            pathoutput(noutpaths).obj_no=ext2intnode(pathoutput(noutpaths).obj_no)
            pathoutput(noutpaths).obj_type=obj_node
         else if (rifld(i) .eq. outpath_res_name) then
            pathoutput(noutpaths).obj_type=obj_reservoir
            pathoutput(noutpaths).obj_name=cstring
         else if (rifld(i) .eq. outpath_res_node) then
            pathoutput(noutpaths).obj_type=obj_reservoir
            if (cstring .ne. 'none') then
               read(cstring,'(i5)',err=810) pathoutput(noutpaths).res_node_no
               pathoutput(noutpaths).res_node_no=ext2intnode(pathoutput(noutpaths).res_node_no) 
            else
               pathoutput(noutpaths).res_node_no=0
            endif
         else if (rifld(i) .eq. outpath_gate) then
            pathoutput(noutpaths).obj_type=obj_gate
            pathoutput(noutpaths).obj_name=cstring
	      gateno = name_to_objno(obj_gate, cstring)
            if (gateno .eq. miss_val_i)then
               write(unit_error,*)"Unknown gate (not allowed for text input)" // cstring
	         istat = -1
	         goto 900
            end if
            pathoutput(noutpaths).obj_no= gateno
         else if (rifld(i) .eq. outpath_gate_device) then
            ! todo: trap possibility that gate is read after device
            pathoutput(noutpaths).obj_type=obj_gate
            if (trim(cstring) .ne. 'none') then
               pathoutput(noutpaths).gate_device 
     &                 = deviceIndex(gateArray(gateno),cstring)   
            else
               pathoutput(noutpaths).gate_device = miss_val_i
            endif
         else if (rifld(i) .eq. outpath_type .or.
     &            rifld(i) .eq. outpath_variable ) then
            pathoutput(noutpaths).meas_type=cstring
            if (index(cstring, 'flow') .gt. 0 .or.
     &           index(cstring, 'pump') .gt. 0) then
               pathoutput(noutpaths).units='cfs'
            else if (cstring(1:3) .eq. 'vel') then
               pathoutput(noutpaths).meas_type='vel'
               pathoutput(noutpaths).units='ft/s'
            else if (cstring .eq. 'stage') then
               pathoutput(noutpaths).units='feet'
            else if (cstring .eq. 'position') then
               pathoutput(noutpaths).units='ft'
            else if (cstring .eq. 'elev') then
               pathoutput(noutpaths).units='ft'
            else if (cstring .eq. 'height') then
               pathoutput(noutpaths).units='ft'
            else if (cstring .eq. 'install') then
               pathoutput(noutpaths).units='boolean'
            else if (cstring .eq. 'width') then
               pathoutput(noutpaths).units='ft'               
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
         else if (rifld(i) .eq. outpath_from_name .or.
     &	        rifld(i) .eq. outpath_source_group  ) then
            pathoutput(noutpaths).source_group_ndx=name_to_objno(obj_group,cstring)
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
            if (index(cstring,'av') .ne. 0)
     &           pathoutput(noutpaths).per_type=per_type_per_aver
            if (index(cstring,'min') .ne. 0)
     &           pathoutput(noutpaths).per_type=per_type_per_min
            if (index(cstring,'max') .ne. 0)
     &           pathoutput(noutpaths).per_type=per_type_per_max
         else if (rifld(i) .eq. outpath_modifier) then
            if (cstring(1:4) .eq. 'none') then
               pathoutput(noutpaths).modifier=' '
            else
               pathoutput(noutpaths).modifier=cstring
            endif
         endif
         pathoutput(noutpaths).use=.true.
         i=i+1
      enddo

      return

c-----char-to-value conversion errors

 810  continue
      write(unit_error, 620) 'Conversion error on field ' //
     &     field_names(rifld(i)), cstring

      istat=-2

 900  continue               ! fatal error

      return
      end

      subroutine input_iofiles(field_names, mxflds, nfields, nflds, ifld,
     &     rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for
c-----output file names
      Use IO_Units
      use iopath_data
      use constants_ptm
      use common_ptm
      implicit none

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


      return !todo disabled
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
      else if (cstring2(1:3) .eq. 'hdf') then
         i2=io_hdf5
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

      subroutine input_quadrature(field_names, mxflds, nfields, nflds,
     &     ifld, rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for
c-----quadrature integration info
      use io_units
      use constants
      use grid_data
      implicit none

      logical
     &     ldefault             ! true if values are for defaults
      common /read_fix_l/ ldefault

      include '../hydrolib/network.inc'
      include '../hydrolib/netcntrl.inc'

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

!todo: is the initialization of nquadpts OK?
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
      use io_units
      use iopath_data
      use constants
      use envvar
      implicit none

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
     &     ,i,j                 ! array indices

      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)
      character*130 new_name

 610  format(/a)
 630  format(/a,i3)
      return  ! todo disabled
c-----name required for each line; empty value indicates erase it
      if (ifld(envvar_name) .eq. 0) then
         write(unit_error, 610) 'No environment variable name given.'
         istat=-1
         goto 900
      endif
      i=ifld(envvar_name)
      new_name = input_line(ibegf(ifld(i)):ibegf(ifld(i)) +
     &     ilenf(ifld(i))-1)
      do j = 1, nenvvars
         if(new_name .eq. envvars(j).name) then
            i=ifld(envvar_value)
	    envvars(j).value = input_line(ibegf(ifld(i)):ibegf(ifld(i)) +
     &           ilenf(ifld(i))-1)
	    return
         endif
      enddo

      envvars(nenvvars).name=new_name
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

      use PhysicalConstants
      use IO_Units
      use logging      
      use constants
      use runtime_data
      use iopath_data
      use common_qual
      use common_ptm
      use envvar   !kc To match process_scalar_SQL
      use grid_data

!c-----process a character line into data arrays for scalar info

      implicit none

      include '../hydrolib/network.inc'
      include '../hydrolib/netcntrl.inc'
      include '../hydrolib/chconnec.inc'

      logical
     &     ldefault             ! true if values are for defaults
      common /read_fix_l/ ldefault

!c-----local variables

      integer
     &     mxflds               ! maximum number of fields
     &     ,nfields             ! number of fields in data line (input)
     &     ,nflds               ! number of fields in headers (input)
     &     ,ifld(mxflds)        ! ifld(i)=order header keyword i occurs in file (input)
     &     ,rifld(mxflds)       ! reverse ifld
     &     ,ibegf(mxflds)       ! beginning position of each field in line (input)
     &     ,ilenf(mxflds)       ! length of each field in line (input)
     &     ,istat               ! conversion status of this line (output)
      
      integer                  :: itmp   !kc

      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)

      character
     &     Param*48          ! string field for keyword or value
     &     ,Value*48         ! string field for keyword or value
     &     ,ctmp*48             ! scratch character variable



!c-----defaults

      data
     &     variabledensity /.false./
     &     ,variablesinuosity /.false./
     &     ,theta /0.6/
     &     ,maxiterations /50/
     &     ,luinc /1/
     &     ,toleranceq /0.0005/
     &     ,tolerancez /0.0005/


	if (nfields .ne. 2) return  ! must have two fields
      Param=line(ibegf(1):ibegf(1)+ilenf(1)-1)
      Value=line(ibegf(2):ibegf(2)+ilenf(2)-1)
      
      !todo disabled
      !call process_scalar(Param, Value, istat)


      
      end subroutine

      subroutine input_particle_flux(field_names, mxflds, nfields, nflds,
     &     ifld, rifld, line, ibegf, ilenf, idelmt, istat)

c-----process a character line into data arrays for particle flux counting
      use IO_Units
	use Groups, only: GROUP_ANY_INDEX
	use iopath_data
      use common_ptm
      use constants_ptm
      implicit none


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
     &     ,itmp                ! index

      integer,external :: name_to_objno,obj_type_code


      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)

      character
     &     cstring*40           ! string field
     &     ,ctmp*80             ! temporary char variable
     &     ,objtmp*32

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
         noutpaths=noutpaths+1
      endif

      lfldndx=1
      kfldndx=1
      pathoutput(noutpaths).obj_type=obj_flux

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
	      
            objtmp=' '
	      objtmp=cstring(1:(index(cstring,":")-1))

	      pathoutput(noutpaths).flux_from_type
     &              =obj_type_code(objtmp)
            objtmp=' '
	      objtmp=cstring((index(cstring,":")+1):len_trim(cstring))
            if(trim(objtmp) .eq. 'all' .and. 
     &        pathoutput(noutpaths).flux_from_type .ne. obj_group)then
	         pathoutput(noutpaths).flux_from_ndx=GROUP_ANY_INDEX
            else    
		     pathoutput(noutpaths).flux_from_ndx=name_to_objno(
     &              pathoutput(noutpaths).flux_from_type,objtmp)
	      end if
            if( pathoutput(noutpaths).flux_from_ndx .eq. miss_val_i)then
	         write(unit_error, 650)trim(cstring)
 650                 format(/'Unrecognized object name: ',a)
               istat=-1
	         goto 900
	      end if
         else if (rifld(kfldndx) .eq. ptm_to_wb) then
	      objtmp=' '
		  objtmp=cstring(1:(index(cstring,":")-1))
	      pathoutput(noutpaths).flux_to_type
     &              =obj_type_code(objtmp)
	      objtmp=' '
            objtmp=cstring((index(cstring,":")+1):len_trim(cstring))
            if(trim(objtmp) .eq. 'all' .and. 
     &        pathoutput(noutpaths).flux_to_type .ne. obj_group)then
	         pathoutput(noutpaths).flux_to_ndx=GROUP_ANY_INDEX

            else    

	         pathoutput(noutpaths).flux_to_ndx=name_to_objno(
     &              pathoutput(noutpaths).flux_to_type,objtmp)
	      end if

            if( pathoutput(noutpaths).flux_to_ndx .eq. miss_val_i)then
	         write(unit_error, 650)trim(cstring)
               istat=-1
	         goto 900
	      end if

            objndx=1
         endif
         lfldndx=lfldndx+1
         kfldndx=kfldndx+1
      enddo

      pathoutput(noutpaths).meas_type='ptm_flux'
      pathoutput(noutpaths).units='percent'
      pathoutput(noutpaths).per_type=per_type_inst_cum


c      noutpaths=noutpaths+1
      if (noutpaths .gt. max_outputpaths) then
      write(unit_error,630)
     &             'Too many particle_flux paths specified; max allowed is:'
     &             ,max_outputpaths
              istat=-1
      endif

      return

 810  continue
      write(unit_error, 620) 'Conversion error on field ' //
     &          field_names(rifld(kfldndx)), cstring

       istat=-2

 900   continue

      return
      end

      subroutine input_group_output(field_names, mxflds, nfields, nflds,
     &     ifld, rifld, line, ibegf, ilenf, idelmt, istat)
 
c-----process a character line into data arrays for particle group output
      use IO_Units
      use iopath_data
      use common_ptm
      use constants_ptm
      !use ptm_local   !todo: why is ptm_local leaking into common?
      implicit none

 
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
     &     ,idelmt(mxflds)      ! type of delimiter for each field
     &     ,istat               ! conversion status of this line (output)
     &     ,i                   ! array index
     &     ,loc                 ! array location number
     &     ,loccarr             ! function to return array location of string
     &     ,itmp                ! index
 
      integer,external :: name_to_objno
      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)
 
      character
     &     cstring*32           ! string field
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
         noutpaths=noutpaths+1
      endif

      i=1
	ptm_igroup=.true.  ! fixme: what does this do?
 
      do while (i .le. nfields)
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)
 
         if (rifld(i) .eq. ptm_interval) then
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
         else if (rifld(i) .eq. ptm_filename) then
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
         else if (rifld(i) .eq. b_part) then
            pathoutput(noutpaths).b_part=cstring
         else if (rifld(i) .eq. ptm_group) then
	       pathoutput(noutpaths).obj_no=name_to_objno(obj_group,cstring)
	       if(pathoutput(noutpaths).obj_no .eq. miss_val_i)then
	          write(unit_error,*)"Unrecognized group name for group output spec: " 
     &             // trim(cstring)
	          goto 900
	        end if
         endif
         i=i+1
      enddo

      pathoutput(noutpaths).meas_type='ptm_group'
      pathoutput(noutpaths).units='percent'
      pathoutput(noutpaths).per_type=per_type_inst_cum

	ngroup_outputs=ngroup_outputs+1

      if (noutpaths .gt. max_outputpaths) then
         write(unit_error,630)
     &        'Too many group output paths specified; max allowed is:'
     &        ,max_outputpaths
         istat=-1
      endif

      return
 
 810  continue
      write(unit_error, 620) 'Conversion error on field ' //
     &     field_names(rifld(i)), cstring
 
      istat=-2
 
 900  continue
 
      return
      end



      subroutine input_partno(field_names, mxflds, nfields, nflds, ifld,
     &     rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for
c-----particle injection over time periods
      use IO_Units
      use constants_ptm
      use common_ptm
      implicit none

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
            part_injection(npartno).start_date(1:9)=cstring(1:9)
         else if (rifld(i) .eq. partno_stime) then
               part_injection(npartno).start_date(11:14)=cstring(1:4)
         else if (rifld(i) .eq. partno_edate) then
               part_injection(npartno).end_date(1:9)=cstring(1:9)
         else if (rifld(i) .eq. partno_etime) then
               part_injection(npartno).end_date(11:14)=cstring(1:4)
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


      subroutine input_groups(field_names, mxflds, nfields, nflds, ifld,
     &     rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for
c-----channnels and open water areas contained in groups
      use io_units
	use groups
      use constants
      use constants_ptm
	
      implicit none

      

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
      Type(GroupMember), pointer :: newmembers(:)

      integer
     &     i                    ! index
     &     ,alloc_stat
     &     ,groupno
     &     ,objtype
     &     ,npattern  

	integer, external :: name_to_objno,obj_type_code

      character
     &     cstring*80           ! string field
     &    ,groupname*32         ! name of group
     &    ,pattern*100          !pattern for matching the identifier of objects


 610  format(/a)
 620  format(/a
     &     /'Input string is: ',a)
 630  format(/a,i5)

      if (ldefault) then
         ngroup=0
      endif

      i=1
      do while (i .le. nfields)
         cstring=' '
         cstring=line(ibegf(i):ibegf(i)+ilenf(i)-1)

         if (rifld(i) .eq. group_name) then
	      groupname=' '
	      groupname=trim(cstring(1:32))
	      ! Ensure group exists, using pseudo-id
	      call process_group(groupname,miss_val_i)
         else if (rifld(i) .eq. group_memtype) then
	      objtype=obj_type_code(cstring)
         else if (rifld(i) .eq. group_memid) then
	      pattern=' '
	      pattern=trim(cstring)
         endif
         i=i+1
      enddo
      call process_group_member(groupname,objtype,pattern)
      return

c-----char-to-value conversion errors

 810  continue
      write(unit_error, 620) 'Conversion error on field ' //
     &     field_names(rifld(i)), cstring

      istat=-2

 900  continue                  ! fatal error

      return
      end



      subroutine input_rate_coeffs(field_names, mxflds, nfields, nflds,
     &     ifld, rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for
c-----channel coefficient info
      use IO_Units
      use common_qual
      use constants
      use grid_data
      use common_xsect
      implicit none



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
               !res_num=name_to_objno(obj_reservoir,name)
               res_num=loccarr(next_res,
     &                         coeff_res_name,
     &                         max_reservoirs,
     &                         EXACT_MATCH)
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

