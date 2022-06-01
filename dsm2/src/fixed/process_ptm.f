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

      subroutine process_particle_flux_output(name, from_wb, to_wb,interval,filename)
c-----process a character line into data arrays for particle group output
      use IO_Units
      use iopath_data
      use common_ptm
      use constants
      use constants_ptm
      use groups
      use utilities, only: loccarr
      !use ptm_local   !todo: why is ptm_local leaking into common?
      implicit none
      character*40  from_wb, to_wb
      character*32  objtmp
      character*32  name
      character*16  interval
      character*128 filename
      character*392 ctmp
      integer itmp
      integer, external :: name_to_objno
      integer, external :: obj_type_code
      call locase(name)
      call locase(from_wb)
      call locase(to_wb)
      call locase(interval)

      noutpaths=noutpaths+1
      if (noutpaths .gt. max_outputpaths) then
         write(unit_error,"(a,i)")
     &        'Too many group output paths specified; max allowed is:'
     &        ,max_outputpaths
         call exit(1)
      endif
      pathoutput(noutpaths).obj_type=obj_flux
      ! process from object
      objtmp=from_wb(1:(index(from_wb,":")-1))
      pathoutput(noutpaths).flux_from_type
     &              =obj_type_code(objtmp)
      objtmp=' '
      objtmp=from_wb((index(from_wb,":")+1):len_trim(from_wb))
      if(trim(objtmp) .eq. 'all' .and.
     &     pathoutput(noutpaths).flux_from_type .ne. obj_group) then
	     pathoutput(noutpaths).flux_from_ndx=GROUP_ANY_INDEX
      else
		   pathoutput(noutpaths).flux_from_ndx=name_to_objno(
     &              pathoutput(noutpaths).flux_from_type,objtmp)
      end if
      if( pathoutput(noutpaths).flux_from_ndx .eq. miss_val_i) then
	    write(unit_error, 650)trim(from_wb)
 650         format(/'Unrecognized object name: ',a)
             call exit(-1)
      end if
	! process to object
      objtmp=to_wb(1:(index(to_wb,":")-1))
      pathoutput(noutpaths).flux_to_type
     &              =obj_type_code(objtmp)
      objtmp=' '
      objtmp=to_wb((index(to_wb,":")+1):len_trim(to_wb))
      if(trim(objtmp) .eq. 'all' .and.
     &     pathoutput(noutpaths).flux_to_type .ne. obj_group) then
	     pathoutput(noutpaths).flux_to_ndx=GROUP_ANY_INDEX
      else
		   pathoutput(noutpaths).flux_to_ndx=name_to_objno(
     &              pathoutput(noutpaths).flux_to_type,objtmp)
      end if
      if( pathoutput(noutpaths).flux_to_ndx .eq. miss_val_i) then
          write(unit_error, 652)trim(to_wb)
 652         format(/'Unrecognized object name: ',a)
             call exit(-1)
      end if

      ! generic output stuff
      pathoutput(noutpaths).a_part=' '
      pathoutput(noutpaths).b_part=name
      pathoutput(noutpaths).c_part="FLUX"
      call split_epart(interval,itmp,ctmp)
      if (itmp .ne. miss_val_i) then ! valid interval, parse it
         pathoutput(noutpaths).e_part=interval
         pathoutput(noutpaths).no_intervals=itmp
         pathoutput(noutpaths).interval=ctmp
      else
         write(unit_error, "('Unknown output time interval: '//a)") Interval
         call exit(-1)
         return
      endif
      pathoutput(noutpaths).f_part=' '
      pathoutput(noutpaths).filename=filename
c-----------accumulate unique dss output filenames
      itmp=loccarr(pathoutput(noutpaths).filename,
     &              outfilenames,
     &              max_dssoutfiles,
     &              EXACT_MATCH)
      if (itmp .lt. 0) then
         if (abs(itmp) .le. max_dssoutfiles) then
            outfilenames(abs(itmp))=pathoutput(noutpaths).filename
            pathoutput(noutpaths).ndx_file=abs(itmp)
         else
            write(unit_error,*)
     &       'Maximum number of unique DSS output files exceeded'
            call exit(-3)
            return
         endif
      else
         pathoutput(noutpaths).ndx_file=itmp
      endif

!     todo: is this fixed later if it isn't percent???
      pathoutput(noutpaths).meas_type='ptm_flux'
      pathoutput(noutpaths).units='percent'
      pathoutput(noutpaths).per_type=per_type_inst_cum
      return
      end subroutine


      subroutine process_particle_group_output(name, groupname,interval,filename)
c-----process a character line into data arrays for particle group output
      use IO_Units
      use iopath_data
      use common_ptm
      use constants_ptm
      use utilities, only: loccarr
      !use ptm_local   !todo: why is ptm_local leaking into common?
      implicit none
      character*40  groupname
      character*32  name
      character*16  interval
      character*128 filename
      character*392 ctmp
      integer itmp
      integer, external :: name_to_objno

      call locase(groupname)
      call locase(name)
      call locase(interval)
      noutpaths=noutpaths+1
      if (noutpaths .gt. max_outputpaths) then
         write(unit_error,"(a,i)")
     &        'Too many group output paths specified; max allowed is:'
     &        ,max_outputpaths
         call exit(1)
      endif
      pathoutput(noutpaths).obj_type=obj_group
	pathoutput(noutpaths).obj_no=name_to_objno(obj_group,groupname)
      pathoutput(noutpaths).flux_group_ndx =pathoutput(noutpaths).obj_no


	if(pathoutput(noutpaths).obj_no .eq. miss_val_i) then
	    write(unit_error,*)"Unrecognized group name for group output spec: "
     &      // trim(groupname)
	    call exit(-3)
	 end if

	ptm_igroup=.true.  ! fixme: what does this do?
      pathoutput(noutpaths).a_part=' '
      pathoutput(noutpaths).b_part=Name
      pathoutput(noutpaths).c_part="ptm_group"
      call split_epart(interval,itmp,ctmp)
      if (itmp .ne. miss_val_i) then ! valid interval, parse it
         pathoutput(noutpaths).e_part=interval
         pathoutput(noutpaths).no_intervals=itmp
         pathoutput(noutpaths).interval=ctmp
      else
         write(unit_error, "('Unknown output time interval: '//a)") Interval
         call exit(-1)
         return
      endif
      pathoutput(noutpaths).f_part=' '
      pathoutput(noutpaths).filename=FileName
c-----------accumulate unique dss output filenames
      itmp=loccarr(pathoutput(noutpaths).filename,
     &              outfilenames,
     &              max_dssoutfiles,
     &              EXACT_MATCH)
      if (itmp .lt. 0) then
         if (abs(itmp) .le. max_dssoutfiles) then
            outfilenames(abs(itmp))=pathoutput(noutpaths).filename
            pathoutput(noutpaths).ndx_file=abs(itmp)
         else
            write(unit_error,*)
     &       'Maximum number of unique DSS output files exceeded'
            call exit(-3)
            return
         endif
      else
         pathoutput(noutpaths).ndx_file=itmp
      endif

!     todo: is this fixed later if it isn't percent???
      pathoutput(noutpaths).meas_type='ptm_group'
      pathoutput(noutpaths).units='percent'
      pathoutput(noutpaths).per_type=per_type_inst_cum

	ngroup_outputs=ngroup_outputs+1

      return
      end subroutine


      subroutine process_particle_injection(node,nparts,delay,duration)
c-----process a character line into data arrays for
c-----particle injection over time periods
      use IO_Units
      use constants_ptm
      use common_ptm
      implicit none
      integer :: node
      integer :: nparts
      character*8 :: delay
      character*16 :: duration
      integer
     &     i                    ! index
      npartno = npartno+1
       if (npartno .gt. max_injection) then
         write(unit_error,"(a,i)")
     &        'Too many input paths specified; max allowed is:'
     &        ,max_injection
         call exit(-1)
      endif

      part_injection(npartno).node = node
      part_injection(npartno).nparts = nparts
      part_injection(npartno).slength=delay
      part_injection(npartno).length=duration
! todo: ask kijin about these
c     part_injection(npartno).start_date
c     part_injection(npartno).end_date
c     part_injection(npartno).type

      return
      end subroutine


      subroutine process_particle_filter(name,node,at_wb,fillin,filename,inpath)
c-----process a character line into data arrays for particle filter (on nodes connecting to channel, boudnary)
      use IO_Units
      use common_ptm
      use constants
      use iopath_data
      use logging
      use utilities, only: loccarr
      implicit none

      character
     &     name*32
     &     ,resname*32
     &     ,at_wb*32
     &     ,fillin*8
     &     ,filename*128
     &     ,inpath*392
      integer node

      character*32  objtmp
      integer, external :: obj_type_code
      integer, external :: name_to_objno
      integer getWaterbodyUniqueId

      character
     &     LocName*32
     &     ,ca*32, cb*32, cc*32, cd*32, ce*32, cf*32
     &     ,ctmp*200

      integer*4
     &     npath,na,nb,nc,nd,ne,nf
     &     ,itmp
     &     ,istat

      integer, external :: ext2intnode
      integer, external :: fillin_code
      real*8 ftmp

      call locase(name)
      call locase(at_wb)
      call locase(fillin)
      call locase(inpath)

      nfilter = nfilter+1
      if (nfilter .gt. max_filter) then
         write(unit_error,"(a,i)")
     &        'Too many input paths specified; max allowed is:'
     &        ,max_filter
         call exit(-1)
      endif

      part_filter(nfilter).ndx = nfilter-1
      part_filter(nfilter).name = trim(name)
      part_filter(nfilter).node = ext2intnode(node)
      part_filter(nfilter).resname = miss_val_c
      part_filter(nfilter).at_wb = at_wb

      ! process filter at_wb object
      objtmp=at_wb(1:(index(at_wb,":")-1))
      part_filter(nfilter).at_wb_type
     &              =obj_type_code(objtmp)
      objtmp=' '
      objtmp=at_wb((index(at_wb,":")+1):len_trim(at_wb))
      part_filter(nfilter).at_wb_ndx=name_to_objno(
     &              part_filter(nfilter).at_wb_type,objtmp)

      if (part_filter(nfilter).at_wb_ndx .eq. miss_val_i) then
         write(unit_error, 650)trim(at_wb)
 650         format(/'Unrecognized object name: ',a)
         call exit(-1)
      end if

      part_filter(nfilter).at_wb_id = getWaterbodyUniqueId
     &          (part_filter(nfilter).at_wb_type,part_filter(nfilter).at_wb_ndx)

c--------------dss timeseries input
      part_filter(nfilter).fillin = fillin
      part_filter(nfilter).filename = filename
      part_filter(nfilter).path = trim(inpath)

      !TODO
      ninpaths=ninpaths+1
      if (ninpaths .gt. max_inputpaths) then
          write(unit_error,630)
     &        'Too many input paths specified; max allowed is:'
     &        ,max_inputpaths
           call exit(-1)
      endif


      pathinput(ninpaths).name=name
      pathinput(ninpaths).useobj=.true.
      write(LocName, '(i)') node
      pathinput(ninpaths).obj_name=LocName
      pathinput(ninpaths).obj_type=obj_node
      pathinput(ninpaths).obj_no=ext2intnode(node)  !part_filter(nfilter).node
      pathinput(ninpaths).variable="part_filter"
      pathinput(ninpaths).sign = 0

      if (FileName(:8) .eq. 'constant' .or.
     &      FileName(:8) .eq. 'CONSTANT') then
          read(InPath, '(1f10.0)') ftmp
          pathinput(ninpaths).constant_value=ftmp
          pathinput(ninpaths).fillin=fill_last
          pathinput(ninpaths).path=trim(InPath)
          pathinput(ninpaths).filename=trim(FileName)
      else
c--------------Break up the input pathname

          pathinput(ninpaths).path=trim(InPath)
          call chrlnb(InPath, npath)
          call zufpn(ca, na, cb, nb, cc, nc, cd, nd, ce, ne,
     &              cf, nf, InPath, npath, istat)
          if (istat .lt. 0) then
              write(unit_error, '(/a/a)')
     &                 'Input TS: Illegal pathname', InPath
              call exit(-1)
          end if

          call split_epart(ce,itmp,ctmp)
          if (itmp .ne. miss_val_i) then ! valid interval, parse it
              pathinput(ninpaths).no_intervals=itmp
              pathinput(ninpaths).interval=ctmp
          else
              write(unit_error,610)
     &                 'Input TS: Unknown input E part or interval: ' // ce
              write(unit_error, '(a)') 'Path: ' // trim(InPath)
              call exit(-1)
          endif
          pathinput(ninpaths).filename=FileName
c--------------accumulate unique dss input filenames
          itmp=loccarr(pathinput(ninpaths).filename,infilenames,
     &              max_dssinfiles, EXACT_MATCH)
          if (itmp .lt. 0) then
              if (abs(itmp) .le. max_dssinfiles) then
                  infilenames(abs(itmp))=pathinput(ninpaths).filename
                  pathinput(ninpaths).ndx_file=abs(itmp)
              else
                  write(unit_error,610)
     &                    'Maximum number of unique DSS input files exceeded'
                  call exit(-3)
               endif
          else
              pathinput(ninpaths).ndx_file=itmp
          endif
          pathinput(ninpaths).fillin=fillin_code(fillin)
      endif

      pathinput(ninpaths).data_type = obj_filter


      if (print_level .ge. 3) then
          write(unit_screen, '(i4,1x,a32,1x,a24,a24)') ninpaths, Name,
     &       trim(InPath(:24)),
     &       trim(FileName(:24))
      end if

 610  format(/a)
 620  format(/a/a)
 630  format(/a,i5)

      return
      end subroutine


      subroutine process_particle_res_filter(name,resname,at_wb,fillin,filename,inpath)
c-----process a character line into data arrays for particle filter (on source flow directly to reservoir)
      use IO_Units
      use common_ptm
      use constants
      use iopath_data
      use logging
      use utilities, only: loccarr
      implicit none

      character
     &     name*32
     &     ,resname*32
     &     ,at_wb*32
     &     ,fillin*8
     &     ,filename*128
     &     ,inpath*392
      integer node

      character*32  objtmp
      integer, external :: obj_type_code
      integer, external :: name_to_objno
      integer getWaterbodyUniqueId

      character
     &     LocName*32
     &     ,ca*32, cb*32, cc*32, cd*32, ce*32, cf*32
     &     ,ctmp*200

      integer*4
     &     npath,na,nb,nc,nd,ne,nf
     &     ,itmp
     &     ,istat

      integer, external :: ext2intnode
      integer, external :: fillin_code
      real*8 ftmp

      call locase(name)
      call locase(at_wb)
      call locase(fillin)
      call locase(inpath)

      nfilter = nfilter+1
      if (nfilter .gt. max_filter) then
         write(unit_error,"(a,i)")
     &        'Too many input paths specified; max allowed is:'
     &        ,max_filter
         call exit(-1)
      endif

      part_filter(nfilter).ndx = nfilter-1
      part_filter(nfilter).name = name
      part_filter(nfilter).node = miss_val_i
      part_filter(nfilter).resname = resname
      part_filter(nfilter).at_wb = at_wb

c--------------process filter at_wb object to internal wb id
      objtmp=at_wb(1:(index(at_wb,":")-1))
      part_filter(nfilter).at_wb_type
     &    =obj_type_code(objtmp)
      objtmp=' '
      objtmp=at_wb((index(at_wb,":")+1):len_trim(at_wb))

      part_filter(nfilter).at_wb_ndx=name_to_objno(
     &    part_filter(nfilter).at_wb_type,objtmp)
      if (part_filter(nfilter).at_wb_ndx .eq. miss_val_i) then
         write(unit_error, 650)trim(at_wb)
 650         format(/'Unrecognized object name: ',a)
         call exit(-1)
      end if

      part_filter(nfilter).at_wb_id = getWaterbodyUniqueId
     &    (part_filter(nfilter).at_wb_type,part_filter(nfilter).at_wb_ndx)

c--------------dss timeseries input
      part_filter(nfilter).fillin = fillin
      part_filter(nfilter).filename = filename
      part_filter(nfilter).path = inpath

      !TODO
      ninpaths=ninpaths+1
      if (ninpaths .gt. max_inputpaths) then
          write(unit_error,630)
     &        'Too many input paths specified; max allowed is:'
     &        ,max_inputpaths
           call exit(-1)
      endif


      pathinput(ninpaths).name=name
      pathinput(ninpaths).useobj=.true.
      write(LocName, '(a32)') resname
      pathinput(ninpaths).obj_name=LocName
      pathinput(ninpaths).obj_type=obj_node
      pathinput(ninpaths).variable="part_filter"
      pathinput(ninpaths).sign = 0

      if (FileName(:8) .eq. 'constant' .or.
     &      FileName(:8) .eq. 'CONSTANT') then
          read(InPath, '(1f10.0)') ftmp
          pathinput(ninpaths).constant_value=ftmp
          pathinput(ninpaths).fillin=fill_last
          pathinput(ninpaths).path=trim(InPath)
          pathinput(ninpaths).filename=trim(FileName)
      else
c--------------Break up the input pathname

          pathinput(ninpaths).path=trim(InPath)
          call chrlnb(InPath, npath)
          call zufpn(ca, na, cb, nb, cc, nc, cd, nd, ce, ne,
     &              cf, nf, InPath, npath, istat)
          if (istat .lt. 0) then
              write(unit_error, '(/a/a)')
     &                 'Input TS: Illegal pathname', InPath
              call exit(-1)
          end if

          call split_epart(ce,itmp,ctmp)
          if (itmp .ne. miss_val_i) then ! valid interval, parse it
              pathinput(ninpaths).no_intervals=itmp
              pathinput(ninpaths).interval=ctmp
          else
              write(unit_error,610)
     &                 'Input TS: Unknown input E part or interval: ' // ce
              write(unit_error, '(a)') 'Path: ' // trim(InPath)
              call exit(-1)
          endif
          pathinput(ninpaths).filename=FileName
          !pathinput(ninpaths).filename=trim(FileName)
c--------------accumulate unique dss input filenames
          itmp=loccarr(pathinput(ninpaths).filename,infilenames,
     &              max_dssinfiles, EXACT_MATCH)
          if (itmp .lt. 0) then
              if (abs(itmp) .le. max_dssinfiles) then
                  infilenames(abs(itmp))=pathinput(ninpaths).filename
                  pathinput(ninpaths).ndx_file=abs(itmp)
              else
                  write(unit_error,610)
     &                    'Maximum number of unique DSS input files exceeded'
                  call exit(-3)
               endif
          else
              pathinput(ninpaths).ndx_file=itmp
          endif
          pathinput(ninpaths).fillin=fillin_code(fillin)
      endif

      pathinput(ninpaths).data_type = obj_filter


      if (print_level .ge. 3) then
          write(unit_screen, '(i4,1x,a32,1x,a24,a24)') ninpaths, Name,
     &       trim(InPath(:24)),
     &       trim(FileName(:24))
      end if

 610  format(/a)
 620  format(/a/a)
 630  format(/a,i5)


      return
      end subroutine
