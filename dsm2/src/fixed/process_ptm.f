      subroutine process_particle_flux_output(name, from_wb, to_wb,interval,filename)
c-----process a character line into data arrays for particle group output
      use IO_Units
      use iopath_data
      use common_ptm
      use constants
      use constants_ptm
      use groups
      !use ptm_local   !todo: why is ptm_local leaking into common?
      implicit none
      character*40  from_wb, to_wb
      character*32  objtmp
      character*32  name
      character*16  interval
      character*128 filename
      character*80 ctmp
      integer itmp
      integer, external :: loccarr       
      integer, external :: name_to_objno
      integer, external :: obj_type_code

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
      !use ptm_local   !todo: why is ptm_local leaking into common?
      implicit none
      character*40  groupname
      character*32  name
      character*16  interval
      character*128 filename
      character*80 ctmp
      integer itmp
      integer, external :: loccarr       
      integer, external :: name_to_objno

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


