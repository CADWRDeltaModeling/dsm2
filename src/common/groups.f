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



c-----Module: Groups
c     Manages groups of DSM2 objects such as boundary flows, channels,...

      Module Groups
      use constants
      Implicit None

      include '../hydrolib/network.inc'
                                ! Maximums for pre-dimensioned arrays
      integer, parameter :: MAX_GROUPS=50 ! Max no. of group definitions
      integer, parameter :: MAX_MEMBERS=2000 ! Max no. of members in one group,
                                ! not including members that are groups
	integer, parameter :: MAX_MEMBER_PATTERNS=100
	integer, parameter :: GROUP_ALL=0               ! group that contains everything
	integer, parameter :: GROUP_ANY_TYPE=-9999        ! wildcard for group contents: matches any object type
	integer, parameter :: GROUP_ANY_INDEX=-9998       ! wildcard for group contents: matches any object internal index number (no check it exists)
      integer, parameter :: MAX_GROUP_PATTERNS=1000 ! Max no. of members of a group that are
                                !groups themselves.  
      integer, save :: nGroup   ! Actual number of groups calc'd at run time

      type GroupMemberPattern
	character*32 :: predicate
	character*32 :: pattern
	integer*4 :: obj_type ! object_type in database
	end type


      Type GroupMember
      character*32 :: name=miss_val_c ! Context-dependent name of group member,
                                ! e.g. "sjr" is name of a time series 
      integer*4    :: obj_type=obj_null ! Type of member, e.g. "flow"
	integer*4    :: number=miss_val_i
      integer*4    :: obj_no=miss_val_i ! Context-dependent internal index 
                                ! of member, e.g. index in pathinput
      End Type

c-----Constants
      Type Group
      Character*32 :: name
      Integer*4      :: id
      Integer*4    :: nMember
	Integer*4    :: nMemberPatterns
	Type(GroupMemberPattern) :: memberPatterns(MAX_MEMBER_PATTERNS)
      Type(GroupMember), allocatable :: members(:)
      Integer*4    :: nSubGroups   
      Integer*4    :: memberGroups(10)
      End Type

      Type (Group), dimension(0:MAX_GROUPS), target, save :: groupArray
      
      contains

      subroutine initGroupAll()
	implicit none
	integer allocstat

      groupArray(0).name='all'
	groupArray(0).id=0
	groupArray(0).nMember=1
      allocate(groupArray(0).members(1),STAT=allocstat)
	groupArray(0).members(1).obj_type=GROUP_ANY_TYPE
	groupArray(0).members(1).obj_no=GROUP_ANY_INDEX
	return
	end subroutine





	logical function GroupContains(grp, obj_type, ndx)
      implicit none
	integer grp,obj_type,ndx,n,i
	integer target_type
      GroupContains = .false.
	n=groupArray(grp).nMember
	target_type = TargetType(obj_type)
      do i=1,n
         if ( groupArray(grp).members(i).obj_type .eq. obj_type .and. 
     &      groupArray(grp).members(i).obj_no .eq. ndx)then
            
	      GroupContains =.true.
	      exit
	   end if
	end do
      return
      end function


      subroutine AddGroupMembers(grp,nmember,addmembers)
	implicit none
	integer grp,nmember
	integer ndx_new(nmember)
	integer nnew,norig,ntotal
	integer alloc_stat
	integer i
	Type(GroupMember):: addmembers(nmember) 
	Type(GroupMember),allocatable :: newmembers(:)
      nnew=0
	norig=groupArray(grp).nMember
	do i = 1, nmember
         if (.not. groupContains(grp,addmembers(i).obj_type,
     &                   addmembers(i).obj_no))then
	      nnew=nnew+1
            ndx_new(nnew)=i
   	    end if
	end do
      ntotal=norig+nnew
      if (nnew .eq. 0) return
	allocate(newmembers(ntotal),STAT=alloc_stat)
	newmembers(1:norig)=groupArray(grp).members(1:norig)

      do i=1, nnew
	  newmembers(norig+i)=addmembers(ndx_new(i))
	end do
      if (allocated( groupArray(grp).members) )
     &   deallocate(groupArray(grp).members)
      allocate(groupArray(grp).members(ntotal),STAT=alloc_stat)
	groupArray(grp).members=newmembers
	groupArray(grp).nMember=ntotal
	if (allocated(newmembers))deallocate(newmembers)
	end subroutine





c     Calculates all objects of type objtype matching the pattern string
      subroutine NumberMatches(objtype,pattern,nmatch)
	Use gates,only:gateArray,nGate
	use constants
	use grid_data
	use io_units, only: unit_error
      implicit none

      integer, intent(in) :: objtype ! the type of object being matched (obj_channel...)
      character*(*), intent(in) :: pattern ! the regular expression to use for matching
	                                        ! see BOOST docs for details, but pretty typical stuff
      character*32 tempstr
      integer, intent(out) :: nmatch ! number of matches
	integer i
	integer istat
	integer,external :: pattern_match_count
	integer rangebegin,rangeend

      nmatch=0
      call pattern_match_clear()
	if(objtype .eq. obj_channel)then
	  rangebegin = miss_val_i
        if(index(pattern,"ange:") .eq. 2)then
 	    call ExtractRange(pattern, rangebegin,rangeend)
        end if
	   do i=1,nchans
          if(rangebegin .ne. miss_val_i)then ! pattern was given as a range
            if (chan_geom(i).chan_no .ge. rangebegin .and.
     &          chan_geom(i).chan_no .le. rangeend)then
	          ! force a match
	          call append_match(i)
	          nmatch=nmatch+1
	      end if
	   else
	      tempstr=' '
            write(tempstr,*)chan_geom(i).chan_no
	      call pattern_match(i,trim(adjustl(tempstr)),trim(pattern),istat)
            if (istat .eq. -1)then
	         write(unit_error,*)"Error in matching text pattern:",trim(pattern)
	         call exit(-3)
	      else if(istat .eq. 1)then
               nmatch=nmatch+1
            end if
          end if
	   end do
      elseif(objtype .eq. obj_reservoir)then
          do i=1,nreser
             call pattern_match(i,trim(res_geom(i).name),trim(pattern),istat)
	       if (istat .eq. -1)then
	         write(unit_error,*)"Error in matching text pattern:",trim(pattern)
	         call exit(-3)
	       else if(istat .eq. 1)then
               nmatch=nmatch+1
             end if
          end do
      elseif(objtype .eq. obj_obj2obj) then
          do i=1,nobj2obj
             call pattern_match(i,trim(obj2obj(i).name),trim(pattern),istat)
	       if (istat .eq. -1)then
	         write(unit_error,*)"Error in matching text pattern:",trim(pattern)
	         call exit(-3)
	      else if(istat .eq. 1)then
               nmatch=nmatch+1
            end if
          end do
      elseif(objtype .eq. obj_gate)then
          do i=1,nobj2obj
             call pattern_match(i,trim(gateArray(i).name),trim(pattern),istat)
	       if (istat .eq. -1)then
	         write(unit_error,*)"Error in matching text pattern:",trim(pattern)
	         call exit(-3)
	       else if(istat .eq. 1)then
               nmatch=nmatch+1
             end if
          end do
      elseif(objtype .eq. obj_stage)then
          do i=1,nstgbnd
             call pattern_match(i,trim(stgbnd(i).name),trim(pattern),istat)
             if (istat .eq. -1)then
	         write(unit_error,*)"Error in matching text pattern:",trim(pattern)
	       else if(istat .eq. 1)then
               nmatch=nmatch+1
             end if
          end do
      elseif(objtype .eq. obj_qext)then  ! coerce to qext index
         do i=1,nqext
            call pattern_match(i,trim(adjustl(qext(i).name)),trim(adjustl(pattern)),istat)
            if (istat .eq. -1)then
	         write(unit_error,*)"Error in matching text pattern:",trim(pattern)
	         call exit(-3)
	      else if(istat .eq. 1)then
               nmatch=nmatch+1
            end if
         end do
      elseif(objtype .eq. obj_source_sink)then  ! coerce to qext index
         do i=1,nqext
            call pattern_match(i,trim(qext(i).name),trim(pattern),istat)
            if (istat .eq. -1)then
	         write(unit_error,*)"Error in matching text pattern:",trim(pattern)
	         call exit(-3)
	      else if(istat .eq. 1)then
               nmatch=nmatch+1
            end if
         end do
      elseif(objtype .eq. obj_boundary_flow)then ! coerce to qext index
         do i=1,nqext
            call pattern_match(i ,trim(qext(i).name),trim(pattern),istat)
            if (istat .eq. -1)then
	         write(unit_error,*)"Error in matching text pattern:",trim(pattern)
	         call exit(-3)
	      else if(istat .eq. 1)then
               nmatch=nmatch+1
            end if
         end do
      else
	    write(unit_error,*)"Object type code not recognized, code: ",objtype
	end if
      if (nmatch .ne. pattern_match_count()) then
         write(unit_error,*)
     &     "Error in pattern matching. Implementation count does not equal count in NumberMatches"
      end if
      return
      end subroutine
	

	
c     Retrieves the index (internal number) of the i'th match of type objtype 
c     matching the pattern string.
      subroutine RetrieveMatch(i,match_ndx)
      implicit none
      integer, intent(in) :: i  ! index of prefetched matches
	                        ! see BOOST docs for details, but pretty typical stuff
      integer, intent(out) :: match_ndx ! number of matches
	integer, external :: pattern_match_index
      
	match_ndx=pattern_match_index(i)
 
      return
      end subroutine


      subroutine ConvertGroupPatternsToMembers
	implicit none
      integer i,j,k,objtype,nmatch,alloc_stat
	character*32 cstring
	Type(GroupMember),allocatable :: newmembers(:) 
	do i=1,ngroup
	  
	  do j=1,groupArray(i).nMemberPatterns
           cstring=groupArray(i).memberPatterns(j).pattern
	     objtype=groupArray(i).memberPatterns(j).obj_type

	     if( trim(cstring) .eq. 'all')then
	        nmatch=1
	        allocate(newmembers(1),STAT=alloc_stat)
	        newmembers(1).obj_type=TargetType(objtype)
	        newmembers(1).obj_no=GROUP_ANY_INDEX
           else
	        call NumberMatches(objtype,cstring,nmatch) ! pre calculate all objects
		                                            ! that match and returns # matches
	        if (nmatch .gt. 0) allocate(newmembers(nmatch),STAT=alloc_stat)  ! allocate space for these matches
 	        do k=1,nmatch
	           newmembers(k).obj_type=TargetType(objtype) ! load objects that match into new members
	           call RetrieveMatch(k,newmembers(k).obj_no)	            
	        end do
	     end if
	     if (nmatch .gt. 0)then
		    call AddGroupMembers(i,nmatch,newmembers)     ! add new members to the group
              deallocate(newmembers)
	     end if
	  end do
	end do
      return
	end subroutine

      logical function IsAllChannelReservoir(a_group)
	implicit none
	Type (Group), intent(in) :: a_group

	integer i

	IsAllChannelReservoir=.false.
       ! loop through patterns, check object type == obj_channel or obj_reservoir
     	do i=1,a_group.nMemberPatterns
         if (not((a_group.memberPatterns(i).obj_type.eq.obj_channel).or.
     &	   (a_group.memberPatterns(i).obj_type.eq.obj_reservoir)) )then
             return 
	   end if
	end do
      IsAllChannelReservoir=.true.
      return 
	end function


*=====Function ExtractRangeLimits
      subroutine ExtractRange(rangestr, rangebegin,rangeend)
	use io_units
	implicit none
*     Purpose: Extract the min and max of a range given using
*              the syntax "range:122-134"
*     Args:
      character*(*) rangestr ! range to be converted
	character*(8) :: low = ''
	character*(8) :: high = ''
	integer :: rangebegin  ! min value of range (inclusive)
	integer :: rangeend    ! max value of range (inclusive)
	integer :: dash = -1
	integer :: strlen = -1
	dash = index(rangestr,"-")
	strlen = len_trim(rangestr)
	if(.not. (rangestr(2:6) .eq. "ange:" .or. rangestr(2:6) .eq. "ANGE:")
     &   .or. (dash .le. 0))then
	  write(unit_error,*)"Range syntax not correct. Range: ", rangestr
	end if      
      low = trim(adjustl(rangestr(7:(dash-1))))
	high = trim(adjustl(rangestr((dash+1):strlen)))
	read(low,'(i)')rangebegin
	read(high,'(i)')rangeend
	if (rangebegin .gt. rangeend)then
	  write(unit_error,*)"Low part of range is greater than " //
     &                     "high part. Range: ", rangestr
	  call exit(2)
	end if
      return
	end subroutine      

c ========================================




! one routine converts single member to string

      character*64 function GroupToString(a_group,j)
      implicit none
	integer,intent(in)::j
      Type (Group), intent(in) :: a_group

c     local variable

      character*16 typename
	character*32 obj_identifier

      call obj_type_name(a_group.members(j).obj_type,typename)
      if(a_group.members(j).obj_no .eq. GROUP_ANY_INDEX) then
              write(obj_identifier,*)'all'
      else 
	        call objno_to_name(a_group.members(j).obj_type,
     &           a_group.members(j).obj_no,obj_identifier)
	end if
      GroupToString="Type: "//trim(typename)//" Identifier: "//trim(obj_identifier)
      return 
      end function

c ========================================
      integer function TargetType(objtype)
	implicit none
	integer objtype
	TargetType=objtype
      if (objtype .eq. obj_boundary_flow .or. 
     &   objtype .eq. obj_source_sink)then
	   TargetType = obj_qext
	end if
      return
      end function


      subroutine GroupTarget(objtype,str_identifier,outtype,outid)
c     Convert the data type and identifier to 
c     the type and index of the corresponding model object
c      (qext, stage boundary)
	use IO_Units, only: unit_error
      implicit none
      integer, intent(in) :: objtype ! the type of object being matched (obj_channel...)
      character*32, intent(in) :: str_identifier
	integer, intent(out) :: outtype
	integer, intent(out) :: outid
      integer, external :: name_to_objno
      outtype=TargetType(objtype)
      outid=name_to_objno(outtype,str_identifier)
      return
      end subroutine




! printGroupMembers loops through groups and members and uses single memeber covert     
      subroutine PrintGroupMembers
      use io_units
	implicit none
	integer i,j
	character*64 member_str
	do i=1,ngroup
         write(unit_screen,*)"Group: ",groupArray(i).name
	   write(unit_screen,*)"Members:"
          do j=1,groupArray(i).nmember 
	      member_str=GroupToString(groupArray(i),j)
	      print*,member_str
	    end do
	end do
	end subroutine

c     same functionallity as PrintGroupMember above, except to a file instead of
c     to screen, jon 4/5/06

	subroutine WriteGroupMembers2File(fileunit)
	implicit none
	integer, intent(in) :: fileunit  ! IO file to be written to
	integer i,j
	character*64 member_str

	do i=1,ngroup
         write (fileunit, *) "Group: ",groupArray(i).name
	   write (fileunit,*) "Members:"
          do j=1,groupArray(i).nmember 
	      member_str=GroupToString(groupArray(i),j)
	      write (fileunit,*)  member_str
	    end do
	end do
	end subroutine

      End Module