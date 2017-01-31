!<license>
!    Copyright (C) 2017 State of California,
!    Department of Water Resources.
!    This file is part of DSM2-GTM.
!
!    The Delta Simulation Model 2 (DSM2) - General Transport Model (GTM) 
!    is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    DSM2 is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!</license>

!>@ingroup process_io
module process_gtm_groups

    contains
   
    !> Process group                      
    subroutine process_gtm_group(name, id)
        use common_variables, only: group
        implicit none
        character*32 :: name
        integer :: id
        call locase(name)
        group(id)%id = id
        group(id)%name = name
        return    
    end subroutine

    !> Process group members
    subroutine process_gtm_group_member(groupname,    &
                                        membertype,   &
                                        pattern,      &
                                        icount)
        use common_variables, only: group_member
        implicit none
        character*32, intent(in) :: groupname
        integer, intent(in) :: membertype
        character*256, intent(in) :: pattern
        integer, intent(in) :: icount
        call locase(groupname)
        call locase(pattern)
        group_member(icount)%groupname = groupname
        group_member(icount)%membertype = membertype
        group_member(icount)%pattern = pattern 
        return
    end subroutine
    
    !> Process group members in details
    subroutine process_members_detail()
        use common_variables
        implicit none        
        integer :: count_member
        integer :: i, j
        
        do i = 1, n_group
            do j = 1, n_group_member
                if (trim(group(i)%name).eq.trim(group_member(j)%groupname)) then
                    group(i)%n_memberpatterns = group(i)%n_memberpatterns + 1
                    call count_num_members(count_member, group_member(j)%pattern, &
                                           group(i)%id, group_member(j)%membertype)
                    group(i)%n_members = group(i)%n_members + count_member
                end if
            end do        
        end do       
        
        do i = 1, n_group
            allocate(group(i)%member_name(group(i)%n_members))
            allocate(group(i)%member_int_id(group(i)%n_members))            
            allocate(group(i)%member_pattern_code(group(i)%n_members))
            group(i)%member_name = ' '
            group(i)%member_int_id = 0
            group(i)%member_pattern_code = 0
            do j = 1, n_group_member
                if (trim(group(i)%name).eq.trim(group_member(j)%groupname)) then
                    call fill_member_property(group_member(j)%pattern,                 &
                                              group(i)%id, group_member(j)%membertype)
                end if
            end do        
        end do        
        return
    end subroutine
    
    
    !> Count members in a group for allocation
    subroutine count_num_members(count_member, pattern, group_id, obj_code)
        use common_variables
        implicit none
        integer, intent(out) :: count_member
        integer, intent(in) :: group_id
        integer, intent(in) :: obj_code
        character*256, intent(in) :: pattern
        character*256 :: buffer
        integer :: pos, pos2, io
        integer :: i, before, after
        integer, parameter :: max_member = 1000    ! max number of members in a group
        character(8) :: v(max_member)
        
        count_member = 0
        if (pattern(1:4).eq."list") then
            buffer = pattern(6:)
            pos = index(buffer, ",")
            do i = 1, max_member 
                v(i) = buffer(1:pos-1)
                ! detect if there is "-"
                pos2 = index(v(i), "-")
                if (pos2.ne.0) then
                    read(buffer(1:pos2-1),'(i)',iostat=io) before
                    read(buffer(pos2+1:),'(i)',iostat=io) after
                    count_member = count_member + after - before + 1
                else
                    count_member = count_member + 1
                end if 
      
                buffer = buffer(pos+1:)
                pos = index(buffer, ",")
                if (pos==0) then
                    v(i+1) = buffer
                    ! detect if there is "-"   
                    pos2 = index(v(i+1), "-")
                    if (pos2.ne.0) then
                        read(buffer(1:pos2-1),'(i)',iostat=io) before
                        read(buffer(pos2+1:),'(i)',iostat=io) after
                        count_member = count_member + after - before + 1
                    else
                        count_member = count_member + 1
                    end if         
                    goto 200
                end if
            end do                 
        else    
            buffer = pattern
        endif    
200     return
    end subroutine        
    

    !> Fill in member properties for group array
    subroutine fill_member_property(pattern, group_id, obj_code)
        use common_variables
        implicit none
        integer, intent(in) :: group_id
        integer, intent(in) :: obj_code
        character*256, intent(in) :: pattern
        character*256 :: buffer
        character*16 :: str
        integer :: pos, pos2, io, count
        integer :: i, j, before, after, int
        integer, parameter :: max_member = 1000    ! max number of members in a group
        character(8) :: v(max_member)
        
        count = 0
        if (pattern(1:4).eq."list") then
            buffer = pattern(6:)
            pos = index(buffer, ",")
            do i = 1, max_member 
                v(i) = buffer(1:pos-1)
                ! detect if there is "-"
                pos2 = index(v(i), "-")
                if (pos2.ne.0) then
                    read(buffer(1:pos2-1),'(i)',iostat=io) before
                    read(buffer(pos2+1:),'(i)',iostat=io) after
                    do j = before, after
                        count = count + 1
                        group(group_id)%member_pattern_code(count) = obj_code
                        write(str,*) j
                        group(group_id)%member_name(count) = adjustl(trim(str))
                    end do
                else
                    count = count + 1
                    group(group_id)%member_pattern_code(count) = obj_code
                    group(group_id)%member_name(count) = v(i)
                end if 
      
                buffer = buffer(pos+1:)
                pos = index(buffer, ",")
                if (pos==0) then
                    v(i+1) = buffer
                    ! detect if there is "-"   
                    pos2 = index(v(i+1), "-")
                    if (pos2.ne.0) then
                        read(buffer(1:pos2-1),'(i)',iostat=io) before
                        read(buffer(pos2+1:),'(i)',iostat=io) after
                        do j = before, after         
                            count = count + 1
                            group(group_id)%member_pattern_code(count) = obj_code
                            write(str,*) j
                            group(group_id)%member_name(count) = adjustl(trim(str)) 
                        end do
                    else
                        count = count + 1
                        group(group_id)%member_pattern_code(count) = obj_code
                        group(group_id)%member_name(count) = v(i+1)                       
                    end if         
                    goto 300
                end if
            end do                 
        else    
            buffer = pattern
        endif    
300     return
    end subroutine        
    
end module    