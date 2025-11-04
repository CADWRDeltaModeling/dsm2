module Group
    use constants
    implicit none
    integer, parameter :: MAX_GROUPS=200 ! Max no. of group definitions
    integer, parameter :: MAX_MEMBER_PATTERNS=100
    integer, parameter :: GROUP_ALL=0               ! group that contains everything
    integer, parameter :: GROUP_ANY_TYPE=-9999        ! wildcard for group contents: matches any object type
    integer, parameter :: GROUP_ANY_INDEX=-9998       ! wildcard for group contents: matches any object internal index number (no check it exists)


    type GroupMemberPattern
        character*32 :: predicate
        character*32 :: pattern
        integer*4 :: obj_type ! object_type in database
    end type


    type GroupMember
        character*32 :: name=miss_val_c ! Context-dependent name of group member,
                                  ! e.g. "sjr" is name of a time series
        integer*4    :: obj_type=obj_null ! Type of member, e.g. "flow"
        integer*4    :: number=miss_val_i
        integer*4    :: obj_no=miss_val_i ! Context-dependent internal index
                              ! of member, e.g. index in pathinput
    end type

!-----Constants
    type t_Group
        character*32 :: name
        integer*4      :: id
        integer*4    :: nMember
        integer*4    :: nMemberPatterns
        type(GroupMemberPattern) :: memberPatterns(MAX_MEMBER_PATTERNS)
        type(GroupMember), allocatable :: members(:)
        integer*4    :: nSubGroups
        integer*4    :: memberGroups(10)
    end type

end module Group