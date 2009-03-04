      subroutine process_rate_coef(group_name, 
     &                             rate_variable,
     &                             constituent,
     &                             coefficient_value)            
	use Groups, only: groupContains,IsAllChannelReservoir,groupArray
	use rate_coeff_assignment,only:assign_rate_to_group,rate_var_require_flag
	use common_qual
	use constants
	use io_units
	use logging      
      implicit none
      character*16 rate_variable,constituent
      
      character*32 group_name
      character errm*128 !todo: this is not good style      
      integer :: rate_variable_id
      integer :: constituent_id
      integer :: groupno
      integer :: istat
      integer, external :: name_to_objno
      real*4  :: coefficient_value
      integer,external :: rate_variable_code
      integer,external :: ncc_code
      
      rate_variable_id = rate_variable_code(rate_variable)
      constituent_id = ncc_code(constituent)
      
      groupno = name_to_objno(obj_group, group_name)
	if (groupno.lt.0) then
	   write(unit_error, '(a)') 'Group not recognized: ' // group_name
	   call exit(-3)
      end if
	if (not(IsAllChannelReservoir(groupArray(groupno)))) then
         write(unit_error, '(a)') 
     &                "Members of group "//group_name//
     &                " are not all channel or reservior"
         call exit(-3)
      end if
      coefficient_value=coefficient_value/24. ! convert per day to per hour
      call assign_rate_to_group(groupno,rate_variable_id,constituent_id,
     &                          coefficient_value,istat,errm)
      ! todo -- this is pretty strange style
	if (istat.lt.0) then
         write(unit_error, '(a)') errm
         call exit(-3)
      end if
      rate_var_require_flag(constituent_id,rate_variable_id)=.true.
      return
      end subroutine