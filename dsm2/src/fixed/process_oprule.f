


      subroutine process_oprule(name, action, trigger)
      use groups
      use constants
      use logging
      use io_units
      implicit none
      character*32 :: name
      character*512 :: action
      character*512 :: trigger
      character*1024 :: ruletext = " "
      logical :: parse_rule
	ruletext=" "
      write(ruletext,"(a,1x,':=',1x,a,1x,'WHEN',1x,a,';')")
     &     trim(name),trim(action),trim(trigger)
	if(print_level .ge. 3) then
	   write(unit_screen,"(/'Parsing rule: ',/a)"),trim(ruletext)
	end if
	if (.not. parse_rule(trim(ruletext)))then
          write(unit_error,"(/'Error parsing rule: ',/a)"),trim(ruletext)
          call exit(-3)
          return
      endif
      end subroutine



      subroutine process_oprule_expression(name, definition)
      use groups
      use constants
      use logging
      use io_units
      implicit none
      character*32 :: name
      character*512 :: definition
      logical :: parse_rule
	definition=" "
      write(definition,"(a,1x,':=',1x,a,';')")
     &     trim(name),trim(definition)
	if(print_level .ge. 3) then
	   write(unit_screen,"(/'Parsing oprule expression: ',/a)"),trim(definition)
	end if
	if (.not. parse_rule(trim(definition)))then
          write(unit_error,"(/'Error parsing expression: ',/a)"), trim(definition)
          call exit(-3)
          return
      endif
      end subroutine

