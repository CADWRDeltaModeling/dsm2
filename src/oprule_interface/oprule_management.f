

      logical function InitOpRules()
	implicit none
      include '../fixed/common.f'
c	character*801 line

      call init_parser_f()
c	open(66,file="c:\delta\studies\historic\oprules.inp")
c	do while (.not. EOF(66))
c        read(66,'(a)')line
c	  linelen=len_trim(line)
c	  if (linelen .eq. 800) print*, "op rule too long"
c          if (linelen .gt. 0)then
c		  print*,trim(line)
c            call parse_rule(line)
c	    end if
c      end do 
	InitOpRules=.true.
      return
	end function

