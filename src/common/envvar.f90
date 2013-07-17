!<license>
!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!    Department of Water Resources.
!    This file is part of DSM2.

!    The Delta Simulation Model 2 (DSM2) is free software:
!    you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.

!    DSM2 is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.

!    You should have received a copy of the GNU General Public License
!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!</license>

module envvar
      
    integer, parameter:: ENVVAR_NAME_LEN = 32
    integer, parameter:: ENVVAR_VALUE_LEN = 128
    !----pseudo (internal) environment variables
    type envvar_t
        sequence
        character (len=ENVVAR_NAME_LEN) :: name
        character (len=ENVVAR_VALUE_LEN) :: value
    end type
            
    ! max number of pseudo (internal) env vars
    integer,parameter :: max_envvars = 128
    type(envvar_t)::  envvars(max_envvars)
    integer::  nenvvars    ! actual number of envvars used
    
    contains

    subroutine add_envvar(name,val)
        use io_units
        implicit none
        integer:: j
        character (len=ENVVAR_NAME_LEN):: name
        character (len=ENVVAR_VALUE_LEN):: val
        do j = 1, nenvvars
            if(name == envvars(j)%name) then
                envvars(j)%value =val
                return
            endif
        enddo
      
        nenvvars=nenvvars+1
        if (nenvvars > max_envvars) then
            write(unit_error,'(a,i)') &
                'Too many envvars specified; max allowed is:',max_envvars
            call exit(-1)
        endif
            
        envvars(nenvvars)%name=name
        envvars(nenvvars)%value=val
        return
    end subroutine add_envvar

    integer function replace_envvars(instring, outstring)
        use io_units
        use ifport

        !----Replace any environment variables in a string with their values.
        !----env vars are of this form: $[({]string[)}]
        !----psuedo/internal env vars (from the ENVVARS section) will be
        !----replaced too.
        !----Returned are the number of env vars found

        implicit none

        !----arguments and local vars

        character*(*):: &
            instring            ! replaced string [OUTPUT]
        character*(*):: outstring            ! replaced string [OUTPUT]

        integer:: &
            start_ndx
        integer:: end_ndx
        integer:: evlen
        integer:: out_ndx
        integer:: i
        integer:: lins
        integer:: index

        character:: &
            estring*20           ! allowable characters in env var name
        character:: evalue*200           ! allowable characters in env var name
        character:: echars1*64           ! allowable characters in env var name

        parameter ( &
            !----if changed, also change declared length, above
            echars1='abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-' &
            )

        lins=len_trim(instring)
        replace_envvars=0
        outstring=' '
        out_ndx=1
        end_ndx=1
        start_ndx=index(instring,'$')
        do while (start_ndx > 0 .and. start_ndx < lins)
            outstring(out_ndx:)=instring(end_ndx:start_ndx-1)
            out_ndx=out_ndx+start_ndx-end_ndx
            !-------construct env var name string by accepting only valid chars
            estring=' '
            evalue=' '
            i=1                    ! env var constructor index
            end_ndx=start_ndx+1    ! end_ndx is just after $
            !-------test for starting ( or {
            if (instring(end_ndx:end_ndx) == '(' .or. &
                instring(end_ndx:end_ndx) == '{') then
                end_ndx=end_ndx+1
            endif
            do while (index(echars1,instring(end_ndx:end_ndx)) > 0)
                estring(i:i)=instring(end_ndx:end_ndx)
                i=i+1
                end_ndx=end_ndx+1
            enddo
            !-------test for ending ) or }
            if (instring(end_ndx:end_ndx) == ')' .or. &
                instring(end_ndx:end_ndx) == '}') then
                end_ndx=end_ndx+1
            endif
            if (estring /= ' ') then ! found an env var
                !----------first check internal env names...
                call getenv_internal(estring,evalue)
                if (evalue == ' ') then ! no internal value found, try external
                    !-------------...then external names
                    !               call getenv(estring, evalue) !! <UNIX>
                    i=getenvqq(trim(estring), evalue) !! <NT>
                endif
                !----------if empty value, print warning
                if (evalue == ' ') then
                    write(unit_error,610) trim(estring)
610                 format(/'Warning: empty value for environment variable ',a,';'&
                        /' could cause unwanted behavior in run.')
                endif
            endif
            evlen=len_trim(evalue)
            if (evlen > 0) then ! env var found
                outstring(out_ndx:)=evalue(:evlen)
                out_ndx=out_ndx+evlen
                replace_envvars=replace_envvars+1
            endif

            start_ndx=index(instring(end_ndx:),'$')
            if (start_ndx > 0) &
                start_ndx=start_ndx + end_ndx - 1 ! start_ndx is at $, or 0
        enddo
        outstring(out_ndx:)=instring(end_ndx:)

        return
    end function replace_envvars
    !=================================================================

    subroutine getenv_internal(estring,evalue)
        !----Look for ESTRING in the internal env vars list, if found return
        !----its value in EVALUE
        use iopath_data
        implicit none
      
        integer:: &
            i                    ! index

        character:: &
            estring*(*)		  ! lower case of envvars.name
        character:: evalue*(*)		  ! lower case of envvars.name
        character:: evarname*130		  ! lower case of envvars.name
      
        call locase(estring)      ! convert to lower case
        evalue=' '

        do i=1,max_envvars
            if (envvars(i)%name == ' ')exit
            evarname=envvars(i)%name
            call locase(evarname)
            if (trim(evarname) == trim(estring)) then
                evalue=envvars(i)%value
                return
            endif
        enddo
        return
    end subroutine getenv_internal
      
end module envvar
