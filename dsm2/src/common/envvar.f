C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!<license>
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

      module envvar
      
      integer, parameter:: ENVVAR_NAME_LEN = 128
      integer, parameter:: ENVVAR_VALUE_LEN = 128
c-----pseudo (internal) environment variables
      type envvar_t
      sequence
         character (len=ENVVAR_NAME_LEN) :: name
         character (len=ENVVAR_VALUE_LEN) :: value
      end type
            
      ! max number of pseudo (internal) env vars
      integer,parameter :: max_envvars = 128
      type(envvar_t) ::  envvars(max_envvars)
      
      contains

      integer function replace_envvars(instring, outstring)
      use io_units
      use ifport

c-----Replace any environment variables in a string with their values.
c-----env vars are of this form: $[({]string[)}]
c-----psuedo/internal env vars (from the ENVVARS section) will be
c-----replaced too.
c-----Returned are the number of env vars found

      implicit none

c-----arguments and local vars

      character*(*)
     &     instring             ! string to search for env vars [INPUT]
     &     ,outstring           ! replaced string [OUTPUT]

      integer
     &     start_ndx            ! start of env var name in instring
     &     ,end_ndx             ! end of env var name in instring
     &     ,evlen               ! length of env var value
     &     ,out_ndx             ! index in outstring to place parsed instring
     &     ,i                   ! index
     &     ,lins                ! last non-blank of instring
     &     ,index

      character
     &     estring*20           ! env var name string
     &     ,evalue*200          ! env var value
     &     ,echars1*64          ! allowable characters in env var name

      parameter (
c-----if changed, also change declared length, above
     &     echars1='abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-'
     &     )

      lins=len_trim(instring)
      replace_envvars=0
      outstring=' '
      out_ndx=1
      end_ndx=1
      start_ndx=index(instring,'$')
      do while (start_ndx .gt. 0 .and. start_ndx .lt. lins)
         outstring(out_ndx:)=instring(end_ndx:start_ndx-1)
         out_ndx=out_ndx+start_ndx-end_ndx
c--------construct env var name string by accepting only valid chars
         estring=' '
         evalue=' '
         i=1                    ! env var constructor index
         end_ndx=start_ndx+1    ! end_ndx is just after $
c--------test for starting ( or {
         if (instring(end_ndx:end_ndx) .eq. '(' .or.
     &        instring(end_ndx:end_ndx) .eq. '{') then
            end_ndx=end_ndx+1
         endif
         do while (index(echars1,instring(end_ndx:end_ndx)) .gt. 0)
            estring(i:i)=instring(end_ndx:end_ndx)
            i=i+1
            end_ndx=end_ndx+1
         enddo
c--------test for ending ) or }
         if (instring(end_ndx:end_ndx) .eq. ')' .or.
     &        instring(end_ndx:end_ndx) .eq. '}') then
            end_ndx=end_ndx+1
         endif
         if (estring .ne. ' ') then ! found an env var
c-----------first check internal env names...
            call getenv_internal(estring,evalue)
            if (evalue .eq. ' ') then ! no internal value found, try external
c--------------...then external names
c               call getenv(estring, evalue) !! <UNIX>
               i=getenvqq(trim(estring), evalue) !! <NT> 
            endif
c-----------if empty value, print warning
            if (evalue .eq. ' ') then
               write(unit_error,610) trim(estring)
 610           format(/'Warning: empty value for environment variable ',a,';'
     &        /' could cause unwanted behavior in run.')
            endif
         endif
         evlen=len_trim(evalue)
         if (evlen .gt. 0) then ! env var found
            outstring(out_ndx:)=evalue(:evlen)
            out_ndx=out_ndx+evlen
            replace_envvars=replace_envvars+1
         endif

         start_ndx=index(instring(end_ndx:),'$')
         if (start_ndx .gt. 0)
     &        start_ndx=start_ndx + end_ndx - 1 ! start_ndx is at $, or 0
      enddo
      outstring(out_ndx:)=instring(end_ndx:)

      return
      end function
c==================================================================

      subroutine getenv_internal(estring,evalue)
c-----Look for ESTRING in the internal env vars list, if found return
c-----its value in EVALUE
      use iopath_data
      implicit none
      
      integer
     &     i                    ! index

      character
     &     estring*(*)          ! env var name string
     &     ,evalue*(*)          ! env var value
     &     ,evarname*130		  ! lower case of envvars.name
      
      call locase(estring)      ! convert to lower case
      evalue=' '

      do i=1,max_envvars
         if (envvars(i).name .eq. ' ')exit    
	   evarname=envvars(i).name
	   call locase(evarname)
         if (trim(evarname) .eq. trim(estring)) then
            evalue=envvars(i).value
            return
         endif
      enddo
      return
      end subroutine
      
      end module