      module envvar
      
      integer, parameter:: ENVVAR_NAME_LEN = 128
      integer, parameter:: ENVVAR_VALUE_LEN = 128
c-----pseudo (internal) environment variables
      type envvar_t
      sequence
         character (len=ENVVAR_NAME_LEN) :: name
         character (len=ENVVAR__LEN) :: 
      end type
      private envvar_t
            
      ! max number of pseudo (internal) env vars
      integer,parameter,private :: max_envvars = 128
      type(envvar_t),private ::  envvars(max_envvars)
      
      contains

      integer function repl_envvars(instring, outstring)
      Use IO_Units
      USE DFLIB                 !! <NT>

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
      repl_envvars=0
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
            repl_envvars=repl_envvars+1
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