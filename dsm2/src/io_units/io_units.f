c-----I/O unit values
      Module IO_Units

      integer, parameter :: unit_error=0     ! error messages
      integer, parameter :: unit_input=11    ! input unit
      integer, parameter :: unit_screen=6    ! output unit to screen (MUST be 6)
      integer, parameter :: unit_output=14   ! output file
      integer, parameter :: unit_text=13     ! temporary (scratch) text file output
      integer, save :: unit_hydro
      End Module IO_Units
