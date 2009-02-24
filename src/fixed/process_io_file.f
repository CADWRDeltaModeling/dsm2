      subroutine process_io_file(model,filetype,io,interval,filename)
c-----process a character line into data arrays for
c-----output file names
      use io_units
      use iopath_data
      use constants_ptm
      use common_ptm
      implicit none

c-----local variables

      integer
     &     i,i1,i2,i3           ! indices
 
      character*8
     &     model                ! string for model
     &     ,filetype            ! string for type
     &     ,io                  ! string for io
     &     ,interval            ! string for interval
      character*128
     &     filename             ! string for filename

 
 610  format(/a)
 620  format(/'Invalid value given in ',a,' field: ',a)
 
c-----fill in structure
      if (model(1:5) .eq. 'hydro') then
         i1=hydro
      else if (model(1:4) .eq. 'qual') then
         i1=qual
      else if (model(1:3) .eq. 'ptm') then
         i1=ptm
      else
         write(unit_error, "('Model not recognized: ',a)")model
         call exit(-1)
      endif

      ! todo: this doesn't make any sense
      if (filetype .eq. 'output') then
         output_filename=filename
         return
      endif      

      if (filetype(1:3) .eq. 'res') then
         i2=io_restart
      else if (filetype(1:3) .eq. 'bin' .or.
     &        filetype(1:3) .eq. 'tid') then
         write(unit_error,*)
     &        "Binary or tidefile should be specified in " //
     &        "IO_FILE section using 'hdf5' file type"
         i2=io_tide
      else if (filetype(1:3) .eq. 'hdf') then
         i2=io_hdf5
      else if (filetype(1:3) .eq. 'ani') then
         i2=io_animation
      else if (filetype(1:3) .eq. 'tra') then
         i2=io_trace
      else if (filetype(1:3) .eq. 'beh') then
         i2=io_behavior
      else if (filetype(1:3) .eq. 'gro') then
         i2=io_group
         ptm_igroup_int=1
         ptm_igroup=.true.
      else
         write(unit_error,*) "IO file type not recognized: ", filetype
         call exit(-1)
      endif

      if (io .eq. 'in') then
         i3=io_read
      else if (io .eq. 'out') then
         i3=io_write
      else
         write(unit_error, *) "IO must be 'in' or 'out' "
         call exit(-1)
       endif

      if (interval .ne. ' ' .and.
     &     interval .ne. 'none') then
         io_files(i1,i2,i3).interval=interval
      endif

      io_files(i1,i2,i3).use=.true.
      io_files(i1,i2,i3).filename=filename

      return
      end subroutine
     
