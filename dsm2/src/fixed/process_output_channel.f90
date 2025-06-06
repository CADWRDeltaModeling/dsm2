!!<license>
!!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!!    Department of Water Resources.
!!    This file is part of DSM2.

!!    The Delta Simulation Model 2 (DSM2) is free software:
!!    you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License as published by
!!    the Free Software Foundation, either version 3 of the License, or
!!    (at your option) any later version.

!!    DSM2 is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License for more details.

!!    You should have received a copy of the GNU General Public License
!!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!!</license>

subroutine process_output_channel(name, &
                                  channo, &
                                  distance, &
                                  param, &
                                  interval, &
                                  perop, &
                                  sourcegroup, &
                                  filename)

      use Groups, only: GROUP_ALL
      use io_units
      use logging
      use iopath_data
      use grid_data
      use envvar
      use utilities, only: loccarr, split_epart
      implicit none

      character*32 name

      character*16 :: param
      character*16 :: interval
      character*8  :: perop
      character*32 :: sourcegroup
      character*128 :: filename

      integer :: channo
      integer :: distance

      integer, external :: name_to_objno
      integer, external :: ext2int

      character*(200) ctmp
      integer itmp

      call locase(name)
      call locase(param)
      call locase(sourcegroup)
      call locase(perop)
      call locase(interval)
      noutpaths=noutpaths+1
      if (noutpaths .gt. max_outputpaths) then
         write(unit_error,630) &
             'Too many pathoutput paths specified; max allowed is:', &
             max_outputpaths
         call exit(-1)
      endif

      pathoutput(noutpaths).use=.true.
      pathoutput(noutpaths).name=Name
      pathoutput(noutpaths).obj_type=obj_channel
      if (len_trim(SourceGroup).eq. 0) then
          pathoutput(noutpaths).source_group_ndx=GROUP_ALL
      else
          pathoutput(noutpaths).source_group_ndx=name_to_objno(obj_group,SourceGroup)
          if (pathoutput(noutpaths).source_group_ndx .eq. miss_val_i) then
             write(unit_error,*)"Source group ",SourceGroup, &
             " not recognized for output request: ", pathoutput(noutpaths).name
             call exit(2)
           end if
      endif
!----------find object number given object ID
      pathoutput(noutpaths).obj_no = ext2int(channo)
      if (pathoutput(noutpaths).obj_no .eq. 0) &
          pathoutput(noutpaths).obj_no = miss_val_i ! quick fix?
         if(pathoutput(noutpaths).obj_no .eq. miss_val_i) then
            write(unit_error,*)'Ignoring output TS: ', trim(name), &
               ' request for unrecognized channel ', channo
               noutpaths=noutpaths-1
            return
         end if
      write(pathoutput(noutpaths).obj_name, '(i)') channo
      pathoutput(noutpaths).chan_dist=distance
      if(pathoutput(noutpaths).chan_dist .eq. chan_length) then
          pathoutput(noutpaths).chan_dist &
            = chan_geom(pathoutput(noutpaths).obj_no).length
      end if

      pathoutput(noutpaths).a_part=' '
      pathoutput(noutpaths).b_part=Name
      pathoutput(noutpaths).c_part=Param
      call split_epart(Interval,itmp,ctmp)
      if (itmp .ne. miss_val_i) then ! valid interval, parse it
         pathoutput(noutpaths).e_part=Interval
         pathoutput(noutpaths).no_intervals=itmp
         pathoutput(noutpaths).interval=ctmp
      else
         write(unit_error, "('Unknown output time interval: '//a)") Interval
         call exit(-1)
         return
      endif
      pathoutput(noutpaths).f_part=' '
      pathoutput(noutpaths).filename=FileName
!-----------accumulate unique dss output filenames
      itmp=loccarr(pathoutput(noutpaths).filename, &
                   outfilenames, &
                   max_dssoutfiles, &
                   EXACT_MATCH)
      if (itmp .lt. 0) then
         if (abs(itmp) .le. max_dssoutfiles) then
            outfilenames(abs(itmp))=pathoutput(noutpaths).filename
            pathoutput(noutpaths).ndx_file=abs(itmp)
         else
            write(unit_error,610) &
            'Maximum number of unique DSS output files exceeded'
            call exit(-3)
            return
         endif
      else
         pathoutput(noutpaths).ndx_file=itmp
      endif

      pathoutput(noutpaths).meas_type=Param
      if (Param(1:3) .eq. 'vel')pathoutput(noutpaths).meas_type='vel'
      call assign_output_units(pathoutput(noutpaths).units,Param)

      if (PerOp(1:4) .eq. 'inst') then
         pathoutput(noutpaths).per_type=per_type_inst_val
      else if (PerOp(1:2) .eq. 'av') then
           pathoutput(noutpaths).per_type=per_type_per_aver
      else if (PerOp(1:3) .eq. 'min') then
           pathoutput(noutpaths).per_type=per_type_per_min
      else if (PerOp(1:3) .eq. 'max') then
           pathoutput(noutpaths).per_type=per_type_per_max
      else
         write(unit_error,610) &
         'Error! Unknown PERIOD_OP type: ', PerOp
         call exit(-1)
         return
      end if
!-----------pathoutput(noutpaths).source.obj_type = SourceTypeID     fixme: this is broken
!-----------if (SourceLocLen .gt. 0)
!-----------&           pathoutput(noutpaths).source.loc_name = SourceLoc

      if (print_level .ge. 3) &
          write(unit_screen, '(i5,a,1x,i,a30,1x,a8,1x,a80)') &
          noutpaths, &
          trim(Name),channo,trim(Param),trim(Interval), &
          trim(FileName)

 610  format(/a)
 630  format(/a,i5)
      return
end subroutine



