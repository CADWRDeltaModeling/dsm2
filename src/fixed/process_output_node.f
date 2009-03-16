C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
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

      subroutine process_output_node(name,
     &                               LocNum, 
     &                               param,
     &                               interval,
     &                               perop,
     &                               sourcegroup,
     &                               filename)

c-----load f90SQL modules
      !use f90SQLConstants
      !use f90SQL
      use Gates, only: gateArray,gateIndex,deviceIndex,WEIR,PIPE
      use Groups, only: GROUP_ALL
      use io_units
      use logging
      use iopath_data
      use grid_data
      use envvar
      implicit none

      character
     &     FileName*128
     &     ,Name*32
     &     ,Param*32
     &     ,Interval*32
     &     ,PerOp*8
     &     ,LocName*32
     &     ,ctmp*200
     &     ,SourceGroup*32      


      integer*4
     &     ID                   ! transfer ID
     &     ,LocNum
     &     ,itmp
          

      integer, external :: name_to_objno
      integer, external :: ext2int, ext2intnode
      integer, external :: loccarr ! locate string in char array function




            noutpaths=noutpaths+1
            if (noutpaths .gt. max_outputpaths) then
               write(unit_error,630)
     &              'Too many pathoutput paths specified; max allowed is:'
     &              ,max_outputpaths
               call exit(-1)
               return
            endif

            pathoutput(noutpaths).use=.true.
            pathoutput(noutpaths).name=Name
            pathoutput(noutpaths).obj_type=obj_node
            if (len_trim(SourceGroup).eq. 0) then
               pathoutput(noutpaths).source_group_ndx=GROUP_ALL
            else
               pathoutput(noutpaths).source_group_ndx=name_to_objno(obj_group,SourceGroup)
               if (pathoutput(noutpaths).source_group_ndx .eq. miss_val_i) then
                   write(unit_error,*)"Source group ",SourceGroup,
     &              " not recognized for output request: ", pathoutput(noutpaths).name
                   call exit(2)
               end if
            endif
c-----------find object number given object ID

            pathoutput(noutpaths).obj_name=' '
            pathoutput(noutpaths).obj_no = ext2intnode(LocNum)
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
c-----------accumulate unique dss output filenames
            itmp=loccarr(pathoutput(noutpaths).filename,outfilenames,
     &           max_dssoutfiles, EXACT_MATCH)
            if (itmp .lt. 0) then
               if (abs(itmp) .le. max_dssoutfiles) then
                  outfilenames(abs(itmp))=pathoutput(noutpaths).filename
                  pathoutput(noutpaths).ndx_file=abs(itmp)
               else
                  write(unit_error,610)
     &                 'Maximum number of unique DSS output files exceeded'
                  call exit(-3)
                  return
               endif
            else
               pathoutput(noutpaths).ndx_file=itmp
            endif

            pathoutput(noutpaths).meas_type=Param
            if (Param(1:3) .eq. 'vel')pathoutput(noutpaths).meas_type='vel'            
            call assign_output_units(pathoutput(noutpaths).units,Param)

            if (PerOp(1:4) .eq. 'inst')
     &           pathoutput(noutpaths).per_type=per_type_inst_val
            if (PerOp(1:2) .eq. 'av')
     &           pathoutput(noutpaths).per_type=per_type_per_aver
            if (PerOp(1:3) .eq. 'min')
     &           pathoutput(noutpaths).per_type=per_type_per_min
            if (PerOp(1:3) .eq. 'max')
     &           pathoutput(noutpaths).per_type=per_type_per_max

c-----------pathoutput(noutpaths).source.obj_type = SourceTypeID     fixme: this is broken
c-----------if (SourceLocLen .gt. 0)
c-----------&           pathoutput(noutpaths).source.loc_name = SourceLoc

            if (print_level .ge. 3)
     &           write(unit_screen, '(i5,i10,a,1x,a,a30,1x,a8,1x,a80)') noutpaths, ID,
     &           trim(Name),trim(LocName),trim(Param),trim(Interval),
     &           trim(FileName)


 610  format(/a)
 630  format(/a,i5)
      
      return
      end subroutine