!<license>
!    Copyright (C) 2017 State of California,
!    Department of Water Resources.
!    This file is part of DSM2-GTM.
!
!    The Delta Simulation Model 2 (DSM2) - General Transport Model (GTM) 
!    is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    DSM2 is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!</license>

!>@ingroup process_io
module process_gtm_output_channel

    contains
    
    !> Process a character line into data arrays for
    !> output file names
    subroutine process_output_channel(name, channo, distance, param, interval, perop, sourcegroup, filename)

        use common_dsm2_vars
        use io_utilities
        implicit none
      
        character*32 name
        character*16 :: param
        character*16 :: interval
        character*8  :: perop
        character*32 :: sourcegroup
        character*128 :: filename
        integer :: channo
        integer :: distance

        character*(200) :: ctmp
        integer :: itmp

        call locase(name)
        call locase(param)
        call locase(sourcegroup)
        call locase(perop)
        call locase(interval)
      
        noutpaths = noutpaths + 1

        pathoutput(noutpaths).a_part = ' '
        pathoutput(noutpaths).b_part = Name
        pathoutput(noutpaths).c_part = Param
        call split_epart(Interval,itmp,ctmp)
        if (itmp .ne. miss_val_i) then ! valid interval, parse it
            pathoutput(noutpaths).e_part = Interval
            pathoutput(noutpaths).no_intervals = itmp
            pathoutput(noutpaths).interval = ctmp
        else
            write(unit_error, "('Unknown output time interval: '//a)") Interval
            call exit(-1)
            return
        endif
        pathoutput(noutpaths).f_part = ' '
        pathoutput(noutpaths).filename = FileName
        pathoutput(noutpaths).distance = distance
        pathoutput(noutpaths).obj_type = 1
        pathoutput(noutpaths).no = channo
        pathoutput(noutpaths).modifier = dsm2_modifier
        !------accumulate unique dss output filenames
        itmp = loccarr(pathoutput(noutpaths).filename,  &
                       outfilenames,                    &
                       max_dssoutfiles,                 &
                       EXACT_MATCH)
        if (itmp .lt. 0) then
            if (abs(itmp) .le. max_dssoutfiles) then
                outfilenames(abs(itmp))=pathoutput(noutpaths).filename
                pathoutput(noutpaths).ndx_file=abs(itmp)
                n_outdssfiles = n_outdssfiles + 1
            else
                write(unit_error,610)  &
                'Maximum number of unique DSS output files exceeded'
                call exit(-3)
                return
            endif
        else
            pathoutput(noutpaths).ndx_file=itmp
        endif
 
        if (PerOp(1:4) .eq. 'inst') pathoutput(noutpaths).per_type=per_type_inst_val
        if (PerOp(1:2) .eq. 'av')   pathoutput(noutpaths).per_type=per_type_per_aver
        if (PerOp(1:3) .eq. 'min')  pathoutput(noutpaths).per_type=per_type_per_min
        if (PerOp(1:3) .eq. 'max')  pathoutput(noutpaths).per_type=per_type_per_max

        !-----pathoutput(noutpaths).source.obj_type = SourceTypeID     fixme: this is broken
        !-----if (SourceLocLen .gt. 0)  pathoutput(noutpaths).source.loc_name = SourceLoc

        if (print_level .ge. 3) write(unit_screen, '(i5,a,1x,i,a30,1x,a8,1x,a80)')  &
                                noutpaths, trim(Name),channo,trim(Param),trim(Interval), trim(FileName)

 610    format(/a)
 630    format(/a,i5)     
        return
    end subroutine
           
end module