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
module process_timeseries_input_sed

    contains

    subroutine process_input_time_series_sed(Name,               &
                                            Param,               &
                                            Fillin,              &
                                            Filename,            &
                                            InPath) 
        use common_dsm2_vars, only: exact_match, fill_last,unit_error,infilenames,   &
                                    ninpaths,max_dssinfiles,obj_group,print_level,unit_screen, &
                                    n_dssfiles, pathinput
        use sed_internal_vars
        use io_utilities
        implicit none
        character :: InPath*80,                                   &
                      FileName*128,                               &
                      Param*16,                                   &
                      LocName*32,                                 &
                      Name*32,                                    &
                      ca*32, cb*32, cc*32, cd*32, ce*32, cf*32,   &
                      ctmp*200,                                   &
                      fillin*8

        integer*4 :: Sign,                                        &  ! sign restriction on input
                      npath,na,nb,nc,nd,ne,nf,                    &
                      itmp,                                       &
                      istat
 
         real*8 :: ftmp, fetch_data 
         call locase(name)
         call locase(param)
         call locase(fillin)
         call locase(inpath)
      
         ninpaths = ninpaths + 1
         pathinput(ninpaths).name = Name 
         pathinput(ninpaths).obj_name = 'group'
         if (FileName(:8) .eq. 'constant' .or.                     &
             FileName(:8) .eq. 'CONSTANT') then
             read(InPath,'(1f10.0)') ftmp
             pathinput(ninpaths).constant_value = ftmp
             pathinput(ninpaths).variable = Param
             pathinput(ninpaths).fillin = fill_last
             pathinput(ninpaths).path = trim(InPath)
             pathinput(ninpaths).filename = trim(FileName)
         else
         !----Break up the input pathname
             pathinput(ninpaths).path = trim(InPath)
             call chrlnb(InPath, npath)
             call zufpn(ca, na, cb, nb, cc, nc, cd, nd, ce, ne,      &
                  cf, nf, InPath, npath, istat)
             if (istat .lt. 0) then
                write(unit_error, '(/a/a)')                          &
                     'Input TS: Illegal pathname', InPath
                call exit(-1)
             end if

             pathinput(ninpaths).variable = Param
             call split_epart(ce,itmp,ctmp)
             if (itmp .ne. miss_val_i) then ! valid interval, parse it
                  pathinput(ninpaths).no_intervals = itmp
                  pathinput(ninpaths).interval = ctmp
             else
                  write(unit_error,610)                                &
                      'Input TS: Unknown input E part or interval: ' // ce
                  write(unit_error,'(a)') 'Path: ' // trim(InPath)
                  call exit(-1)
             endif
               pathinput(ninpaths).filename = FileName
               !------accumulate unique dss 80input filenames
               itmp = loccarr(pathinput(ninpaths).filename,infilenames,   &
                    max_dssinfiles, EXACT_MATCH)
               if (itmp .lt. 0) then
                  if (abs(itmp) .le. max_dssinfiles) then
                     infilenames(abs(itmp)) = pathinput(ninpaths).filename
                     pathinput(ninpaths).ndx_file = abs(itmp)
                     n_dssfiles = n_dssfiles + 1
                  else
                     write(unit_error,610)                                &
                          'Maximum number of unique DSS input files exceeded'
                     call exit(-3)
                  endif
               else
                  pathinput(ninpaths).ndx_file = itmp
               endif
               pathinput(ninpaths).fillin = fillin_code(fillin)
          endif

       !-----set data type fixme:groups is this right
       pathinput(ninpaths).data_type = obj_group
            
       if (print_level .ge. 3) then
              write(unit_screen,'(i4,1x,a32,1x,a24,a24)') ninpaths, Name,    &
                    trim(InPath(:24)),                                       &
                    trim(FileName(:24))
       end if
610    format(/a)
620    format(/a/a)
630    format(/a,i5)

end subroutine

end module