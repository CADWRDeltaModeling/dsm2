C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    The Delta Simulation Model 2 (DSM2) is free software:
C!    you can redistribute it and/or modify
C!    it under the terms of the GNU General Public License as published by
C!    the Free Software Foundation, either version 3 of the License, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public License for more details.

C!    You should have received a copy of the GNU General Public License
C!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
C!</license>

      subroutine process_input_climate(Name,
     &                                 Param,
     &                                 Fillin,
     &                                 Filename,
     &                                 InPath)

      use Gates
      use io_units
      use iopath_data
      use logging
      use grid_data
      use envvar
      use utilities, only: loccarr, split_epart, fillin_code
      implicit none


      character
     &     InPath*392
     &     ,FileName*128
     &     ,Param*16
     &     ,LocName*32
     &     ,Name*32
     &     ,ca*32, cb*32, cc*32, cd*32, ce*32, cf*32
     &     ,ctmp*200
     &     ,fillin*8


      integer*4
     &     Sign                ! sign restriction on input
     &     ,npath,na,nb,nc,nd,ne,nf
     &     ,itmp
     &     ,istat

      integer, external :: data_types

      real*8 ftmp
      real*8, external :: fetch_data
      call locase(name)
      call locase(param)
      call locase(fillin)
      call locase(inpath)


      ninpaths=ninpaths+1
            if (ninpaths .gt. max_inputpaths) then
               write(unit_error,630)
     &              'Too many input paths specified; max allowed is:'
     &              ,max_inputpaths
               call exit(-1)
            endif

            pathinput(ninpaths).name=Name
            pathinput(ninpaths).useobj=.true.
            pathinput(ninpaths).obj_name=LocName
            pathinput(ninpaths).obj_type=obj_climate

!               if (sign .eq. -1) then
!                  pathinput(ninpaths).sign = -1
!               elseif (sign .eq. 1) then
!                  pathinput(ninpaths).sign = 1
!               elseif (sign .eq. 0) then
!                  pathinput(ninpaths).sign = 1
!               else
!                  write(unit_error,'(a,i)')"Incorrect sign for input time series:"
!                  call exit(-3)
!               end if

            pathinput(ninpaths).obj_name='global'
            pathinput(ninpaths).obj_no = miss_val_i  ! this could be an index into climate variables.
            if (FileName(:8) .eq. 'constant' .or.
     &             FileName(:8) .eq. 'CONSTANT') then
               read(InPath,'(1f10.0)') ftmp
               pathinput(ninpaths).constant_value=ftmp
               pathinput(ninpaths).variable=Param
               pathinput(ninpaths).fillin=fill_last
               pathinput(ninpaths).path=trim(InPath)
               pathinput(ninpaths).filename=trim(FileName)
            else
c--------------Break up the input pathname

               pathinput(ninpaths).path=trim(InPath)
               call chrlnb(InPath, npath)
               call zufpn(ca, na, cb, nb, cc, nc, cd, nd, ce, ne,
     &              cf, nf, InPath, npath, istat)
               if (istat .lt. 0) then
                  write(unit_error, '(/a/a)')
     &                 'Input TS: Illegal pathname', InPath
                  call exit(-1)
               end if

               pathinput(ninpaths).variable=Param
               call split_epart(ce,itmp,ctmp)
               if (itmp .ne. miss_val_i) then ! valid interval, parse it
                  pathinput(ninpaths).no_intervals=itmp
                  pathinput(ninpaths).interval=ctmp
               else
                  write(unit_error,610)
     &                 'Input TS: Unknown input E part or interval: ' // ce
                  write(unit_error,'(a)') 'Path: ' // trim(InPath)
                  call exit(-1)
               endif
               pathinput(ninpaths).filename=FileName
c--------------accumulate unique dss input filenames
               itmp=loccarr(pathinput(ninpaths).filename,infilenames,
     &              max_dssinfiles, EXACT_MATCH)
               if (itmp .lt. 0) then
                  if (abs(itmp) .le. max_dssinfiles) then
                     infilenames(abs(itmp))=pathinput(ninpaths).filename
                     pathinput(ninpaths).ndx_file=abs(itmp)
                  else
                     write(unit_error,610)
     &                    'Maximum number of unique DSS input files exceeded'
                     call exit(-3)
                  endif
               else
                  pathinput(ninpaths).ndx_file=itmp
               endif
               pathinput(ninpaths).fillin=fillin_code(fillin)
            endif
                                !fixme: the next line should probably be based on RoleName
c-----------set data type fixme:groups is this right
            pathinput(ninpaths).data_type=obj_climate

            if (print_level .ge. 3) then
               write(unit_screen,'(i4,1x,a32,1x,a24,a24)') ninpaths, Name,
     &              trim(InPath(:24)),
     &              trim(FileName(:24))
            end if

 610  format(/a)
 620  format(/a/a)
 630  format(/a,i5)

      end subroutine