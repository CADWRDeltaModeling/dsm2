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

subroutine process_input_gate(LocName, &
                              SubLoc, &
                              Param, &
                              Fillin, &
                              Filename, &
                              InPath)

      use Gates
      use io_units
      use iopath_data
      use logging
      use envvar
      use utilities, only: loccarr, fillin_code, split_epart
      implicit none


      character &
          InPath*392 &
          ,FileName*128 &
          ,Param*16 &
          ,LocName*32 &
          ,SubLoc*32 &           ! Object-dependent sublocation (gate device, reservoir node connect..)
          ,Name*32 &
          ,ca*32, cb*32, cc*32, cd*32, ce*32, cf*32 &
          ,ctmp*200 &
          ,fillin*8


      integer*4 &
          Sign &                ! sign restriction on input
          ,npath,na,nb,nc,nd,ne,nf &
          ,itmp &
          ,name_to_objno &       ! function to get object number
          ,gateNo,devNo,devType &
          ,istat

      integer :: gatendx

      integer, external :: data_types

      real*8 ftmp
      real*8, external :: fetch_data

      real*8  :: fetcheddata
      logical free

      call locase(locname)
      call locase(subloc)

      call locase(param)
      call locase(fillin)
      call locase(inpath)

      ninpaths=ninpaths+1
      if (ninpaths .gt. max_inputpaths) then
         write(unit_error,630) &
             'Too many input paths specified; max allowed is:' &
             ,max_inputpaths
         istat=-1
         return
      endif



      pathinput(ninpaths).name=Name
      pathinput(ninpaths).useobj=.true.
      pathinput(ninpaths).obj_name=LocName
      pathinput(ninpaths).obj_type=obj_gate
      sign = 0
      pathinput(ninpaths).sign = sign

      pathinput(ninpaths).obj_name=trim(LocName)
      pathinput(ninpaths).obj_no=name_to_objno(obj_gate,LocName)
      if (pathinput(ninpaths).obj_no .eq.miss_val_i ) then
         write(unit_error,'(a,a)') &
           'Gate Time Series Input: ',trim(pathinput(ninpaths).name), &
           ' attached to unrecognized object: ',LocName
            istat=-3
            return
         end if

         if (FileName(:8) .eq. 'constant' .or. &
               FileName(:8) .eq. 'CONSTANT') then
            read(InPath,'(1f10.0)') ftmp
            pathinput(ninpaths).constant_value=ftmp
            pathinput(ninpaths).variable=Param
            pathinput(ninpaths).fillin=fill_last
            pathinput(ninpaths).path=trim(InPath)
            pathinput(ninpaths).filename=trim(FileName)
         else
!--------------Break up the input pathname

            pathinput(ninpaths).path=trim(InPath)
            call chrlnb(InPath, npath)
            call zufpn(ca, na, cb, nb, cc, nc, cd, nd, ce, ne, &
                cf, nf, InPath, npath, istat)
            if (istat .lt. 0) then
               write(unit_error, '(/a/a)') &
                   'Input TS: Illegal pathname', InPath
               istat = -1
               return
            end if

            pathinput(ninpaths).variable=Param
            call split_epart(ce,itmp,ctmp)
            if (itmp .ne. miss_val_i) then ! valid interval, parse it
               pathinput(ninpaths).no_intervals=itmp
               pathinput(ninpaths).interval=ctmp
            else
               write(unit_error,610) &
                   'Input TS: Unknown input E part or interval: ' // ce
               write(unit_error,'(a)') 'Path: ' // trim(InPath)
               istat=-1
               return
            endif
            pathinput(ninpaths).filename=FileName
!--------------accumulate unique dss input filenames
            itmp=loccarr(pathinput(ninpaths).filename,infilenames, &
                max_dssinfiles, EXACT_MATCH)
            if (itmp .lt. 0) then
               if (abs(itmp) .le. max_dssinfiles) then
                  infilenames(abs(itmp))=pathinput(ninpaths).filename
                  pathinput(ninpaths).ndx_file=abs(itmp)
               else
                  write(unit_error,610) &
                      'Maximum number of unique DSS input files exceeded'
                  istat=-3
                  return
               endif
            else
               pathinput(ninpaths).ndx_file=itmp
            endif
            pathinput(ninpaths).fillin=fillin_code(fillin)
         endif
         !fixme: the next line should probably be based on RoleName
!-----------set data type fixme:groups is this right
         pathinput(ninpaths).data_type=obj_gate

         if (pathinput(ninpaths).data_type .eq. obj_gate) then
            if ( len_trim(subloc).le. 0 ) then
               if ( param(1:7) .eq. 'install') then
                  pathinput(ninpaths).gate_param=gate_install
                  pathinput(ninpaths).locnum=devNo
                  gatendx=pathinput(ninpaths).obj_no
                  call datasource_from_path( &
                            gateArray(gatendx).install_datasource, &
                            ninpaths, &
                            pathinput(ninpaths))
                  fetcheddata=fetch_data( &
                              gateArray(gatendx).install_datasource)
                  free = fetcheddata .eq. 0.
                  call setFree(gateArray(gatendx), free)
               else   ! device wasn't specified and not installation
                  write(unit_error,*) &
                      'Input TS: For gate: ' // trim(LocName) // &
                      ' a time series was entered with no device name.'
                  write(unit_error,*) &
                      'This is only allowed if the time series variable is "install"'
                  istat=-3
                  return
               end if
            else
               gateNo=pathinput(ninpaths).obj_no
               devNo=deviceIndex(gateArray(gateNo),subLoc)
               devType=gateArray(gateNo).Devices(devNo).structureType
               if( gateNo .ne. miss_val_i .and. devNo .ne. miss_val_i) then
                  if( devType .eq. miss_val_i .and. param(1:3).eq.'pos') then
                     write(unit_error,*) &
                         "Time series for device position in a device with no flow control. Gate: " &
                         // trim(LocName) // &
                         ", Device: "// trim(subloc) // ", Parameter: " // param
                     istat=-3
                     return
                  end if

                  if (param(1:3) .eq. 'pos' .and. len_trim(Param) .eq. 3) then
                     write(unit_error,*) &
                      "Time series variable 'pos' has been deprecated. "
                     istat=-3
                     return
                  end if
                  if (param(1:8) .eq. 'position' .and. len_trim(Param) .eq. 8) then
                     write(unit_error,*) &
                      "Time series variable 'position' has been deprecated. "
                     istat=-3
                     return
                  end if

                  if (param(1:2) .eq. 'op') then
                     if (param(4:5) .eq. 'to' .and. &
                         param(7:10) .eq. 'node') then
                        pathinput(ninpaths).gate_param = gate_op_to_node
                     else if (param(4:7) .eq. 'from' .and. &
                            param(9:12) .eq. 'node') then
                        pathinput(ninpaths).gate_param = gate_op_from_node
                     else
                        pathinput(ninpaths).gate_param = gate_operation
                     end if


                  if( pathinput(ninpaths).gate_param .eq. gate_operation &
                      .or. &
                      pathinput(ninpaths).gate_param .eq. gate_op_to_node) &
                      then ! bidirectional or to_node
                     call datasource_from_path( &
                         gateArray(gateNo).Devices( &
                         devNo).op_to_node_datasource, &
                         ninpaths,pathinput(ninpaths))
                     gateArray(pathinput(ninpaths).obj_no).Devices( &
                         devNo).opCoefToNode=fetch_data( &
                           gateArray(gateNo).Devices( &
                         devNo).op_to_node_datasource)

                  endif

                  if( pathinput(ninpaths).gate_param .eq. gate_operation &
                      .or. &
                      pathinput(ninpaths).gate_param .eq. gate_op_from_node) &
                      then !bidirectional or from_node
                     call datasource_from_path( &
                         gateArray(gateNo).Devices( &
                         devNo).op_from_node_datasource, &
                         ninpaths,pathinput(ninpaths))
                     gateArray(pathinput(ninpaths).obj_no).Devices( &
                         devNo).opCoefFromNode=fetch_data( &
                         gateArray( &
                         pathinput(ninpaths).obj_no &
                         ).Devices(devNo).op_from_node_datasource)

                  end if
               else if (param(1:5) .eq. 'width') then
                  pathinput(ninpaths).gate_param = gate_width
                  pathinput(ninpaths).locnum=devNo
                  call datasource_from_path( &
                         gateArray(gateNo).Devices( &
                         devNo).width_datasource, &
                         ninpaths,pathinput(ninpaths))
                     gateArray(pathinput(ninpaths).obj_no).Devices( &
                         devNo).maxWidth=fetch_data( &
                         gateArray( &
                         pathinput(ninpaths).obj_no &
                         ).Devices(devNo).width_datasource)
               else if (param(1:6) .eq. 'height') then
                  pathinput(ninpaths).gate_param = gate_height
                  pathinput(ninpaths).locnum=devNo
                  call datasource_from_path( &
                         gateArray(gateNo).Devices( &
                         devNo).height_datasource, &
                         ninpaths,pathinput(ninpaths))
                     gateArray(pathinput(ninpaths).obj_no).Devices( &
                         devNo).height=fetch_data( &
                         gateArray( &
                         pathinput(ninpaths).obj_no &
                         ).Devices(devNo).height_datasource)
               else if (param(1:4) .eq. 'elev') then
                  pathinput(ninpaths).gate_param = gate_elev
                  pathinput(ninpaths).locnum=devNo
                  call datasource_from_path( &
                         gateArray(gateNo).Devices( &
                         devNo).elev_datasource, &
                         ninpaths,pathinput(ninpaths))
                     gateArray(pathinput(ninpaths).obj_no).Devices( &
                         devNo).baseElev=fetch_data( &
                         gateArray( &
                         pathinput(ninpaths).obj_no &
                         ).Devices(devNo).elev_datasource)

               else
                  write(unit_error,*) &
                      "Unknown time series parameter. Gate: " &
                      // trim(LocName) // &
                      ", Device: "// trim(subloc) // &
                      ", Parameter: " // param
                  istat=-3
                  return
               end if  ! parameter
            else
               write(unit_error,'(a/a/a/a/)') &
                   'Input TS: Input attached to unrecognized device', &
                   '  Gate: ' // trim(LocName),'  Device: ' // trim(subLoc), &
                   '  Path: ' // trim(pathinput(ninpaths).path)
            end if        ! device was recognized
         end if           ! subloclen >0 (ie, both gate and device given)
      end if              ! input is a gate
      if (print_level .ge. 3) then
         write(unit_screen,'(i4,1x,a32,1x,a24,a24)') ninpaths, Name, &
             trim(InPath(:24)), &
             trim(FileName(:24))
      end if

 610  format(/a)
 620  format(/a/a)
 630  format(/a,i5)

end