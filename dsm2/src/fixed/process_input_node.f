      subroutine process_input_node (name,
     &                               LocNum,
     &                               InPath,    
     &                               param,
     &                               Sign,
     &                               Fillin,
     &                               RoleName,    
     &                               filename)

      use Gates
      use io_units
      use iopath_data
      use logging
      use grid_data
      use envvar
      implicit none

c-----local variables

      character
     &     InPath*80
     &     ,FileName*128
     &     ,Param*32
     &     ,LocName*32
     &     ,RoleName*32
     &     ,Name*64
     &     ,ca*32, cb*32, cc*32, cd*32, ce*32, cf*32
     &     ,ctmp*200

      integer*4
     &     Fillin              ! code for fill in type (last, none, linear)
     &     ,Sign                ! sign restriction on input
     &     ,LocNum              ! object map id of input data if applicable (channel,node)
     &     ,npath,na,nb,nc,nd,ne,nf
     &     ,itmp
     &     ,nenv                ! environment var replacement
     &     ,istat

      integer, external :: data_types
      integer, external :: ext2intnode
      integer, external :: loccarr 

      real*8 ftmp
      real*8, external :: fetch_data



            ninpaths=ninpaths+1
            if (ninpaths .gt. max_inputpaths) then
               write(unit_error,630)
     &              'Too many input paths specified; max allowed is:'
     &              ,max_inputpaths
               call exit(-1)
            endif

c-----------clean up character variables, replace environment variables

            call trim_null_space(InPath)
            InPath=trim(InPath)
            nenv=replace_envvars(InPath,ctmp)
            InPath=ctmp
            call locase(InPath)


            write(LocName, '(i)')LocNum


            call trim_null_space(RoleName)
            RoleName = trim(RoleName)
            call locase(RoleName)


            call trim_null_space(FileName)
            FileName = trim(FileName)
            nenv=replace_envvars(FileName,ctmp)
            FileName=ctmp



            pathinput(ninpaths).name=Name
            pathinput(ninpaths).useobj=.true.
            pathinput(ninpaths).obj_name=LocName
            pathinput(ninpaths).obj_type=obj_node


               if (sign .eq. -1) then
                  pathinput(ninpaths).sign = -1
               elseif (sign .eq. 1) then
                  pathinput(ninpaths).sign = 1
               elseif (sign .eq. 0) then
                  ! do nothing
               else
                  write(unit_error,*)"Incorrect sign for input time series"
                  call exit(-3)
               end if


c-----------find object number given external object number 
            pathinput(ninpaths).obj_no=ext2intnode(LocNum)
            pathinput(ninpaths).obj_name=LocName
            if (FileName(:8) .eq. 'constant' .or.
     &             FileName(:8) .eq. 'CONSTANT') then
               read(InPath, '(1f10.0)') ftmp
               pathinput(ninpaths).constant_value=ftmp
               pathinput(ninpaths).variable=Param
               pathinput(ninpaths).fillin=fill_last
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
                  write(unit_error, '(a)') 'Path: ' // trim(InPath)
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
               pathinput(ninpaths).fillin=Fillin
            endif
                                !fixme: the next line should probably be based on RoleName
c-----------set data type fixme:groups is this right
            if (
     &           RoleName .eq. "inflow")then
               pathinput(ninpaths).data_type=obj_boundary_flow
            else if (RoleName .eq. "source-sink")then
               pathinput(ninpaths).data_type=obj_source_sink
            else if (RoleName .eq. "stage")then
               pathinput(ninpaths).data_type=obj_stage
            end if

            if ( pathinput(ninpaths).data_type .eq. obj_stage) then
               if (pathinput(ninpaths).obj_type .eq. obj_node) then
                  node_geom(pathinput(ninpaths).obj_no).boundary_type=stage_boundary
               else
                  write(unit_error, '(a)')
     &                 'Input TS: Stage boundary must be at a node.'
               end if
            end if

            if (print_level .ge. 3)then
               write(unit_screen, '(i4,1x,a32,1x,a24,a24)') ninpaths, Name,
     &              trim(InPath(:24)),
     &              trim(FileName(:24))
            end if

 610  format(/a)
 620  format(/a/a)
 630  format(/a,i5)

      return
      end subroutine