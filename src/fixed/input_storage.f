
c=======================================================================
      subroutine input_text(filename)
!     Read in all text starting from input filename
      use hdf5
      use input_storage_fortran
      use envvar

      implicit none
      character*(*) filename

c-----Do a first pass reading the input, activating only ENVVARS for use in later text substitution
      call clear_all_buffers()
      !todo: testing whether this is optional
      call init_text_substitution("PARAMETER")    ! Get ready for the pass
      call process_text_substitution(filename)    ! Do the reading and prep the environment variables
      call envvar_clear_buffer()                  ! Clear the envvar buffer
      print*,"Read and processed text substitution (ENVVARS)"

c-----Do a second pass on all the input, making use of the text substitution we just prepped
      call init_file_reader()                     ! Prepare for a read of everything
      call read_buffer_from_text(filename)        ! Perform the read into buffers
      print*,"Read text into buffers"
      print*,"No of layers=",xsect_layer_buffer_size()
      call prioritize_all_buffers()               ! Enforce the "layering"
      print*,"Prioritized buffer"
 
      return
      end subroutine
c==================================================================

      subroutine write_input_buffers()
!     Writes in all text starting from input filename
      use hdf5
      use input_storage_fortran
      use envvar
      implicit none
      character(LEN=7),parameter :: hdf_filename = "echo.h5" 
      integer(HID_T) :: file_id
      integer :: error
      logical, parameter :: append_text=.TRUE.
      logical :: ext
c-----Write all buffers to text in the order they were defined
      call write_all_buffers_to_text("testout.txt",append_text)
      print*, "text written"

c-----Write all buffers to hdf5
      ! for the moment we are deleting and recreating the file, but this is an open question
      inquire(file=hdf_filename, exist=ext)
      if (ext)then
      call unlink(hdf_filename,error)
      end if

      call h5open_f (error)
      call h5fcreate_f(hdf_filename, H5F_ACC_TRUNC_F, file_id, error)
      if (error .ne. 0) then
      print*,"Could not open file, hdf error: ", error
      print*,"Check if it already exists and delete if so -- failure to replace seems to be an HDF5 bug"
      call exit(2)
      end if

      call write_all_buffers_to_hdf5(file_id)      ! Do the actual write

      call h5fclose_f(file_id, error)              ! Close down ! todo: this is too verbose
      print *, "file close status: ", error
      call h5close_f(error)
      print*, "hdf5 shutdown status: ", error

 
      return
      end subroutine



c====================================================================
      subroutine process_initial_text
      
      use hdf5
      use input_storage_fortran
      use envvar
      implicit none
      integer :: nitem
      integer :: icount
      character*(32) name,value
      character*(32) envname
      character*(128) envval
      integer err
      nitem = envvar_buffer_size()
      do icount = 1,nitem
           err=envvar_query_from_buffer(icount,envname,envval)
           call add_envvar(envname,envval)
      end do
      print *,"Number of envvar: ", nitem

      nitem = scalar_buffer_size()
      do icount = 1,nitem
           err=scalar_query_from_buffer(icount,name,value)
           call process_scalar(name,value)
      end do
      print *,"Number of scalars: ", nitem
       
      return
      end subroutine



c====================================================================
      subroutine process_text_grid_input()
      use input_storage_fortran
      use io_units
      !use constants
      use hdf5, only: hid_t
      implicit none
      
      integer :: node  
      integer :: nitem
      character*(128) filename
      integer :: icount
      character*(32) name
      integer err
      
      integer chan_no,length,up_node,down_node
      real*8 manning,dispersion
      
      real*8 dist,elev,width,wet_perim
      
      real*8 area, bottom_elev,coef_in,coef_out
      logical,external :: order_nodes

      character(len=16) :: from_obj
      character(len=32) :: from_identifier
      character(len=16) :: to_obj
      character(len=32) :: to_identifier
      integer :: idummy = 0
      

       character(len=32) :: gate_name
       character(len=32) :: device_name
       character(len=16) :: default_op
       character(len=16) :: position_control
       character(len=8) :: struct_name
       integer :: nduplicate
       !real(8) :: width
       !real(8) :: elev
       real(8) :: height
       real(8) :: cf_to_node
       real(8) :: cf_from_node


       if ((xsect_buffer_size() .gt. 0) .and. 
     &     (xsect_layer_buffer_size() .gt. 0))then
         write(unit_error,*)
     &      "Mixing of CSDP-style cross-sections and XSECT_LAYER input not supported"
         call exit(-3)
       end if

      
      nitem = channel_buffer_size()
      do icount = 1,nitem
         err=channel_query_from_buffer(icount,
     &                                 chan_no,
     &                                 length,
     &                                 manning,
     &                                 dispersion,
     &                                 up_node,
     &                                 down_node)

      nitem = channel_buffer_size()         
         call process_channel(idummy,0,chan_no,
     &                          length,
     &                          manning,
     &                          dispersion,
     &                          up_node,
     &                          down_node)
       end do
       print *,"Number of channels: ", nitem
       
       if (.not. order_nodes())then
         write(unit_error,'(a)')'Error reordering nodes.'
         call exit(-3)
         return
       end if



! note: this must be done befere xsect_layer_buffer. what it will actually do 
! is convert layers to xsect_layers (unless this turns out to take way too long).
! I did this so that later when we write out the buffers we get these in the xsect_layer
! format (ie, the complete input can still be in one file).
! If this causes terrible performance probs, another way to do it would be
! to process the xsect layers as we go along, still add them to buffers for reading,
! make sure we don't redundantly call xsect_layer_buffer processing.
      nitem = xsect_buffer_size()         
      do icount = 1,nitem
         err=xsect_query_from_buffer(icount,
     &                                 chan_no,
     &                                 dist,
     &                                 filename)
         
         call process_xsect_csdp(chan_no,
     &                           dist,
     &                           filename)
       end do
       print *,"Number of channel xsects (csdp format): ", nitem

      nitem = xsect_layer_buffer_size()         
      do icount = 1,nitem
         err=xsect_layer_query_from_buffer(icount,
     &                                 chan_no,
     &                                 dist,
     &                                 elev,
     &                                 area,
     &                                 width,
     &                                 wet_perim)
         
         call process_xsect_layer_full(chan_no,
     &                                 dist,
     &                                 elev,
     &                                 area,
     &                                 width,
     &                                 wet_perim)
       end do
       print *,"Number of channel xsect layers: ", nitem


      nitem = reservoir_buffer_size()
      do icount = 1,nitem
         err=reservoir_query_from_buffer(icount,
     &                                   name,
     &                                   area,
     &                                   bottom_elev)
         
         call process_reservoir(0,name,  !todo: get rid of the id argument
     &                          area,
     &                          bottom_elev)
       end do
       print *,"Number of reservoirs: ", nitem

      nitem = reservoir_connection_buffer_size()
      do icount = 1,nitem
         err=reservoir_connection_query_from_buffer(icount,
     &                                   name,
     &                                   node,
     &                                   coef_in,     
     &                                   coef_out)
         
         call process_reservoir_connection(name,
     &                                     node, 
     &                                     coef_in, 
     &                                     coef_out)
       end do
       print *,"Number of reservoir connections: ", nitem

      nitem = gate_buffer_size()
      do icount = 1,nitem
         err=gate_query_from_buffer(icount,
     &                              name,
     &                              from_obj,
     &                              from_identifier,
     &                              node)
         
         call process_gate(0,
     &                     name,
     &                     from_obj,
     &                     from_identifier,
     &                     node)
       end do
       print *,"Number of gates: ", nitem

      nitem = gate_device_buffer_size()
      do icount = 1,nitem
         err=gate_device_query_from_buffer(icount,
     &                              gate_name,
     &                              device_name,
     &                              struct_name,
     &                              nduplicate,
     &                              width,
     &                              elev,
     &                              height,
     &                              cf_to_node,
     &                              cf_from_node,
     &                              default_op,
     &                              position_control)
         
         call process_gate_device(
     &                              gate_name,
     &                              device_name,
     &                              struct_name,
     &                              nduplicate,
     &                              width,
     &                              elev,
     &                              height,
     &                              cf_to_node,
     &                              cf_from_node,
     &                              default_op,
     &                              position_control)
       end do
       print *,"Number of gate devices: ", nitem



      nitem = transfer_buffer_size()
      do icount = 1,nitem
         err=transfer_query_from_buffer(icount,
     &                                  name,
     &                                  from_obj,
     &                                  from_identifier,
     &                                  to_obj,
     &                                  to_identifier)
         
         call process_transfer(0,name,!todo: get rid of the id argument
     &                         from_obj,
     &                         from_identifier,
     &                         to_obj,
     &                         to_identifier)
       end do
       print *,"Number of transfers: ", nitem      

       return
       end subroutine

c======================================================================
      subroutine process_text_input()      
      use input_storage_fortran
      use constants
      
      implicit none
      integer :: nitem
      character*(128) filename
      integer :: icount
      character*(32) name
      character*8,model,filetype,io
      character*16 interval
      character*128 iofile
      integer err

      ! input_node

      character*32 :: rolename 


      ! output_channel
      integer channo
      character*8  distance
      integer      idistance
      character*16 variable,
     &                perop
      character*32 :: sourcegroup
      
      ! output_reservoir
      character*32 reservoir
      character*80 inpath
      character*8  fillin
      integer      sign
      integer node      
      
       ! output_gate
      character*32 gate, device
      character*512 action, trigger, definition


      character*(16) :: sdate,edate



      nitem = io_file_buffer_size()
      do icount = 1,nitem
         err=io_file_query_from_buffer(icount,model,filetype,io,interval,iofile)
         call process_io_file(model,filetype,io,interval,iofile)
      end do
      print *,"Number of iofiles: ", nitem


      nitem = tidefile_buffer_size()
      do icount = 1,nitem
         err=tidefile_query_from_buffer(icount,sdate,edate,iofile)
         call process_tidefile(model,sdate,edate,iofile)
      end do
      print *,"Number of tidefiles: ", nitem


      nitem = oprule_time_series_buffer_size()
      do icount = 1,nitem
          err=oprule_time_series_query_from_buffer(icount,
     &                                        name,
     &                                        filename,
     &                                        inpath,
     &                                        fillin)

          sign=0 ! currently hardwired
          call process_input_oprule(name,
     &                              filename,
     &                              inpath,
     &                              sign,
     &                              fillin)
      end do
      print *,"Number of oprule time series processed: ", nitem



      nitem = oprule_expression_buffer_size()
      do icount = 1,nitem
         err=oprule_expression_query_from_buffer(icount,
     &                                           name,
     &                                           definition)
      call process_oprule_expression(name,
     &                               definition)
      end do
      print *,"Number of operating rule expressions processed: ", nitem


      nitem = operating_rule_buffer_size()
      do icount = 1,nitem
         err=operating_rule_query_from_buffer(icount,
     &                                        name,
     &                                        action,
     &                                        trigger)

      call process_oprule(name,
     &                    action,
     &                    trigger)
      end do
      print *,"Number of operating rules processed: ", nitem





c======================== Input and output ======================


      nitem = input_climate_buffer_size()
      do icount = 1,nitem
         err=input_climate_query_from_buffer(icount,
     &                                       name,
     &                                       variable,
     &                                       fillin,
     &                                       filename,
     &                                       inpath) 

         sign = 1

         call process_input_climate(name,
     &                              variable,
     &                              sign,
     &                              fillin,
     &                              filename,
     &                              inpath)
 
      end do
      print *,"Number of climate inputs processed: ", nitem



      nitem = input_transfer_flow_buffer_size()
      do icount = 1,nitem
         err=input_transfer_flow_query_from_buffer(icount,
     &                                             name,
     &                                             fillin,   
     &                                             filename,
     &                                             inpath)
         variable = 'flow'
         call process_input_transfer(name,
     &                               variable,
     &                               fillin,   
     &                               filename,
     &                               inpath)

      end do
      print *,"Number of transfer inputs processed: ", nitem


      nitem = input_gate_buffer_size()
      do icount = 1,nitem
         err=input_gate_query_from_buffer(icount,
     &                                    name,
     &                                    device,
     &                                    variable,
     &                                    fillin,   
     &                                    filename,
     &                                    inpath)


         call process_input_gate(name,
     &                           device,
     &                           variable,
     &                           fillin,   
     &                           filename,
     &                           inpath)
      end do
      print *,"Number of gate inputs processed: ", nitem


      nitem = input_node_buffer_size()
      do icount = 1,nitem
         err=input_node_query_from_buffer(icount,
     &                                    name,
     &                                    node,
     &                                    variable,     
     &                                    sign,
     &                                    rolename,
     &                                    fillin,   
     &                                    filename,
     &                                    inpath)

         call process_input_node(name,
     &                           node,
     &                           variable,     
     &                           sign,
     &                           rolename,
     &                           fillin,   
     &                           filename,
     &                           inpath)

      end do
      print *,"Number of node inputs processed: ", nitem



      nitem = input_reservoir_buffer_size()
      do icount = 1,nitem
         err=input_reservoir_query_from_buffer(icount,
     &                                         name,
     &                                         reservoir,
     &                                         variable,
     &                                         sign,
     &                                         fillin,
     &                                         filename,
     &                                         inpath)

         call process_input_reservoir(name,
     &                                reservoir,
     &                                variable,
     &                                sign,
     &                                fillin,
     &                                filename,
     &                                inpath)

      end do
      print *,"Number of reservoir inputs processed: ", nitem








      nitem = output_channel_buffer_size()
      do icount = 1,nitem
         err=output_channel_query_from_buffer(icount,
     &                                        name,
     &                                        channo,
     &                                        distance,
     &                                        variable,
     &                                        interval,
     &                                        perop,
     &                                        filename)
         sourcegroup = ""
         call locase(distance)
         if (distance(:6) .eq. "length") then 
            idistance = chan_length
         else 
            read(distance,'(i)')idistance
         end if
         call process_output_channel(name,
     &                               channo,
     &                               idistance,
     &                               variable,
     &                               interval,
     &                               perop,
     &                               sourcegroup,
     &                               filename)
      end do
      print *,"Number of channel output requests: ", nitem



      nitem = output_reservoir_buffer_size()
      do icount = 1,nitem
         err=output_reservoir_query_from_buffer(icount,
     &                                        name,
     &                                    reservoir,
     &                                    node,
     &                                    variable,
     &                                    interval,
     &                                    perOp,
     &                                    filename) 
         sourcegroup = ""
         call process_output_reservoir(name,
     &                                    reservoir,
     &                                    node,
     &                                    variable,
     &                                    interval,
     &                                    perOp,
     &                                    sourceGroup,
     &                                    filename) 
      end do
      print *,"Number of reservoir output requests: ", nitem


      nitem = output_gate_buffer_size()
      do icount = 1,nitem
         err=output_gate_query_from_buffer(icount,
     &                                     name,
     &                                     gate,
     &                                     device,
     &                                     variable,
     &                                     interval,
     &                                     perop,
     &                                     filename)

         call process_output_gate(name,
     &                            gate,
     &                            device,
     &                            variable,
     &                            interval,
     &                            perop,
     &                            filename)
      end do
      print *,"Number of gate output requests: ", nitem

      end subroutine
