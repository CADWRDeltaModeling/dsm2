c====================================================================
      subroutine buffer_input_grid() !process_text_grid_input()
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