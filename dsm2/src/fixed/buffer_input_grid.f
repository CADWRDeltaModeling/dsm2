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
c====================================================================
      subroutine buffer_input_grid() !process_text_grid_input()
      use input_storage_fortran
      use io_units
      !use constants
      use hdf5, only: hid_t
      use gates, only: process_gate, process_gate_device
      !@# For dynamic allocation
      use common_variables, only: allocate_chan_geom_hq
      implicit none
      
      integer :: node  
      integer :: nitem
      character*(128) filename
      integer :: icount
      character*(32) name
      integer :: ierror = 0
      
      integer chan_no,length,up_node,down_node
      real*8 manning,dispersion
      
      real*8 dist,elev,width,wet_perim
      
      real*8 area, bottom_elev,coef_in,coef_out, volume
      logical,external :: order_nodes

      character(len=16) :: from_obj
      character(len=32) :: from_identifier
      character(len=16) :: to_obj
      character(len=32) :: to_identifier
      integer :: idummy = 0
      

       character(len=32) :: gate_name
       character(len=32) :: device_name
       character(len=16) :: default_op
       character(len=8) :: struct_name
       integer :: nduplicate
       !real(8) :: width
       !real(8) :: elev
       real(8) :: height
       real(8) :: cf_to_node
       real(8) :: cf_from_node


       if ((xsect_buffer_size() .gt. 0) .and. 
     &     (xsect_layer_buffer_size() .gt. 0)) then
         write(unit_error,*)
     &      "Mixing of CSDP-style cross-sections and XSECT_LAYER input not supported"
         call exit(-3)
       end if

      
      nitem = channel_buffer_size()
      do icount = 1,nitem
         call channel_query_from_buffer(icount,
     &                                 chan_no,
     &                                 length,
     &                                 manning,
     &                                 dispersion,
     &                                 up_node,
     &                                 down_node,
     &                                 ierror)

      nitem = channel_buffer_size()         
      !@# Dynamic allocation for chan_geom
      call allocate_chan_geom_hq
         call process_channel(idummy,0,chan_no,
     &                          length,
     &                          manning,
     &                          dispersion,
     &                          up_node,
     &                          down_node)
       end do
       print *,"Number of channels: ", nitem              
       if (.not. order_nodes()) then
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
         call xsect_query_from_buffer(icount,
     &                                 chan_no,
     &                                 dist,
     &                                 filename,
     &                                 ierror)
         
         call process_xsect_csdp(chan_no,
     &                           dist,
     &                           filename)
       end do
       print *,"Number of channel xsects (csdp format): ", nitem

      nitem = xsect_layer_buffer_size()         
      do icount = 1,nitem
         call xsect_layer_query_from_buffer(icount,
     &                                 chan_no,
     &                                 dist,
     &                                 elev,
     &                                 area,
     &                                 width,
     &                                 wet_perim,
     &                                 ierror)
         
         call process_xsect_layer_full(chan_no,
     &                                 dist,
     &                                 elev,
     &                                 area,
     &                                 width,
     &                                 wet_perim)
       end do
       print *,"Number of channel xsect layers: ", nitem

c------ Clear the xsect buffer. Why? Because we have already converted
c       the cross sections to cross-section layer format, and when we
c       write out the input buffers we don't want to include the xsects that
c       are file-based
      call xsect_clear_buffer()

      nitem = reservoir_buffer_size()
      do icount = 1,nitem
         call reservoir_query_from_buffer(icount,
     &                                   name,
     &                                   area,
     &                                   bottom_elev,
     &                                   ierror)
         
         call process_reservoir(0,name,  !todo: get rid of the id argument
     &                          area,
     &                          bottom_elev)
       end do
       print *,"Number of reservoirs: ", nitem

      nitem = reservoir_vol_buffer_size()
      do icount = 1,nitem
         call reservoir_vol_query_from_buffer(icount,
     &                                   name,
     &                                   elev,
     &                                   area,
     &                                   volume, ! this is not used beyond this point
     &                                   ierror)
         
         call process_reservoir_vol(name,
     &                              elev,
     &                              area)
       end do
       print *,"Number of reservoir elevations/volume: ", nitem

      nitem = reservoir_connection_buffer_size()
      do icount = 1,nitem
         call reservoir_connection_query_from_buffer(icount,
     &                                   name,
     &                                   node,
     &                                   coef_in,     
     &                                   coef_out,
     &                                   ierror)
         
         call process_reservoir_connection(name,
     &                                     node, 
     &                                     coef_in, 
     &                                     coef_out)
       end do
       print *,"Number of reservoir connections: ", nitem

       
      nitem = gate_buffer_size()
      do icount = 1,nitem
         call gate_query_from_buffer(icount,
     &                              name,
     &                              from_obj,
     &                              from_identifier,
     &                              node,
     &                              ierror)
         
         call process_gate(0,
     &                     name,
     &                     from_obj,
     &                     from_identifier,
     &                     node)
       end do
       print *,"Number of gates: ", nitem

      nitem = gate_weir_device_buffer_size()
      do icount = 1,nitem
         call gate_weir_device_query_from_buffer(icount,
     &                              gate_name,
     &                              device_name,
     &                              nduplicate,
     &                              width,
     &                              elev,
     &                              height,
     &                              cf_from_node,
     &                              cf_to_node,
     &                              default_op,
     &                              ierror)
         
      struct_name = "weir"
         call process_gate_device(
     &                              gate_name,
     &                              device_name,
     &                              struct_name,
     &                              nduplicate,
     &                              width,
     &                              elev,
     &                              height,
     &                              cf_from_node,
     &                              cf_to_node,
     &                              default_op)
       end do
       print *,"Number of gate weir devices: ", nitem

      nitem = gate_pipe_device_buffer_size()
      do icount = 1,nitem
         call gate_pipe_device_query_from_buffer(icount,
     &                              gate_name,
     &                              device_name,
     &                              nduplicate,
     &                              width,
     &                              elev,
     &                              cf_from_node,
     &                              cf_to_node,
     &                              default_op,
     &                              ierror)
         struct_name = "pipe"
         height = 9999.D0
         call process_gate_device(
     &                              gate_name,
     &                              device_name,
     &                              struct_name,
     &                              nduplicate,
     &                              width,
     &                              elev,
     &                              height,
     &                              cf_from_node,
     &                              cf_to_node,
     &                              default_op)
       end do
       print *,"Number of gate pipe devices: ", nitem

      nitem = transfer_buffer_size()
      do icount = 1,nitem
         call transfer_query_from_buffer(icount,
     &                                  name,
     &                                  from_obj,
     &                                  from_identifier,
     &                                  to_obj,
     &                                  to_identifier,
     &                                  ierror)
         
         call process_transfer(0,name,!todo: get rid of the id argument
     &                         from_obj,
     &                         from_identifier,
     &                         to_obj,
     &                         to_identifier)
       end do
       print *,"Number of transfers: ", nitem      

      

       return
       end subroutine