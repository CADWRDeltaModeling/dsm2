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

      subroutine buffer_input_common()      
      use input_storage_fortran
      use constants
      use io_units
      use groups, only:convertgrouppatternstomembers
      
      implicit none
      integer :: nitem
      character*(128) filename
      integer :: icount
      character*(32) name
      character*8,model,filetype,io
      character*16 interval
      character*128 iofile
      integer :: ierror = 0
      ! input_node

      character*32 :: rolename 


      ! output_channel
      integer channo
      character*8  distance
      integer      idistance
      character*16 variable,
     &                perop
      character*32 :: sourcegroup
      
      character*32 :: group_name
      character*16 :: constituent
      real*8  :: value

      integer :: channel
      character*32 ::resname
      character*8 cdist
      real*8 stage
      real*8 flow
      
      
      ! output_reservoir
      character*32 reservoir
      character*80 inpath
      character*8  fillin
      character*8  node_str
      integer      sign
      integer node      
      
       ! output_gate
      character*32 gate, device
      character*(16) :: sdate,edate

      character*32 groupname
      character*16 member_type
      character*256 pattern
      integer*4 obj_type
      integer*4, external :: obj_type_code

      nitem = group_buffer_size()
      do icount = 1,nitem
         call group_query_from_buffer(icount,
     &                                name,
     &                                ierror)
         call  process_group(name,
     &                       icount)
      end do
      print *,"Number of groups processed: ", nitem

      nitem = group_member_buffer_size()
      do icount = 1,nitem
         call group_member_query_from_buffer(icount,
     &                                       groupname,
     &                                       member_type,
     &                                       pattern,
     &                                       ierror)
         obj_type = obj_type_code(member_type)
         call  process_group_member(groupname,
     &                              obj_type,
     &                              pattern)
      end do

      print *,"Number of group members processed: ", nitem
c---- convert group members from patterns to actual objects&indexes
c     This must come after tidefile is loaded
	
      call ConvertGroupPatternsToMembers



      nitem = io_file_buffer_size()
      do icount = 1,nitem
         call io_file_query_from_buffer(icount,model,filetype,io,interval,iofile,ierror)
         call process_io_file(model,filetype,io,interval,iofile)
      end do
      print *,"Number of iofiles: ", nitem



      nitem = output_channel_buffer_size()
      do icount = 1,nitem
         call output_channel_query_from_buffer(icount,
     &                                        name,
     &                                        channo,
     &                                        distance,
     &                                        variable,
     &                                        interval,
     &                                        perop,
     &                                        filename,ierror)
         sourcegroup = ""
         call locase(distance)
         if (distance(:6) .eq. "length") then 
            idistance = chan_length
         else 
            read(distance,*,err=120) idistance
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
         call output_reservoir_query_from_buffer(icount,
     &                                        name,
     &                                    reservoir,
     &                                    node_str,
     &                                    variable,
     &                                    interval,
     &                                    perOp,
     &                                    filename,
     &                                    ierror) 
         sourcegroup = ""
         if (node_str .eq. "none") then
             node=miss_val_i
         else
             node=miss_val_i
             read(node_str,*,err=118)node
         end if
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
         call output_gate_query_from_buffer(icount,
     &                                     name,
     &                                     gate,
     &                                     device,
     &                                     variable,
     &                                     interval,
     &                                     perop,
     &                                     filename,
     &                                     ierror)

         call process_output_gate(name,
     &                            gate,
     &                            device,
     &                            variable,
     &                            interval,
     &                            perop,
     &                            filename)
      end do
      print *,"Number of gate output requests: ", nitem

      return  ! normal return
      
118   write(unit_error,'(a,//,a)')"Failed to convert reservoir node from text to integer",
     &   "Valid entries are an integer or 'none' (case sensitive)"
      call exit(-3)

120   write(unit_error,'(a,//,a,//,a,a,//,a,i5,//,a,a)')
     &   "Failed to convert channel length from text to integer. ",
     &   "Valid entries are an integer or 'length' (case sensitive)",
     &   "Output name: ", name,
     &   "Channel: ",channo, ", " , "Distance: " , distance

      call exit(-3)
      end subroutine

