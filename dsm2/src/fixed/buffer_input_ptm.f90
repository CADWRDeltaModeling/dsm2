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

subroutine buffer_input_ptm()
      use input_storage_fortran
      use constants
      
      implicit none
      integer nitem
      integer icount
      integer :: ierror = 0
      
      integer node
      integer nparts
      character*8 delay
      character*16 duration
      
      character*32 groupname
      character*16 member_type
      character*32 pattern
      integer*4 obj_type
      
      character*32 name
      character*40 from_wb
      character*40 to_wb
      character*16 interval
      character*128 filename
      
      character*40 group_name
      
      character*32 resname
      character*32 at_wb
      character*8 fillin
      character*392 inpath
      
      integer*4, external :: obj_type_code
      


      nitem = particle_insertion_buffer_size()
      do icount = 1,nitem
         call particle_insertion_query_from_buffer(icount, &
                                                   node, &
                                                   nparts, &
                                                   delay, &
                                                   duration, &
                                                   ierror)
         call  process_particle_injection(node, &
                                         nparts, &
                                         delay, &
                                         duration)
      end do
      print *,"Number of particle insertions processed: ", nitem

      nitem = particle_flux_output_buffer_size()
      do icount = 1,nitem
         call particle_flux_output_query_from_buffer(icount, &
                                                   name, &
                                                   from_wb, &
                                                   to_wb, &
                                                   interval, &
                                                   filename, &
                                                   ierror)
         
         call process_particle_flux_output(name, &
                                          from_wb, &
                                          to_wb, &
                                          interval, &
                                          filename)
      end do
      print *,"Number of particle flux outputs processed: ", nitem
      
      nitem = particle_group_output_buffer_size()
      do icount = 1,nitem
         call particle_group_output_query_from_buffer(icount, &
                                                    name, &
                                                    group_name, &
                                                    interval, &
                                                    filename, &
                                                    ierror)
         
         call process_particle_group_output(name, &
                                           group_name, &
                                           interval, &
                                           filename)
      end do
      print *,"Number of particle group outputs processed: ", nitem

      nitem = particle_filter_buffer_size()
      do icount = 1,nitem
         call particle_filter_query_from_buffer (icount, &
                                                name, &
                                                node, &
                                                at_wb, &
                                                fillin, &
                                                filename, &
                                                inpath, &
                                                ierror)
         call process_particle_filter(name, &
                                     node, &
                                     at_wb, &
                                     fillin, &
                                     filename, &
                                     inpath)
      end do
      print *,"Number of particle filters processed: ", nitem

      nitem = particle_res_filter_buffer_size()
      do icount = 1,nitem
         call particle_res_filter_query_from_buffer (icount, &
                                                    name, &
                                                    resname, &
                                                    at_wb, &
                                                    fillin, &
                                                    filename, &
                                                    inpath, &
                                                    ierror)
         call process_particle_res_filter(name, &
                                         resname, &
                                         at_wb, &
                                         fillin, &
                                         filename, &
                                         inpath)
      end do
      print *,"Number of particle reservoir filters processed: ", nitem
      
end subroutine

