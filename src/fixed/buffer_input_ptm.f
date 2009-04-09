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

      integer*4, external :: obj_type_code
      


      nitem = particle_insertion_buffer_size()
      do icount = 1,nitem
         call particle_insertion_query_from_buffer(icount,
     &                                             node,
     &                                             nparts,   
     &                                             delay,
     &                                             duration,
     &                                             ierror)
         call  process_particle_injection(node,
     &                                    nparts,
     &                                    delay,
     &                                    duration)
      end do
      print *,"Number of particle insertions processed: ", nitem

      nitem = particle_flux_output_buffer_size()
      do icount = 1,nitem
         call particle_flux_output_query_from_buffer(icount,
     &                                              name,
     &                                              from_wb,
     &                                              to_wb,
     &                                              interval,
     &                                              filename,
     &                                              ierror)
         
         call process_particle_flux_output(name,
     &                                     from_wb,
     &                                     to_wb,
     &                                     interval,
     &                                     filename)
      end do
      print *,"Number of particle flux outputs processed: ", nitem
      
      nitem = particle_group_output_buffer_size()
      do icount = 1,nitem
         call particle_group_output_query_from_buffer(icount,
     &                                               name,
     &                                               group_name,
     &                                               interval,
     &                                               filename,
     &                                               ierror)
         
         call process_particle_group_output(name,
     &                                      group_name,
     &                                      interval,
     &                                      filename)
      end do
      print *,"Number of particle group outputs processed: ", nitem

      end subroutine

