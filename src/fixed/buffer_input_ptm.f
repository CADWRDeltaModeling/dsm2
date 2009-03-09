      subroutine buffer_input_ptm()
      use input_storage_fortran
      use constants
      
      implicit none
      integer nitem
      integer icount
      integer err
      
      integer node
      integer nparts
      character*8 delay
      character*16 duration
      
      character*32 name
      character*40 from_wb
      character*40 to_wb
      character*16 interval
      character*128 filename
      
      character*40 groupname

      nitem = particle_insertion_buffer_size()
      do icount = 1,nitem
         err=particle_insertion_query_from_buffer(icount,
     &                                            node,
     &                                            nparts,   
     &                                            delay,
     &                                            duration)
         call  process_particle_injection(node,
     &                                    nparts,
     &                                    delay,
     &                                    duration)
      end do
      print *,"Number of particle insertions processed: ", nitem

      nitem = particle_flux_output_buffer_size()
      do icount = 1,nitem
         err=particle_flux_output_query_from_buffer(icount,
     &                                              name,
     &                                              from_wb,
     &                                              to_wb,
     &                                              interval,
     &                                              filename)
         
         call process_particle_flux_output(name,
     &                                     from_wb,
     &                                     to_wb,
     &                                     interval,
     &                                     filename)
      end do
      print *,"Number of particle flux outputs processed: ", nitem
      
      nitem = particle_group_output_buffer_size()
      do icount = 1,nitem
         err=particle_group_output_query_from_buffer(icount,
     &                                               name,
     &                                               groupname,
     &                                               interval,
     &                                               filename)
         
         call process_particle_group_output(name,
     &                                      groupname,
     &                                      interval,
     &                                      filename)
      end do
      print *,"Number of particle group outputs processed: ", nitem

      end subroutine

