
      subroutine buffer_input_common()      
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
     &                                    node_str,
     &                                    variable,
     &                                    interval,
     &                                    perOp,
     &                                    filename) 
         sourcegroup = ""
         if (node_str .eq. "none")node=miss_val_i
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

      nitem = output_channel_concentration_buffer_size()
      do icount = 1,nitem
         err=output_channel_concentration_query_from_buffer(icount,
     &                                        name,
     &                                        channo,
     &                                        distance,
     &                                        variable,
     &                                        sourcegroup,    
     &                                        interval,
     &                                        perop,
     &                                        filename)
         if (sourcegroup .eq. "none")sourcegroup = ""
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



      nitem = output_reservoir_concentration_buffer_size()
      do icount = 1,nitem
         err=output_reservoir_concentration_query_from_buffer(icount,
     &                                    name,
     &                                    reservoir,
     &                                    variable,
     &                                    sourcegroup,         
     &                                    interval,
     &                                    perOp,
     &                                    filename) 
      if (sourcegroup .eq. "none")sourcegroup = ""

      call process_output_reservoir(name,
     &                                    reservoir,
     &                                    miss_val_i,
     &                                    variable,
     &                                    interval,
     &                                    perOp,
     &                                    sourceGroup,
     &                                    filename) 
      end do
      print *,"Number of reservoir output requests: ", nitem


      end subroutine