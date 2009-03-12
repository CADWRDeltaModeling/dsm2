      subroutine buffer_input_qual()
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

c======================== Input and output ======================
      nitem = rate_coefficient_buffer_size()
      do icount = 1,nitem
         call rate_coefficient_query_from_buffer(icount,
     &                                          group_name,
     &                                          constituent,
     &                                          variable,
     &                                          value,
     &                                          ierror) 

         sign = 1

         call process_rate_coef(group_name,
     &                          constituent,
     &                          variable,
     &                          value)
 
      end do
      print *,"Number of rate coefficients processed: ", nitem




      nitem = input_climate_buffer_size()
      do icount = 1,nitem
         call input_climate_query_from_buffer(icount,
     &                                       name,
     &                                       variable,
     &                                       fillin,
     &                                       filename,
     &                                       inpath,
     &                                       ierror) 

         sign = 1

         call process_input_climate(name,
     &                              variable,
     &                              sign,
     &                              fillin,
     &                              filename,
     &                              inpath)
 
      end do
      print *,"Number of climate inputs processed: ", nitem


      nitem = node_concentration_buffer_size()
      do icount = 1,nitem
         call node_concentration_query_from_buffer(icount,
     &                                    name,
     &                                    node,
     &                                    variable,
     &                                    fillin,   
     &                                    filename,
     &                                    inpath,
     &                                    ierror)
      rolename="inflow"
      sign=0
         call process_input_node(name,
     &                           node,
     &                           variable,     
     &                           sign,
     &                           rolename,
     &                           fillin,   
     &                           filename,
     &                           inpath)

      end do
      print *,"Number of node concentration inputs processed: ", nitem

      nitem = reservoir_concentration_buffer_size()
      do icount = 1,nitem
         call reservoir_concentration_query_from_buffer(icount,
     &                                    name,
     &                                    resname,
     &                                    variable,
     &                                    fillin,   
     &                                    filename,
     &                                    inpath,
     &                                    ierror)
      sign=0
         call process_input_reservoir(name,
     &                               resname,
     &                               variable,     
     &                               sign,
     &                               fillin,   
     &                               filename,
     &                               inpath)

      end do
      print *,"Number of reservoir concentration inputs processed: ", nitem


      end subroutine