!<license>
!    Copyright (C) 2013 State of California,
!    Department of Water Resources.
!    This file is part of DSM2-GTM.
!
!    The Delta Simulation Model 2 (DSM2) - General Transport Model (GTM) 
!    is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    DSM2 is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!</license>

!>@ingroup process_io
module buffer_gtm_input_qual

    contains
    
    subroutine buffer_input_qual()
        use input_storage_fortran
        use process_gtm_input_climate 
        use process_gtm_node_conc
        use common_dsm2_vars, only: pathinput, n_inputpaths,infilenames, &
                                    n_dssfiles, indssfiles, ifltab_in
      
        implicit none
        integer :: nitem_climate, nitem_node_conc, nitem_resv_conc
        character*(128) :: filename
        integer :: icount
        character*(32) :: name
        character*8 :: model,filetype,io
        character*16 :: interval
        character*128 :: iofile
        integer :: ierror = 0

        ! input_node
        character*32 :: rolename 

        ! output_channel
        integer :: channo
        character*8 :: distance
        integer :: idistance
        character*16 :: variable, perop
        character*32 :: sourcegroup
      
        character*32 :: group_name
        character*16 :: constituent
        real*8  :: value

        integer :: channel
        character*32 ::resname
        character*8 :: cdist
        real*8 :: stage
        real*8 :: flow
      
      
        ! output_reservoir
        character*32 :: reservoir
        character*80 :: inpath
        character*8  :: fillin
        character*8  :: node_str
        integer :: sign
        integer :: node      
      
        ! output_gate
        character*32 :: gate, device
        character*(16) :: sdate,edate  

        !======================== Input and output ======================
        !nitem = rate_coefficient_buffer_size()
        !do icount = 1,nitem
        !   call rate_coefficient_query_from_buffer(icount,      &
        !                                          group_name,   &
        !                                          constituent,  &
        !                                          variable,     &
        !                                          value,        &
        !                                          ierror) 
        !   sign = 1
        !   call process_rate_coef(group_name,     &
        !                          constituent,    &
        !                          variable,       &
        !                          value) 
        !end do
        !print *,"Number of rate coefficients processed: ", nitem


        nitem_climate = input_climate_buffer_size()
        nitem_node_conc = node_concentration_buffer_size()
        nitem_resv_conc = reservoir_concentration_buffer_size()      
        n_inputpaths = nitem_climate + nitem_node_conc + nitem_resv_conc
        allocate(pathinput(n_inputpaths))
      
        do icount = 1,nitem_climate
           call input_climate_query_from_buffer(icount,     &
                                                name,       &
                                                variable,   &
                                                fillin,     &
                                                filename,   &
                                                inpath,     &
                                                ierror)
           sign = 1
           call process_input_climate(name,        &
                                      variable,    &
                                      fillin,      &
                                      filename,    &
                                      inpath)
 
        end do
        print *,"Number of climate inputs processed: ", nitem_climate

     
        do icount = 1,nitem_node_conc
           call node_concentration_query_from_buffer(icount,    &
                                                     name,      &
                                                     node,      &
                                                     variable,  &
                                                     fillin,    &
                                                     filename,  &
                                                     inpath,    &
                                                     ierror)
                                                            
           call process_node_conc(name,       &
                                  node,       &
                                  variable,   &    
                                  fillin,     &
                                  filename,   &
                                  inpath)
        end do
        print *,"Number of node concentration inputs processed: ", nitem_node_conc


        do icount = 1,nitem_resv_conc
            call reservoir_concentration_query_from_buffer(icount,    &
                                                           name,      &
                                                           resname,   &
                                                           variable,  &
                                                           fillin,    &
                                                           filename,  &
                                                           inpath,    &
                                                           ierror)
            sign=0
         !   call process_input_reservoir(name,       &
         !                                resname,    &
         !                                variable,   & 
         !                                sign,       &
         !                                fillin,     &
         !                                filename,   &
         !                                inpath)
         end do
         print *,"Number of reservoir concentration inputs processed: ", nitem_resv_conc

         !nitem = output_channel_source_track_buffer_size()
         !do icount = 1,nitem
         !   call output_channel_source_track_query_from_buffer(icount,       &
         !                                                      name,         &
         !                                                      channo,       &
         !                                                      distance,     &
         !                                                      variable,     &
         !                                                      sourcegroup,  &  
         !                                                      interval,     &
         !                                                      perop,        &
         !                                                      filename,     &
         !                                                      ierror)
         !   call locase(sourcegroup)
         !   if (sourcegroup .eq. "none")sourcegroup = ""
         !   call locase(distance)
         !   if (distance(:6) .eq. "length") then 
         !      idistance = chan_length
         !   else 
         !      read(distance,'(i)',err=120)idistance
         !   end if
         !   call process_output_channel(name,           &
         !                               channo,         &
         !                               idistance,      &
         !                               variable,       &
         !                               interval,       &
         !                               perop,          &
         !                               sourcegroup,    &
         !                               filename)
         !end do
         !print *,"Number of concentration (source track) channel output requests: ", nitem


         !nitem = output_reservoir_source_track_buffer_size()
         !do icount = 1,nitem
         !   call output_reservoir_source_track_query_from_buffer(icount,       &
         !                                                        name,         &
         !                                                        reservoir,    &
         !                                                        variable,     &
         !                                                        sourcegroup,  &      
         !                                                        interval,     &
         !                                                        perOp,        &
         !                                                        filename,     &
         !                                                        ierror) 
         !if (sourcegroup .eq. "none")sourcegroup = ""
         !     call process_output_reservoir(name,          &
         !                                   reservoir,     &
         !                                   miss_val_i,    &
         !                                   variable,      &
         !                                   interval,      &
         !                                   perOp,         &
         !                                   sourceGroup,   &
         !                                   filename) 
         !end do
         !print *,"Number of reservoir concentration (source track) output requests: ", nitem
      
         call get_dss_each_npath
         allocate(indssfiles(n_dssfiles))
         indssfiles = infilenames
         allocate(ifltab_in(600, n_dssfiles))
         return

!120   write(unit_error,*)"Failed to convert channel length from text to integer:" /   &
!         "Valid entries are an integer or 'length' (case sensitive)" /                &
!         "Output name: ", name,                                                       &
!         "Channel: ",channo, ", " , "Distance: " , distance
!      call exit(-3)
!      return
    end subroutine
        

    !> Create DSS input pathnames, check for sign change for each path
    subroutine get_dss_each_npath()
    
        use gtm_dss
        use common_dsm2_vars, only: pathinput, n_inputpaths, miss_val_r
        use common_variables, only: unit_error
        
        implicit none

        integer :: p
        
        npthsin_min15 = 0
        npthsin_hour1 = 0
        npthsin_day1 = 0
        npthsin_week1 = 0
        npthsin_month1 = 0
        npthsin_year1 = 0
        npthsin_irr = 0

        do p = 1,n_inputpaths
         if (pathinput(p).no_intervals .eq. 1 .and. pathinput(p).interval .eq. '15min') then 
            npthsin_min15 = npthsin_min15 + 1
            pathinput(p).intvl_path = npthsin_min15
            ptin_min15(npthsin_min15) = p
         else if (pathinput(p).no_intervals .eq. 1 .and. pathinput(p).interval(:5) .eq. '1hour') then
            npthsin_hour1=npthsin_hour1+1
            pathinput(p).intvl_path=npthsin_hour1
            ptin_hour1(npthsin_hour1)=p
         else if (pathinput(p).no_intervals .eq. 1 .and. pathinput(p).interval(:4) .eq. '1day') then
            npthsin_day1=npthsin_day1+1
            pathinput(p).intvl_path=npthsin_day1
            ptin_day1(npthsin_day1)=p
         else if (pathinput(p).no_intervals .eq. 1 .and. pathinput(p).interval(:5) .eq. '1week') then
            npthsin_week1=npthsin_week1+1
            pathinput(p).intvl_path=npthsin_week1
            ptin_week1(npthsin_week1)=p
         else if (pathinput(p).no_intervals .eq. 1 .and. pathinput(p).interval(:4) .eq. '1mon') then
            npthsin_month1=npthsin_month1+1
            pathinput(p).intvl_path=npthsin_month1
            ptin_month1(npthsin_month1)=p
         else if ((pathinput(p).no_intervals .eq. 1 .and. pathinput(p).interval(:5) .eq. '1year') .or. &
                   pathinput(p).constant_value .ne. miss_val_r) then
            pathinput(p).no_intervals = 1
            pathinput(p).interval = 'year'
            npthsin_year1 = npthsin_year1 + 1
            pathinput(p).intvl_path = npthsin_year1
            ptin_year1(npthsin_year1) = p
         else if (pathinput(p).interval(:3) .eq. 'ir-') then ! irregular interval
            npthsin_irr = npthsin_irr + 1
            pathinput(p).intvl_path = npthsin_irr
            ptin_irr(npthsin_irr) = p
         else                   ! unrecognized interval
            write(unit_error,650) 'input', pathinput(p).no_intervals, trim(pathinput(p).interval)
            write(*,*) "Error in get_dss_each_npath()"
         endif
         call upcase(pathinput(p).path) ! convert to upper case
      end do
 650  format(/'Unrecognized ',a,' data interval: ',i4,1x,a)
 
      return
    end subroutine  
      
      
end module      