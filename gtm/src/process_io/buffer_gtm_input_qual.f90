!<license>
!    Copyright (C) 2017 State of California,
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
    
    !> Buffer input for temporal data (climate data, node concentration, 
    !> reservoir concentration) and spatial data (rate coefficient)
    subroutine buffer_input_qual()
        use input_storage_fortran
        use process_gtm_input_climate 
        use process_gtm_node_conc
        use process_gtm_reservoir_conc
        use process_gtm_group_variable
        use process_gtm_input_time_series
        use common_dsm2_vars, only: pathinput, n_inputpaths,infilenames,     &
                                    n_dssfiles, indssfiles, ifltab_in,       &
                                    n_outdssfiles, outdssfiles, ifltab_out,  &
                                    outfilenames
        use gtm_dss, only: get_dss_each_npath
        use gtm_precision
        use common_variables, only: n_var, constituents_tmp, n_input_ts, n_node_ts,                            &
                                    n_sediment, n_sediment_bc, sediment, sediment_bc, ssc_index, run_sediment, &
                                    group_var, run_mercury, mercury_start_ivar, mercury_ivar
        implicit none
        integer :: nitem_climate, nitem_node_conc, nitem_resv_conc, nitem_input_time_series
        integer :: nitem_group_variable
        character*128 :: filename
        integer :: icount
        character*32 :: name
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
        integer :: i      
        character*(16) :: sdate,edate  
        
        ! variables
        integer :: nvar
        integer :: nts
        type const_t
            integer :: conc_no                        !< constituent id
            character*32 :: name = ''                 !< constituent name
            character*32 :: use_module = ' '          !< use module
            logical :: conservative = .true.          !< conservative
            logical :: simulate = .true.              !< simulate or not
        end type     
        type(const_t) :: const(20)
        character*32 :: tmp_const

        group_var = LARGEREAL
        nitem_group_variable = group_variable_buffer_size()
        do icount = 1,nitem_group_variable
           call group_variable_query_from_buffer(icount,       &
                                                 group_name,   &
                                                 constituent,  &
                                                 variable,     &
                                                 value,        &
                                                 ierror) 
           sign = 1
           call process_group_variable(group_name,     &
                                       constituent,    &
                                       variable,       &
                                       value) 
        end do
        print *,"Number of group variables processed: ", nitem_group_variable

        nitem_climate = input_climate_buffer_size()
        nitem_node_conc = node_concentration_buffer_size()
        nitem_resv_conc = reservoir_concentration_buffer_size()      
        nitem_input_time_series = input_time_series_buffer_size()        
        n_inputpaths = nitem_climate + nitem_node_conc + nitem_resv_conc + nitem_input_time_series
        n_input_ts = nitem_input_time_series
        n_node_ts = n_inputpaths - n_input_ts
        allocate(pathinput(n_inputpaths))     
        
        nvar = 0 
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
           if (icount.eq.1 .and. nvar.eq.0)then
               nvar = 1
               const(nvar)%conc_no = 1
               const(nvar)%name = variable
           else
               do i = 1, nvar
                   if (trim(variable).eq.trim(const(i)%name)) then
                       exit
                   else
                       tmp_const = variable
                       if (i.eq.nvar) then
                           nvar = nvar + 1
                           const(nvar)%conc_no = nvar
                           const(nvar)%name = tmp_const
                       end if
                   end if
               end do           
           end if
        end do

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
            call process_input_reservoir(name,       &
                                         resname,    &
                                         variable,   & 
                                         sign,       &
                                         fillin,     &
                                         filename,   &
                                         inpath)
         end do
         print *,"Number of reservoir concentration inputs processed: ", nitem_resv_conc


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

        do icount = 1,nitem_input_time_series
             call input_time_series_query_from_buffer(icount,     &
                                                      name,       &
                                                      variable,   &
                                                      fillin,     &
                                                      filename,   &
                                                      inpath,     &
                                                      ierror)
             sign = 1
             call process_input_time_series(name,        &
                                            variable,    &
                                            fillin,      &
                                            filename,    &
                                            inpath)
        end do
        print *,"Number of input time series processed: ", nitem_input_time_series
 
        n_var = nvar + n_sediment
        allocate(constituents_tmp(n_var))
        do i = 1, nvar      
            constituents_tmp(i)%conc_no = i
            constituents_tmp(i)%name = const(i)%name
            constituents_tmp(i)%use_module = const(i)%use_module
            constituents_tmp(i)%conservative = const(i)%conservative
            constituents_tmp(i)%simulate = const(i)%simulate
            if (trim(constituents_tmp(i)%name).eq.'ssc') then
                ssc_index = i
                constituents_tmp(i)%use_module = ''
                constituents_tmp(i)%conservative = .false.  
                constituents_tmp(i)%simulate = .false.
            elseif (trim(constituents_tmp(i)%name).eq.'turbidity') then
                constituents_tmp(i)%use_module = 'turbidity'
                constituents_tmp(i)%conservative = .false.
            elseif (trim(constituents_tmp(i)%name) =='hgii') then
                mercury_ivar(1) = i
                constituents_tmp(i)%use_module = 'mercury'
                constituents_tmp(i)%conservative = .false.
            elseif (trim(constituents_tmp(i)%name) =='mehg') then
                mercury_ivar(2) = i
                constituents_tmp(i)%use_module = 'mercury'
                constituents_tmp(i)%conservative = .false.                
            elseif (trim(constituents_tmp(i)%name) =='hg0') then
                mercury_ivar(3) = i
                constituents_tmp(i)%use_module = 'mercury'
                constituents_tmp(i)%conservative = .false.
            elseif (trim(constituents_tmp(i)%name) =='hgii_s1') then 
                mercury_ivar(4) = i
                constituents_tmp(i)%use_module = 'mercury'
                constituents_tmp(i)%conservative = .false.
            elseif (trim(constituents_tmp(i)%name) =='hgii_s2') then
                mercury_ivar(5) = i
                constituents_tmp(i)%use_module = 'mercury'
                constituents_tmp(i)%conservative = .false.
            elseif (trim(constituents_tmp(i)%name) =='hgii_s3') then
                mercury_ivar(6) = i
                constituents_tmp(i)%use_module = 'mercury'
                constituents_tmp(i)%conservative = .false.
            end if
        end do
        do i = 1, n_sediment
            constituents_tmp(nvar+i)%conc_no = nvar + i
            constituents_tmp(nvar+i)%name = trim(sediment(i)%composition)
            constituents_tmp(nvar+i)%use_module = 'sediment'
            constituents_tmp(nvar+i)%conservative = .false.
        end do
        if (n_sediment.gt.0) run_sediment = .true.
        print *,"Number of constituents processed: ", n_var
     
        allocate(indssfiles(n_dssfiles))
        indssfiles = infilenames
        allocate(ifltab_in(600, n_dssfiles))
        call get_dss_each_npath         
        return

    end subroutine      
      
end module      