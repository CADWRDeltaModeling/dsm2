!<license>
!    Copyright (C) 2015 State of California,
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
        !use process_gtm_rate_coef
        use common_dsm2_vars, only: pathinput, n_inputpaths,infilenames,     &
                                    n_dssfiles, indssfiles, ifltab_in,       &
                                    n_outdssfiles, outdssfiles, ifltab_out,  &
                                    outfilenames
        use gtm_dss, only: get_dss_each_npath
        use common_variables, only: n_var, constituents, n_sediment, n_sediment_bc, sediment, sediment_bc, ssc_index
      
        implicit none
        integer :: nitem_climate, nitem_node_conc, nitem_resv_conc
        integer :: nitem_rate_coeff
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
        integer :: i      
        character*(16) :: sdate,edate  
        
        ! variables
        integer :: nvar
        type const_t
            integer :: conc_no                        !< constituent id
            character*32 :: name = ''                 !< constituent name
        end type     
        type(const_t) :: const(20)
        character*32 :: tmp_const

        nitem_rate_coeff = rate_coefficient_buffer_size()
        do icount = 1,nitem_rate_coeff
           call rate_coefficient_query_from_buffer(icount,      &
                                                  group_name,   &
                                                  constituent,  &
                                                  variable,     &
                                                  value,        &
                                                  ierror) 
           sign = 1
        !   call process_rate_coef(group_name,     &
        !                          constituent,    &
        !                          variable,       &
        !                          value) 
        end do
        print *,"Number of rate coefficients processed: ", nitem_rate_coeff


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
           if (icount.eq.1)then
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

        n_var = nvar + n_sediment
        allocate(constituents(n_var))
        do i = 1, nvar      
            constituents(i)%conc_no = i
            constituents(i)%name = const(i)%name
            if (trim(constituents(i)%name).eq.'ssc') then
                ssc_index = i
                constituents(i)%use_module = ''
                constituents(i)%conservative = .false.  
                constituents(i)%method = 0 
            end if
        end do
        do i = 1, n_sediment
            constituents(nvar+i)%conc_no = nvar + i
            constituents(nvar+i)%name = trim(sediment(i)%composition)
            if (trim(sediment(i)%method).eq.'nc') then
                constituents(nvar+i)%method = 1
            elseif (trim(sediment(i)%method).eq.'c') then
                constituents(nvar+i)%method = 2
            elseif (trim(sediment(i)%method).eq.'o') then 
                constituents(nvar+i)%method = 3
            end if
            constituents(nvar+i)%use_module = 'sediment'
            constituents(nvar+i)%grain_size = sediment(i)%grain_size
            constituents(nvar+i)%conservative = .false.
        end do
                
        print *,"Number of constituents processed: ", n_var


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

         allocate(indssfiles(n_dssfiles))
         indssfiles = infilenames
         allocate(ifltab_in(600, n_dssfiles))
         call get_dss_each_npath         
         return

    end subroutine      
      
end module      