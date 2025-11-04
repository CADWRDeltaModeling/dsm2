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
module process_gtm_group_variable

    contains

    subroutine process_group_variable(group_name,     &
                                      constituent,    &
                                      rate_variable,  &
                                      rate_value)
        use common_vars
        implicit none
        character*16, intent(in) :: rate_variable
        character*16, intent(in) :: constituent
        character*32, intent(in) :: group_name
        real(gtm_real), intent(in) :: rate_value
        integer :: group_id, ncc_code, var_code
        integer :: i

        group_id = 0
        ncc_code = 0
        var_code = 0

        call locase(group_name)
        call locase(rate_variable)
        call locase(constituent)
        do i = 1, n_group
            if (group(i)%name.eq.group_name) then
                group_id = group(i)%id
            end if
        end do
        if (group_id.eq.0) then
            write(*,*) group_name," is not specified in GROUP. Please verify the typing!!"
        end if

        if (constituent.eq."do") then
            ncc_code = ncc_do
        elseif (constituent.eq."organic_n") then
            ncc_code = ncc_organic_n
        elseif (constituent.eq."nh3") then
            ncc_code = ncc_nh3
        elseif (constituent.eq."no2") then
            ncc_code = ncc_no2
        elseif (constituent.eq."no3") then
            ncc_code = ncc_no3
        elseif (constituent.eq."organic_p") then
            ncc_code = ncc_organic_p
        elseif (constituent.eq."po4") then
            ncc_code = ncc_po4
        elseif (constituent.eq."algae") then
            ncc_code = ncc_algae
        elseif (constituent.eq."bod") then
            ncc_code = ncc_bod
        elseif (constituent.eq."temp") then
            ncc_code = ncc_temp
        elseif (constituent.eq."ssc") then
            ncc_code = ncc_ssc
        elseif (constituent.eq."turbidity") then
            ncc_code = ncc_turbidity
        elseif (constituent.eq."hgii") then
            ncc_code = ncc_hgii
        elseif (constituent.eq."mehg") then
            ncc_code = ncc_mehg
        elseif (constituent.eq."hg0") then
            ncc_code = ncc_hg0
        elseif (constituent.eq."hgii_s1") then
            ncc_code = ncc_hgii_s1
        elseif (constituent.eq."hgii_s2") then
            ncc_code = ncc_hgii_s2
        elseif (constituent.eq."hgii_s3") then
            ncc_code = ncc_hgii_s3
        else
            write(*,*) "Check group_variable, cannot find a matched constituent!!"
        end if

        if (rate_variable.eq."input") then
            var_code = input
        elseif (rate_variable.eq."decay") then
            var_code = decay
        elseif (rate_variable.eq."settle") then
            var_code = settle
        elseif (rate_variable.eq."benthic") then
            var_code = benthic
        elseif (rate_variable.eq."alg_grow") then
            var_code = alg_grow
        elseif (rate_variable.eq."alg_resp") then
            var_code = alg_resp
        elseif (rate_variable.eq."alg_die") then
            var_code = alg_die
        else
            do i = n_coef + 1, n_coef + n_floating
                if (trim(rate_variable) .eq. trim(coeff_name(i))) var_code = i
            end do
            if (var_code .eq. 0) then
                n_rate_var = n_rate_var + 1
                var_code = n_rate_var
                coeff_name(n_rate_var) = rate_variable
            end if
        end if

        if (((group_id.ne.0).and.(ncc_code.ne.0)).and.(var_code.ne.0)) then
            group_var(ncc_code, var_code, group_id) = rate_value
        else
            write(*,*) "Can't find a matched coefficient name for", rate_variable
        end if


        !rate_var_id = rate_variable_code(rate_variable)
        !if (rate_var_id .eq. miss_val_i)then
        !  write (unit_error,'(a,1x,a)') "Rate variable not recognized:", trim(rate_variable)
        !  call exit(-3)
      !end if
      !ncc_id = ncc_code(constituent)
      !if (ncc_id .eq. miss_val_i)then
      !    write(unit_error,'(a,1x,a)')"Constituent in rate coefficient assignment "//  &
      !    "not recognized:",constituent
      !    call exit(-3)
      !end if
      !groupno = name_to_objno(obj_group, group_name)
	  !if (groupno.lt.0) then
	  !    write(unit_error, '(a,1x,a)') 'Group in rate coefficient assignment: ' //    &
      !      "not recognized: ",group_name
	  !    call exit(-3)
      !end if
	  !if (not(IsAllChannelReservoir(groupArray(groupno)))) then
      !   write(unit_error, '(a)')                            &
      !               "Members of group "//group_name//       &
      !               " are not all channel or reservior"
      !   call exit(-3)
      !end if
      !rate_value=rate_value/24. ! convert per day to per hour

	  !do 200 ichan=1,nchans
	  !do 200 ichan = 1, n_chan
	  !   if (groupContains(groupno,obj_channel,ichan)) then
      !      if ( rcoef_chan(ncc_id,rate_var_id,ichan) .eq. miss_val_r) then
!		        rcoef_chan(ncc_id,rate_var_id,ichan)=rate_value
 !           else
      !          call ncc_code_to_name(ncc_id, constituent)
      !          call rate_variable_code_to_name(rate_var_id, constituent)
      !          write(unit_error,*) "rate coefficient of: ",  trim(rate_variable),                          &
      !                              " for constituent: ",      trim(constituent),                           &
      !                              " is assigned more than once to Channel #", chan_geom(ichan).chan_no,   &
      !                              " in group: ", trim(groupArray(groupno).name)
      !          call exit(-1)
      !    end if
	  !   end if
     !200 end do

   	  !do 300 ires = 1, n_resv
      !    if(GroupContains(groupno,obj_reservoir,ires)) then
      !        rcoef_res(ncc_id,rate_var_id,ires)=rate_value
      !    end if
!300   end do
 !     rate_var_require_flag(ncc_id,rate_var_id)=.true.
      return
    end subroutine

end module