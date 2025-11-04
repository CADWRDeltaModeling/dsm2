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

submodule (mod_fixed) dsm2_fixed_process_rate_coef
    use io_units
    use constants
    use constants_qual

contains
module subroutine process_rate_coef(group_name, &
                                   constituent, &
                                   rate_variable, &
                                   rate_value)
	use groups_data, only: groupArray
    use Groups, only: groupContains,IsAllChannelReservoir
    use mod_name_to_objno
	use rate_coeff_assignment
	use common_qual, only: rcoef_chan, rcoef_res
	use logging
      implicit none
      character*16 rate_variable,constituent

      character*32 group_name
      character errm*128 !todo: this is not good style
      integer :: rate_var_id
      integer :: ncc_id

      integer :: groupno
      integer :: istat
      real*8  :: rate_value
      integer :: ichan,ires

      call locase(group_name)
      call locase(rate_variable)
      call locase(constituent)
      rate_var_id = rate_variable_code(rate_variable)
      if (rate_var_id .eq. miss_val_i)then
          write (unit_error,'(a,1x,a)')"Rate variable not recognized:", &
              trim(rate_variable)
          call exit(-3)
      end if
      ncc_id = ncc_code(constituent)
      if (ncc_id .eq. miss_val_i)then
          write(unit_error,'(a,1x,a)')"Constituent in rate coefficient assignment "// &
              "not recognized:",constituent
          call exit(-3)
      end if
      groupno = name_to_objno(obj_group, group_name)
	if (groupno.lt.0) then
	   write(unit_error, '(a,1x,a)') 'Group in rate coefficient assignment: ' //  &
           "not recognized: ",group_name
	   call exit(-3)
      end if
	if (not(IsAllChannelReservoir(groupArray(groupno)))) then
         write(unit_error, '(a)')  &
             "Members of group "//group_name// &
             " are not all channel or reservior"
         call exit(-3)
      end if
      rate_value=rate_value/24. ! convert per day to per hour
	do 200 ichan=1,nchans
	  if (groupContains(groupArray,groupno,obj_channel,ichan)) then
          if ( rcoef_chan(ncc_id,rate_var_id,ichan) .eq. miss_val_r) then
		      rcoef_chan(ncc_id,rate_var_id,ichan)=rate_value
          else
              call ncc_code_to_name(ncc_id, constituent)
              call rate_variable_code_to_name(rate_var_id, constituent)
                    write(unit_error,*) "rate coefficient of: ", trim(rate_variable), &
                                        " for constituent: ",    trim(constituent),   &
                                        " is assigned more than once to Channel #", chan_geom(ichan).chan_no, &
                                        " in group: ", trim(groupArray(groupno).name)
              call exit(-1)
          end if
	  end if
200   end do

	do 300 ires = 1, nreser
        if(GroupContains(groupArray,groupno,obj_reservoir,ires)) then
          rcoef_res(ncc_id,rate_var_id,ires)=rate_value
        end if
300   end do
      rate_var_require_flag(ncc_id,rate_var_id)=.true.
      return
end subroutine

integer function rate_variable_code(name)
      implicit none
      character*16 name
      rate_variable_code = miss_val_i
      if (trim(name) .eq. "decay") then
          rate_variable_code=decay
      elseif(trim(name) .eq. "settle") then
          rate_variable_code=settle
      elseif(trim(name) .eq. "benthic") then
          rate_variable_code=benthic
      elseif(trim(name) .eq. "alg_grow") then
          rate_variable_code=alg_grow
      elseif(trim(name) .eq. "alg_resp") then
          rate_variable_code=alg_resp
      elseif(trim(name) .eq. "alg_die") then
          rate_variable_code=alg_die
      end if
      return
end function

subroutine rate_variable_code_to_name(id, name)
      implicit none
      integer id
      character*(16) name
      name = " "
      if (id .eq. decay) then
          name="decay"
      elseif(id .eq. settle) then
          name="settle"
      elseif(id .eq. benthic ) then
          name="benthic"
      elseif(id .eq. alg_grow) then
          name="alg_grow"
      elseif(id .eq. alg_resp) then
          name = "alg_resp"
      elseif(id .eq. alg_die) then
          name = "alg_die"
      else
          write(unit_error,*) "Unknown constituent code in rate_variable_code_to_name"
          call exit(-3)
      end if
      return
end subroutine


integer function ncc_code(name)

      implicit none
      character*16 name
      ncc_code = miss_val_i
      if (name .eq. "do") then
          ncc_code=ncc_do
      elseif(name .eq. "organic_n") then
          ncc_code=ncc_organic_n
      elseif(name .eq. "nh3") then
          ncc_code=ncc_nh3
      elseif(name .eq. "no2") then
          ncc_code=ncc_no2
      elseif(name .eq. "no3") then
          ncc_code=ncc_no3
      elseif(name .eq. "organic_p") then
          ncc_code=ncc_organic_p
      elseif(name .eq. "po4") then
          ncc_code=ncc_po4
      elseif(name .eq. "algae") then
          ncc_code=ncc_algae
      elseif(name .eq. "bod") then
          ncc_code=ncc_bod
      elseif(name .eq. "temp") then
          ncc_code=ncc_temp
      end if
      return
end function

subroutine ncc_code_to_name(id, name)
      implicit none
      character*(16) name
      integer id

      name = " "
      if (id .eq. ncc_do) then
          name="do"
      elseif(id .eq. ncc_organic_n) then
          name="organic_n"
      elseif(id .eq. ncc_nh3 ) then
          name="nh3"
      elseif(id .eq. ncc_no2) then
          name="ncc_o2"
      elseif(id .eq. ncc_no3) then
          name = "no3"
      elseif(id .eq. ncc_organic_p) then
          name = "organic_p"
      elseif(id .eq. ncc_po4) then
          name = "po4"
      elseif(id .eq. ncc_algae) then
          name = "algae"
      elseif(id .eq. ncc_bod) then
          name = "bod"
      elseif(id .eq. ncc_temp) then
          name = "temp"
      else
          write(unit_error,*) "Unknown constituent code in ncc_code_to_name"
          call exit(-3)
      end if
      return
end subroutine


end submodule dsm2_fixed_process_rate_coef