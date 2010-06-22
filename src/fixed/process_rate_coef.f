C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    The Delta Simulation Model 2 (DSM2) is free software: 
C!    you can redistribute it and/or modify
C!    it under the terms of the GNU General Public License as published by
C!    the Free Software Foundation, either version 3 of the License, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public License for more details.

C!    You should have received a copy of the GNU General Public License
C!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
C!</license>

      subroutine process_rate_coef(group_name,
     &                             constituent,
     &                             rate_variable,
     &                             rate_value)            
	use Groups, only: groupContains,IsAllChannelReservoir,groupArray
	use rate_coeff_assignment
	use common_qual
	use constants
	use io_units
	use logging      
      implicit none
      character*16 rate_variable,constituent
      
      character*32 group_name
      character errm*128 !todo: this is not good style      
      integer :: rate_var_id
      integer :: ncc_id

      integer :: groupno
      integer :: istat
      integer, external :: name_to_objno
      real*8  :: rate_value
      integer,external :: rate_variable_code
      integer,external :: ncc_code
      integer :: ichan,ires

      call locase(group_name)      
      call locase(rate_variable)
      call locase(constituent)
      rate_var_id = rate_variable_code(rate_variable)
      if (rate_var_id .eq. miss_val_i)then
          write (unit_error,'(a,1x,a)')"Rate variable not recognized:",
     &    trim(rate_variable)
          call exit(-3)
      end if
      ncc_id = ncc_code(constituent)
      if (ncc_id .eq. miss_val_i)then
          write(unit_error,'(a,1x,a)')"Constituent in rate coefficient assignment "//
     &    "not recognized:",constituent
          call exit(-3)
      end if
      groupno = name_to_objno(obj_group, group_name)
	if (groupno.lt.0) then
	   write(unit_error, '(a,1x,a)') 'Group in rate coefficient assignment: ' // 
     &     "not recognized: ",group_name
	   call exit(-3)
      end if
	if (not(IsAllChannelReservoir(groupArray(groupno)))) then
         write(unit_error, '(a)') 
     &                "Members of group "//group_name//
     &                " are not all channel or reservior"
         call exit(-3)
      end if
      rate_value=rate_value/24. ! convert per day to per hour
	do 200 ichan=1,nchans 
	  if (groupContains(groupno,obj_channel,ichan)) then
          if ( rcoef_chan(ncc_id,rate_var_id,ichan) .eq. miss_val_r) then 
		      rcoef_chan(ncc_id,rate_var_id,ichan)=rate_value
          else
              call ncc_code_to_name(ncc_id, constituent)
              call rate_variable_code_to_name(rate_var_id, constituent)
                    write(unit_error,*) "rate coefficient of: ",  trim(rate_variable), 
     &                                  " for constituent: ",      trim(constituent),                              
     &                                  " is assigned more than once to Channel #", chan_geom(ichan).chan_no, 
     &                                  " in group: ", trim(groupArray(groupno).name)
              call exit(-1)
          end if
	  end if
200   end do
            
	do 300 ires = 1, nreser
        if(GroupContains(groupno,obj_reservoir,ires)) then
          rcoef_res(ncc_id,rate_var_id,ires)=rate_value
        end if
300   end do
      rate_var_require_flag(ncc_id,rate_var_id)=.true.
      return
      end subroutine
      
