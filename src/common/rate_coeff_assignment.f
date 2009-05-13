C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public !<license as published by
C!    the Free Software Foundation, either version 3 of the !<license, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public !<license for more details.

C!    You should have received a copy of the GNU General Public !<license
C!    along with DSM2.  If not, see <http://www.gnu.org/!<licenses/>.
C!</license>


c      defines a local structure for rate coefficient assignment and some 
c      utility functions Jon 4/12/06

      module rate_coeff_assignment
      use common_qual
      implicit none

	integer, parameter :: MAX_RATE_ASSIGNS_SIZE = 400 ! int for th size of assigment struct array
      integer,save:: struct_index !iterating index for assignment struct arrary
      logical,dimension(max_constituent,ncoef_type),save:: rate_var_require_flag 

      type rate_coeff
	  integer group_index
	  integer rate_variable_index
	  integer ncc_index
	  real*8  rate_value
      end type
      type(rate_coeff), allocatable,save::rate_assign(:)


      contains

	subroutine allocate_assign_s(isat)
     	integer,intent(inout)::isat
	isat=0
	if (not(allocated(rate_assign))) then
           allocate(rate_assign(MAX_RATE_ASSIGNS_SIZE),STAT=isat)
	     struct_index=1
	end if
      end subroutine


      subroutine deallocate_assign_s(isat)
      use constants
      implicit none
      
     	integer,intent(inout)::isat
     	isat=0
      if (allocated(rate_assign)) then
           deallocate(rate_assign,STAT=isat)
	     struct_index=miss_val_i
	end if
	end subroutine

           
      subroutine assign_rate_to_group(groupid,rate_var_id,ncc_id,rate_value,isat,errm)

      integer,intent(inout)::isat !error code
	integer,intent(in)::groupid,rate_var_id,ncc_id
	real*8,intent(in)::rate_value
	character*(*),intent(out)::errm !error message

      
	isat=0
	errm=""
      if (not(allocated(rate_assign))) then
     	 call allocate_assign_s(isat)
	 if (isat.ne.0) then
	    errm="error in allocate memory for  rate coefficent assignment struct arrary!"
	    return
	 end if 
 	end if
	
	if (allocated(rate_assign).and.(struct_index.gt.0)) then
	    if (struct_index.gt.MAX_RATE_ASSIGNS_SIZE) then
             isat=MAX_RATE_ASSIGNS_SIZE
	       errm="rate coefficent assignment struct arrary is too small, increase MAX_RATE_ASSIGNS_SIZE!"
             return
	    end if
          rate_assign(struct_index).group_index=groupid
	    rate_assign(struct_index).rate_variable_index=rate_var_id
	    rate_assign(struct_index).ncc_index=ncc_id
	    rate_assign(struct_index).rate_value=rate_value
          struct_index=struct_index+1
	end if
	  


	end subroutine

c     this sub will give the value saved in tempory assign arrary to formarry
c      rcoef_chan,rcoef_res if corresponding channel and reservoir are contained in
c     groups, if assignment finished ok, the struct will be deleted

	subroutine rate_coeffs_to_waterbodies(isat,errm)
      use IO_Units
      use Groups, only: groupContains
      integer,intent(inout)::isat !error code
	character*(*),intent(out)::errm !error message

c     local variables
      integer i,j,groupno,rate_var_id,ncc_id
	real*8 rate_value
      
	isat=0
	errm=""
	do 100 i=1,struct_index-1
        groupno=rate_assign(i).group_index
    	  rate_var_id=rate_assign(i).rate_variable_index
	  ncc_id=rate_assign(i).ncc_index
	  rate_value=rate_assign(i).rate_value
	  do 200 j=1,nchans 
		  if (groupContains(groupno,obj_channel,j)) then
			    rcoef_chan(ncc_id,rate_var_id,j)=rate_value
		  end if
200      end do
            
	do 300 j = 1, nreser
            if(GroupContains(groupno,obj_reservoir,j)) then
                  rcoef_res(ncc_id,rate_var_id,j)=rate_value
		   end if
300   end do
100	end do
      call deallocate_assign_s(isat)
      if (isat.ne.0) then
	    errm="error in deallocate memory for rate coefficent assignment struct arrary!"
	end if 
	end subroutine

 
c     output the rate coefficents values to a file ,if there are missiong value
c     at certain location, error message will be print out also

      subroutine output_rate_to_file(funit)
      use groups, only: groupContains
      integer,intent(in)::funit     
c     local variables
      integer i,j,k
	character*(16) rate_name(ncoef_type)
	character*32 :: constituent_name(max_constituent),name

      real*8 :: HOUR_TO_DAY = 24.

	do 20 j=1,ncoef_type
         rate_name(j)=rate_var_index_to_string(j)
20	end do
	do 30 i=1,max_constituent
	   constituent_name(i)=constituent_index_to_string(i)
30	end do

c      output reservoirs first
      write(funit,*) "non-conservative constituents rate coefficients for reservoirs:"
      write(funit,2000) "reservoir","constituent","rate_variable","value"
2000  format(A16,3A16)

      do 900 k=1,nreser
	   call objno_to_name(obj_reservoir,k,name)
	   do 901 j=1,ncoef_type
     	      do 902 i=1,max_constituent
              if (rate_var_require_flag (i,j)) then
               if(rcoef_res(i,j,k).eq.miss_val_r) then
	            write(funit,4000) trim(name)//" miss ",trim(constituent_name(i))//" "
     &            ,trim(rate_name(j))//" coefficient value "           
	         else
                  write(funit,3000) trim(name), trim(constituent_name(i)),trim(rate_name(j)),rcoef_res(i,j,k)*HOUR_TO_DAY
	         endif
	        end if
902         end do
901      end do
900   end do
4000  format(A16,A10,A24)

      write(funit,2000) "channel","constituent","rate_variable","value"
      do 903 k=1,nchans
	   call objno_to_name(obj_channel,k,name)
	   do 904 j=1,ncoef_type
    	      do 905 i=1,max_constituent
     	        if (rate_var_require_flag(i,j)) then
               if(rcoef_chan(i,j,k).eq.miss_val_r) then
     	            write(funit,4000) trim(name)//" miss ",trim(constituent_name(i))//" "
     &            ,trim(rate_name(j))//" coefficient value "
	         else
                  write(funit,3000) trim(name), trim(constituent_name(i)),trim(rate_name(j)),rcoef_chan(i,j,k)*HOUR_TO_DAY
	         endif
              end if
905         end do
904      end do
903   end do


3000  format(A8,2A16,F16.4)
	      
      end subroutine

c     check the required rate coefficients ara input for every nonconservative
c     constituent within every waterbody(channel and reservoir)

      logical function check_rate_for_waterbody(funit)
     
	integer,intent(in)::funit

c     local variables
      integer i,j,k
	character*(16) rate_name(ncoef_type)
	character*32 :: constituent_name(max_constituent),name

      check_rate_for_waterbody=.true.
	do 20 j=1,ncoef_type
         rate_name(j)=rate_var_index_to_string(j)
20	end do

	do 30 i=1,max_constituent
	   constituent_name(i)=constituent_index_to_string(i)
30	end do

c      check reservoirs first
      do  k=1,nreser
	   do  j=1,ncoef_type
     	      do  i=1,max_constituent
              if (rate_var_require_flag (i,j)) then
               if(rcoef_res(i,j,k).eq.miss_val_r) then
                  call objno_to_name(obj_reservoir,k,name)
	            write(funit,*) "fatal error:"
	            write(funit,4000) trim(name)//" miss ",trim(constituent_name(i))//" "
     &            ,trim(rate_name(j))//" coefficient value "
	            check_rate_for_waterbody=.false.
	            return           
	         endif
	        end if
         end do
       end do
      end do
4000  format(A16,A10,A24)

c      check channel second

      do  k=1,nchans
	   do  j=1,ncoef_type
     	      do  i=1,max_constituent
    	        if (rate_var_require_flag(i,j)) then
               if(rcoef_chan(i,j,k).eq.miss_val_r) then
                  call objno_to_name(obj_channel,k,name) 
	            write(funit,4000) trim(name)//" miss ",trim(constituent_name(i))//" "
     &            ,trim(rate_name(j))//" coefficient value "
	            check_rate_for_waterbody=.false.
	            return
	         endif
              end if
         end do
       end do
      end do
	      
      end function




c     initialize all the value in channel and reservoir to miss_value_i or r

      subroutine initialize_rate_coefficient
      implicit none
      integer i,j,k

	do 100 k=1,max_reservoirs
	   do 200 j=1,ncoef_type
	      do 300 i=1,max_constituent
               rcoef_res(i,j,k)=miss_val_r
300         end do
200      end do
100   end do

      do 400 k=1,max_channels
	   do 500 j=1,ncoef_type
	      do 600 i=1,max_constituent
               rcoef_chan(i,j,k)=miss_val_r
600         end do
500      end do
400   end do


      do 700 j=1,ncoef_type
	      do 800 i=1,max_constituent
               rate_var_require_flag(i,j)=.false.
800         end do
700   end do
      return
	end subroutine



	character*(32) function constituent_index_to_string(index)
	implicit none
      integer,intent(in)::index
       if (index.eq.ncc_do) then
          constituent_index_to_string="DO"
	 else if (index.eq.ncc_organic_n) then
           constituent_index_to_string="Organic_N"
	 else if (index.eq.ncc_nh3) then
           constituent_index_to_string="NH3"
	 else if (index.eq.ncc_no2) then
           constituent_index_to_string="NO2"
	 else if (index.eq.ncc_no3) then
           constituent_index_to_string="NO3"
	 else if (index.eq.ncc_organic_p) then
           constituent_index_to_string="Organic_P"
	 else if (index.eq.ncc_po4) then
           constituent_index_to_string="PO4"
	 else if (index.eq.ncc_algae) then
           constituent_index_to_string="Algae"
	 else if (index.eq.ncc_bod) then
           constituent_index_to_string="BOD"
	 else if (index.eq.ncc_temp) then
           constituent_index_to_string="Temperature"
       else
	     constituent_index_to_string=miss_val_c
       end if
	return
	end function



	character*(16) function rate_var_index_to_string(index)
	implicit none
	integer,intent(in)::index
       if (index.eq.decay) then
          rate_var_index_to_string="decay"
	 else if (index.eq.settle) then
           rate_var_index_to_string="settle"
	 else if (index.eq.benthic) then
           rate_var_index_to_string="benthic"
	 else if (index.eq.alg_grow) then
           rate_var_index_to_string="alg_grow"
       else if (index.eq.alg_resp) then
           rate_var_index_to_string="alg_resp"
	 else if (index.eq.alg_die) then
           rate_var_index_to_string="alg_die"
	 else
           rate_var_index_to_string=miss_val_c
       end if
      return 
	end function


     
	end module