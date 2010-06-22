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


c      defines a local structure for rate coefficient assignment and some 
c      utility functions Jon 4/12/06

      module rate_coeff_assignment
      use common_qual
      implicit none
      logical,dimension(max_constituent,ncoef_type),save:: rate_var_require_flag 

      contains

 
c     output the rate coefficents values to a file ,if there are missing values
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
	            write(funit,4000) trim(name)//" missing ",trim(constituent_name(i))//" "
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
	            write(funit,4000) trim(name)//" missing ",
     &            trim(constituent_name(i))//" "
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