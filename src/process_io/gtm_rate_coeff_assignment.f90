


module gtm_rate_coeff_assignment

    use common_variables
    use common_dsm2_qual
    implicit none
    logical,dimension(max_constituent,ncoef_type), save:: rate_var_require_flag

    contains
    
    !> output the rate coefficents values to a file ,if there are missing values
    !> at certain location, error message will be print out also
    subroutine output_rate_to_file(funit)
        use common_dsm2_groups, only: groupContains
        integer,intent(in)::funit
        !     local variables
        integer:: i
        integer:: j
        integer:: k
        character(len=16):: rate_name(ncoef_type)
        character(len=32)::constituent_name(max_constituent)
        character(len=32)::name

        real*8:: HOUR_TO_DAY = 24.

        do j=1,ncoef_type
            rate_name(j)=rate_var_index_to_string(j)
        end do
        do  i=1,max_constituent
            constituent_name(i)=constituent_index_to_string(i)
        end do

        !      output reservoirs first
        write(funit,*) "non-conservative constituents rate coefficients for reservoirs:"
        write(funit,2000) "reservoir","constituent","rate_variable","value"
2000    format(A16,3A16)

        !do k=1,nreser
        do k = 1, n_resv
            call objno_to_name(obj_reservoir,k,name)
            do  j=1,ncoef_type
                do  i=1,max_constituent
                    if (rate_var_require_flag (i,j)) then
                        if(rcoef_res(i,j,k)==miss_val_r) then
                            write(funit,4000) trim(name)//" missing ",trim(constituent_name(i))//" " &
                                ,trim(rate_name(j))//" coefficient value "
                        else
                            write(funit,3000) trim(name), trim(constituent_name(i)),trim(rate_name(j)),rcoef_res(i,j,k)*HOUR_TO_DAY
                        endif
                    end if
                end do
            end do
        end do
4000    format(A16,A10,A24)

        write(funit,2000) "channel","constituent","rate_variable","value"
        !do  k=1,nchans
        do k = 1, n_chan
            call objno_to_name(obj_channel,k,name)
            do  j=1,ncoef_type
                do  i=1,max_constituent
                    if (rate_var_require_flag(i,j)) then
                        if(rcoef_chan(i,j,k)==miss_val_r) then
                            write(funit,4000) trim(name)//" miss ",trim(constituent_name(i))//" "&
                                ,trim(rate_name(j))//" coefficient value "
                        else
                            write(funit,3000) trim(name), trim(constituent_name(i)),trim(rate_name(j)),rcoef_chan(i,j,k)*HOUR_TO_DAY
                        endif
                    end if
                end do
            end do
        end do


3000    format(A8,2A16,F16.4)
	      
    end subroutine

    !> check the required rate coefficients ara input for every nonconservative
    !> constituent within every waterbody(channel and reservoir)
    logical function check_rate_for_waterbody(funit)
     
        integer,intent(in)::funit

        !     local variables
        integer:: i
        integer:: j
        integer:: k
        character(len=16):: rate_name(ncoef_type)
        character(len=32)::constituent_name(max_constituent)
        character(len=32)::name

        check_rate_for_waterbody=.true.
        do  j=1,ncoef_type
            rate_name(j)=rate_var_index_to_string(j)
        end do

        do  i=1,max_constituent
            constituent_name(i)=constituent_index_to_string(i)
        end do

        !      check reservoirs first
        !do  k=1,nreser
        do k = 1, n_resv
            do  j=1,ncoef_type
                do  i=1,max_constituent
                    if (rate_var_require_flag (i,j)) then
                        if(rcoef_res(i,j,k)==miss_val_r) then
                            call objno_to_name(obj_reservoir,k,name)
                            write(funit,*) "fatal error:"
                            write(funit,4000) trim(name)//" missing ", &
                                trim(constituent_name(i))//" " &
                                ,trim(rate_name(j))//" coefficient value "
                            check_rate_for_waterbody=.false.
                            return
                        endif
                    end if
                end do
            end do
        end do
4000    format(A16,A10,A24)

        !      check channel second

        !do  k=1,nchans
        do k = 1, n_chan
            do  j=1,ncoef_type
                do  i=1,max_constituent
                    if (rate_var_require_flag(i,j)) then
                        if(rcoef_chan(i,j,k)==miss_val_r) then
                            call objno_to_name(obj_channel,k,name)
                            write(funit,4000) trim(name)//" miss ",trim(constituent_name(i))//" " &
                                ,trim(rate_name(j))//" coefficient value "
                            check_rate_for_waterbody=.false.
                            return
                        endif
                    end if
                end do
            end do
        end do
	      
    end function




    !> initialize all the value in channel and reservoir to miss_value_i or r
    subroutine initialize_rate_coefficient
        implicit none
        integer:: i
        integer:: j
        integer:: k
        do  k=1,max_reservoirs
            do  j=1,ncoef_type
                do  i=1,max_constituent
                    rcoef_res(i,j,k)=miss_val_r
                end do
            end do
        end do
        do  k=1,max_channels
            do  j=1,ncoef_type
                do  i=1,max_constituent
                    rcoef_chan(i,j,k)=miss_val_r
                end do
            end do
        end do
        do  j=1,ncoef_type
            do  i=1,max_constituent
                rate_var_require_flag(i,j)=.false.
            end do
        end do
        return
    end subroutine



    !> statement function to transform constituent index to string
    character(len=32) function constituent_index_to_string(index)
        implicit none
        integer,intent(in)::index
        if (index == ncc_do) then
            constituent_index_to_string="DO"
        else if (index==ncc_organic_n) then
            constituent_index_to_string="Organic_N"
        else if (index==ncc_nh3) then
            constituent_index_to_string="NH3"
        else if (index==ncc_no2) then
            constituent_index_to_string="NO2"
        else if (index==ncc_no3) then
            constituent_index_to_string="NO3"
        else if (index==ncc_organic_p) then
            constituent_index_to_string="Organic_P"
        else if (index==ncc_po4) then
            constituent_index_to_string="PO4"
        else if (index==ncc_algae) then
            constituent_index_to_string="Algae"
        else if (index==ncc_bod) then
            constituent_index_to_string="BOD"
        else if (index==ncc_temp) then
            constituent_index_to_string="Temperature"
        else
            constituent_index_to_string=miss_val_c
        end if
        return
    end function

    !> statement function to transform rate var index to string
    character(len=16) function rate_var_index_to_string(index)
        implicit none
        integer,intent(in)::index
        if (index==decay) then
            rate_var_index_to_string="decay"
        else if (index==settle) then
            rate_var_index_to_string="settle"
        else if (index==benthic) then
            rate_var_index_to_string="benthic"
        else if (index==alg_grow) then
            rate_var_index_to_string="alg_grow"
        else if (index==alg_resp) then
            rate_var_index_to_string="alg_resp"
        else if (index==alg_die) then
            rate_var_index_to_string="alg_die"
        else
            rate_var_index_to_string=miss_val_c
        end if
        return
    end function

end module
