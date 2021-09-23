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

module virt_xsect
    use network
    use common_xsect, disabled => virt_xsect    !@# variable name same as module name.
    implicit none
    double precision, save :: x, x1, x2, y1, y2 & ! interpolation variables
        ,temp_all_elev(max_elevations) & ! all elevations in current channel incl duplicates
        ,temparea(max_assg_sec,max_elevations) & ! all areas in current section in current channel
        ,tempwet_p(max_assg_sec,max_elevations) & ! all wet_p in current section in current channel
        ,tempwidth(max_assg_sec,max_elevations) & ! all widths in current section in current channel
        ,tempz_centroid(max_assg_sec,max_elevations)  ! all z cent. values in current sec in current chan
contains
    subroutine virtual_xsect

        !-----Creates virtual cross-section lookup table:
        !-----Interpolates within cross-sections to make tables for all unique elev.
        !-----then interpolates in x-direction to make virtual cross-sections
        !-----which are then used as lookup tables.
        !-----Elevations are now heights of layers.  A layer consists of all cross-section
        !-----properties in a cross-section that have the same elevation (or height).
        !-----The height is the distance above the lowest point in the cross-section.
        use common_xsect, disabled => virt_xsect    !@# variable name same as module name.
        use constants, only: miss_val_r !@# Before merging, 'miss_val_r' came from DSM2's common_xsect.
        implicit none

        integer &
            channo  &              ! dsm channel number
            ,vsecno &             ! number of virtual xsect within chan. (upstream=1)
            ,i      &             ! do loop counters
            ,j

        !-----initialize index arrays
        do vsecno=1,max_virt_xsects
            virt_min_elev(vsecno)=miss_val_r
        enddo
        do channo=1,nchans
            chan_index(channo)=0
            elev_index(channo)=0
            minelev_index(channo)=0
        enddo
        elev_index(1)=1
        chan_index(1)=1
        minelev_index(1)=1

        !-----used by interp_width
        if (levee_slope <= 0) then
            levee_slope = 0.5      ! default levee slope
        endif

        !-----used by check_area
        if (area_tolerance <= 0) then
            area_tolerance = 2.0   ! default area tolerance
        endif

        do channo=1,nchans
            !--------initialize temporary data arrays--these must be re-initialized for each
            !--------channel because of the way the arrays are sorted
            do i=1,max_elevations
                temp_all_elev(i)=0
                do j=1,max_assg_sec
                    temparea(j,i)=0
                    tempwet_p(j,i)=0
                    tempwidth(j,i)=0
                    tempz_centroid(j,i)=0
                enddo
            enddo
            !--------indices must be cumulative:  add current indices to indices for next chan
            if ( chan_geom(channo)%length <= 0 .or. &
                xsect_assg(channo)%num_sec_assg <= 0 ) then
                write(*,*) &
                        'the channel length or num_sec_assg is le. zero: ', chan_geom(channo)%length, &
                         xsect_assg(channo)%num_sec_assg
                elev_index(channo+1)=elev_index(channo+1)+elev_index(channo)
                chan_index(channo+1)=chan_index(channo+1)+chan_index(channo)
                minelev_index(channo+1)=minelev_index(channo+1)+minelev_index(channo)
            endif
            !--------ALL ELEVATIONS ARE NOW > 0 (THEY ARE ACTUALLY HEIGHTS of layers above
            !--------the lowest point in the cross-section.  A layer consists of all
            !--------cross-section properties in a cross-section that have the same height.
            !--------Elevations were converted to heights in readirreg by subtracting the
            !--------bottom elevation.

            if (chan_geom(channo)%length > 0) then
                call combine_heights(channo)

                !-----------calculate elevation index for next channel
                if (channo+1 <= max_channels) then
                    elev_index(channo+1) = elev_index(channo) + num_layers(channo)
                endif
                call first_interpolation(channo)
                call second_interpolation(channo)
            endif
        enddo

        !-----copy bottom elevations at ends of channel to chan_geom()%BottomElev.
        !-----This is done for the use of qual and ptm only; hydro doesn't need this.
        do channo=1,nchans
            if (chan_geom(channo)%length > 0) then
                chan_geom(channo)%BottomElev(1) = virt_min_elev(minelev_index(channo))
                chan_geom(channo)%BottomElev(2) = virt_min_elev(minelev_index(channo)+ &
                    num_virt_sec(channo)-1)
            endif
        enddo

        return
    end subroutine

    subroutine combine_heights(channo)

        !-----This subroutine makes a list of all of the unique layer heights in a channel
        !-----by making a list of all heights from real cross-sections in the channel,
        !-----and sorting and filtering the list.  A layer consists of all cross-section
        !-----properties in a cross-section that have the same  height.
        !-----If the maximum height in the list is < = the specified maximum layer height,
        !-----then a top height defined by the user in common_irreg_geom.f is
        !-----added to the list.
        use IO_Units
        use common_xsect, disabled => virt_xsect    !@# variable name same as module name.
        use constants
        implicit none

        integer &
            channo &              ! dsm channel number
            ,secno &              ! number of current cross-section (not within chan)
            ,i     &              ! do loop counters
            ,j     &
            ,k     &
            ,numvirtelev &        ! # of virt elev. in current chan., incl. duplicates
            ,num_layers_total    ! total number of layers assigned to this channel



        !-----check array bounds
        if(xsect_assg(channo)%num_sec_assg > max_assg_sec) then
            write(unit_error,*) 'Too many irregular cross-sections assigned to channel ', channo
        endif

        !-----count number of elevations assigned to channel (total number of points
        !-----in all irregular cross-sections).  Add 2 to the number (elevations are
        !-----added at 100ft and at MSL) and compare to max_elevations.  Print
        !-----error message if too large

        num_layers_total=0
        do i=1,xsect_assg(channo)%num_sec_assg
            secno = xsect_assg(channo)%sec_index(i)
            num_layers_total=num_layers_total+irreg_geom(secno)%num_elev
        enddo

        if(num_layers_total+2*xsect_assg(channo)%num_sec_assg &
            > max_elevations) then
            call array_bounds_exceeded(channo)
        endif

        !-----combine all elev (for all real xsect in the chan) including duplicates
        k=1
        do i=1,xsect_assg(channo)%num_sec_assg
            secno=xsect_assg(channo)%sec_index(i)
            do j=1,irreg_geom(secno)%num_elev
                temp_all_elev(k)=irreg_geom(secno)%elevation(j)
                if (temp_all_elev(k)== 0.0)then
                    !this is added to avoid 0 problem
                    temp_all_elev(k)=0.001
                endif
                k=k+1
            enddo
        enddo
        !-----if first and last cross-sec are not at nodes or outside channel,
        !-----assign elev for rectangular section
        if (xsect_assg(channo)%num_sec_assg == 0) then
            temp_all_elev(k)=0.0
            k=k+1
            temp_all_elev(k)=max_layer_height
            k=k+1
        endif
        numvirtelev=k-1

        !-----add top height
        if (temp_all_elev(numvirtelev) < max_layer_height) then
            temp_all_elev(numvirtelev+1)=max_layer_height
            numvirtelev=numvirtelev+1
        endif

        !-----sort list of elevations in the channel:
        !-----ascending order with zeros at end (see function compar)
        call qsort(temp_all_elev,max_elevations,REAL_PRECISION,compar)

        !-----copy unique values onto virt_elevation()
        !      virt_elevation(elev_index(channo))=0.0
        !---    use-elevation
        virt_elevation(elev_index(channo))=temp_all_elev(1)
        j=0
        do i=1,numvirtelev
            if ( temp_all_elev(i) /= &
                virt_elevation(elev_index(channo)+j) ) then
                virt_elevation(elev_index(channo)+j+1) = temp_all_elev(i)
                j=j+1
            endif
        enddo
        !      num_layers(channo)=j
        !--     use-elevation
        num_layers(channo)=j+1

        return
    end subroutine

    subroutine first_interpolation(channo)

        !-----This is the first of three stages of interpolation.  The first two stages
        !-----occur before the run begins.  ALL ELEVATIONS ARE NOW HEIGHTS OF LAYERS.
        !-----A layer consists of all cross-section properties in a cross-section that have
        !-----the same elevation (or height).
        !-----The first interpolation uses the list of all unique heights in the channel
        !-----(which is made in combine_elevations) to interpolate cross-section
        !-----properties within each real cross-section.  Cross-section properties are
        !-----extrapolated if the maximum height of any layer in the channel is greater than
        !-----the maximum height in the current cross-section.  Extrapolation is done by
        !-----calculating a top width using the user-specified value levee_slope, which is
        !-----rise over run.
        use IO_Units
        use common_xsect, disabled => virt_xsect    !@# variable name same as module name.
        implicit none

        integer &
            channo &               ! dsm channel number
            ,secno &              ! number of current cross-section (not within chan)
            ,rsecno &             ! number of temporary real cross-section
            ,i &                  ! loop counter
            ,assgindex &          ! the index of the arrays in the irreg_geom str.
            ,virtelev &           ! number of layer (within xsect)
            ,eindex &             ! function:  index of elevation array
            ,ei                  ! stores value of eindex

        double precision height

        !-----statement function to calculate indices of virtual data arrays
        eindex(channo,virtelev) &
            =elev_index(channo)+virtelev-1

        !-----loop through all assigned sections
        do i=1,xsect_assg(channo)%num_sec_assg
            secno=xsect_assg(channo)%sec_index(i)
            assgindex=1
            do virtelev=1,num_layers(channo)
                !-----------if the elevation matches, copy data, don't interpolate
                ei = eindex(channo,virtelev)
                if ( virt_elevation(ei) == &
                    irreg_geom(secno)%elevation(assgindex) ) then
                    temparea(i,virtelev)=irreg_geom(secno)%area(assgindex)
                    tempwet_p(i,virtelev)=irreg_geom(secno)%wet_p(assgindex)
                    tempwidth(i,virtelev)=irreg_geom(secno)%width(assgindex)
                    !               if (virt_elevation(ei).eq.0) then
                    if (virt_elevation(ei)==irreg_geom(secno)%elevation(1)) then
                        tempz_centroid(i,virtelev)=virt_elevation(ei)
                    else
                        tempz_centroid(i,virtelev)=&
                            irreg_geom(secno)%z_centroid(assgindex)
                    !---z_centroid(assgindex) was not real calculated,so use interp_z_centroid to calculate

                    endif
                    assgindex=assgindex+1
                !--------------if the virt_elevation lower than bottom of original x-section
                elseif ( virt_elevation(ei) < &
                    irreg_geom(secno)%elevation(1) ) then
                    temparea(i,virtelev)=0
                    tempwet_p(i,virtelev)=0
                    tempwidth(i,virtelev)=0
                    tempz_centroid(i,virtelev)=0
                else
                    call interp_width(secno,channo,virtelev,tempwidth(i,virtelev-1),tempwidth(i,virtelev))
                    height=virt_elevation(ei)-virt_elevation(ei-1)
                    call interp_area( temparea(i,virtelev-1) ,tempwidth(i,virtelev-1) &
                        ,tempwidth(i,virtelev) ,height ,temparea(i,virtelev))
                    call interp_wet_p(secno,channo ,virtelev ,tempwet_p(i,virtelev))
                    rsecno=i
                    !----use-elevation change, the following disabled
                    call interp_z_centroid( rsecno ,secno ,channo &
                        ,assgindex ,virtelev ,tempz_centroid(i,virtelev))
                    assgindex=assgindex+1
                !                endif
                endif
            enddo
        enddo
        !-----assign rectangular xsect if none available for interpolation at chan. ends
        if (xsect_assg(channo)%num_sec_assg == 0) then
            write(unit_error,*) 'No cross-sections available for channel: ',chan_geom(channo)%chan_no
            call exit(2)
        endif

        return
    end subroutine

    subroutine second_interpolation(channo)

        !-----This is the second of three stages of interpolation.  The first two stages
        !-----occur before the run begins.  ALL ELEVATIONS ARE NOW HEIGHTS OF LAYERS.
        !-----A layer consists of all cross-section properties in a cross-section that have
        !-----the same height.
        !-----The second interpolation makes the virtual cross-sections which are used by
        !-----the model.  The cross-section properties are interpolated in the
        !-----x direction (along the length of the channel) to make virtual cross-sections
        !-----at each computational point.  Computational points are located at the ends of
        !-----the channel and at distances along the channel length defined by (deltax/2).
        use IO_Units
        use common_xsect, disabled => virt_xsect    !@# variable name same as module name.
        implicit none


        integer  channo               ! dsm channel number

        real*8  dx_r                ! stores value of deltax_requested


        !-----find the virtual deltax (requested value divided by 2)
        if (deltax_requested == 0) then
            dx_r = chan_geom(channo)%length
        elseif (deltax_requested /= 0) then
            dx_r = deltax_requested
        endif

        !-----find number of virtual cross-sections that will be assigned to the channel
        if (float(chan_geom(channo)%length) <= dx_r) then
            num_virt_sec(channo) = 3
        elseif (float(chan_geom(channo)%length) > dx_r) then
            num_virt_sec(channo) = 2* int( float(chan_geom(channo)%length) /dx_r) +1
        endif

        if (num_virt_sec(channo) < 3) then
            write (unit_error,*) 'Error:  num_virt_sec(channo) < 3.'
        endif
        virt_deltax(channo) = dfloat(chan_geom(channo)%length) / (num_virt_sec(channo)-1)

        !-----if the current channel is not the last channel number, calculate indices
        if (channo+1 <= max_channels) then
            chan_index(channo+1) = chan_index(channo)+ num_layers(channo) * num_virt_sec(channo)
            minelev_index(channo+1) = minelev_index(channo)+num_virt_sec(channo)
        endif

        !-----interpolate bottom elevations and all cross-section properties in the
        !-----horizontal direction
        call interp_botelv_horizontal(channo)
        call interp_xs_prop_horizontal(channo)

        return
    end subroutine

    subroutine interp_botelv_horizontal(channo)

        !-----Bottom elevations--interpolate from 2 nearest xsect.  xsects at channel ends
        !-----could be outside channel (dist could be < 0 or > channel length).  If no xsect
        !-----upstream or downstream of virtual xsect, then use rectangular bottom elev.
        !-----upstream end
        use common_xsect, disabled => virt_xsect    !@# variable name same as module name.
        implicit none


        real*8   interp              ! interpolation function

        integer  channo  &        ! dsm channel number
            ,i &                  ! do loop counter
            ,vsecno &             ! number of virt xsect within channel (upstream=1)
            ,upindex &            ! index of temporary data arrays:  upstream sec
            ,downindex &          ! index of temporary data arrays:  downstream sec
            ,num_sec &            ! number of real cross-sections in current chan
            ,xs                   ! rectangular cross-section number

        !-----statement functions to interpolate and extrapolate wrt two points
        interp(x1,x2,y1,y2,x) =-((y2-y1)/(x2-x1))*(x2-x) + y2

        !-----upstream
        x=0
        x1=xsect_assg(channo)%dist(1)
        y1=irreg_geom(xsect_assg(channo)%sec_index(1))%min_elev
        x2=xsect_assg(channo)%dist(2)
        y2=irreg_geom(xsect_assg(channo)%sec_index(2))%min_elev
        virt_min_elev(minelev_index(channo)) = interp(x1,x2,y1,y2,x)

        !-----downstream end
        x=float(chan_geom(channo)%length)
        num_sec=xsect_assg(channo)%num_sec_assg
        vsecno=num_virt_sec(channo)
        if (xsect_assg(channo)%rect(xsect_assg(channo)%num_sec_assg)) then
            xs=chan_geom(channo)%xsect(2)
            virt_min_elev(minelev_index(channo)+vsecno-1) = xsect_geom(xs)%botelv
        else
            if ( xsect_assg(channo)%dist(num_sec) < chan_geom(channo)%length) then
                xs=chan_geom(channo)%xsect(2)
                virt_min_elev(minelev_index(channo)+vsecno-1) = xsect_geom(xs)%botelv
            elseif(xsect_assg(channo)%dist(num_sec) >= chan_geom(channo)%length) then
                x1=xsect_assg(channo)%dist(num_sec-1)
                x2=xsect_assg(channo)%dist(num_sec)
                y1=irreg_geom(xsect_assg(channo)%sec_index(num_sec-1))%min_elev
                y2=irreg_geom(xsect_assg(channo)%sec_index(num_sec))%min_elev
                virt_min_elev(minelev_index(channo)+vsecno-1) = interp(x1,x2,y1,y2,x)
            endif
        endif

        !-----interpolate interior virtual xsect bottom elevations
        do vsecno=2,num_virt_sec(channo)-1
            x = (float(vsecno)-1)*(virt_deltax(channo))
            num_sec=xsect_assg(channo)%num_sec_assg
            upindex=0
            downindex=0
            do i=1,num_sec
                if ( xsect_assg(channo)%dist(i) <= x .and. .not. xsect_assg(channo)%rect(i) ) upindex=i
            enddo
            do i=num_sec,1,-1
                if ( xsect_assg(channo)%dist(i) >= x .and. .not. xsect_assg(channo)%rect(i) ) downindex=i
            enddo
            !--------if there is a xsect with dist=x, then both indices will be the same
            if  ( (upindex == downindex) .and.   (upindex >= 1) .and. (downindex >= 1) ) then
                virt_min_elev(minelev_index(channo)+vsecno-1) =  irreg_geom(xsect_assg(channo)%sec_index(upindex))%min_elev
            else
                if (upindex == 0) then
                    x1=0
                    xs=chan_geom(channo)%xsect(1)
                    y1=xsect_geom(xs)%botelv
                elseif (upindex > 0) then
                    x1=xsect_assg(channo)%dist(upindex)
                    y1=irreg_geom(xsect_assg(channo)%sec_index(upindex))%min_elev
                endif
                if (downindex == 0) then
                    x2=float(chan_geom(channo)%length)
                    xs=chan_geom(channo)%xsect(2)
                    y2=xsect_geom(xs)%botelv
                elseif (downindex > 0) then
                    x2=xsect_assg(channo)%dist(downindex)
                    y2=irreg_geom(xsect_assg(channo)%sec_index(downindex))%min_elev
                endif
                virt_min_elev(minelev_index(channo)+vsecno-1) = interp(x1,x2,y1,y2,x)
            endif
        enddo

        return
    end subroutine

    subroutine interp_xs_prop_horizontal(channo)

        !-----interpolate cross-section properties in the x direction
        use common_xsect, disabled => virt_xsect    !@# variable name same as module name.
        use IO_Units
        implicit none

        integer &
            channo &               ! dsm channel number
            ,i &                  ! do loop counter
            ,vsecno &             ! number of virt xsect within channel (upstream=1)
            ,virtelev &           ! number of the current layer (within xsect)
            ,upindex &            ! index of temporary data arrays:  upstream sec
            ,downindex &          ! index of temporary data arrays:  downstream sec
            ,dindex &             ! function:  index of permanent data arrays
            ,di &                 ! stores value of dindex
            ,num_sec             ! number of real cross-sections in current chan

        real*8 &
            y3 &          ! extra interpolation variables
            ,y4 &
            ,y5 &
            ,y6 &
            ,y7 &
            ,y8 &
            ,interp &              ! interpolation function
            ,extrap              ! extrapolation function

        !-----statement functions to interpolate and extrapolate wrt two points
        interp(x1,x2,y1,y2,x) =-((y2-y1)/(x2-x1))*(x2-x) + y2
        extrap(x1,x2,y1,y2,x) = ((y2-y1)/(x2-x1))*(x-x1) + y1
        !-----statement function to calculate indices of virtual data arrays
        dindex(channo,vsecno,virtelev) &
            =chan_index(channo) + (vsecno-1)*num_layers(channo) + virtelev-1

        do vsecno=1, num_virt_sec(channo)
            do virtelev=1,num_layers(channo)
                !-----------find upindex, downindex,x1,x2,y1-8
                x = (dfloat(vsecno)-1)*(virt_deltax(channo))
                num_sec=xsect_assg(channo)%num_sec_assg
                upindex=0
                downindex=0
                do i=1,num_sec
                    if (xsect_assg(channo)%dist(i) <= x) upindex=i
                enddo
                do i=num_sec,1,-1
                    if (real(xsect_assg(channo)%dist(i)) >= real(x)) downindex=i
                enddo
                !-----------print error if upstream or downstream xsect not found
                if (upindex <= 0) then
                    write(unit_error,*) 'Error in second xsect property interpolation: &
                  & channel,vsecno,virtelev,upindex' &
                        ,channo,vsecno,virtelev,upindex
                    call exit(2)
                elseif (downindex <= 0) then
                    write(unit_error,*) 'Error in second xsect property interpolation:  &
                  & channel,vsecno,virtelev,downindex=' &
                        ,channo,vsecno,virtelev,downindex
                    call exit(2)
                endif

                x1=xsect_assg(channo)%dist(upindex)
                x2=xsect_assg(channo)%dist(downindex)

                if ( (x < 0) .or. (real(x) > float(chan_geom(channo)%length)) ) then
                    write(unit_error,*) &
                        'Desired cross-section is outside channel ',&
                        chan_geom(channo)%chan_no
                    call exit(2)
                endif

                y1=tempwidth(upindex,virtelev)
                y2=tempwidth(downindex,virtelev)
                y3=temparea(upindex,virtelev)
                y4=temparea(downindex,virtelev)
                y5=tempwet_p(upindex,virtelev)
                y6=tempwet_p(downindex,virtelev)
                y7=tempz_centroid(upindex,virtelev)
                y8=tempz_centroid(downindex,virtelev)
                !-----------calculate xsect property array index for current layer
                di = dindex(channo,vsecno,virtelev)
                
                if (di > max_layers) then
                    write(unit_error,*) '***error'
                    write(unit_error,*) 'Maximum number of max_layers exceeded.'
                    write(unit_error,*) 'Returning...'
                endif

                if (x <= x2) then
                    if ( (x1 == x2) .and. (upindex > 0) .and. &
                        (downindex > 0) ) then
                        virt_width(di)=y1
                        virt_area(di)=y3
                        virt_wet_p(di)=y5
                        virt_z_centroid(di)=y7
                    else
                        virt_width(di)=interp(x1,x2,y1,y2,x)
                        virt_area(di)=interp(x1,x2,y3,y4,x)
                        virt_wet_p(di)=interp(x1,x2,y5,y6,x)
                        virt_z_centroid(di)=interp(x1,x2,y7,y8,x)
                    endif
                elseif (x > x2) then
                    write(unit_screen,*) &
                        'Should not be extrapolating in the X direction!'
                    virt_width(di)=extrap(x1,x2,y1,y2,x)
                    virt_area(di)=extrap(x1,x2,y3,y4,x)
                    virt_wet_p(di)=extrap(x1,x2,y5,y6,x)
                    virt_z_centroid(di)=extrap(x1,x2,y7,y8,x)
                endif
            enddo
        enddo

        return
    end subroutine

    subroutine interp_width(secno,channo,virtelev,previous_width,w)

        !-----This subroutine interpolates or extrapolates width values in the vertical
        !-----direction.
        use common_xsect, disabled => virt_xsect    !@# variable name same as module name.
        implicit none

        integer &
            secno &              ! real cross-section number
            ,channo &             ! dsm channel number
            ,assgindex &          ! index of real cross-section data arrays
            ,virtelev &           ! number of current layer (within xsect)
            ,i &                  ! do loop counter
            ,eindex &             ! function: calcultes index of elevation array
            ,ei                  ! stores value of eindex

        real*8 &
            w &
            ,interp &             ! interpolation function
            ,previous_width      ! width of lower layer

        !-----statement functions to interpolate/extrapolate wrt to two points
        interp(x1,x2,y1,y2,x) =-((y2-y1)/(x2-x1))*(x2-x) + y2


        !-----statement function to calculate index of elevation array
        eindex(channo,virtelev)=elev_index(channo)+virtelev-1

        !-----if the elevation index is greater than the number of elevations in the
        !-----section, then extrapolate
        !-----find the index of the largest real elevation which is <= current virt elev
        ei = eindex(channo,virtelev)
        !-----increment assgindex until the real elevation is le the current virt elev.
        assgindex=1
        do i=1,irreg_geom(secno)%num_elev
            if ( irreg_geom(secno)%elevation(assgindex) < &
                virt_elevation(ei) ) then
                assgindex=assgindex+1
            endif
        enddo
        assgindex=assgindex-1
        if (assgindex >= irreg_geom(secno)%num_elev) then
            !--------calculate widths using specified levee slope
            w=2.0*(1.0/levee_slope)*(virt_elevation(ei) &
                -virt_elevation(ei-1)) + &
                previous_width

        !--------otherwise, interpolate
        elseif (assgindex < irreg_geom(secno)%num_elev) then
            x1=irreg_geom(secno)%elevation(assgindex)
            x2=irreg_geom(secno)%elevation(assgindex+1)
            y1=irreg_geom(secno)%width(assgindex)
            y2=irreg_geom(secno)%width(assgindex+1)

            x=virt_elevation(ei)
            w = interp(x1,x2,y1,y2,x)
        endif

        return
    end subroutine

    subroutine interp_area(a1,b1,b2,h,a)

        !-----This subroutine interpolates area values in the vertical direction.  This
        !-----subroutine is always called after interp_width.
        use common_xsect, disabled => virt_xsect    !@# variable name same as module name.
        implicit none


        real*8 &
            a  &                  ! interpolated area
            ,a1 &                 ! area at lower layer
            ,b1 &                 ! lower base for trapezoidal area calculation
            ,b2 &                 ! upper base for trapezoidal area calculation
            ,h                    ! height of trapezoid

        !-----to interpolate area, add previous area to area of trapezoid between current
        !-----and previous layers

        a = a1+(.5*(b1+b2))*h

        return
    end subroutine

    subroutine interp_wet_p(secno,channo,virtelev,w)

        !-----This subroutine interpolates or extrapolates wetted perimeter values in the
        !-----vertical direction.
        use common_xsect, disabled => virt_xsect    !@# variable name same as module name.
        implicit none

        integer &
            secno &               ! real cross-section number
            ,channo &             ! dsm channel number
            ,assgindex &          ! index of real cross-section data arrays
            ,virtelev &           ! number of layer (within xsect)
            ,i &                  ! do loop counter
            ,eindex &             ! function:  calculates index of elevation array
            ,ei                  ! stores value of eindex

        real*8 &
            w &
            ,interp &             ! interpolation function
            ,extrap              ! extrapolation function

        !-----statement functions to interpolate/extrapolate wrt to two points
        extrap(x1,x2,y1,y2,x) = ((y2-y1)/(x2-x1))*(x-x1) + y1
        interp(x1,x2,y1,y2,x) =-((y2-y1)/(x2-x1))*(x2-x) + y2
        !-----statement function to calculate index of elevation array
        eindex(channo,virtelev) = elev_index(channo)+virtelev-1

        !-----if only one elevation (layer), then return that value
        if (irreg_geom(secno)%num_elev == 1) then
            w=irreg_geom(secno)%wet_p(1)
            return
        endif

        !-----if the elevation index is greater than the number of elevations in the
        !-----section, then extrapolate
        !-----find the index of the largest real elevation which is <= current virt elev
        ei = eindex(channo,virtelev)
        !-----decrement assgindex until the real elevation is le the current virt elev.
        assgindex=1
        do i=1,irreg_geom(secno)%num_elev
            if ( irreg_geom(secno)%elevation(assgindex) < virt_elevation(ei) ) then
                assgindex=assgindex+1
            endif
        enddo
        assgindex=assgindex-1
        !-----if it's outside, extrapolate
        if (assgindex >= irreg_geom(secno)%num_elev) then
            x1=irreg_geom(secno)%elevation(assgindex-1)
            x2=irreg_geom(secno)%elevation(assgindex)
            y1=irreg_geom(secno)%wet_p(assgindex-1)
            y2=irreg_geom(secno)%wet_p(assgindex)

            x=virt_elevation(ei)
            w = extrap(x1,x2,y1,y2,x)
        !--------otherwise, interpolate
        elseif (assgindex < irreg_geom(secno)%num_elev) then
            x1=irreg_geom(secno)%elevation(assgindex)
            x2=irreg_geom(secno)%elevation(assgindex+1)
            y1=irreg_geom(secno)%wet_p(assgindex)
            y2=irreg_geom(secno)%wet_p(assgindex+1)

            x=virt_elevation(ei)
            w = interp(x1,x2,y1,y2,x)
        endif

        return
    end subroutine

    subroutine interp_z_centroid(rsecno,secno,channo,assgindex,virtelev,zc)

        !-----This subroutine interpolates z centroid values in the vertical direction.
        use common_xsect, disabled => virt_xsect    !@# variable name same as module name.
        implicit none


        integer &
            rsecno &              ! real cross-section number
            ,secno &
            ,channo &             ! dsm channel number
            ,assgindex &          ! index of real cross-section property arrays
            ,virtelev &           ! number of layer (within xsect)
            ,eindex &             ! function to calculate elevation array index
            ,ei                  ! stores value of eindex

        real*8 &
            zc &                  ! z centroid at current elevation
            ,aprev &              ! area at previous width
            ,Arect &              ! area of rectangular portion of current trapezoid
            ,Atria &              ! area of triangular portion of current trapezoid
            ,Cprev &              ! Z centroid at previous width
            ,Crect &              ! Z centroid of rectangular portion of current trap.
            ,Ctria &              ! Z centroid of triangular portion of current trap.
            ,trap_height         ! height of current trapezoid


        !-----statement function to calculate index of elevation array
        eindex(channo,virtelev)=elev_index(channo)+virtelev-1

        ei = eindex(channo,virtelev)
        trap_height=virt_elevation(ei)-virt_elevation(ei-1)
        !-----decrement assgindex until the real elevation is le the current virt elev.
        do while (assgindex > 1 .and. &
            (irreg_geom(secno)%elevation(assgindex) > virt_elevation(ei) .or. &
            irreg_geom(secno)%elevation(assgindex) == 0.0) )
            assgindex=assgindex-1
        enddo

        !-----to interpolate z centroid values, find centroids and areas of
        !-----rectangular and triangular portions of area between the two layers.
        Aprev = temparea(rsecno,virtelev-1)
        Arect = min(tempwidth(rsecno,virtelev),tempwidth(rsecno,virtelev-1)) * &
            trap_height
        Atria = abs( 0.5 * (tempwidth(rsecno,virtelev)-tempwidth(rsecno,virtelev-1)) &
            * trap_height )
        Cprev = tempz_centroid(rsecno,virtelev-1)
        Crect = virt_elevation(ei) + trap_height/2.

        if ( tempwidth(rsecno,virtelev) > tempwidth(rsecno,virtelev-1) ) then
            Ctria = virt_elevation(ei) + (2./3.)*trap_height
        elseif ( tempwidth(rsecno,virtelev) < &
            tempwidth(rsecno,virtelev-1) ) then
            Ctria = virt_elevation(ei) + (1./3.)*trap_height
        elseif ( tempwidth(rsecno,virtelev) == &
            tempwidth(rsecno,virtelev-1) ) then
            Ctria = 0.
        endif
        zc = ((Aprev*Cprev) + (Arect*Crect) + (Atria*Ctria)) / (Aprev+Arect+Atria)

        return
    end subroutine

    integer*2 function compar(a,b)
        implicit none

        !-----this function is required by the qsort function (a FORTRAN library function)
        !-----values will be sorted in ascending order with one exception:zeros will be last

        real*8 a,b
        if (a < b) compar=-1
        if (a == b) compar= 0
        if (a > b) compar= 1

        if (a == 0) compar= 1
        if (b == 0) compar=-1
        return
    end function

    subroutine assgrect(channo,loc)

        !-----This subroutine calculates cross-section properties for rectangular sections.
        use IO_Units
        use common_xsect, disabled => virt_xsect    !@# variable name same as module name.
        use constants
        implicit none


        integer &
            channo &              ! dsm channel number
            ,rsecno &             ! number of rectangular cross-section
            ,sec &                ! number of cross-section in channel
            ,virtelev &           ! number of layer (within xsect)
            ,eindex &             ! function: calculates index of elevation array
            ,ei &                 ! stores value of eindex
            ,i &                  ! do loop counter
            ,loc                 ! upstream or downstream end of channel


        !-----statement function to calculate index of elevation array
        eindex(channo,virtelev) &
            =elev_index(channo)+virtelev-1


        if (loc == chan_up) then
            sec=1
            !--------increment number of assigned sections
            xsect_assg(channo)%num_sec_assg= &
                xsect_assg(channo)%num_sec_assg + 1
            xsect_assg(channo)%rect(sec)=.true.
            rsecno=chan_geom(channo)%xsect(1)
            do i=xsect_assg(channo)%num_sec_assg,2,-1
                xsect_assg(channo)%sec_index(i)=xsect_assg(channo)%sec_index(i-1)
                xsect_assg(channo)%dist(i)=xsect_assg(channo)%dist(i-1)
                xsect_assg(channo)%original(i)= xsect_assg(channo)%original(i-1)
            enddo
            xsect_assg(channo)%original(1)=.false.
            xsect_assg(channo)%dist(1)=0.0

        elseif (loc == chan_down) then
            !--------increment number of assigned sections
            xsect_assg(channo)%num_sec_assg= xsect_assg(channo)%num_sec_assg + 1
            sec=xsect_assg(channo)%num_sec_assg
            if (sec == 1) then
                sec=2
                xsect_assg(channo)%rect(sec)=.true.
            elseif (sec >= 2) then
                xsect_assg(channo)%rect(sec)=.true.
                rsecno=chan_geom(channo)%xsect(chan_geom(channo)%nxsect)
                xsect_assg(channo)%sec_index( xsect_assg(channo)%num_sec_assg) = &
                    xsect_assg(channo)%sec_index(xsect_assg(channo)%num_sec_assg-1)
                xsect_assg(channo)%dist(xsect_assg(channo)%num_sec_assg) = &
                    real(chan_geom(channo)%length)
                xsect_assg(channo)%original(xsect_assg(channo)%num_sec_assg)=.false.
            else
                write(unit_error,*) 'Subroutine assgrect called, but end of channel undefined'
                call exit(2)
            endif
        endif

        !-----use width to calculate area and wetted perimeter
        do virtelev=1,num_layers(channo)
            ei = eindex(channo,virtelev)
            tempwidth(sec,virtelev)=xsect_geom(rsecno)%width
            temparea(sec,virtelev)=tempwidth(sec,virtelev)*virt_elevation(ei)
            tempwet_p(sec,virtelev)=tempwidth(sec,virtelev)+2*(virt_elevation(ei))
            tempz_centroid(sec,virtelev)=virt_elevation(ei) / 2
        enddo

        return
    end subroutine

    subroutine virt_output

        !-----This subroutine prints a list of all cross-section properties for all layers
        !-----of all virtual cross-sections.
        use IO_Units
        use common_xsect, disabled => virt_xsect    !@# variable name same as module name.
        implicit none


        integer &
            channo &               ! dsm channel number
            ,vsecno &             ! number of cross-section within channel
            ,dindex &             ! function: calculates index of data arrays
            ,di &                 ! stores value of dindex
            ,virtelev &           ! number of layer (within xsect)
            ,eindex &             ! function: calculates index of elevation array
            ,ei &                 ! stores value of eindex
            ,mindex &             ! function: calculates index of min. elev. array
            ,mi                  ! stores value of mindex

        !-----statement function to calculate indices of virtual data arrays
        dindex(channo,vsecno,virtelev)=chan_index(channo) + (vsecno-1)*num_layers(channo) + virtelev-1
        !-----statement function to calculate index of elevation array
        eindex(channo,virtelev)=elev_index(channo)+virtelev-1
        !-----statement function to calculate index of minimum elevation array
        mindex(channo,vsecno)=minelev_index(channo)+vsecno-1

        write(unit_output,*) 'VIRTUAL CROSS-SECTION LOOKUP TABLE'
        do channo=1,nchans
            if ( (chan_geom(channo)%length > 0) .and.(xsect_assg(channo)%num_sec_assg > 0) ) then
                do vsecno=1,num_virt_sec(channo)
                    write(unit_output,*) 'Channel ',channo, ',Virtual Section ',vsecno
                    write(unit_output,*)' Height             Width            Area       Wet_perim      Min_elev'
                    write(unit_output,*)'-----------------------------------------------------------------------'
                    mi = mindex(channo,vsecno)
                    do virtelev=1,num_layers(channo)
                        di = dindex(channo,vsecno,virtelev)
                        ei = eindex(channo,virtelev)
                        write(unit_output,20)  virt_elevation(ei),virt_width(di),&
                            virt_area(di) ,virt_wet_p(di),virt_min_elev(mi)
                    enddo
                enddo
            endif
        enddo
20      format(f8.2,4x,f14.4,2x,f14.4,2x,f14.4,2x,f12.4)
        return
    end subroutine



    subroutine check_area

        !-----This subroutine compares the MSL areas of virtual cross-sections within
        !-----channels.  If the ratio is > a user-specified area tolerance, an error
        !-----message is printed with the names of the cross-sections.
        use IO_Units
        use common_xsect, disabled => virt_xsect    !@# variable name same as module name.
        implicit none


        integer &
            channo &              ! do loop counters
            ,vsecno &             ! number of virtual cross-section (upstream=1)
            ,virtelev &           ! number of layer (within xsect)
            ,dindex &             ! statement function to calculate data array index
            ,msl_area(max_assg_virtsec) & ! MSL area for all virt. sec in current chan
            ,max_vsecno &         ! number of largest xsect
            ,min_vsecno          ! number of smallest xsect
        !-----output variables
        integer &
            veindex &            ! index of elevation that is at or below MSL
            ,di                  ! virtual xsect data index

        real*8 &
            max_area &           ! area of largest xsect in current chan
            ,min_area &           ! area of largest xsect in current chan
            ,x1 &                 ! interpolation variables
            ,x2 &
            ,b1 &
            ,b2 &
            ,a1

        !-----statement function to calculate index of data arrays
        dindex(channo,vsecno,virtelev) &
            =chan_index(channo) + (vsecno-1)*num_layers(channo) + virtelev-1

        !-----this subroutine checks every real cross-section to see if the area at MSL
        !-----is within the range of 0.5 to 2 times the area at MSL of the rectangular
        !-----cross-section that was replaced by the virtual cross-section

        write(unit_error,620)
620     format('Checking channels for unacceptable changes in area(MSL),' &
            /'check output file for details.')
        write(unit_output,*) &
            'Warning:  The following channels have unacceptable changes in area(MSL)'
        write(unit_output,*) &
            'based on a comparison of largest and smallest xsect in each channel.'
        write(unit_output,*) &
            'channo  vsecnoA  vsecnoB   MSLheight1  MSLheight2    MSLarea1    MSLarea2       ratio'
        write(unit_output,*) &
            '-------------------------------------------------------------------------------------'
        do channo=1,nchans
            if (chan_geom(channo)%length > 0) then
                !-----------find index of layer which is at MSL
                max_area=-901
                min_area=999999.9
                max_vsecno=-901
                min_vsecno=-901
                do vsecno=1,num_virt_sec(channo)
                    virtelev=1
                    do while ( elev_index(channo)+virtelev < max_total_elevations .and. &
                        virtelev <= num_layers(channo) .and. &
                        -virt_min_elev(minelev_index(channo)+vsecno-1) >= &
                        virt_elevation(elev_index(channo)+virtelev) )
                        virtelev=virtelev+1
                    enddo
                    veindex=elev_index(channo)+virtelev-1
                    di=dindex(channo,vsecno,virtelev)
                    x1=virt_elevation(veindex)
                    x2=-virt_min_elev(minelev_index(channo)+vsecno-1)
                    a1=virt_area(di)
                    b1=virt_width(di)
                    b2=virt_width(di+1)
                    msl_area(vsecno) = a1+(0.5*(b1+b2))*(x2-x1)
                    if (min_area > msl_area(vsecno)) then
                        min_area = msl_area(vsecno)
                        min_vsecno = vsecno
                    elseif (max_area < msl_area(vsecno)) then
                        max_area = msl_area(vsecno)
                        max_vsecno = vsecno
                    endif
                enddo
                !-----------compare area(MSL) of all xsect--find max & min

700             format(i7,3x,2(i6,3x),2(f10.2,2x),3(f10.2,2x))
                if (max_area > min_area * area_tolerance) then
                    if (min_area > 0) then
                        write(unit_output,700) &
                            channo &
                            ,max_vsecno &
                            , min_vsecno &
                            ,-virt_min_elev(minelev_index(channo)+max_vsecno-1) &
                            ,-virt_min_elev(minelev_index(channo)+min_vsecno-1) &
                            ,max_area &
                            ,min_area &
                            ,max_area/min_area
                    else
                        write(unit_output,*) &
                            'Cross-section in channel ',channo,' has min_elev > MSL.  Unable to calculate area(MSL)'
                    endif
                endif
            endif
        enddo

        return
    end subroutine

    subroutine array_bounds_exceeded(channo)
        use IO_Units
        use common_xsect, disabled => virt_xsect    !@# variable name same as module name.
        implicit none


        integer channo

        write(unit_error,*) 'Too many elevations assigned to channel '
        write(unit_error,*) channo, '.  Maximum number of elevations'
        write(unit_error,*) 'allowed=',max_elevations,'. Try reducing'
        write(unit_error,*) 'the number of points in the cross-sections'
        write(unit_error,*) 'assigned to this channel'
        call exit(2)

        return
    end subroutine

end module
