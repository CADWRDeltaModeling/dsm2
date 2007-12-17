
c     functions to query the hydro model time
c     fixme: the jliymd function is expensive, and it might be
c            better for hydro to keep track of its own time
      integer function getModelTime()
	implicit none
	include '../fixed/common.f'
	getModelTime=julmin
	return
	end function

      integer function getModelJulianDay()
	implicit none
	integer,parameter :: MIN_PER_DAY=60*24
      integer getModelTime
	getModelJulianDay=getModelTime()/MIN_PER_DAY
	return
	end function


      integer function getModelYear()
	implicit none
      integer m,d,y
      integer getModelJulianDay
	call jliymd(getModelJulianDay(),y,m,d)
	getModelYear=y
	return
	end function

      integer function getModelMonth()
	implicit none
	include '../fixed/common.f'
      integer m,d,y
      integer getModelJulianDay
	call jliymd(getModelJulianDay(),y,m,d)
	getModelMonth=m
	return
	end function


      integer function getModelDay()
	implicit none
      integer m,d,y
      integer getModelJulianDay
	call jliymd(getModelJulianDay(),y,m,d)
	getModelDay=d
	return
	end function


      integer function getModelDayOfYear()
	implicit none
      integer m,d,y,yearstart,jday
      integer getModelJulianDay,iymdjl
	jday=getModelJulianDay()
	call jliymd(jday,y,m,d)
	yearstart=iymdjl(y,1,1)
	getModelDayOfYear=jday-yearstart
	return
	end function


      integer function getModelMinuteOfDay()
	implicit none
	include '../fixed/common.f'
	integer,parameter :: MIN_PER_DAY=60*24
	getModelMinuteOfDay=mod(julmin,MIN_PER_DAY)
	return
	end function

      integer function getModelHour()
	implicit none
	include '../fixed/common.f'
	integer,parameter :: MIN_PER_HOUR=60
	integer getModelMinuteOfDay
	getModelHour=(getModelMinuteOfDay()/MIN_PER_HOUR)
	return
	end function

      integer function getReferenceMinuteOfYear(mon,day,hour,min)
	implicit none
	include '../fixed/common.f'
	integer iymdjl  ! function converts ymd to julian day
	integer yr,dayyr,jday,mon,day,hour,min,getModelYear,yearstart
	integer,parameter :: MIN_PER_DAY=60*24

	yr=getModelYear()
	jday=iymdjl(yr,mon,day)
	yearstart=iymdjl(yr,1,1)
	dayyr=jday-yearstart
	getReferenceMinuteOfYear=MIN_PER_DAY*dayyr + 60*hour + min
	return
	end function

      integer function getModelMinuteOfYear()
	implicit none
	include '../fixed/common.f'
	integer,parameter :: MIN_PER_DAY=60*24
	integer getModelMinuteOfDay,getModelDayOfYear
	getModelMinuteOfYear=MIN_PER_DAY* getModelDayOfYear() + getModelMinuteOfDay()
	return
	end function

      integer function getModelMinute()
	implicit none
	include '../fixed/common.f'
	integer,parameter :: MIN_PER_HOUR=60
	integer getModelMinuteOfDay
	getModelMinute=mod(getModelMinuteOfDay(),MIN_PER_HOUR)
	return
	end function


      integer function getModelTicks()
	implicit none
	include '../fixed/common.f'
	getModelTicks=julmin;
	return
	end function


      subroutine set_datasource(source,expr,val,timedep)
	implicit none
	include '../fixed/defs.f'
	include '../fixed/misc.f'
	record/datasource_s/source
	integer expr
	real*8 val
      logical timedep
      source.indx_ptr=expr
	source.value=val
	if (timedep)then
	   source.source_type=expression_data
      else 
	   source.source_type=const_data
      end if
      return
	end subroutine

      


      subroutine chan_comp_point(intchan, distance,
     &                           comp_points,weights)

*-----Purpose: Wrapper to CompPointAtDist that uses arrays so that
*     the arguments are pass-by-reference (mainly for calls from C)
!fixme: probably could change the interface of CompPointAtDist and
! do away with this wrapper
      implicit none

*   Arguments:
      integer :: intchan  ! Channel where comp point is being requested
	real*8  :: distance     ! Downstream distance along intchan
	integer :: comp_points(2)
	real*8 :: weights(2)
      call CompPointAtDist(intchan,distance,comp_points(1),
     &	  comp_points(2), weights(1), weights(2))
      return
	end subroutine

      integer function resNdx(name)
	implicit none
      include '../fixed/common.f'
	integer i
	character*(*) name
	resNdx=miss_val_i
	call locase(name)
	do i=1,nreser
	   if (res_geom(i).name .eq. name)	then
	     resNdx = i
	     exit
	   end if
	end do
	return
	end function

      integer function gateNdx(name)
	use Gates, only: GateArray,nGate
	implicit none
	include '../fixed/misc.f'
	integer i
	character*(*) name
	gateNdx=miss_val_i
	call locase(name)
	do i=1,nGate
	   if (name .eq. GateArray(i).name)	then
	     gateNdx = i
	     exit
	   end if
	end do
	return
	end function

      integer function deviceNdx(gatendx, devname)
	use Gates, only: GateArray,deviceIndex
	implicit none
      integer gatendx
	character*(*) devname
	character*32 ldevname
	ldevName=devName
	call locase(ldevname)
	deviceNdx=deviceIndex(GateArray(gatendx),ldevName)
	return
	end function

c     return the constant, so FORTRAN and C can share it
	integer function direct_to_node()
	use GATES, only: FLOW_COEF_TO_NODE
	implicit none
	direct_to_node=FLOW_COEF_TO_NODE
	return
	end function

c     return the constant, so FORTRAN and C can share it
	integer function direct_from_node()
	use GATES, only: FLOW_COEF_FROM_NODE
	implicit none
	direct_from_node=FLOW_COEF_FROM_NODE
	return
	end function


      real*8 function get_external_flow(ndx)
	implicit none
      include '../fixed/defs.f'
      include '../hdf_tidefile/tide.inc'
      integer ndx
      get_external_flow=qext(ndx).flow
	return
	end function

      subroutine set_external_flow(ndx,val)
	implicit none
      include '../fixed/defs.f'
      include '../hdf_tidefile/tide.inc'
      integer ndx
      real*8 val
      qext(ndx).flow=val
	return
	end subroutine

	subroutine set_external_flow_datasource(ndx,expr,val,timedep)
	implicit none
	include '../fixed/defs.f'
      include '../hdf_tidefile/tide.inc'
	integer ndx, expr
	real*8 val
	logical timedep  
      call set_datasource(qext(ndx).datasource,expr,val,timedep)
	return
	end subroutine

      real*8 function get_transfer_flow(ndx)
	implicit none
      include '../fixed/common.f'
      integer ndx
      get_transfer_flow=obj2obj(ndx).flow
	return
	end function

      subroutine set_transfer_flow(ndx,val)
	implicit none
      include '../fixed/common.f'
      integer ndx
      real*8 val
      obj2obj(ndx).flow=val
	return
	end subroutine

	subroutine set_transfer_flow_datasource(ndx,expr,val,timedep)
	implicit none
      include '../fixed/common.f'
	integer ndx, expr
	real*8 val
	logical timedep  
      call set_datasource(obj2obj(ndx).datasource,expr,val,timedep)
	return
	end subroutine




	subroutine set_gate_install(ndx, install)
	use Gates, only: GateArray,setFree
      implicit none
	integer ndx
	real*8 install
	if(install .eq. 0.D0)then
	  call setFree(GateArray(ndx), .true.)
      else
	  call setFree(GateArray(ndx), .false.)
	end if
      return
	end subroutine


	real*8 function is_gate_install(ndx)
	use Gates, only: GateArray
	implicit none
	integer ndx
	if (GateArray(ndx).free)then
	   is_gate_install=0.0
	else
	   is_gate_install=1.0
	endif
      return
	end function


      real(8) function get_device_op_coef(gndx, devndx, direction)
	use Gates, only: GateArray
      implicit none
	integer gndx, devndx, direction, direct_to_node
	if (direction .eq. direct_to_node())then
        get_device_op_coef=GateArray(gndx).Devices(devndx).opCoefToNode
	else
	  get_device_op_coef=GateArray(gndx).Devices(devndx).opCoefFromNode
	end if
      return 
      end function

      subroutine set_device_op_coef(gndx, devndx, direction, val)
	use Gates, only: GateArray
      implicit none
	integer gndx, devndx,direction, direct_to_node
	real(8) val
	if (direction .eq. direct_to_node())then
        GateArray(gndx).Devices(devndx).opCoefToNode=val
	else
        GateArray(gndx).Devices(devndx).opCoefFromNode=val
	end if
	return
      end subroutine

      subroutine set_device_op_datasource(gndx, devndx, direction, expr, val, timedep)
	use Gates, only: GateArray
      implicit none
	include '../fixed/misc.f'
	integer gndx, devndx, direction, direct_to_node
	integer expr
	real*8 val
	logical timedep
	if (direction .eq. direct_to_node())then
	 call set_datasource(
     &    GateArray(gndx).Devices(devndx).op_to_node_datasource,expr,val,timedep)
	else
	 call set_datasource(
     &    GateArray(gndx).Devices(devndx).op_from_node_datasource,expr,val,timedep)
      end if
      return
 	end subroutine

!fixme: these shouldn't be interms of height

      real(8) function get_device_position(gndx, devndx)
	use Gates, only: GateArray
      implicit none
	integer gndx, devndx
      get_device_position=GateArray(gndx).Devices(devndx).position
      return 
      end function

!fixme: these shouldn't be interms of height
      subroutine set_device_position(gndx, devndx, val)
	use Gates, only: GateArray
      implicit none
	integer gndx, devndx
	real(8) val
      GateArray(gndx).Devices(devndx).position=val
      end subroutine

      subroutine set_device_position_datasource(gndx, devndx, expr, val, timedep)
	use Gates, only: GateArray
      implicit none
	include '../fixed/misc.f'
	integer gndx, devndx
	integer expr
	real*8 val
	logical timedep
	call set_datasource(
     &    GateArray(gndx).Devices(devndx).pos_datasource,expr,val,timedep)
      return
 	end subroutine

      real(8) function get_device_height(gndx, devndx)
	use Gates, only: GateArray
      implicit none
	integer gndx, devndx
      get_device_height=GateArray(gndx).Devices(devndx).height
      return 
      end function

      subroutine set_device_height(gndx, devndx, val)
	use Gates, only: GateArray
      implicit none
	integer gndx, devndx
	real(8) val
      GateArray(gndx).Devices(devndx).height=val
      end subroutine

      real(8) function get_device_width(gndx, devndx)
	use Gates, only: GateArray
      implicit none
	integer gndx, devndx
      get_device_width=GateArray(gndx).Devices(devndx).maxWidth
      return 
      end function

      subroutine set_device_width(gndx, devndx, val)
	use Gates, only: GateArray
      implicit none
	integer gndx, devndx
	real(8) val
      GateArray(gndx).Devices(devndx).maxWidth=val
	return
      end subroutine

      real(8) function get_device_nduplicate(gndx, devndx)
	use Gates, only: GateArray
      implicit none
	integer gndx, devndx
      get_device_nduplicate=GateArray(gndx).Devices(devndx).nduplicate
      return 
      end function

      subroutine set_device_nduplicate(gndx, devndx, val)
	use Gates, only: GateArray
      implicit none
	integer gndx, devndx
	real(8) val
      GateArray(gndx).Devices(devndx).nduplicate=nint(val)
	return
      end subroutine

      real(8) function get_device_elev(gndx, devndx)
	use Gates, only: GateArray
      implicit none
	integer gndx, devndx
      get_device_elev=GateArray(gndx).Devices(devndx).baseElev
      return 
      end function

      subroutine 	set_device_elev(gndx, devndx, val)
	use Gates, only: GateArray
      implicit none
	integer gndx, devndx
	real(8) val
      GateArray(gndx).Devices(devndx).baseElev=val
	return
	end subroutine



      real(8) function get_device_flow_coef(gndx, devndx, direct)
	use Gates, only: GateArray
	use IO_Units
      implicit none
	integer gndx, devndx, direct
	integer direct_to_node,direct_from_node
	if (direct .eq. direct_to_node())then
	   get_device_flow_coef=GateArray(gndx).Devices(devndx).flowCoefToNode
	else if (direct .eq. direct_from_node()) then
	   get_device_flow_coef=GateArray(gndx).Devices(devndx).flowCoefFromNode
	else
	   write(unit_error,*)"Flow direction not recognized in get_device_flow_coef"
	   call exit(3)
	end if
      return 
      end function

      subroutine set_device_flow_coef(gndx, devndx, direct, val)
	use Gates, only: GateArray
	use IO_Units
      implicit none
	integer gndx, devndx, direct
	integer direct_to_node,direct_from_node
	real(8) val
	if (direct .eq. direct_to_node())then
         GateArray(gndx).Devices(devndx).flowCoefToNode=val
	else if (direct .eq. direct_from_node())then
         GateArray(gndx).Devices(devndx).flowCoefFromNode=val
	else
	   write(unit_error,*)"Flow direction not recognized in set_device_flow_coef"
	   call exit(3)
	end if
	return
      end subroutine



      real*8 function value_from_inputpath(i)
      implicit none
	include '../fixed/common.f'
      integer i
	value_from_inputpath=pathinput(i).value
      return
	end function


      integer function ts_index(name)
	implicit none
      include '../fixed/common.f'
	character*(*) name
	integer i
      ts_index = -1
	do i=1,ninpaths
	   if (trim(pathinput(i).name) .eq. trim(name))then
	      ts_index=i
	      return
	   end if
	end do
	end function

      integer function qext_index(name)
	implicit none
	include '../fixed/defs.f'
     
      include '../hdf_tidefile/tide.inc'
	character*(*) name
	integer i
      qext_index = -901
	do i=1,nqext
	   if (qext(i).name .eq. name)then
	      qext_index=i
	      return
	   end if
	end do
	end function

      integer function transfer_index(name)
	implicit none
     
      include '../fixed/common.f'
	character*(*) name
	integer i
      transfer_index = -901
	do i=1,nobj2obj
	   if (obj2obj(i).name .eq. name)then
	      transfer_index=i
	      return
	   end if
	end do
	end function



      real*8 function channel_length(intno)
	implicit none
     
      include '../fixed/common.f'
	integer intno
	channel_length= chan_geom(intno).length
	return
	end function


