*=================================================

      real*8 function fetch_data(source)
      implicit none
c----- Fetch time varying data from a data source such as
c      DSS, an expression or a constant value
      real*8 get_expression_data
	external get_expression_data
      include '../fixed/common.f'
	record /datasource_s/ source

      if (source.source_type .eq. const_data)  then
       fetch_data=source.value
      else if (source.source_type .eq. dss_data) then   !fetch from dss path 
        fetch_data=pathinput(source.indx_ptr).value
      else if (source.source_type .eq. expression_data) then
    	  fetch_data=get_expression_data(source.indx_ptr)
	else
	  fetch_data=miss_val_r
	end if
      ! fixme: need expressions
c      print*,source.source_type, source.value
	return
	end function



