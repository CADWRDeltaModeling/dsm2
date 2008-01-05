<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public License as published by
C!    the Free Software Foundation, either version 3 of the License, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public License for more details.

C!    You should have received a copy of the GNU General Public License
C!    along with DSM2.  If not, see <http://www.gnu.org/licenses/>.
</license>

*=================================================

      real*8 function fetch_data(source)
      use type_defs
      use constants
      use iopath_data
      implicit none
c----- Fetch time varying data from a data source such as
c      DSS, an expression or a constant value
      real*8 get_expression_data
	external get_expression_data
      
	type(datasource_t) ::  source

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



