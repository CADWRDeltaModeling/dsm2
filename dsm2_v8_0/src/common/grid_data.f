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


      module grid_data
      use type_defs
      integer
     &     max_channels         ! maximum number of channels
     &     ,max_xsects_tot      ! maximum number of cross sections total
     &     ,max_nodes           ! maximum number of nodes
     &     ,max_obj2obj         ! Maximum number of object to object connections
     &     ,max_stgbnd          ! maximum number of stage boundaries
     &     ,nstgbnd             ! actual number of stage boundaries
     &     ,nobj2obj            ! Actual number of object to object connections
     &     ,nchans              ! actual number of channels
     &     ,nintnodes           ! actual number of internal nodes
     &     ,nnodes              ! actual number of total nodes
     &     ,nxsects             ! actual number of cross sections    
      parameter (
     &     max_channels=800     ! MaxChannels should equal this
     &     ,max_xsects_tot=5*max_channels
     &     ,max_nodes=max_channels+10
     &     ,max_obj2obj=50
     &     ,max_stgbnd=5
     &     )
     
      type(channel_t) chan_geom(0:max_channels)
      type(node_t) node_geom(0:max_nodes)
      type(xsect_t) xsect_geom(0:max_xsects_tot)
      
      real*8
     &     area_tolerance       ! max allowable ratio of virt_area(MSL) @ chan ends
     &     ,levee_slope         ! slope of levees for xsect width extrapolation      
     
          
      integer
     &     max_reservoirs       ! maximum number of reservoirs
     &     ,nreser              ! actual number of reservoirs

      parameter (
     &     max_reservoirs=100
     &     )

      type(reservoir_t) res_geom(0:max_reservoirs)
      
c-----Node id numbers

      integer node_id(0:max_nodes)
      common /node_i_dentification/ node_id

      integer
     &     nchan_list           ! actual number of channel sequences

      integer
     &     int2ext(0:max_channels)
     &     ,resext2int(0:max_reservoirs)
     &     ,resint2ext(0:max_reservoirs)
     &     ,nodelist(0:max_nodes*2+1)

C-----Direct object to object flow transfer
      type(obj2obj_t) obj2obj(max_obj2obj)
      
c-----stage boundary object
      type(stgbnd_t) stgbnd(max_stgbnd)



c-----quad points
      integer :: nquadpts = 1             ! number of quadrature points

c-----used by virtual_xsect
      real*8 deltax_requested   ! delta x to use in spatial discretization

c-----accounting and object names, value codes, period type names
      integer,parameter :: max_group_memberships=20 ! max number of group memberships

      integer
     &     max_qext             ! maximum number of external flows
     &     ,nqext               ! number of actual external flows

      parameter (
     &     max_qext=1000
     &     )

      type(qext_t) qext(max_qext)
      integer const_qext(max_qext,max_conqext)
      integer n_conqext(max_qext) ! number of constituents at external flow
      common /com_conqext/ const_qext, n_conqext
           
      end module

   
     
      
    
    
     