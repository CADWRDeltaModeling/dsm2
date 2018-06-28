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

      subroutine res_rate(res, direction, group_ndx,
     &     objflow, massrate)

c-----Return flows and massrates, either into or out of, for qual reservoir,
c-----for given group label index (if none given ignore group
c-----labels).
      Use IO_Units
      Use Groups, only: groupArray
      use common_tide
      use constants
      use type_defs
      use iopath_data
      use grid_data
      use network
      implicit none

      include 'param.inc'


      include 'bltm1.inc'
      include 'bltm3.inc'
      include 'bltm2.inc'

c-----local variables
      logical group_ndx_ok      ! true if flow's group label match input
      logical, save :: err_res(max_reservoirs) ! nodal error counter

      integer
     &     res                  ! qual reservoir number [INPUT]
     &     ,direction           ! either FROM_OBJ or TO_OBJ [INPUT]
     &     ,group_ndx           ! group index, if 0 ignore [INPUT]
     &     ,qndx                ! external and internal flow index
     &     ,pndx                ! pathname index
     &     ,intnode             ! int node number
     &     ,i,j,k               ! loop indices
     &     ,ich                 ! channel number

      real*8
     &     objflow              ! total external and internal flow at reservoir [OUTPUT]
     &     ,massrate(max_constituent) ! total external and internal massrate at reservoir [OUTPUT]
     &     ,conc                ! flow concentration
     &     ,node_qual           ! node quality function
     

      type(from_to_t) from,to ! from and to objects
      real*8 :: tol = 1.0d-3         ! "zero" with a tolerance

      objflow=0.0
      massrate=0.0

c-----external flows
      i=1
      do while (res_geom(res).qext(i) .ne. 0)
         qndx=res_geom(res).qext(i)
c--------fixme:group need multiple group membership
         group_ndx_ok=
     &        group_ndx .eq. ALL_FLOWS .or.
     &        group_ndx .eq. NO_CONNECT .or.
     &        group_ndx .eq. QEXT_FLOWS .or.
     &        group_ndx .eq. 0 .or.
     &        qext(qndx).group_ndx .eq. 0 .or.
     &        (group_ndx .gt. 0 .and. group_ndx .eq. qext(qndx).group_ndx)

         if (group_ndx_ok) then
            if (direction .eq. TO_OBJ .and.
     &           qext(qndx).avg .gt. tol) then ! direction and flow to reservoir
               objflow=objflow + qext(qndx).avg
               if (n_conqext(qndx) .eq. 0 .and. .not. err_res(res)) then
                  err_res(res)=.true.
                  write(unit_error,610)
     &                 trim(obj_names(qext(qndx).attach_obj_type)),
     &                 trim(qext(qndx).attach_obj_name),
     &                 trim(qext(qndx).name),
     &                 trim(groupArray(qext(qndx).group_ndx).name)
 610              format(/'Warning; no constituent input',/
     &                 ' for 'a,' ',a,' group (flow input name: ',a,') type ',a,'; assumed zero.')
                  conc=0.0
               else
                  do k=1,n_conqext(qndx)
                     pndx=const_qext(qndx,k) ! input path index to constituents for this flow
                     conc=pathinput(pndx).value
                     do j=1,pathinput(pndx).n_consts
                        massrate(pathinput(pndx).const_ndx(j))=
     &                       massrate(pathinput(pndx).const_ndx(j))
     &                       + qext(qndx).avg * conc
                     enddo
                  enddo
               endif
            else if (direction .eq. FROM_OBJ .and.
     &              qext(qndx).avg .lt. -tol) then ! direction and flow from reservoir
               objflow=objflow + qext(qndx).avg
               do j=1,neq
                  massrate(j)=massrate(j)
     &                 + qext(qndx).avg * cres(res,j)
     &                 * qext(qndx).mass_frac
               enddo
            endif
         endif
         i=i+1
      enddo

c-----internal flows
      i=1
      do while (res_geom(res).qinternal(i) .ne. 0)
         qndx=res_geom(res).qinternal(i)

         call obj2obj_direc(obj2obj(qndx).flow_avg,
     &        obj2obj(qndx), from, to)

         if (direction .eq. TO_OBJ) then ! flow to reservoir wanted
c-----------is the object correct type and number?
            if (to.obj_type .eq. obj_reservoir .and.
     &           to.obj_no .eq. res) then
c--------------does group label match?
               group_ndx_ok=
     &              group_ndx .eq. ALL_FLOWS .or.
     &              group_ndx .eq. NO_CONNECT .or.
     &              group_ndx .eq. QINT_FLOWS .or.
     &              group_ndx .eq. 0 .or.
     &              to.group_ndx .eq. 0 .or.
     &              (group_ndx .gt. 0 .and. group_ndx .eq. to.group_ndx)

               if (group_ndx_ok) then
                  objflow=objflow + abs(obj2obj(qndx).flow_avg)
                  do j=1,neq
c--------------------determine concentration of 'from' object
                     if (from.obj_type .eq. obj_node) then
                        conc=node_qual(from.obj_no,j)
                     else if (from.obj_type .eq. obj_reservoir) then
                        conc=cres(from.obj_no,j)
                     endif
                     massrate(j)= massrate(j)
     &                    + abs(obj2obj(qndx).flow_avg) * conc * from.mass_frac
                  enddo
               endif            ! group label ok
            endif
         else                   ! flow from reservoir wanted
c-----------is the object correct type and number?
            if (from.obj_type .eq. obj_reservoir .and.
     &           from.obj_no .eq. res) then
c--------------does group label match?
               group_ndx_ok=
     &              group_ndx .eq. ALL_FLOWS .or.
     &              group_ndx .eq. NO_CONNECT .or.
     &              group_ndx .eq. QINT_FLOWS .or.
     &              group_ndx .eq. 0 .or.
     &              to.group_ndx .eq. 0 .or.
     &              (group_ndx .gt. 0 .and. group_ndx .eq. from.group_ndx)

               if (group_ndx_ok) then
                  objflow=objflow - abs(obj2obj(qndx).flow_avg)
                  do j=1,neq
                     massrate(j)=massrate(j)
     &                    - abs(obj2obj(qndx).flow_avg) * cres(res,j)
     &                    * from.mass_frac
                  enddo
               endif
            endif
         endif
         i=i+1
      enddo

      group_ndx_ok=
     &     group_ndx .eq. ALL_FLOWS .or.
     &     group_ndx .eq. QEXT_FLOWS .or.
     &     group_ndx .eq. QINT_FLOWS .or.
     &     group_ndx .eq. 0

      if (group_ndx_ok) then
c--------reservoir flows connected to nodes

c--------positive qres means flow from reservoir to node
         do i=1,res_geom(res).nnodes
            if (direction .eq. TO_OBJ .and.
     &           qres(res,i) .lt. -tol) then ! flow to reservoir
               objflow=objflow - qres(res,i)
               intnode=res_geom(res).node_no(i)
               do j=1,neq
                  if (node_geom(intnode).boundary_type .NE. stage_boundary) then
c--------------------internal node or external non-stage-BC nodes connected to a reservoir
                     conc=node_qual(intnode,j)
                     massrate(j)=massrate(j)
     &                    - qres(res,i) * conc
                  else
c--------------------external node (stage BC) connected to a reservoir
                     if (listup(intnode,1).gt.0) then
c-----------------------reservoir connected at the upstream boundary
                        ich=listup(intnode,1)
                        massrate(j)=massrate(j)+gptu(j,ich)/(dt*3600.)
                     else if (listdown(intnode,1).gt.0) then
c-----------------------reservoir connected at the downstream boundary
                        ich=listdown(intnode,1)
                        massrate(j)=massrate(j)+gptd(j,ich)/(dt*3600.)
                     else
                        write(unit_error,905)
 905                    format('Program Bug in res_rate')
                        call exit(2)
                     endif
                  endif
               enddo
            else if (direction .eq. FROM_OBJ .and.
     &              qres(res,i) .gt. tol) then ! flow from reservoir
               objflow=objflow - qres(res,i)
               do j=1,neq
                  massrate(j)=massrate(j)
     &                 - qres(res,i) * cres(res,j)
               enddo
            endif
         enddo
      endif

      return
      end
