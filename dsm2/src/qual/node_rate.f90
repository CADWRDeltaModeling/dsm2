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

      subroutine node_rate(intnode, direction, group_ndx, &
          objflow, massrate)

!-----Return flows and massrates, either into or out of, for qual node,
!-----for given group index (if none given ignore group).

      Use IO_Units
      Use Groups,only: groupArray
      use common_tide
      use iopath_data
      use grid_data
      use network
      use utilities
      implicit none

      include 'param.inc'
      include 'bltm1.inc'
      include 'bltm3.inc'
      include 'bltm2.inc'

!-----local variables
      logical group_ndx_ok      ! true if flow's group match input
      logical, save :: err_node(max_nodes) ! nodal error counter

      integer &
          intnode, &              ! internal node number [INPUT]
          direction, &           ! either FROM_OBJ or TO_OBJ [INPUT]
          group_ndx, &           ! group index, if 0 ignore [INPUT]
          qndx, &                ! external and internal flow index
          pndx, &                ! pathname index
          res, &                 ! reservoir number
          conndx, &              ! reservoir connection index
          i,j,k               ! loop indices

      real*8 &
          objflow, &              ! total external and internal flow at node [OUTPUT]
          massrate(max_constituent), & ! total external and internal massrate at node [OUTPUT]
          conc, &                ! flow concentration
          node_qual           ! node quality function
      real*8 :: tol	= 1.0D-8    ! error tolerance

      type(from_to_t) from,to ! from and to objects

      objflow=0.0
      massrate=0.0

!-----external flows
      i=1
      do while (node_geom(intnode).qext(i) .gt. 0) ! todo: bad way of looping nqext
         qndx=node_geom(intnode).qext(i)
         group_ndx_ok= &
             group_ndx .eq. ALL_FLOWS .or. &
             group_ndx .eq. NO_CONNECT .or. &
             group_ndx .eq. QEXT_FLOWS .or. &
             group_ndx .eq. 0 .or. &
!--------fixme:group need multiple group membership
             qext(qndx).group_ndx .eq. 0 .or. &
             (group_ndx .gt. 0 .and. group_ndx .eq. qext(qndx).group_ndx)

         if (group_ndx_ok) then
            if (direction .eq. TO_OBJ .and. &
                qext(qndx).avg .gt. tol) then ! direction and flow to node
               objflow=objflow + qext(qndx).avg
               if (n_conqext(qndx) .eq. 0 .and. .not. err_node(intnode)) then
                  err_node(intnode)=.true.
                  write(unit_error,610) &
                      trim(obj_names(qext(qndx).attach_obj_type)), &
                      trim(qext(qndx).attach_obj_name), &
                      trim(qext(qndx).name), &
                      trim(groupArray(qext(qndx).group_ndx).name)
 610              format(/'Warning; no input path constituent'/ &
                      ' for 'a,' ',a,' (flow input name: ',a,') group ',a,'; assumed zero.')
                  conc=0.0
               else
                  do k=1,n_conqext(qndx)
                     pndx=const_qext(qndx,k) ! input path index to constituents for this flow
                     conc=pathinput(pndx).value
                     do j=1,pathinput(pndx).n_consts
                        massrate(pathinput(pndx).const_ndx(j))= &
                            massrate(pathinput(pndx).const_ndx(j)) &
                            + qext(qndx).avg * conc
                     enddo
                  enddo
               endif
            else if (direction .eq. FROM_OBJ .and. &
                   qext(qndx).avg .lt. -tol) then ! direction and flow from node
               objflow=objflow + qext(qndx).avg
               do j=1,neq
                  conc=node_qual(intnode,j)
                  massrate(j)=massrate(j) &
                      + qext(qndx).avg * conc * qext(qndx).mass_frac
               enddo
            endif
         endif
         i=i+1
      enddo

!-----internal flows
      i=1
      do while (node_geom(intnode).qinternal(i) .ne. 0)
         qndx=node_geom(intnode).qinternal(i)

         call obj2obj_direc(obj2obj(qndx).flow_avg, &
             obj2obj(qndx), from, to)

         if (direction .eq. TO_OBJ) then ! flow to node wanted
!-----------is the object correct type and number?
            if (to.obj_type .eq. obj_node .and. &
                to.obj_no .eq. intnode) then
!--------------does group label match?
               group_ndx_ok= &
                   group_ndx .eq. ALL_FLOWS .or. &
                   group_ndx .eq. NO_CONNECT .or. &
                   group_ndx .eq. QINT_FLOWS .or. &
                   group_ndx .eq. 0 .or. &
                   to.group_ndx .eq. 0 .or. &
                   (group_ndx .gt. 0 .and. group_ndx .eq. to.group_ndx)

               if (group_ndx_ok) then
                  objflow=objflow + abs(obj2obj(qndx).flow_avg)
                  do j=1,neq
!--------------------determine concentration of 'from' object
                     if (from.obj_type .eq. obj_node) then
                        conc=node_qual(from.obj_no,j)
                     else if (from.obj_type .eq. obj_reservoir) then
                        conc=cres(from.obj_no,j)
                     endif
                     massrate(j)= massrate(j) &
                         + abs(obj2obj(qndx).flow_avg) * conc * from.mass_frac
                  enddo
               endif            ! group label ok
            endif
         else                   ! flow from node wanted
!-----------is the object correct type and number?
            if (from.obj_type .eq. obj_node .and. &
                from.obj_no .eq. intnode) then
!--------------does group label match?
               group_ndx_ok= &
                   group_ndx .eq. ALL_FLOWS .or. &
                   group_ndx .eq. NO_CONNECT .or. &
                   group_ndx .eq. QINT_FLOWS .or. &
                   group_ndx .eq. 0 .or. &
                   to.group_ndx .eq. 0 .or. &
                   (group_ndx .gt. 0 .and. group_ndx .eq. from.group_ndx)

               if (group_ndx_ok) then
                  objflow=objflow - abs(obj2obj(qndx).flow_avg)
                  do j=1,neq
                     conc=node_qual(intnode,j)
                     massrate(j)=massrate(j) &
                         - abs(obj2obj(qndx).flow_avg) * conc * from.mass_frac
                  enddo
               endif
            endif
         endif
         i=i+1
      enddo

!-----reservoir flows connected to node

      group_ndx_ok= &
          group_ndx .eq. ALL_FLOWS .or. &
!     &     group_ndx .eq. QEXT_FLOWS .or. &
          group_ndx .eq. QINT_FLOWS .or. &
          group_ndx .eq. 0

      if (group_ndx_ok) then    ! no group
         do i=1,nconres(intnode)
            res=lconres(intnode,i,1)
            conndx=lconres(intnode,i,2)
!-----------positive qres means flow from reservoir to node
            if (direction .eq. TO_OBJ .and. &
                qres(res,conndx) .gt. tol) then ! flow to node
               objflow=objflow + qres(res,conndx)
               do j=1,neq
                  massrate(j)=massrate(j) + qres(res,conndx) * cres(res,j)
               enddo
               if(intnode.eq.220) then
               endif
            else if (direction .eq. FROM_OBJ .and. &
                   qres(res,conndx) .lt. -tol) then ! flow from node
               objflow=objflow + qres(res,conndx)
               do j=1,neq
                  conc=node_qual(intnode,j)
                  massrate(j)=massrate(j) + qres(res,conndx) * conc
               enddo
            endif
         enddo
      endif

      return
      end
