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

      subroutine load_channel_initcond(StmtHndl, ModelID, param, istat)

c-----load f90SQL modules
      use f90SQLConstants
      use f90SQL
      use IO_Units
      use DSM2_database
      use logging
      use constants
      use grid_data
      implicit none

      include '../hydrolib/network.inc'
      include '../hydrolib/chinitcd.inc'

c-----arguments
      integer(SQLHANDLE_KIND):: StmtHndl
      integer(DBASE_ID_KIND) ModelID           ! which ModelID to select
     &     ,istat               ! status

c-----f90SQL variables
      character(len=1000)::StmtStr
      integer(SQLRETURN_KIND)::iRet
      integer(SQLSMALLINT_KIND)::ColNumber ! SQL table column number

c-----local variables
      integer UseObj

      integer
     &     ID                   ! transfer ID
     &     ,chan,extchan
     &     ,Distance
     &     ,prev_extchan
     &     ,ninitcond
     &     ,k
     &     ,layer
     &     ,prev_layer

      integer ext2int
      real*8
     &     Value

      character*(*)
     &     param                ! parameter (stage,flow...) being loaded
     &     ,trim                ! trim blanks function

      logical :: distZeroInit=.false. ! IC provided for current channel, dist zero
      logical :: distLenInit=.false. ! IC provided for current channel, dist 'length'
      logical,save :: xLocationsLoaded = .false.
      integer :: origNumber=miss_val_i

c-----Bind the parameter representing ModelID
      call f90SQLBindParameter (StmtHndl, int(1,SQLUSMALLINT_KIND), SQL_PARAM_INPUT,
     &     SQL_F_SLONG, SQL_INTEGER, int(4,SQLUINTEGER_KIND),  int(0,SQLSMALLINT_KIND),
     &     ModelID, f90SQL_NULL_PTR, iRet)

c-----Execute SQL statement

      StmtStr="SELECT icid,used,channel_number,distance," //
     &     "initial_value,layer " //
     &     "FROM channel_init_condition, model_component " //
     &     "WHERE layer_id=Model_Component.Component_ID " //
     &     "AND model_component.component_type = 'grid' " //
     &     "AND model_component.model_id = ? " //
     &     "AND variable_name='" // trim(param) //"' " //
     &     "ORDER BY channel_number, ABS(distance), model_component.layer DESC;"

      call f90SQLExecDirect(StmtHndl, StmtStr,iRet)

      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5/)') 'Error in making initial condition SQL request',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3) write(unit_screen,'(a)') 'Made initial condition SQL request'
      endif

c-----Bind variables to columns in result set
      ColNumber=1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, ID,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, UseObj,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLong, extchan,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLong, Distance,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_Double, Value,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLong, Layer,
     &     f90SQL_NULL_PTR, iRet)

      if (print_level .ge. 3) write(unit_screen,'(a)') 'Made initial condition bind request'

c-----Initialize x locations of initial conditions if no other initial
c-----conditions have been loaded for other parameters. If other initial conditions
c-----have been loaded, the x locations must match (e.g. the channel distances
c-----for flow and stage. fixme: do we really want this?
      if (.not. xLocationsLoaded ) then
         NUserInitLocations=0
         InitialX=0.0
      end if

c-----Loop to fetch records, one at a time
      distZeroInit=.true.
      distLenInit=.true.
      ninitcond=0
      prev_extchan=miss_val_i
      prev_layer=miss_val_i
      k=0

      do while (.true.)

c--------Fetch a record from the result set
         call f90SQLFetch(StmtHndl,iRet)
         if (iRet .eq. SQL_NO_DATA) then
            if (print_level .gt.2) then
               write(unit_screen,
     &              "('Warning: no default initial channel data found for ',a)") param
            end if
            exit
         end if

c--------use only the last version of the initial condition

         if ( ((extchan .ne. prev_extchan) .or.
     &        (extchan .eq. prev_extchan .and. layer .eq. prev_layer))  .and.
     &        UseObj) then
            k=k+1
            if(extchan .ne. prev_extchan) then
                                ! Check to make sure that IC were provided at beginning
                                ! and end of last channel
               if ( .not. (distZeroInit .and. distLenInit)) then
                  istat=-3
                  write(unit_error,'(a/,i5,a)')
     &                 'Initial ' // param //
     &                 ' not provided for the up or downstream end of channel ',
     &                 prev_extchan
                  return
               end if
               chan=ext2int(extchan)
               if (chan .le. 0) then ! non-existant channel
                  write(unit_error,*)"Attempt to initialize unused channel ignored."
	            write(unit_error,*)"Channel: ",extchan
               else
                  FirstLocation(chan) = k
                  distZeroInit=.false.
                  distLenInit=.false.
                  prev_layer=layer !only update layer once per channel
               end if
            end if
            if (chan .eq. 0) then ! non-existant channel
               k=k-1
               goto 100
            endif
            if (distance .eq. chan_length) distance=chan_geom(chan).length
            if (xLocationsLoaded) then
               if( InitialX(k) .ne. dble(distance)) then
                  write(unit_error,'(a,1x,i4,1x,a)')
     &                 'Initial condition points for flow and stage in channel',
     &                 extchan,
     &                 'not the same'
                  istat=-3
                  return
               end if
            else
               NUserInitLocations(chan)=NUserInitLocations(chan)+1
               InitialX(k)=dble(distance)
            end if
            if(distance .eq. 0) distZeroInit = .true.
            if(distance .eq. chan_geom(chan).length)distLenInit=.true.
            if (param .eq.'stage') InitialWS(k)=value
            if (param .eq. 'flow') InitialQ(k)=value

         endif
         prev_extchan=extchan
         ninitcond=ninitcond+1
 100     continue
      enddo

      if (ninitcond .eq. 0) then
         write(unit_error, '(a)')
     &        'Error: no channel initial conditions records retrieved.'
         istat=-3
         return
      endif

!     check last channel. fixme: this seems kludgy, but harmless.
      if ( .not. distZeroInit .and. distLenInit) then
         istat=-3
         write(unit_error,'(a/,i5)')
     &        'Initial condition not provided for the up or downstream end of channel ',prev_extchan
         return
      end if

      if (print_level .ge. 2)
     &     write(unit_screen,'(a,i4,a/)') 'Read in ',ninitcond,' init conditions for '//trim(param)

      call f90SQLFreeStmt(StmtHndl,SQL_UNBIND, iRet)
      call f90SQLCloseCursor (StmtHndl, iRet)
      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5//)') 'Error in unbinding initial condition SQL',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3)
     &        write(unit_screen,'(a//)') 'Unbound initial condition SQL'
      endif
      xlocationsLoaded=.true.
      if ((orignumber .ne. miss_val_i) .and. (ninitcond .ne. orignumber)) then
         write(unit_error,'(a/,a/)') 'Initial conditions for flow/stage must be specified at the same points',
     &        'Some are missing.'
         istat=-3
         return
      else
         orignumber=ninitcond
      end if

 630  format(/a,i5)

      istat=ninitcond
      return
      end

      subroutine load_reservoir_initcond(StmtHndl, ModelID, param, istat)

c-----load f90SQL modules
      use f90SQLConstants
      use f90SQL
      use IO_Units
      use DSM2_database
      use constants
      use logging
      use grid_data
      implicit none

      include '../hydrolib/network.inc'
      include '../hydrolib/chconnec.inc'

c-----arguments
      integer(SQLHANDLE_KIND):: StmtHndl
      integer(DBASE_ID_KIND) ModelID           ! which ModelID to select
     &     ,istat               ! status

c-----f90SQL variables
      character(len=1000)::StmtStr
      integer(SQLRETURN_KIND)::iRet
      integer(SQLSMALLINT_KIND)::ColNumber ! SQL table column number

c-----local variables
      integer UseObj
      character
     &     resname*32
     &     ,prev_resname*32

      integer
     &     ID                   !database id of initial condition
     &     ,ninitcond
     &     ,i
     &     ,resno
     &     ,name_to_objno

      real*8
     &     Value

      character*(*)
     &     param                ! parameter whose initial condition is being sought

      integer(SQLINTEGER_KIND):: namelen

c-----Bind the parameter representing ModelID
      call f90SQLBindParameter (StmtHndl, int(1,SQLUSMALLINT_KIND), SQL_PARAM_INPUT,
     &     SQL_F_SLONG, SQL_INTEGER, int(4,SQLUINTEGER_KIND),  int(0,SQLSMALLINT_KIND),
     &     ModelID, f90SQL_NULL_PTR, iRet)

c-----Execute SQL statement
      StmtStr="SELECT icid,used,reservoir_name,initial_value " //
     &     "FROM reservoir_init_condition,model_component " //
     &     "WHERE layer_id = model_component.component_id " //
     &     "AND model_component.component_type = 'grid' " //
     &     "AND model_component.model_id = ? " //
     &     "ORDER BY reservoir_name, model_component.layer DESC;"

      call f90SQLExecDirect(StmtHndl, StmtStr,iRet)

      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5/)') 'Error in making initial condition SQL request',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3) write(unit_screen,'(a)') 'Made initial condition SQL request'
      endif

c-----Bind variables to columns in result set
      ColNumber=1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, ID,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, UseObj,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, resname,
     &     loc(namelen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_Double, value,
     &     f90SQL_NULL_PTR, iRet)

      if (print_level .ge. 3) write(unit_screen,'(a)') 'Made initial condition bind request'
c-----Loop to fetch records, one at a time
      ninitcond=0
      prev_resname = miss_val_c
      YRes=miss_val_i           ! initialize to missing

      do while (.true.)

c--------Fetch a record from the result set
         call f90SQLFetch(StmtHndl,iRet)
         if (iRet .eq. SQL_NO_DATA) exit

         resname=resname(1:namelen)
	   resname=trim(resname)
         call locase(resname)

c--------use only the last version of a transfer, and skip
c--------if the transfer is marked as not-use
         if ( (resname .ne. prev_resname)  .and.
     &        UseObj) then
            resno=name_to_objno(obj_reservoir,resname)
            if (resno .eq. miss_val_i) then ! reservoir doesn't exist for init val
               write(unit_error, '(a)') 'Reservoir '
     &              // trim(resname) // ' does not exist to load initial conditions.'
            else
               if(param .eq. 'stage') YRes(resno)=value
               prev_resname=resname
               ninitcond=ninitcond+1
            endif
         endif
      enddo
c@@@      if (ninitcond .eq. 0) then
c@@@         write(unit_error, '(a)')
c@@@     &        'Error: no reservoir initial conditions records retrieved.'
c@@@         istat=-3
c@@@         return
c@@@      endif
                                ! Verify a complete set of IC
      do i=1,nreser
         if (YRes(i).eq.miss_val_i .and. print_level .gt.2) then
            write(unit_error,'(a,a)')
     &           'Warning: No default initial condition for reservoir: ',
     &           res_geom(i).name
         end if
      end do

      if (print_level .ge. 2)
     &     write(unit_screen,'(a/)') 'Read in all reservoir init condition for ', param

      call f90SQLFreeStmt(StmtHndl,SQL_UNBIND, iRet)
      call f90SQLCloseCursor (StmtHndl, iRet)
      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5//)') 'Error in unbinding initial condition SQL',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3)
     &        write(unit_screen,'(a//)') 'Unbound initial condition SQL'
      endif

 630  format(/a,i5)

      istat=ninitcond
      return
      end