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

      subroutine load_channels_SQL(StmtHndl, ModelID, istat)
      use IO_Units
      use dsm2_database
c-----load f90SQL modules
      use f90SQLConstants
      use f90SQL
      use logging
      use grid_data

      implicit none

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
     &     ID
     &     ,channo              ! channel number
     &     ,prev_channo         ! track same channel numbers
     &     ,chan_len            ! channel length
     &     ,chan_downnode       ! channel downstream node
     &     ,chan_upnode         ! channel upstream node
     &     ,counter

      real*4
     &     chan_manning
     &     ,chan_dispersion

      logical order_nodes       ! function to set channel-node connections
                                ! in chan_geom and node_geom to internal rather
                                ! than external numbers

c-----Bind the parameter representing ModelID	
      call f90SQLBindParameter (StmtHndl, int(1,SQLUSMALLINT_KIND), SQL_PARAM_INPUT,
     &     SQL_F_SLONG, SQL_INTEGER, int(4,SQLUINTEGER_KIND),  int(0,SQLSMALLINT_KIND),
     &     ModelID, f90SQL_NULL_PTR, iRet) 

c-----Execute SQL statement
      StmtStr="SELECT " //
     &     "channel.channel_id, channel.used, channel.channel_number, " //
     &     "channel.length_ft, channel.manning, channel.dispersion, " //
     &     "channel.down_node, channel.up_node " //
     &     "FROM channel INNER JOIN model_component ON channel.layer_id "//
     &     "= model_component.component_id " //
     &     "WHERE model_component.model_id = ? " //
     &     "AND model_component.component_type = 'grid' " //
     &     "ORDER BY channel.channel_number,model_component.layer DESC;"

      call f90SQLExecDirect(StmtHndl, StmtStr,iRet)

      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5/)') 'Error in making channel SQL request',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3)
     &        write(unit_screen,'(a)') 'Made channel SQL request'
      endif

c-----Bind variables to columns in result set
      ColNumber=1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, ID,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, UseObj,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, channo,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, chan_len,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_FLOAT, chan_manning,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_FLOAT, chan_dispersion,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, chan_downnode,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, chan_upnode,
     &     f90SQL_NULL_PTR, iRet)

      if (print_level .ge. 3)
     &     write(unit_screen,'(a)') 'Made channel bind request'

c-----Loop to fetch records, one at a time
      counter=0
      prev_channo=miss_val_i

      do while (.true.)

c--------Fetch a record from the result set
         call f90SQLFetch(StmtHndl,iRet)
         if (iRet .eq. SQL_NO_DATA) exit
c--------use only the last version of a channel, and
c--------skip the channel if marked not-use
         if (channo .ne. prev_channo .and.
     &        UseObj) then
            call process_channel(
     &                           counter,
     &                           id,
     &                           channo,
     &                           chan_len,
     &                           chan_manning,
     &                           chan_dispersion,
     &                           chan_upnode,
     &                           chan_downnode)

      endif
         prev_channo=channo
      enddo
      if (counter .eq. 0) then
         write(unit_error, '(a)') 'Error: no channel records retrieved.'
         istat=-3
         return
      endif
      nchans=counter
      if (.not. order_nodes())then
         write(unit_error,'(a)')'Error reordering nodes in read_channels_sql.f.'
         istat=-3
         return
      end if

      if (print_level .ge. 2)
     &     write(unit_screen,'(a,i4/)') 'Read in all channel data', counter

      call f90SQLFreeStmt(StmtHndl,SQL_UNBIND, iRet)
      call f90SQLCloseCursor (StmtHndl, iRet)
      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5//)') 'Error in unbinding channel SQL',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3)
     &        write(unit_screen,'(a//)') 'Unbound channel SQL'
      endif

      return
      end
c------------------------------------------------------------------
c------------------------------------------------------------------

      subroutine load_channel_xsects_SQL(StmtHndl, ModelID, istat)
      use IO_Units
c-----load f90SQL modules
      use f90SQLConstants
      use f90SQL
      use grid_data
      !use constants
      use common_xsect
      use logging
      implicit none


c-----arguments
      integer(SQLHANDLE_KIND):: StmtHndl
     &     ,istat               ! status
     &     ,ModelID

c-----f90SQL variables
      character(len=1000)::StmtStr
      integer(SQLRETURN_KIND)::iRet
      integer(SQLSMALLINT_KIND)::ColNumber = 0! SQL table column number

      integer*2 use_obj
c-----local variables
      integer
     &     channo,prev_chan    ! channel number
     &     ,layer,prev_layer
     &     ,xsectID             ! cross section ID
     &     ,i                   ! loop index
     &     ,ext2int
     &     ,counter

      real*8
     &     chan_fdist

	real*8,parameter :: DISTANCE_RESOLUTION = 0.001

c-----Bind the parameter representing ModelID
      call f90SQLBindParameter (StmtHndl, int(1,SQLUSMALLINT_KIND), SQL_PARAM_INPUT,
     &     SQL_F_SLONG, SQL_INTEGER, int(4,SQLUINTEGER_KIND),  int(0,SQLSMALLINT_KIND),
     &     ModelID, f90SQL_NULL_PTR, iRet)

c-----Execute SQL statement
      StmtStr="SELECT channel.channel_number," //
     &     "channel_xsect.channel_fract_dist, channel_xsect.xsect_id, " //
     &     "layer,channel.used " //
     &     "FROM (channel_xsect inner join channel " //
     &     "ON channel_xsect.channel_id = channel.channel_id) " //
     &     "INNER JOIN model_component ON "//
     &     "channel.layer_id = model_component.component_id " //
     &     "WHERE model_component.component_type = 'grid' " // " " //
     &     "AND model_component.model_id = ? " //
     &     "ORDER BY channel.channel_number, model_component.layer DESC, " //
     &     "channel_xsect.channel_fract_dist;"

      call f90SQLExecDirect(StmtHndl, StmtStr,iRet)
      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5/)')
     &        'Error in making Channel Xsect SQL request ',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-1
         return
      endif

      ColNumber=0
c-----Bind variables to columns in result set
      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, channo,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_DOUBLE, chan_fdist,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, xsectID,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, layer,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, use_obj,
     &     f90SQL_NULL_PTR, iRet)
      
c-----Loop to fetch records, one at a time
      counter=0

      prev_chan=miss_val_i
      prev_layer=miss_val_i
      do while (.true.)
c--------Fetch a record from the result set
         call f90SQLFetch(StmtHndl,iRet)
         if (iRet .eq. SQL_NO_DATA) exit
         if (.not.(channo .eq. prev_chan .and.
     &        layer .ne. prev_layer)) then
            chan_fdist = NINT(chan_fdist/DISTANCE_RESOLUTION)*DISTANCE_RESOLUTION
            if (ext2int(channo) .lt. 0) then ! valid channel number
               write(unit_error, '(a,i8,f10.6)')
     &              "Invalid channel in xsect specification",channo,chan_fdist
               call exit(-3)
            end if
            ! This channel is not just a lower priority version of the last channel
            if( use_obj )then ! don't move this
                !todo: Eli moved this, make sure OK
                  
                call process_xsect(channo,chan_fdist,xsectId,counter)
             end if           ! object is in use
             prev_chan=channo
             prev_layer=layer
             counter=counter+1
         endif
      enddo

      if (counter .eq. 0) then
         write(unit_error, '(a)') 'Error: no channel xsect records retrieved.'
         istat=-3
         return
      endif

      if (print_level .ge. 2)
     &     write(unit_screen,'(a,i5/)') 'Read in all channel xsect data', nirg

      call f90SQLFreeStmt(StmtHndl,SQL_UNBIND, iRet)
      call f90SQLCloseCursor (StmtHndl, iRet)
      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5/)') 'Error in unbinding channel xsect SQL',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      endif

      return
      end

      subroutine load_xsect_layers_SQL(StmtHndl, istat)
      use IO_Units
c-----load f90SQL modules
      use f90SQLConstants
      use f90SQL
      use grid_data
      use common_xsect  
      use logging

      implicit none


c-----arguments
      integer(SQLHANDLE_KIND):: StmtHndl
     &     ,istat               ! status

c-----f90SQL variables
      character(len=1000)::StmtStr
      integer(SQLRETURN_KIND)::iRet
      integer(SQLSMALLINT_KIND)::ColNumber ! SQL table column number

c-----local variables

      integer*4
     &     xsectID              ! cross section ID
     &     ,xsectno_gbl         ! global cross section number
     &     ,nl                  ! number of layers, and layer number
     &     ,nl_gbl              ! global number of layers
     &     ,counter

      real*8
     &     elev, prev_elev
     &     ,width,prev_width
     &     ,area,prev_area,calc_area
     &     ,wetperim
      real*8,parameter :: VERT_RESOLUTION = 0.001
      

c-----prepare statement, with a parameter representing xsectID

      xsectno_gbl=0
      nl_gbl=0

      StmtStr="SELECT " //
     &     "Elev, Width, Area, Wet_Perimeter " //
     &     "FROM XSect_Layer " //
     &     "WHERE XSect_ID = ? " //
     &     "ORDER BY Elev;"

      call f90SQLPrepare(StmtHndl, StmtStr, iRet) 
      call f90SQLBindParameter(StmtHndl, int(1,SQLUSMALLINT_KIND), SQL_PARAM_INPUT,
     &     SQL_F_SLONG, SQL_INTEGER, int(0,SQLUINTEGER_KIND), int(0,SQLSMALLINT_KIND), 
     &     xsectID, f90SQL_NULL_PTR, iRet)

c-----Bind variables to columns in result set
      ColNumber=1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_DOUBLE, elev,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_DOUBLE, width,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_DOUBLE, area,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_DOUBLE, wetperim,
     &     f90SQL_NULL_PTR, iRet)

c-----loop over channels and cross sections, adding layers from
c-----XSectLayer table

      do xsectno_gbl=1,nirg
         irreg_geom(xsectno_gbl).num_elev=0
         xsectID=irreg_geom(xsectno_gbl).ID
         if (print_level .ge. 3)
     &        write(unit_screen,'(a,i10,i10)')
     &        '  and for xsect ', xsectno_gbl, xsectID

         call f90SQLExecDirect(StmtHndl, StmtStr,iRet)

         if (iRet.ne.SQL_SUCCESS) then
            write(unit_error,'(a,i10/a,i5)')
     &           'For xsect ', xsectID,
     &           'error in making channel xsect layer SQL request ',iRet
            call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
            istat=-1
            return
         endif

         counter=0
	   prev_area=0.
	   prev_width=0.
         prev_elev=0.
c--------Loop to fetch records, one at a time
         do while (.true.)
c-----------Fetch a record from the result set
            call f90SQLFetch(StmtHndl,iRet)
            if (iRet .eq. SQL_NO_DATA) exit
            elev = NINT(elev/VERT_RESOLUTION)*VERT_RESOLUTION
            call process_xsect_layer(xsectno_gbl,elev,area,width,wetperim)
            nl_gbl = nl_gbl+1
            counter=counter+1
	      prev_area=area
	      prev_width=width
	      prev_elev=elev
         enddo
         if (counter .eq. 0) then
            write(unit_error, 607) chan_geom(irreg_geom(xsectno_gbl).chan_no).chan_no,
     &           xsectID
 607        format(/'Error: no xsect layer records retrieved for channel ',i5
     &           /' xsect ',i5)
            istat=-3
            return
         endif

         call f90SQLCloseCursor (StmtHndl, iRet)
         if (iRet.ne.SQL_SUCCESS) then
            write(unit_error,'(a,i5/a,i5)') 'For xsect ', xsectno_gbl,
     &           'error in unbinding xsect layer SQL',iRet
            call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
            istat=-3
            return
         endif
      enddo

      call f90SQLFreeStmt(StmtHndl,SQL_CLOSE, iRet)
      if (print_level .ge. 2)
     &     write(unit_screen,'(i5,a)') nl_gbl,' xsect layers loaded'

      return
      end
      

      
      
      
      
      
      
      
      
      
      
      
      
      
      
      