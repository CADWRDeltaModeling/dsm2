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

!==== BOF fileutil ============================================================
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!     This file contains utility functions for manipulating FORTRAN files.
!
!
!     Function Name                 Description
!     -------------                 -----------
!
!     (OpenNewText) - Open a new text file for output and delete the old file
!                       if one exists.
!     (OpenOldText) - Open an old text file for input.
!     (OpenAppBin)  - Open an existing sequential binary file for output, place
!                       new output at the end of the file.
!     (DeleteFile)  - Delete an existing file.
!     (FileExist)   - Does the file name exist?
!
!     Input:
!     ------
!
!     The open functions expect the following arguments:
!
!       UnitNumber  - an INTEGER FORTRAN unit number, and
!       FileName    - a variable dimension character string containing the
!                       name of the file to be manipulated.
!
!     In addition, the direct access file opening function expects:
!
!       RecordLength - an INTEGER logical record length, and
!       Formatted    - a LOGICAL variable indicating whether the file is
!                      to be FORMATTED (.TRUE.) or UNFORMATTED (.FALSE.;
!                      binary).
!
!     DeleteFile and FileExist expect only one argument:
!
!        FileName - a variable dimension character string containing the
!                   name of the file to be deleted or tested for existence.
!
!
!     Output:
!     -------
!
!     Each function returns .TRUE. if the operation was successful and
!       .FALSE. if unsuccessful.
!
!

!
!     Notes:
!     ------
!
!     These utilities have not been tested on the Prime mini as of
!       Fri 11-16-1990.  Therefore, there may be undetermined bugs to be
!       corrected for successful operations on the Prime.  In addition, it is
!       not clear that these routines will function on platforms other than
!       PCompatibles using MickeySoft FORTRAN.  Tests will be required.
!
!   Programmed by: Dave Thompson
!   Date:          Mon  12-17-1990
!   Modified by:   Dave Thompson
!   Last modified: Tue  05-21-1991
!   Version 93.01, January, 1993
!
!   History:
!   --------
!
!   Tue  05-21-1991  -- Added IMPLICIT NONE to all subprograms in this unit.
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
module fileutil
    implicit none

    integer,parameter::   MaxFiles   = 26, NameLength = 12
    character*12,save::  DefaultName(MaxFiles)
    character*12,save::  UserName(MaxFiles)
    logical,save::       FileUsed(MaxFiles)
    character*12,save::  MasterFileName
    integer,save::       DefaultUnit(MaxFiles)
    integer,save::       UserUnit(MaxFiles)
    integer,save::       MasterUnit
    logical,save::      MasterInit
contains
    !==---Public (OpenNewText) ======================================================

    logical   function OpenNewText ( &
        UnitNumber, FileName &
        )
        use IO_Units
        implicit none

        !-----Purpose:  Open a new text file, deleting any existing file with the
        !-----same name.

        !-----Arguments:

        integer       UnitNumber
        character*(*) FileName
        !-----Argument Definitions:
        !-----UnitNumber  - an INTEGER FORTRAN unit number, and
        !-----FileName    - a variable dimension character string containing the
        !-----name of the file to be manipulated.

        !-----Local Variables:

        integer   IOStatus


        !-----Programmed by: Dave Thompson
        !-----Date:          September 1989
        !-----Modified by:   Dave Thompson
        !-----Last modified: Wed  12-12-1990
        !   Version 93.01, January, 1993

        !-----Implementation ------------------------------------------------------------

        !-----Debugging information

        !     WRITE(*,*) ' In routine OpenNewText'
        !     WRITE(*,*) ' Incoming file = ', FileName
        !     WRITE(*,*) ' Unit number = ', UnitNumber

        !-----Try to delete the file (assuming it exists).

        if (DeleteFile(FileName)) then

            !--------WRITE(*,*) ' IOStatus = ',IOS

            open ( &
                unit   = UnitNumber, &
                file   = FileName, &
                access = 'SEQUENTIAL', &
                form   = 'FORMATTED', &
                iostat = IOStatus &
                )
            if     (IOStatus == 0) then

                OpenNewText = .true.

            elseif (IOStatus /= 0) then

                OpenNewText = .false.

            endif

        else

            write(UNIT_ERROR,*) ' File not deleted (probably doesn''t exist)'
            !--------WRITE(*,*) ' IOStatus = ',IOS

            OpenNewText = .false.

        endif

        return
    end function



    !==---Public (OpenOldText) ======================================================

    logical   function OpenOldText ( &
        UnitNumber, FileName &
        )

        implicit none

        !-----Purpose:  Open an existing flat file for input.

        !-----Arguments:

        integer       UnitNumber
        character*(*) FileName

        !-----Argument Definitions:
        !-----UnitNumber  - an INTEGER FORTRAN unit number, and
        !-----FileName    - a variable dimension character string containing the
        !-----name of the file to be manipulated.

        !-----Local Variables:

        integer   IOStatus

        !-----Programmed by: Dave Thompson
        !-----Date:          September 1989
        !-----Modified by:   Dave Thompson
        !-----Last modified: Wed  12-12-1990
        !   Version 93.01, January, 1993

        !-----Implementation ------------------------------------------------------------

        !-----Debugging information

        !     WRITE(*,*) ' In routine OpenOldText'
        !     WRITE(*,*) ' Incoming file = ', FileName
        !     WRITE(*,*) ' Unit number   = ', UnitNumber

        !-----Open the existing file.

        open ( &
            unit   = UnitNumber, &
            file   = FileName, &
            status = 'OLD', &
            access = 'SEQUENTIAL', &
            form   = 'FORMATTED', &
            iostat = IOStatus &
            )

        if     (IOStatus == 0) then

            OpenOldText = .true.

        elseif (IOStatus /= 0) then

            OpenOldText = .false.

        endif

        return
    end function



    !==---Public (DeleteFile) =======================================================

    logical   function DeleteFile ( &
        FileName &
        )

        implicit none

        !-----Purpose:  Delete an existing file.  This routine contains system
        !-----specific calls.

        !-----Arguments:

        character*(*) FileName

        !-----Argument Definitions:
        !-----FileName    - a variable dimension character string containing the
        !-----name of the file to be manipulated.

        !-----Local Variables:

        integer      DefaultUnit
        logical      PC, Prime

        ! NOTE: when running in UNIX, DefaultUnit must be no bigger than 2 digits.

        parameter   ( &
            DefaultUnit = 99, &
            PC          = .true., &
            Prime       = .false. &
            !-----*             PC          = .FALSE.,
            !-----*             Prime       = .TRUE. &
            )

        integer      IOStatus
        logical      Exists
        !**** Uncomment the following for Prime applications only!!!!!

        !     INTEGER       Code
        !     INCLUDE      'SYSCOM>A$KEYS.INS.FTN'
        !     EXTERNAL      FIL$DL

        !**** End of Prime specific code.

        !-----Programmed by: Dave Thompson
        !-----Date:          September 1989
        !-----Modified by:   Dave Thompson
        !-----Last modified: Wed  12-12-1990
        !   Version 93.01, January, 1993

        !-----Implementation ------------------------------------------------------------

        !-----Debugging information

        !-----Check for file existence.

        Exists = FileExist(FileName)

        !-----If it doesn't exist, no need to continue.

        if     (.not. Exists) then

            DeleteFile = .true.

        !--------Otherwise, test for the type of file and take appropriate action.

        elseif (Exists .and. PC) then

            open  ( &
                unit   = DefaultUnit, &
                file   = FileName, &
                status = 'OLD' &
                )

            close ( &
                unit   = DefaultUnit, &
                status = 'DELETE', &
                iostat = IOStatus &
                )

            !--------Was the file deleted?

            if     (IOStatus == 0) then

                DeleteFile = .true.

            elseif (IOStatus /= 0) then

                DeleteFile = .false.

            endif

        elseif (Exists .and. Prime) then

        !****----Uncomment the following for Prime applications only!!!!!
        !
        !       CALL FIL$DL (
        !    I               FileName,
        !    O               Code
        !    *              )
        !
        !       IF (Code .NE. 0) THEN
        !
        !         DeleteFile = .FALSE.
        !
        !       ELSE
        !
        !         DeleteFile = .TRUE.
        !
        !       ENDIF
        !
        !****----End of Prime specific code.

        endif

        return
    end function

    !==---Public (FileExist) =======================================================

    logical   function FileExist ( &
        FileName &
        )

        implicit none

        !-----Purpose:  Determine if file exists.  This routine contains system
        !-----specific calls.

        !-----Arguments:

        character*(*)  FileName

        !-----Argument Definitions:
        !-----FileName    - a variable dimension character string containing the
        !-----name of the file to be manipulated.

        !-----Local Variables:

        integer    UnitNumber
        integer    IOStatus
        logical    PC, Prime

        ! NOTE: when running in UNIX, UnitNumber must be no bigger than 2 digits.

        parameter ( &
            UnitNumber = 99, &
            PC         = .true., &
            Prime      = .false. &
            !-----*           PC         = .FALSE.,
            !-----*           Prime      = .TRUE. &
            )

        !**** Uncomment the following for Prime applications only!!!!!

        !     INTEGER       FUnit, Code
        !     INCLUDE      'SYSCOM>A$KEYS.INS.FTN'
        !     EXTERNAL      SRCH$$

        !**** End of Prime specific code.

        !-----Programmed by: Dave Thompson
        !-----Date:          June 1990
        !-----Modified by:   Dave Thompson
        !-----Last modified: Wed  12-12-1990
        !   Version 93.01, January, 1993

        !-----Implementation ------------------------------------------------------------

        !-----Debugging information

        !     WRITE (*,*) ' FileExist Target = ', FileName

        if (PC) then

            !--------Try to open the file.  If it exists, report success.
            !--------Otherwise, the file doesn't exist, so report failure.

            open ( &
                unit   = UnitNumber, &
                file   = FileName, &
                iostat = IOStatus &
                )

            if     (IOStatus == 0) then

                FileExist = .true.

            elseif (IOStatus /= 0) then

                FileExist = .false.

            endif

            close (UnitNumber)

        elseif (Prime) then

        !****----Uncomment the following for Prime applications only!!!!!

        !@@@       CALL SRCH$$ (
        !@@@    I               K$EXST, FileName, 12, 0,
        !@@@    O               Type,   Code
        !@@@    *              )
        !@@@
        !@@@       IF (Code .EQ. E$FNTF) THEN
        !@@@
        !@@@         FileExist = .FALSE.
        !@@@
        !@@@       ELSE
        !@@@
        !@@@         FileExist = .TRUE.
        !@@@
        !@@@       ENDIF

        !****----End of Prime specific code.

        endif

        return
    end function

    !==== EOF fileutil ============================================================


    !==---Public (GetFileUnit) ======================================================

    integer   function GetFileUnit ( &
        Class &
        )

        !-----Purpose:  This routine takes a target file name and searches the list of
        !-----available class names.  If the name is found, it returns the
        !-----unit number of the target and marks the file as used.  Otherwise,
        !-----it returns a 0 to indicate failure.

        implicit none

        !-----Arguments:

        character*12   Class

        !-----Argument definitions:
        !     Class - target file name.

        !-----Local variables:

        integer         FileNumber


        !-----Data Initializations:

        !-----Programmed by:  Dave Thompson
        !-----Date:           Wed  12-12-1990
        !-----Modified by:
        !-----Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation ------------------------------------------------------------

        FileNumber = SearchForFile(Class)

        if (FileNumber == 0) then
            GetFileUnit = 0
        else
            GetFileUnit          = UserUnit(FileNumber)
            FileUsed(FileNumber) = .true.
        endif

        return
    end function

    !==---Public (GetFileName) ======================================================

    character*12 function GetFileName ( &
        Class &
        )

        !-----Purpose:  Search module data for the file Class and return its value
        !-----to the client prorgram.

        implicit none

        !-----Arguments:

        character*12 Class

        !-----Argument definitions:
        !     Class - target file name.

        !-----Local variables:

        integer       FileNumber


        !-----Data Initializations:

        !-----Programmed by:  Dave Thompson
        !-----Date:           Wed  12-12-1990
        !-----Modified by:
        !-----Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation ------------------------------------------------------------

        FileNumber = SearchForFile(Class)

        if (FileNumber == 0) then
            GetFileName = ' '
        else
            GetFileName          = UserName(FileNumber)
            FileUsed(FileNumber) = .true.
        endif

        return
    end function



    !==---Private (SearchForFile) ===================================================

    integer   function SearchForFile ( &
        TargetName &
        )

        !-----Purpose:  Search the default (standard) file names (module data) for the
        !-----occurrence of TargetName.

        implicit none

        !-----Arguments:

        character*12 TargetName


        !-----Local variables:

        integer     FileNumber
        logical     Done

        !-----Programmed by: Dave Thompson
        !-----Date:          Tue  12-11-1990
        !-----Modified by:
        !-----Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation ------------------------------------------------------------

        FileNumber    = 0
        Done          = .false.
        SearchForFile = 0

10      FileNumber = FileNumber + 1

        if (TargetName == DefaultName(FileNumber)) then
            SearchForFile = FileNumber
            Done = .true.
        endif

        if (FileNumber < MaxFiles .and. .not. Done) goto 10

        return
    end function



end module
