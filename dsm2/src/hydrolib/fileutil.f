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

*==== BOF fileutil ============================================================
*^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*
*     This file contains utility functions for manipulating FORTRAN files.
*
*
*     Function Name                 Description
*     -------------                 -----------
*
*     (OpenNewText) - Open a new text file for output and delete the old file
*                       if one exists.
*     (OpenOldText) - Open an old text file for input.
*     (OpenAppBin)  - Open an existing sequential binary file for output, place
*                       new output at the end of the file.
*     (DeleteFile)  - Delete an existing file.
*     (FileExist)   - Does the file name exist?
*
*     Input:
*     ------
*
*     The open functions expect the following arguments:
*
*       UnitNumber  - an INTEGER FORTRAN unit number, and
*       FileName    - a variable dimension character string containing the
*                       name of the file to be manipulated.
*
*     In addition, the direct access file opening function expects:
*
*       RecordLength - an INTEGER logical record length, and
*       Formatted    - a LOGICAL variable indicating whether the file is
*                      to be FORMATTED (.TRUE.) or UNFORMATTED (.FALSE.;
*                      binary).
*
*     DeleteFile and FileExist expect only one argument:
*
*        FileName - a variable dimension character string containing the
*                   name of the file to be deleted or tested for existence.
*
*
*     Output:
*     -------
*
*     Each function returns .TRUE. if the operation was successful and
*       .FALSE. if unsuccessful.
*
*

*
*     Notes:
*     ------
*
*     These utilities have not been tested on the Prime mini as of
*       Fri 11-16-1990.  Therefore, there may be undetermined bugs to be
*       corrected for successful operations on the Prime.  In addition, it is
*       not clear that these routines will function on platforms other than
*       PCompatibles using MickeySoft FORTRAN.  Tests will be required.
*
*   Programmed by: Dave Thompson
*   Date:          Mon  12-17-1990
*   Modified by:   Dave Thompson
*   Last modified: Tue  05-21-1991
*   Version 93.01, January, 1993
*
*   History:
*   --------
*
*   Tue  05-21-1991  -- Added IMPLICIT NONE to all subprograms in this unit.
*
*^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

C==---Public (OpenNewText) ======================================================

      LOGICAL   FUNCTION OpenNewText (
     &     UnitNumber, FileName
     &     )
      use IO_Units
      IMPLICIT NONE

C-----Purpose:  Open a new text file, deleting any existing file with the
C-----same name.

C-----Arguments:

      INTEGER       UnitNumber
      CHARACTER*(*) FileName
C-----Argument Definitions:
C-----UnitNumber  - an INTEGER FORTRAN unit number, and
C-----FileName    - a variable dimension character string containing the
C-----name of the file to be manipulated.

C-----Local Variables:

      INTEGER   IOStatus

C-----Functions:

      LOGICAL   DeleteFile
      EXTERNAL  DeleteFile

C-----Programmed by: Dave Thompson
C-----Date:          September 1989
C-----Modified by:   Dave Thompson
C-----Last modified: Wed  12-12-1990
*   Version 93.01, January, 1993

C-----Implementation ------------------------------------------------------------

C-----Debugging information

C     WRITE(*,*) ' In routine OpenNewText'
C     WRITE(*,*) ' Incoming file = ', FileName
C     WRITE(*,*) ' Unit number = ', UnitNumber

C-----Try to delete the file (assuming it exists).

      IF (DeleteFile(FileName)) THEN

C--------WRITE(*,*) ' IOStatus = ',IOS

         OPEN (
     &        UNIT   = UnitNumber,
     &        FILE   = FileName,
     &        ACCESS = 'SEQUENTIAL',
     &        FORM   = 'FORMATTED',
     &        IOSTAT = IOStatus
     &        )
         IF     (IOStatus .EQ. 0) THEN

            OpenNewText = .TRUE.

         ELSEIF (IOStatus .NE. 0) THEN

            OpenNewText = .FALSE.

         ENDIF

      ELSE

         WRITE(UNIT_ERROR,*) ' File not deleted (probably doesn''t exist)'
C--------WRITE(*,*) ' IOStatus = ',IOS

         OpenNewText = .FALSE.

      ENDIF

      RETURN
      END



C==---Public (OpenOldText) ======================================================

      LOGICAL   FUNCTION OpenOldText (
     &     UnitNumber, FileName
     &     )

      IMPLICIT NONE

C-----Purpose:  Open an existing flat file for input.

C-----Arguments:

      INTEGER       UnitNumber
      CHARACTER*(*) FileName

C-----Argument Definitions:
C-----UnitNumber  - an INTEGER FORTRAN unit number, and
C-----FileName    - a variable dimension character string containing the
C-----name of the file to be manipulated.

C-----Local Variables:

      INTEGER   IOStatus

C-----Programmed by: Dave Thompson
C-----Date:          September 1989
C-----Modified by:   Dave Thompson
C-----Last modified: Wed  12-12-1990
*   Version 93.01, January, 1993

C-----Implementation ------------------------------------------------------------

C-----Debugging information

C     WRITE(*,*) ' In routine OpenOldText'
C     WRITE(*,*) ' Incoming file = ', FileName
C     WRITE(*,*) ' Unit number   = ', UnitNumber

C-----Open the existing file.

      OPEN (
     &     UNIT   = UnitNumber,
     &     FILE   = FileName,
     &     STATUS = 'OLD',
     &     ACCESS = 'SEQUENTIAL',
     &     FORM   = 'FORMATTED',
     &     IOSTAT = IOStatus
     &     )

      IF     (IOStatus .EQ. 0) THEN

         OpenOldText = .TRUE.

      ELSEIF (IOStatus .NE. 0) THEN

         OpenOldText = .FALSE.

      ENDIF

      RETURN
      END



C==---Public (DeleteFile) =======================================================

      LOGICAL   FUNCTION DeleteFile (
     &     FileName
     &     )

      IMPLICIT NONE

C-----Purpose:  Delete an existing file.  This routine contains system
C-----specific calls.

C-----Arguments:

      CHARACTER*(*) FileName

C-----Argument Definitions:
C-----FileName    - a variable dimension character string containing the
C-----name of the file to be manipulated.

C-----Local Variables:

      INTEGER      DefaultUnit
      LOGICAL      PC, Prime

* NOTE: when running in UNIX, DefaultUnit must be no bigger than 2 digits.

      PARAMETER   (
     &     DefaultUnit = 99,
     &     PC          = .TRUE.,
     &     Prime       = .FALSE.
C-----*             PC          = .FALSE.,
C-----*             Prime       = .TRUE.
     &     )

      INTEGER      IOStatus
      LOGICAL      Exists

      LOGICAL      FileExist
      EXTERNAL     FileExist

***** Uncomment the following for Prime applications only!!!!!

C     INTEGER       Code
C     INCLUDE      'SYSCOM>A$KEYS.INS.FTN'
C     EXTERNAL      FIL$DL

***** End of Prime specific code.

C-----Programmed by: Dave Thompson
C-----Date:          September 1989
C-----Modified by:   Dave Thompson
C-----Last modified: Wed  12-12-1990
*   Version 93.01, January, 1993

C-----Implementation ------------------------------------------------------------

C-----Debugging information

C-----Check for file existence.

      Exists = FileExist(FileName)

C-----If it doesn't exist, no need to continue.

      IF     (.NOT. Exists) THEN

         DeleteFile = .TRUE.

C--------Otherwise, test for the type of file and take appropriate action.

      ELSEIF (Exists .AND. PC) THEN

         OPEN  (
     &        UNIT   = DefaultUnit,
     &        FILE   = FileName,
     &        STATUS = 'OLD'
     &        )

         CLOSE (
     &        UNIT   = DefaultUnit,
     &        STATUS = 'DELETE',
     &        IOSTAT = IOStatus
     &        )

C--------Was the file deleted?

         IF     (IOStatus .EQ. 0) THEN

            DeleteFile = .TRUE.

         ELSEIF (IOStatus .NE. 0) THEN

            DeleteFile = .FALSE.

         ENDIF

      ELSEIF (Exists .AND. Prime) THEN

*****----Uncomment the following for Prime applications only!!!!!
*
*       CALL FIL$DL (
*    I               FileName,
*    O               Code
*    *              )
*
*       IF (Code .NE. 0) THEN
*
*         DeleteFile = .FALSE.
*
*       ELSE
*
*         DeleteFile = .TRUE.
*
*       ENDIF
*
*****----End of Prime specific code.

      ENDIF

      RETURN
      END

C==---Public (FileExist) =======================================================

      LOGICAL   FUNCTION FileExist (
     &     FileName
     &     )

      IMPLICIT NONE

C-----Purpose:  Determine if file exists.  This routine contains system
C-----specific calls.

C-----Arguments:

      CHARACTER*(*)  FileName

C-----Argument Definitions:
C-----FileName    - a variable dimension character string containing the
C-----name of the file to be manipulated.

C-----Local Variables:

      INTEGER    UnitNumber
      INTEGER    IOStatus
      LOGICAL    PC, Prime

* NOTE: when running in UNIX, UnitNumber must be no bigger than 2 digits.

      PARAMETER (
     &     UnitNumber = 99,
     &     PC         = .TRUE.,
     &     Prime      = .FALSE.
C-----*           PC         = .FALSE.,
C-----*           Prime      = .TRUE.
     &     )

***** Uncomment the following for Prime applications only!!!!!

C     INTEGER       FUnit, Code
C     INCLUDE      'SYSCOM>A$KEYS.INS.FTN'
C     EXTERNAL      SRCH$$

***** End of Prime specific code.

C-----Programmed by: Dave Thompson
C-----Date:          June 1990
C-----Modified by:   Dave Thompson
C-----Last modified: Wed  12-12-1990
*   Version 93.01, January, 1993

C-----Implementation ------------------------------------------------------------

C-----Debugging information

C     WRITE (*,*) ' FileExist Target = ', FileName

      IF (PC) THEN

C--------Try to open the file.  If it exists, report success.
C--------Otherwise, the file doesn't exist, so report failure.

         OPEN (
     &        UNIT   = UnitNumber,
     &        FILE   = FileName,
     &        IOSTAT = IOStatus
     &        )

         IF     (IOStatus .EQ. 0) THEN

            FileExist = .TRUE.

         ELSEIF (IOStatus .NE. 0) THEN

            FileExist = .FALSE.

         ENDIF

         CLOSE (UnitNumber)

      ELSEIF (Prime) THEN

*****----Uncomment the following for Prime applications only!!!!!

c@@@       CALL SRCH$$ (
c@@@    I               K$EXST, FileName, 12, 0,
c@@@    O               Type,   Code
c@@@    *              )
c@@@
c@@@       IF (Code .EQ. E$FNTF) THEN
c@@@
c@@@         FileExist = .FALSE.
c@@@
c@@@       ELSE
c@@@
c@@@         FileExist = .TRUE.
c@@@
c@@@       ENDIF

*****----End of Prime specific code.

      ENDIF

      RETURN
      END

*==== EOF fileutil ============================================================


C==---Public (GetFileUnit) ======================================================

      INTEGER   FUNCTION GetFileUnit (
     &     Class
     &     )

C-----Purpose:  This routine takes a target file name and searches the list of
C-----available class names.  If the name is found, it returns the
C-----unit number of the target and marks the file as used.  Otherwise,
C-----it returns a 0 to indicate failure.

      IMPLICIT NONE

C-----Arguments:

      CHARACTER*12   Class

C-----Argument definitions:
C     Class - target file name.

C-----Module data:

      INCLUDE         'master.inc'

C-----Local variables:

      INTEGER         FileNumber

C-----Functions:

      INTEGER         SearchForFile
      EXTERNAL        SearchForFile

C-----Data Initializations:

C-----Programmed by:  Dave Thompson
C-----Date:           Wed  12-12-1990
C-----Modified by:
C-----Last modified:
*   Version 93.01, January, 1993

C-----Implementation ------------------------------------------------------------

      FileNumber = SearchForFile(Class)

      IF (FileNumber .EQ. 0) THEN
         GetFileUnit = 0
      ELSE
         GetFileUnit          = UserUnit(FileNumber)
         FileUsed(FileNumber) = .TRUE.
      ENDIF

      RETURN
      END

C==---Public (GetFileName) ======================================================

      CHARACTER*12 FUNCTION GetFileName (
     &     Class
     &     )

C-----Purpose:  Search module data for the file Class and return its value
C-----to the client prorgram.

      IMPLICIT NONE

C-----Arguments:

      CHARACTER*12 Class

C-----Argument definitions:
C     Class - target file name.

C-----Module data:

      INCLUDE       'master.inc'

C-----Local variables:

      INTEGER       FileNumber

C-----Functions:

      INTEGER       SearchForFile
      EXTERNAL      SearchForFile

C-----Data Initializations:

C-----Programmed by:  Dave Thompson
C-----Date:           Wed  12-12-1990
C-----Modified by:
C-----Last modified:
*   Version 93.01, January, 1993

C-----Implementation ------------------------------------------------------------

      FileNumber = SearchForFile(Class)

      IF (FileNumber .EQ. 0) THEN
         GetFileName = ' '
      ELSE
         GetFileName          = UserName(FileNumber)
         FileUsed(FileNumber) = .TRUE.
      ENDIF

      RETURN
      END



C==---Private (SearchForFile) ===================================================

      INTEGER   FUNCTION SearchForFile (
     &     TargetName
     &     )

C-----Purpose:  Search the default (standard) file names (module data) for the
C-----occurrence of TargetName.

      IMPLICIT NONE

C-----Arguments:

      CHARACTER*12 TargetName

C-----Module data:

      INCLUDE       'master.inc'

C-----Local variables:

      INTEGER     FileNumber
      LOGICAL     Done

C-----Programmed by: Dave Thompson
C-----Date:          Tue  12-11-1990
C-----Modified by:
C-----Last modified:
*   Version 93.01, January, 1993

C-----Implementation ------------------------------------------------------------

      FileNumber    = 0
      Done          = .FALSE.
      SearchForFile = 0

 10   FileNumber = FileNumber + 1

      IF (TargetName .EQ. DefaultName(FileNumber)) THEN
         SearchForFile = FileNumber
         Done = .TRUE.
      ENDIF

      IF (FileNumber .LT. MaxFiles .AND. .NOT. Done) GOTO 10

      RETURN
      END



