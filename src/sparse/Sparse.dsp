# Microsoft Developer Studio Project File - Name="Sparse" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=Sparse - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Sparse.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Sparse.mak" CFG="Sparse - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Sparse - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "Sparse - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "Sparse - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
# ADD F90 /architecture:pn4 /assume:buffered_io /assume:noaccuracy_sensitive /check:noflawed_pentium /compile_only /extend_source:132 /fpconstant /fpscomp:nolibs /include:"Release/" /include:"release" /include:"../../hydro/release" /include:"../../../lib/hdf5" /math_library:fast /nologo /traceback /transform_loops /warn:argument_checking /warn:nofileopt /fast
# SUBTRACT F90 /check:bounds /check:overflow /fltconsistency
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "Sparse - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Sparse__"
# PROP BASE Intermediate_Dir "Sparse__"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /debug:full /include:"Sparse__/" /nologo /warn:nofileopt
# ADD F90 /architecture:pn4 /assume:buffered_io /browser /check:bounds /check:format /check:noflawed_pentium /check:power /check:output_conversion /check:overflow /compile_only /dbglibs /debug:full /error_limit:500 /extend_source:132 /fltconsistency /fpconstant /include:"..\..\hydro" /libdir:noauto /nologo /warn:argument_checking /warn:declarations /warn:nofileopt /pdbfile:"Debug"
# SUBTRACT F90 /check:underflow /nopdbfile
# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /Z7 /Od /I "C:\Delta\OperatingRule\src" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "Sparse - Win32 Release"
# Name "Sparse - Win32 Debug"
# Begin Source File

SOURCE=.\dsm2_spfortran.c
# End Source File
# Begin Source File

SOURCE=.\spallocate.c
# End Source File
# Begin Source File

SOURCE=.\spbuild.c
# End Source File
# Begin Source File

SOURCE=.\spConfig.h
# End Source File
# Begin Source File

SOURCE=.\spDefs.h
# End Source File
# Begin Source File

SOURCE=.\spfactor.c
# End Source File
# Begin Source File

SOURCE=.\spfortran.c
# End Source File
# Begin Source File

SOURCE=.\spmatrix.h
# End Source File
# Begin Source File

SOURCE=.\spoutput.c
# End Source File
# Begin Source File

SOURCE=.\spsolve.c
# End Source File
# Begin Source File

SOURCE=.\sputils.c
# End Source File
# End Target
# End Project
