# Microsoft Developer Studio Project File - Name="dsm2modules" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=dsm2modules - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "dsm2modules.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "dsm2modules.mak" CFG="dsm2modules - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "dsm2modules - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "dsm2modules - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "dsm2modules - Win32 Release"

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
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /architecture:pn4 /assume:buffered_io /check:noflawed_pentium /compile_only /extend_source:132 /include:"Release/" /include:"../../lib/hdf5" /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /I "..\..\oprule" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "dsm2modules - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "dsm2modules___Win32_Debug"
# PROP BASE Intermediate_Dir "dsm2modules___Win32_Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /architecture:pn4 /assume:buffered_io /browser /check:bounds /check:noflawed_pentium /compile_only /dbglibs /debug:full /error_limit:500 /extend_source:132 /include:"Debug/" /include:"../../lib/hdf5" /nologo /traceback /warn:argument_checking /warn:nofileopt /pdbfile:"Debug"
# SUBTRACT F90 /nopdbfile
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /I "..\..\oprule" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\..\modules\debug\dsm2modules.lib"

!ENDIF 

# Begin Target

# Name "dsm2modules - Win32 Release"
# Name "dsm2modules - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\constants.f
DEP_F90_CONST=\
	".\Release\IO_Units.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\dsm2_database.f
DEP_F90_DSM2_=\
	"..\input\fixed\common.f"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	".\Release\IO_Units.mod"\
	{$(INCLUDE)}"f90SQL.mod"\
	{$(INCLUDE)}"f90SQLConstants.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gates.f
DEP_F90_GATES=\
	"..\hydro\network.inc"\
	"..\input\fixed\common.f"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	".\Release\IO_Units.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\groups.f
DEP_F90_GROUP=\
	"..\hydro\network.inc"\
	"..\input\fixed\common.f"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\input\time-varying\tide.inc"\
	".\Release\Gates.mod"\
	".\Release\IO_Units.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\hdf5_modules.f
DEP_F90_HDF5_=\
	"..\..\lib\hdf5\HDF5.mod"\
	"..\hydro\chconnec.inc"\
	"..\hydro\chcxtbl.inc"\
	"..\hydro\chnlcomp.inc"\
	"..\hydro\chstatus.inc"\
	"..\hydro\network.inc"\
	"..\input\fixed\common.f"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\input\time-varying\common_tide.f"\
	"..\input\time-varying\dss.inc"\
	"..\input\time-varying\readdss.inc"\
	"..\input\time-varying\tide.inc"\
	

!IF  "$(CFG)" == "dsm2modules - Win32 Release"

# ADD F90 /check:bounds

!ELSEIF  "$(CFG)" == "dsm2modules - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\io_units.f
# End Source File
# Begin Source File

SOURCE=.\pattern_match.cpp
# End Source File
# Begin Source File

SOURCE=.\rate_coeff_assignment.f
DEP_F90_RATE_=\
	"..\input\fixed\common.f"\
	"..\input\fixed\common_qual.inc"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	".\Release\Groups.mod"\
	".\Release\IO_Units.mod"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=.\pattern_match.hpp
# End Source File
# End Group
# End Target
# End Project
