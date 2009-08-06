# Microsoft Developer Studio Project File - Name="ptm" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=ptm - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "ptm.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "ptm.mak" CFG="ptm - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "ptm - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "ptm - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "ptm - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /include:"Release/" /dll /nologo /warn:nofileopt
# ADD F90 /architecture:pn4 /assume:buffered_io /check:noflawed_pentium /compile_only /debug:none /extend_source:132 /include:"../modules/release" /include:"../../lib/hdf5" /noaltparam /nologo /module:"../modules/release"
# SUBTRACT F90 /threads
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /G4 /Ot /I "D:\Java\j2sdk1.4.2\include" /I "D:\Java\j2sdk1.4.2\include\win32" /Fo"Debug/" /GD /TP /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 f90sql.lib heclib-6-ND-dvf-mtd.lib  hdf5_fortran.lib hdf5_hl.lib szlib.lib zdll.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /nodefaultlib:"libcmtd" /libpath:"..\..\..\lib" /libpath:"..\..\..\lib\hdf5"

!ELSEIF  "$(CFG)" == "ptm - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /debug:full /include:"Debug/" /dll /nologo /warn:nofileopt
# ADD F90 /architecture:pn4 /assume:buffered_io /check:bounds /check:noflawed_pentium /check:overflow /compile_only /dbglibs /debug:full /extend_source:132 /include:"../modules/debug" /noaltparam /nologo /warn:argument_checking /module:"../modules/debug" /pdbfile:"Debug"
# SUBTRACT F90 /nopdbfile /warn:unused
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /G4 /Ot /GD /TP /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 f90sql.lib heclib-6-ND-dvf-mtd.lib hdf5_fortran.lib szlib.lib zdll.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /nodefaultlib:"LIBCMTD" /nodefaultlib:"LIBC" /pdbtype:sept
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Cmds=copy debug\ptm.* d:\programs\ptm\lib\.
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "ptm - Win32 Release"
# Name "ptm - Win32 Debug"
# Begin Source File

SOURCE=.\native\DWR_DMS_PTM_Globals.c
DEP_CPP_DWR_D=\
	"..\..\..\..\Java\j2sdk1.4.2\include\jni.h"\
	"..\..\..\..\Java\j2sdk1.4.2\include\win32\jni_md.h"\
	".\native\DWR_DMS_PTM_Globals.h"\
	".\native\fixedData.h"\
	
# End Source File
# Begin Source File

SOURCE=.\native\DWR_DMS_PTM_Globals.h
# End Source File
# Begin Source File

SOURCE=.\native\DWR_DMS_PTM_PTMFixedData.c
DEP_CPP_DWR_DM=\
	"..\..\..\..\Java\j2sdk1.4.2\include\jni.h"\
	"..\..\..\..\Java\j2sdk1.4.2\include\win32\jni_md.h"\
	".\native\DWR_DMS_PTM_PTMFixedData.h"\
	".\native\fixedData.h"\
	
# End Source File
# Begin Source File

SOURCE=.\native\DWR_DMS_PTM_PTMFixedData.h
# End Source File
# Begin Source File

SOURCE=.\native\DWR_DMS_PTM_PTMFluxOutput.c
DEP_CPP_DWR_DMS=\
	"..\..\..\..\Java\j2sdk1.4.2\include\jni.h"\
	"..\..\..\..\Java\j2sdk1.4.2\include\win32\jni_md.h"\
	".\native\DWR_DMS_PTM_PTMFluxOutput.h"\
	".\native\fixedData.h"\
	
# End Source File
# Begin Source File

SOURCE=.\native\DWR_DMS_PTM_PTMFluxOutput.h
# End Source File
# Begin Source File

SOURCE=.\native\DWR_DMS_PTM_PTMHydroInput.c
DEP_CPP_DWR_DMS_=\
	"..\..\..\..\Java\j2sdk1.4.2\include\jni.h"\
	"..\..\..\..\Java\j2sdk1.4.2\include\win32\jni_md.h"\
	".\native\DWR_DMS_PTM_PTMHydroInput.h"\
	".\native\dynamicData.h"\
	
# End Source File
# Begin Source File

SOURCE=.\native\DWR_DMS_PTM_PTMHydroInput.h
# End Source File
# Begin Source File

SOURCE=.\native\dynamicData.f
DEP_F90_DYNAM=\
	"..\input\fixed\common.f"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\input\time-varying\common_qual_bin.inc"\
	"..\input\time-varying\common_tide.f"\
	"..\input\time-varying\tide.inc"\
	".\native\ptmLocal.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\native\dynamicData.h
# End Source File
# Begin Source File

SOURCE=.\native\fixedData.f
DEP_F90_FIXED=\
	"..\hydro\chcxtbl.inc"\
	"..\hydro\network.inc"\
	"..\input\fixed\common.f"\
	"..\input\fixed\common_irreg_geom.f"\
	"..\input\fixed\common_ptm.inc"\
	"..\input\fixed\constants_ptm.inc"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\defs_ptm.inc"\
	"..\input\fixed\misc.f"\
	"..\input\time-varying\common_qual_bin.inc"\
	"..\input\time-varying\common_tide.f"\
	"..\input\time-varying\tide.inc"\
	"..\modules\Release\Groups.mod"\
	"..\modules\Release\IO_Units.mod"\
	".\native\ptmLocal.inc"\
	".\native\version.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\native\fixedData.h
# End Source File
# Begin Source File

SOURCE=.\native\miscData.f
# End Source File
# Begin Source File

SOURCE=.\native\ptmLocal.f
DEP_F90_PTMLO=\
	"..\hydro\network.inc"\
	"..\input\fixed\common.f"\
	"..\input\fixed\common_ptm.inc"\
	"..\input\fixed\constants_ptm.inc"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\defs_ptm.inc"\
	"..\input\fixed\misc.f"\
	"..\input\time-varying\common_tide.f"\
	"..\input\time-varying\tide.inc"\
	"..\modules\Release\dsm2_database.mod"\
	"..\modules\Release\Groups.mod"\
	"..\modules\Release\IO_Units.mod"\
	".\native\ptmLocal.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\native\ptmLocal.inc
# End Source File
# Begin Source File

SOURCE=.\native\version.inc
# End Source File
# End Target
# End Project
