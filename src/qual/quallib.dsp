# Microsoft Developer Studio Project File - Name="quallib" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=quallib - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "quallib.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "quallib.mak" CFG="quallib - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "quallib - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "quallib - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "quallib - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "./lib/Release"
# PROP Intermediate_Dir "./lib/Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /architecture:pn4 /assume:buffered_io /check:noflawed_pentium /compile_only /extend_source:132 /include:"../modules/release" /include:"../../lib/hdf5" /nologo /warn:nofileopt /module:"../modules/release"
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /W3 /GR /GX /O2 /I "..\..\oprule" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"quallib.lib"

!ELSEIF  "$(CFG)" == "quallib - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "quallib___Win32_Debug"
# PROP BASE Intermediate_Dir "quallib___Win32_Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "./lib/Debug"
# PROP Intermediate_Dir "./lib/Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /architecture:pn4 /assume:buffered_io /browser /check:bounds /check:noflawed_pentium /compile_only /dbglibs /debug:full /error_limit:500 /extend_source:132 /fpe:0 /include:"../modules/debug" /include:"../../lib/hdf5" /nologo /traceback /warn:argument_checking /warn:nofileopt /warn:unused /module:"../modules/debug" /pdbfile:"Debug" /assume:nobuffered_io
# SUBTRACT F90 /nopdbfile
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GR /GX /ZI /Od /I "..\..\oprule" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"quallib.lib"

!ENDIF 

# Begin Target

# Name "quallib - Win32 Release"
# Name "quallib - Win32 Debug"
# Begin Source File

SOURCE=.\balanceflo.f
DEP_F90_BALAN=\
	"..\input\fixed\common.f"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\input\time-varying\common_tide.f"\
	"..\modules\debug\IO_Units.mod"\
	".\bltm1.inc"\
	".\bltm2.inc"\
	".\bltm3.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\bltminit.f
DEP_F90_BLTMI=\
	"..\input\fixed\common.f"\
	"..\input\fixed\common_qual.inc"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\input\time-varying\common_tide.f"\
	"..\modules\debug\Groups.mod"\
	"..\modules\debug\IO_Units.mod"\
	".\bltm1.inc"\
	".\bltm2.inc"\
	".\bltm3.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\calscsk.f
DEP_F90_CALSC=\
	"..\input\fixed\common.f"\
	"..\input\fixed\common_qual.inc"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	".\bltm1.inc"\
	".\bltm3.inc"\
	".\kinetic1.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\checkerror.f
DEP_F90_CHECK=\
	"..\input\fixed\common.f"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\modules\debug\IO_Units.mod"\
	".\bltm1.inc"\
	".\bltm2.inc"\
	".\bltm3.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\get_output.f
DEP_F90_GET_O=\
	"..\input\fixed\common.f"\
	"..\input\fixed\common_qual.inc"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\input\time-varying\common_tide.f"\
	"..\modules\debug\Groups.mod"\
	"..\modules\debug\IO_Units.mod"\
	".\bltm1.inc"\
	".\bltm2.inc"\
	".\bltm3.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\heat.f
DEP_F90_HEAT_=\
	"..\input\fixed\common.f"\
	"..\input\fixed\common_qual.inc"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\modules\debug\IO_Units.mod"\
	".\bltm3.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\interpx.f
DEP_F90_INTER=\
	"..\hydro\network.inc"\
	"..\input\fixed\common.f"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\input\time-varying\common_tide.f"\
	".\bltm1.inc"\
	".\bltm2.inc"\
	".\bltm3.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\kinetic.f
DEP_F90_KINET=\
	"..\input\fixed\common.f"\
	"..\input\fixed\common_qual.inc"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	".\bltm3.inc"\
	".\kinetic1.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\masstrack.f
DEP_F90_MASST=\
	"..\input\fixed\common.f"\
	"..\input\fixed\common_qual.inc"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\input\time-varying\common_tide.f"\
	"..\modules\debug\IO_Units.mod"\
	".\bltm1.inc"\
	".\bltm2.inc"\
	".\bltm3.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\node_rate.f
DEP_F90_NODE_=\
	"..\hydro\network.inc"\
	"..\input\fixed\common.f"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\input\time-varying\common_tide.f"\
	"..\input\time-varying\tide.inc"\
	"..\modules\debug\Groups.mod"\
	"..\modules\debug\IO_Units.mod"\
	".\bltm1.inc"\
	".\bltm2.inc"\
	".\bltm3.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\input\fixed\oprule_management.f
DEP_F90_OPRUL=\
	"..\input\fixed\common.f"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	
# End Source File
# Begin Source File

SOURCE=.\process_tide.f
DEP_F90_PROCE=\
	"..\hydro\network.inc"\
	"..\input\fixed\common.f"\
	"..\input\fixed\common_qual.inc"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\input\time-varying\common_tide.f"\
	"..\input\time-varying\tide.inc"\
	"..\modules\debug\IO_Units.mod"\
	".\bltm1.inc"\
	".\bltm2.inc"\
	".\bltm3.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\rate_chanres.f
DEP_F90_RATE_=\
	"..\input\fixed\common.f"\
	"..\input\fixed\common_qual.inc"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	".\bltm3.inc"\
	".\kinetic1.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\res_rate.f
DEP_F90_RES_R=\
	"..\hydro\network.inc"\
	"..\input\fixed\common.f"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\input\time-varying\common_tide.f"\
	"..\input\time-varying\tide.inc"\
	"..\modules\debug\Groups.mod"\
	"..\modules\debug\IO_Units.mod"\
	".\bltm1.inc"\
	".\bltm2.inc"\
	".\bltm3.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\route.f
DEP_F90_ROUTE=\
	"..\input\fixed\common.f"\
	"..\input\fixed\common_qual.inc"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\modules\debug\IO_Units.mod"\
	".\bltm1.inc"\
	".\bltm2.inc"\
	".\bltm3.inc"\
	".\kinetic1.inc"\
	".\param.inc"\
	
# End Source File
# End Target
# End Project
