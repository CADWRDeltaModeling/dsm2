# Microsoft Developer Studio Project File - Name="qual" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=qual - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "qual.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "qual.mak" CFG="qual - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "qual - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "qual - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "qual - Win32 Release"

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
# ADD BASE F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
# ADD F90 /assume:buffered_io /assume:noaccuracy_sensitive /check:noflawed_pentium /compile_only /extend_source:132 /fpconstant /fpscomp:nolibs /include:"../modules/release" /include:"../../lib/hdf5" /math_library:fast /nologo /traceback /transform_loops /tune:pn4 /warn:argument_checking /warn:nofileopt /fast /module:"../modules/release"
# SUBTRACT F90 /fltconsistency
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /I "..\..\oprule" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 f90sql.lib heclib-6-ND-dvf-mtd.lib oprule_parser_msvc6_st.lib  hdf5_fortran.lib hdf5_hl.lib szlib.lib zdll.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386 /libpath:"..\..\lib" /libpath:"..\..\lib\hdf5"

!ELSEIF  "$(CFG)" == "qual - Win32 Debug"

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
# ADD BASE F90 /compile_only /debug:full /include:"Debug/" /nologo /warn:nofileopt
# ADD F90 /browser /check:bounds /check:format /check:noflawed_pentium /check:power /check:output_conversion /check:overflow /check:underflow /compile_only /dbglibs /debug:full /extend_source:132 /fltconsistency /fpconstant /fpe:0 /include:"../modules/debug" /include:"../../lib/hdf5" /nologo /traceback /warn:argument_checking /warn:declarations /warn:nofileopt /module:"../modules/debug" /include:"/Debug/" /include:"/Debug/"
# SUBTRACT F90 /warn:unused
# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /I "..\..\oprule" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /FR /YX /FD /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
# SUBTRACT RSC /x
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 f90sql.lib heclib-6-ND-dvf-mtd.lib oprule_parser_msvc6_std.lib hdf5_fortran.lib szlib.lib zdll.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /profile /map /debug /machine:I386 /nodefaultlib:"LIBC" /libpath:"..\..\lib" /libpath:"..\..\lib\hdf5"
# SUBTRACT LINK32 /nodefaultlib

!ENDIF 

# Begin Target

# Name "qual - Win32 Release"
# Name "qual - Win32 Debug"
# Begin Source File

SOURCE=.\qual.f
DEP_F90_QUAL_=\
	"..\hydro\network.inc"\
	"..\input\fixed\common.f"\
	"..\input\fixed\common_qual.inc"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\input\time-varying\common_tide.f"\
	"..\input\time-varying\dss.inc"\
	"..\input\time-varying\readdss.inc"\
	"..\input\time-varying\tide.inc"\
	"..\input\time-varying\writedss.inc"\
	"..\modules\Release\dsm2_database.mod"\
	"..\modules\Release\IO_Units.mod"\
	".\bltm1.inc"\
	".\bltm2.inc"\
	".\bltm3.inc"\
	".\kinetic1.inc"\
	".\param.inc"\
	".\version.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\version.inc
# End Source File
# End Target
# End Project
