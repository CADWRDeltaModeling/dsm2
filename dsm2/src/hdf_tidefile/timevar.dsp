# Microsoft Developer Studio Project File - Name="timevar" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=TIMEVAR - WIN32 DEBUG
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "timevar.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "timevar.mak" CFG="TIMEVAR - WIN32 DEBUG"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "timevar - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "timevar - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "timevar - Win32 Release"

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
# ADD F90 /architecture:pn4 /assume:buffered_io /assume:noaccuracy_sensitive /check:noflawed_pentium /compile_only /extend_source:132 /fpconstant /fpscomp:nolibs /include:"../../modules/release" /include:"../../../lib/hdf5" /math_library:fast /nologo /traceback /transform_loops /warn:argument_checking /warn:nofileopt /fast /module:"../../modules/release"
# SUBTRACT F90 /fltconsistency
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /I "..\..\..\oprule" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "timevar - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /debug:full /include:"Debug/" /nologo /warn:nofileopt
# ADD F90 /assume:buffered_io /browser /check:bounds /check:format /check:noflawed_pentium /check:power /check:output_conversion /check:overflow /compile_only /dbglibs /debug:full /error_limit:500 /extend_source:132 /fltconsistency /fpconstant /include:"../../modules/debug" /include:"../../../lib/hdf5" /nologo /traceback /warn:argument_checking /warn:declarations /warn:nofileopt /module:"../../modules/debug" /pdbfile:"Debug" /assume:nobuffered_io /include:"/debug"
# SUBTRACT F90 /nopdbfile
# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /Z7 /Od /I "..\..\..\oprule" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
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

# Name "timevar - Win32 Release"
# Name "timevar - Win32 Debug"
# Begin Source File

SOURCE="..\time-varying\get_tidefile_dates.f"
DEP_F90_GET_T=\
	"..\..\modules\release\HDFVARS.mod"\
	"..\..\modules\Release\IO_Units.mod"\
	"..\fixed\common.f"\
	"..\fixed\defs.f"\
	"..\fixed\misc.f"\
	".\common_tide.f"\
	
# End Source File
# Begin Source File

SOURCE=.\hdf5_diagnostic.f
DEP_F90_HDF5_=\
	"..\..\modules\release\HDFVARS.mod"\
	"..\..\modules\Release\inclvars.mod"\
	"..\..\modules\Release\IO_Units.mod"\
	"..\..\modules\Release\qextvars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\hdf5_init.f
DEP_F90_HDF5_I=\
	"..\..\..\lib\hdf5\HDF5.mod"\
	"..\..\modules\release\HDFVARS.mod"\
	"..\..\modules\Release\inclvars.mod"\
	"..\..\modules\Release\IO_Units.mod"\
	"..\..\modules\Release\objvars.mod"\
	"..\..\modules\Release\qextvars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\hdf5_read.f
DEP_F90_HDF5_R=\
	"..\..\..\lib\hdf5\HDF5.mod"\
	"..\..\modules\release\HDFVARS.mod"\
	"..\..\modules\Release\inclvars.mod"\
	"..\..\modules\Release\qextvars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\hdf5_write.f
DEP_F90_HDF5_W=\
	"..\..\..\lib\hdf5\HDF5.mod"\
	"..\..\modules\release\HDFVARS.mod"\
	"..\..\modules\Release\inclvars.mod"\
	"..\..\modules\Release\objvars.mod"\
	"..\..\modules\Release\qextvars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\hdf_attach_dimension_scales.cpp
# End Source File
# Begin Source File

SOURCE="..\time-varying\init_store_outpaths.f"
DEP_F90_INIT_=\
	"..\..\modules\Release\dsm2_database.mod"\
	"..\..\modules\Release\IO_Units.mod"\
	"..\fixed\common.f"\
	"..\fixed\defs.f"\
	"..\fixed\misc.f"\
	".\dss.inc"\
	".\writedss.inc"\
	
# End Source File
# Begin Source File

SOURCE="..\time-varying\read_mult_tide.f"
DEP_F90_READ_=\
	"..\..\modules\Release\IO_Units.mod"\
	"..\fixed\common.f"\
	"..\fixed\defs.f"\
	"..\fixed\misc.f"\
	".\common_tide.f"\
	".\tide.inc"\
	
# End Source File
# Begin Source File

SOURCE="..\time-varying\read_tide_flow.f"
DEP_F90_READ_T=\
	"..\..\modules\release\HDFVARS.mod"\
	"..\..\modules\Release\IO_Units.mod"\
	"..\fixed\common.f"\
	"..\fixed\defs.f"\
	"..\fixed\misc.f"\
	".\common_tide.f"\
	".\tide.inc"\
	
# End Source File
# Begin Source File

SOURCE="..\time-varying\read_tide_head.f"
DEP_F90_READ_TI=\
	"..\..\modules\Release\Groups.mod"\
	"..\..\modules\release\HDFVARS.mod"\
	"..\..\modules\Release\IO_Units.mod"\
	"..\fixed\common.f"\
	"..\fixed\defs.f"\
	"..\fixed\misc.f"\
	".\common_tide.f"\
	".\tide.inc"\
	
# End Source File
# Begin Source File

SOURCE="..\time-varying\readdss.f"
DEP_F90_READD=\
	"..\..\modules\Release\IO_Units.mod"\
	"..\fixed\common.f"\
	"..\fixed\defs.f"\
	"..\fixed\misc.f"\
	".\dss.inc"\
	
# End Source File
# Begin Source File

SOURCE="..\time-varying\readtvd.f"
DEP_F90_READT=\
	"..\..\hydro\chconnec.inc"\
	"..\..\hydro\netbnd.inc"\
	"..\..\hydro\network.inc"\
	"..\..\modules\Release\IO_Units.mod"\
	"..\fixed\common.f"\
	"..\fixed\defs.f"\
	"..\fixed\misc.f"\
	
# End Source File
# Begin Source File

SOURCE="..\time-varying\store_outpaths.f"
DEP_F90_STORE=\
	"..\fixed\common.f"\
	"..\fixed\defs.f"\
	"..\fixed\misc.f"\
	".\dss.inc"\
	".\intervals.inc"\
	".\writedss.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\tide_time.f
DEP_F90_TIDE_=\
	"..\..\..\lib\hdf5\HDF5.mod"\
	"..\..\modules\release\HDFVARS.mod"\
	"..\..\modules\Release\inclvars.mod"\
	"..\fixed\common.f"\
	"..\fixed\defs.f"\
	"..\fixed\misc.f"\
	".\common_tide.f"\
	
# End Source File
# Begin Source File

SOURCE="..\time-varying\update_intervals.f"
DEP_F90_UPDAT=\
	"..\fixed\common.f"\
	"..\fixed\defs.f"\
	"..\fixed\misc.f"\
	".\intervals.inc"\
	
# End Source File
# Begin Source File

SOURCE="..\time-varying\writedss.f"
DEP_F90_WRITE=\
	"..\..\modules\Release\IO_Units.mod"\
	"..\fixed\common.f"\
	"..\fixed\defs.f"\
	"..\fixed\misc.f"\
	".\dss.inc"\
	".\writedss.inc"\
	
# End Source File
# Begin Source File

SOURCE="..\time-varying\wrt_outpaths.f"
DEP_F90_WRT_O=\
	"..\..\modules\Release\IO_Units.mod"\
	"..\fixed\common.f"\
	"..\fixed\defs.f"\
	"..\fixed\misc.f"\
	".\dss.inc"\
	".\writedss.inc"\
	
# End Source File
# End Target
# End Project
