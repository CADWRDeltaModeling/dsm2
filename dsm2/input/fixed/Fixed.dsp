# Microsoft Developer Studio Project File - Name="Fixed" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=Fixed - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Fixed.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Fixed.mak" CFG="Fixed - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Fixed - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "Fixed - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "Fixed - Win32 Release"

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
# ADD F90 /architecture:pn4 /assume:buffered_io /assume:noaccuracy_sensitive /check:noflawed_pentium /compile_only /extend_source:132 /fpconstant /fpscomp:nolibs /include:"../../modules/release" /include:"../../../lib/hdf5" /math_library:fast /noaltparam /nologo /optimize:1 /traceback /warn:argument_checking /warn:nofileopt /fast /module:"../../modules/release"
# SUBTRACT F90 /check:bounds /check:overflow /fltconsistency
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

!ELSEIF  "$(CFG)" == "Fixed - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Fixed___"
# PROP BASE Intermediate_Dir "Fixed___"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /debug:full /include:"Fixed___/" /nologo /warn:nofileopt
# ADD F90 /architecture:pn4 /assume:buffered_io /browser /check:bounds /check:format /check:noflawed_pentium /check:power /check:output_conversion /check:overflow /compile_only /dbglibs /debug:full /error_limit:500 /extend_source:132 /fltconsistency /fpconstant /include:"../../modules/debug" /include:"../../../lib/hdf5" /nologo /traceback /warn:argument_checking /warn:declarations /warn:nofileopt /module:"../../modules/debug" /pdbfile:"Debug"
# SUBTRACT F90 /check:underflow /nopdbfile
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

# Name "Fixed - Win32 Release"
# Name "Fixed - Win32 Debug"
# Begin Group "Source"

# PROP Default_Filter ".c .cpp"
# Begin Source File

SOURCE=.\dsm2_expressions.cpp
# End Source File
# End Group
# Begin Group "Headers"

# PROP Default_Filter ".h .hpp"
# Begin Source File

SOURCE=.\dsm2_expressions.h
# End Source File
# End Group
# Begin Source File

SOURCE=.\check_fixed.f
DEP_F90_CHECK=\
	"..\..\hydro\chconnec.inc"\
	"..\..\hydro\network.inc"\
	"..\..\modules\debug\Gates.mod"\
	"..\..\modules\debug\Groups.mod"\
	"..\..\modules\debug\IO_Units.mod"\
	"..\time-varying\common_tide.f"\
	"..\time-varying\dss.inc"\
	"..\time-varying\readdss.inc"\
	"..\time-varying\tide.inc"\
	"..\time-varying\writedss.inc"\
	".\common.f"\
	".\common_irreg_geom.f"\
	".\common_ptm.inc"\
	".\common_qual.inc"\
	".\constants_ptm.inc"\
	".\defs.f"\
	".\defs_ptm.inc"\
	".\misc.f"\
	
# End Source File
# Begin Source File

SOURCE=.\check_fixed_hydro.f
DEP_F90_CHECK_=\
	"..\..\hydro\chconnec.inc"\
	"..\..\hydro\chcxtbl.inc"\
	"..\..\hydro\chnluser.inc"\
	"..\..\hydro\netcntrl.inc"\
	"..\..\hydro\network.inc"\
	"..\..\modules\debug\Gates.mod"\
	"..\..\modules\debug\IO_Units.mod"\
	"..\..\modules\debug\PhysicalConstants.mod"\
	"..\time-varying\common_tide.f"\
	"..\time-varying\dss.inc"\
	"..\time-varying\readdss.inc"\
	"..\time-varying\tide.inc"\
	"..\time-varying\writedss.inc"\
	".\common.f"\
	".\common_irreg_geom.f"\
	".\defs.f"\
	".\misc.f"\
	
# End Source File
# Begin Source File

SOURCE=.\check_fixed_ptm.f
DEP_F90_CHECK_F=\
	"..\..\hydro\chconnec.inc"\
	"..\..\hydro\chcxrec1.inc"\
	"..\..\hydro\chnluser.inc"\
	"..\..\hydro\netcntrl.inc"\
	"..\..\hydro\network.inc"\
	"..\..\modules\debug\IO_Units.mod"\
	"..\time-varying\common_qual_bin.inc"\
	"..\time-varying\common_tide.f"\
	"..\time-varying\dss.inc"\
	"..\time-varying\readdss.inc"\
	"..\time-varying\writedss.inc"\
	".\common.f"\
	".\common_ptm.inc"\
	".\constants_ptm.inc"\
	".\defs.f"\
	".\defs_ptm.inc"\
	".\misc.f"\
	
# End Source File
# Begin Source File

SOURCE=.\check_fixed_qual.f
DEP_F90_CHECK_FI=\
	"..\..\modules\debug\IO_Units.mod"\
	"..\..\modules\debug\rate_coeff_assignment.mod"\
	"..\..\qual\bltm1.inc"\
	"..\..\qual\bltm2.inc"\
	"..\..\qual\bltm3.inc"\
	"..\..\qual\param.inc"\
	"..\time-varying\common_tide.f"\
	"..\time-varying\dss.inc"\
	"..\time-varying\readdss.inc"\
	"..\time-varying\writedss.inc"\
	".\common.f"\
	".\common_qual.inc"\
	".\defs.f"\
	".\misc.f"\
	
# End Source File
# Begin Source File

SOURCE=.\data_source.f
DEP_F90_DATA_=\
	".\common.f"\
	".\defs.f"\
	".\misc.f"\
	
# End Source File
# Begin Source File

SOURCE=.\dsm2_init.f
DEP_F90_DSM2_=\
	"..\..\hydro\network.inc"\
	"..\..\modules\debug\Gates.mod"\
	"..\..\modules\debug\Groups.mod"\
	"..\..\modules\debug\IO_Units.mod"\
	"..\..\modules\debug\rate_coeff_assignment.mod"\
	"..\time-varying\common_tide.f"\
	"..\time-varying\dss.inc"\
	"..\time-varying\readdss.inc"\
	"..\time-varying\tide.inc"\
	"..\time-varying\writedss.inc"\
	".\common.f"\
	".\common_ptm.inc"\
	".\common_qual.inc"\
	".\constants_ptm.inc"\
	".\defs.f"\
	".\defs_ptm.inc"\
	".\misc.f"\
	

!IF  "$(CFG)" == "Fixed - Win32 Release"

!ELSEIF  "$(CFG)" == "Fixed - Win32 Debug"

# ADD F90 /assume:source_include

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\init_ptm.f
DEP_F90_INIT_=\
	"..\time-varying\common_qual_bin.inc"\
	".\common_ptm.inc"\
	".\constants_ptm.inc"\
	".\defs_ptm.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\..\qual\interpx.f
DEP_F90_INTER=\
	"..\..\hydro\network.inc"\
	"..\..\qual\bltm1.inc"\
	"..\..\qual\bltm2.inc"\
	"..\..\qual\bltm3.inc"\
	"..\..\qual\param.inc"\
	"..\time-varying\common_tide.f"\
	".\common.f"\
	".\defs.f"\
	".\misc.f"\
	
# End Source File
# Begin Source File

SOURCE=.\name_to_objno.f
DEP_F90_NAME_=\
	"..\..\modules\debug\Gates.mod"\
	"..\..\modules\debug\Groups.mod"\
	"..\..\modules\debug\IO_Units.mod"\
	"..\time-varying\tide.inc"\
	".\common.f"\
	".\defs.f"\
	".\misc.f"\
	
# End Source File
# Begin Source File

SOURCE=.\prep_irreg.f
DEP_F90_PREP_=\
	".\common.f"\
	".\common_irreg_geom.f"\
	".\defs.f"\
	".\misc.f"\
	
# End Source File
# Begin Source File

SOURCE=.\print_output.f
DEP_F90_PRINT=\
	"..\..\hydro\chconnec.inc"\
	"..\..\hydro\netcntrl.inc"\
	"..\..\hydro\network.inc"\
	"..\..\modules\debug\Gates.mod"\
	"..\..\modules\debug\IO_Units.mod"\
	".\common.f"\
	".\common_irreg_geom.f"\
	".\defs.f"\
	".\misc.f"\
	

!IF  "$(CFG)" == "Fixed - Win32 Release"

!ELSEIF  "$(CFG)" == "Fixed - Win32 Debug"

# ADD F90 /assume:source_include /module:"/Debug"
# SUBTRACT F90 /fpp

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\print_outqual.f
DEP_F90_PRINT_=\
	"..\..\modules\debug\Groups.mod"\
	"..\..\modules\debug\IO_Units.mod"\
	"..\..\modules\debug\rate_coeff_assignment.mod"\
	"..\..\qual\bltm1.inc"\
	"..\..\qual\bltm2.inc"\
	"..\..\qual\bltm3.inc"\
	"..\..\qual\param.inc"\
	"..\time-varying\dss.inc"\
	"..\time-varying\readdss.inc"\
	"..\time-varying\writedss.inc"\
	".\common.f"\
	".\defs.f"\
	".\misc.f"\
	
# End Source File
# Begin Source File

SOURCE=.\process_fixed.f
DEP_F90_PROCE=\
	"..\..\hydro\chconnec.inc"\
	"..\..\hydro\netcntrl.inc"\
	"..\..\hydro\network.inc"\
	"..\..\modules\debug\dsm2_database.mod"\
	"..\..\modules\debug\Groups.mod"\
	"..\..\modules\debug\IO_Units.mod"\
	"..\..\modules\debug\PhysicalConstants.mod"\
	"..\time-varying\common_qual_bin.inc"\
	"..\time-varying\common_tide.f"\
	".\common.f"\
	".\common_irreg_geom.f"\
	".\common_ptm.inc"\
	".\common_qual.inc"\
	".\constants_ptm.inc"\
	".\defs.f"\
	".\defs_ptm.inc"\
	".\misc.f"\
	
# End Source File
# Begin Source File

SOURCE=.\process_irreg.f
DEP_F90_PROCES=\
	"..\..\modules\debug\IO_Units.mod"\
	".\common.f"\
	".\common_irreg_geom.f"\
	".\defs.f"\
	".\misc.f"\
	
# End Source File
# Begin Source File

SOURCE=..\..\qual\rate_chanres.f
DEP_F90_RATE_=\
	"..\..\qual\bltm3.inc"\
	"..\..\qual\kinetic1.inc"\
	"..\..\qual\param.inc"\
	".\common.f"\
	".\common_qual.inc"\
	".\defs.f"\
	".\misc.f"\
	
# End Source File
# Begin Source File

SOURCE=.\read_fixed.f
DEP_F90_READ_=\
	"..\..\hydro\network.inc"\
	"..\..\modules\debug\IO_Units.mod"\
	"..\time-varying\common_tide.f"\
	"..\time-varying\dss.inc"\
	"..\time-varying\readdss.inc"\
	"..\time-varying\tide.inc"\
	".\common.f"\
	".\common_ptm.inc"\
	".\constants_ptm.inc"\
	".\defs.f"\
	".\defs_ptm.inc"\
	".\misc.f"\
	
# End Source File
# Begin Source File

SOURCE=.\read_fixed_sql.f
DEP_F90_READ_F=\
	"..\..\hydro\chconnec.inc"\
	"..\..\hydro\netcntrl.inc"\
	"..\..\hydro\network.inc"\
	"..\..\modules\debug\dsm2_database.mod"\
	"..\..\modules\debug\Gates.mod"\
	"..\..\modules\debug\Groups.mod"\
	"..\..\modules\debug\IO_Units.mod"\
	"..\..\modules\debug\PhysicalConstants.mod"\
	".\common.f"\
	".\common_ptm.inc"\
	".\common_qual.inc"\
	".\constants_ptm.inc"\
	".\defs.f"\
	".\defs_ptm.inc"\
	".\misc.f"\
	{$(INCLUDE)}"f90SQL.mod"\
	{$(INCLUDE)}"f90SQLConstants.mod"\
	{$(INCLUDE)}"f90SQLStructures.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\read_grid_sql.f
DEP_F90_READ_G=\
	"..\..\modules\debug\dsm2_database.mod"\
	"..\..\modules\debug\Gates.mod"\
	"..\..\modules\debug\IO_Units.mod"\
	".\common.f"\
	".\common_irreg_geom.f"\
	".\defs.f"\
	".\misc.f"\
	{$(INCLUDE)}"f90SQL.mod"\
	{$(INCLUDE)}"f90SQLConstants.mod"\
	{$(INCLUDE)}"f90SQLStructures.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\read_groups_sql.f
DEP_F90_READ_GR=\
	"..\..\modules\debug\Groups.mod"\
	"..\..\modules\debug\IO_Units.mod"\
	".\common.f"\
	".\defs.f"\
	".\misc.f"\
	{$(INCLUDE)}"f90SQL.mod"\
	{$(INCLUDE)}"f90SQLConstants.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\read_initcond_sql.f
DEP_F90_READ_I=\
	"..\..\hydro\chconnec.inc"\
	"..\..\hydro\chinitcd.inc"\
	"..\..\hydro\network.inc"\
	"..\..\modules\debug\dsm2_database.mod"\
	"..\..\modules\debug\IO_Units.mod"\
	".\common.f"\
	".\defs.f"\
	".\misc.f"\
	{$(INCLUDE)}"f90SQL.mod"\
	{$(INCLUDE)}"f90SQLConstants.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\read_input_ts_sql.f
DEP_F90_READ_IN=\
	"..\..\modules\debug\Gates.mod"\
	"..\..\modules\debug\IO_Units.mod"\
	".\common.f"\
	".\defs.f"\
	".\misc.f"\
	{$(INCLUDE)}"f90SQL.mod"\
	{$(INCLUDE)}"f90SQLConstants.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\read_op_rules_sql.f
DEP_F90_READ_O=\
	"..\..\modules\debug\dsm2_database.mod"\
	"..\..\modules\debug\IO_Units.mod"\
	".\common.f"\
	".\defs.f"\
	".\misc.f"\
	{$(INCLUDE)}"f90SQL.mod"\
	{$(INCLUDE)}"f90SQLConstants.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\read_output_ts_sql.f
DEP_F90_READ_OU=\
	"..\..\modules\debug\Gates.mod"\
	"..\..\modules\debug\Groups.mod"\
	"..\..\modules\debug\IO_Units.mod"\
	".\common.f"\
	".\defs.f"\
	".\misc.f"\
	{$(INCLUDE)}"f90SQL.mod"\
	{$(INCLUDE)}"f90SQLConstants.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\read_rate_coeffs_sql.f
DEP_F90_READ_R=\
	"..\..\modules\debug\dsm2_database.mod"\
	"..\..\modules\debug\Groups.mod"\
	"..\..\modules\debug\IO_Units.mod"\
	"..\..\modules\debug\rate_coeff_assignment.mod"\
	".\common.f"\
	".\common_qual.inc"\
	".\defs.f"\
	".\misc.f"\
	{$(INCLUDE)}"f90SQL.mod"\
	{$(INCLUDE)}"f90SQLConstants.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\utilities.f
DEP_F90_UTILI=\
	"..\..\modules\debug\IO_Units.mod"\
	".\common.f"\
	".\defs.f"\
	".\misc.f"\
	
# End Source File
# End Target
# End Project
