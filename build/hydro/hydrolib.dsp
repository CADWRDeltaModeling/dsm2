# Microsoft Developer Studio Project File - Name="hydrolib" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=hydrolib - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "hydrolib.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "hydrolib.mak" CFG="hydrolib - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "hydrolib - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "hydrolib - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "hydrolib - Win32 Release"

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
# ADD LIB32 /nologo /out:"hydrolib.lib"

!ELSEIF  "$(CFG)" == "hydrolib - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "hydrolib___Win32_Debug"
# PROP BASE Intermediate_Dir "hydrolib___Win32_Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "./lib/Debug"
# PROP Intermediate_Dir "./lib/Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /architecture:pn4 /assume:buffered_io /browser /check:bounds /check:noflawed_pentium /compile_only /dbglibs /debug:full /error_limit:500 /extend_source:132 /include:"../modules/debug" /include:"../../lib/hdf5" /nologo /traceback /warn:argument_checking /warn:nofileopt /module:"../modules/debug" /pdbfile:"Debug" /assume:nobuffered_io
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
# ADD LIB32 /nologo /out:"hydrolib.lib"

!ENDIF 

# Begin Target

# Name "hydrolib - Win32 Release"
# Name "hydrolib - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=..\input\fixed\dsm2_hydro_named_value_lookup.cpp
# End Source File
# Begin Source File

SOURCE=..\input\fixed\dsm2_model_action_factory.cpp
# End Source File
# Begin Source File

SOURCE=..\INPUT\FIXED\dsm2_model_interface.cpp
# End Source File
# Begin Source File

SOURCE=..\input\fixed\dsm2_model_interface_gate.cpp
# End Source File
# Begin Source File

SOURCE=..\input\fixed\dsm2_named_value_factories.cpp
# End Source File
# Begin Source File

SOURCE=..\input\fixed\dsm2_oprule_management.cpp
# End Source File
# Begin Source File

SOURCE=..\input\fixed\dsm2_time_interface.cpp
# End Source File
# Begin Source File

SOURCE=..\INPUT\FIXED\model_interface.f
DEP_F90_MODEL=\
	"..\input\fixed\common.f"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\input\time-varying\tide.inc"\
	"..\modules\debug\Gates.mod"\
	"..\modules\debug\IO_Units.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\input\fixed\oprule_management.f
DEP_F90_OPRUL=\
	"..\input\fixed\common.f"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=..\input\fixed\dsm2_interface_fortran.h
# End Source File
# Begin Source File

SOURCE=..\input\fixed\dsm2_model_action_factory.h
# End Source File
# Begin Source File

SOURCE=..\INPUT\FIXED\dsm2_model_interface.h
# End Source File
# Begin Source File

SOURCE=..\input\fixed\dsm2_model_interface_gate.h
# End Source File
# Begin Source File

SOURCE=..\input\fixed\dsm2_model_interface_resolver.h
# End Source File
# Begin Source File

SOURCE=..\input\fixed\dsm2_named_value_factories.h
# End Source File
# Begin Source File

SOURCE=..\input\fixed\dsm2_named_value_lookup.h
# End Source File
# Begin Source File

SOURCE=..\INPUT\FIXED\dsm2_time_interface.h
# End Source File
# Begin Source File

SOURCE=..\INPUT\FIXED\dsm2_time_interface_fortran.h
# End Source File
# Begin Source File

SOURCE=..\input\fixed\dsm2_time_node_factory.h
# End Source File
# Begin Source File

SOURCE=..\input\fixed\dsm2_time_series_node.h
# End Source File
# End Group
# Begin Source File

SOURCE=.\boundary.f
DEP_F90_BOUND=\
	"..\input\fixed\common.f"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\input\time-varying\dss.inc"\
	"..\input\time-varying\readdss.inc"\
	"..\input\time-varying\tide.inc"\
	"..\modules\debug\Gates.mod"\
	"..\modules\debug\IO_Units.mod"\
	".\chconnec.inc"\
	".\netbnd.inc"\
	".\netcntrl.inc"\
	".\network.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\channel_constraints.f
DEP_F90_CHANN=\
	"..\modules\debug\IO_Units.mod"\
	".\chconnec.inc"\
	".\network.inc"\
	".\solver.inc"\
	".\strmcnst.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\channel_schematic.f
DEP_F90_CHANNE=\
	"..\input\fixed\common.f"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\modules\debug\IO_Units.mod"\
	".\chconnec.inc"\
	".\chnlcomp.inc"\
	".\chnluser.inc"\
	".\network.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\channel_status.f
# End Source File
# Begin Source File

SOURCE=.\channel_xsect_tbl.f
# End Source File
# Begin Source File

SOURCE="..\..\..\..\Program Files\Microsoft Visual Studio\DF98\INCLUDE\f90SQLStructures.mod"
# End Source File
# Begin Source File

SOURCE=.\fileutil.f
DEP_F90_FILEU=\
	"..\modules\debug\IO_Units.mod"\
	".\master.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\floweq1d.f
DEP_F90_FLOWE=\
	"..\modules\debug\PhysicalConstants.mod"\
	".\network.inc"\
	".\solver.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\gate_calc.f
DEP_F90_GATE_=\
	"..\input\fixed\common.f"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\modules\debug\Gates.mod"\
	"..\modules\debug\IO_Units.mod"\
	"..\modules\debug\PhysicalConstants.mod"\
	".\chconnec.inc"\
	".\network.inc"\
	".\solver.inc"\
	".\strmcnst.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\get_output.f
DEP_F90_GET_O=\
	"..\input\fixed\common.f"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\modules\debug\Gates.mod"\
	"..\modules\debug\IO_Units.mod"\
	".\chconnec.inc"\
	".\network.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\linear.f
DEP_F90_LINEA=\
	"..\modules\debug\IO_Units.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\netblnce.f
DEP_F90_NETBL=\
	"..\modules\debug\IO_Units.mod"\
	".\netblnce.inc"\
	".\network.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\netcntr1.f
DEP_F90_NETCN=\
	"..\input\fixed\common.f"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\modules\debug\IO_Units.mod"\
	".\netcntrl.inc"\
	".\network.inc"\
	".\solver.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\netdense.f
DEP_F90_NETDE=\
	"..\modules\debug\IO_Units.mod"\
	".\netdense.inc"\
	".\network.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\process_tide.f
# End Source File
# Begin Source File

SOURCE=.\reservoirs.f
DEP_F90_RESER=\
	"..\input\fixed\common.f"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\modules\debug\IO_Units.mod"\
	".\chconnec.inc"\
	".\network.inc"\
	".\solver.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\solvealloc.f
DEP_F90_SOLVE=\
	"..\input\fixed\common.f"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\modules\debug\Gates.mod"\
	"..\modules\debug\IO_Units.mod"\
	".\chconnec.inc"\
	".\chnlcomp.inc"\
	".\network.inc"\
	".\solver.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\solver_diagnostics.f
DEP_F90_SOLVER=\
	"..\input\fixed\common.f"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\modules\debug\Gates.mod"\
	"..\modules\debug\IO_Units.mod"\
	".\chconnec.inc"\
	".\chnlcomp.inc"\
	".\network.inc"\
	".\solver.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\solvesparse.f
DEP_F90_SOLVES=\
	"..\input\fixed\common.f"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\modules\debug\Gates.mod"\
	"..\modules\debug\IO_Units.mod"\
	".\chconnec.inc"\
	".\chstatus.inc"\
	".\network.inc"\
	".\solver.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\solveutil.f
DEP_F90_SOLVEU=\
	".\network.inc"\
	".\solver.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\strings.f
DEP_F90_STRIN=\
	"..\modules\debug\IO_Units.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\table3.f
DEP_F90_TABLE=\
	"..\modules\debug\IO_Units.mod"\
	".\table3.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\tidefile.f
DEP_F90_TIDEF=\
	"..\input\fixed\common.f"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\input\time-varying\common_tide.f"\
	"..\input\time-varying\dss.inc"\
	"..\input\time-varying\readdss.inc"\
	"..\input\time-varying\tide.inc"\
	"..\modules\Debug\hdfvars.mod"\
	"..\modules\debug\IO_Units.mod"\
	".\chconnec.inc"\
	".\chcxrec1.inc"\
	".\chcxtbl.inc"\
	".\chnlcomp.inc"\
	".\chstatus.inc"\
	".\netcntrl.inc"\
	".\network.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\virtual_xsect.f
DEP_F90_VIRTU=\
	"..\input\fixed\common.f"\
	"..\input\fixed\common_irreg_geom.f"\
	"..\input\fixed\defs.f"\
	"..\input\fixed\misc.f"\
	"..\modules\debug\IO_Units.mod"\
	".\virt_xsect.inc"\
	
# End Source File
# End Target
# End Project
