# Microsoft Developer Studio Project File - Name="OperatingRule" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=OperatingRule - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "OperatingRule.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "OperatingRule.mak" CFG="OperatingRule - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "OperatingRule - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "OperatingRule - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "OperatingRule - Win32 Release"

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
# ADD F90 /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /I ".\..\.." /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\..\lib\oprule_msvc6_st.lib"
# Begin Special Build Tool
TargetDir=\Delta\Models\oprule\lib
TargetPath=\Delta\Models\oprule\lib\oprule_msvc6_st.lib
SOURCE="$(InputPath)"
PostBuild_Desc=Copy to common library location
PostBuild_Cmds=IF EXIST $(TargetDir)\..\..\lib copy $(TargetPath) $(TargetDir)\..\..\lib\.	IF NOT EXIST $(TargetDir)\..\..\lib ECHO Common library location does not exist -- library must be copied manually.
# End Special Build Tool

!ELSEIF  "$(CFG)" == "OperatingRule - Win32 Debug"

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
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GR /GX /ZI /Od /I ".\..\.." /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\..\lib\oprule_msvc6_std.lib"
# Begin Special Build Tool
TargetDir=\Delta\Models\oprule\lib
TargetPath=\Delta\Models\oprule\lib\oprule_msvc6_std.lib
SOURCE="$(InputPath)"
PostBuild_Desc=Copy to common library location
PostBuild_Cmds=IF EXIST $(TargetDir)\..\..\lib copy $(TargetPath) $(TargetDir)\..\..\lib\.	IF NOT EXIST $(TargetDir)\..\..\lib ECHO Common library location does not exist -- library must be copied manually.
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "OperatingRule - Win32 Release"
# Name "OperatingRule - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\..\lib\rule\ActionChain.cpp
# End Source File
# Begin Source File

SOURCE=..\..\lib\rule\ActionSet.cpp
# End Source File
# Begin Source File

SOURCE=..\..\lib\rule\ExpressionTrigger.cpp
# End Source File
# Begin Source File

SOURCE=..\..\lib\rule\ModelAction.cpp
# End Source File
# Begin Source File

SOURCE=..\..\lib\rule\ModelInterface.cpp
# End Source File
# Begin Source File

SOURCE=..\..\lib\rule\OperatingRule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\lib\rule\OperationAction.cpp
# End Source File
# Begin Source File

SOURCE=..\..\lib\rule\OperationManager.cpp
# End Source File
# Begin Source File

SOURCE=..\..\lib\rule\Trigger.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\..\oprule\rule\ActionChain.h
# End Source File
# Begin Source File

SOURCE=..\..\oprule\rule\ActionResolver.h
# End Source File
# Begin Source File

SOURCE=..\..\oprule\rule\ActionSet.h
# End Source File
# Begin Source File

SOURCE=..\..\oprule\rule\ExpressionTrigger.h
# End Source File
# Begin Source File

SOURCE=..\..\oprule\rule\ModelAction.h
# End Source File
# Begin Source File

SOURCE=..\..\oprule\rule\ModelInterface.h
# End Source File
# Begin Source File

SOURCE=..\..\oprule\rule\ModelInterfaceActionResolver.h
# End Source File
# Begin Source File

SOURCE=..\..\oprule\rule\ModelTimer.h
# End Source File
# Begin Source File

SOURCE=..\..\oprule\rule\OperatingRule.h
# End Source File
# Begin Source File

SOURCE=..\..\oprule\rule\OperationAction.h
# End Source File
# Begin Source File

SOURCE=..\..\oprule\rule\OperationManager.h
# End Source File
# Begin Source File

SOURCE=..\..\oprule\rule\Trigger.h
# End Source File
# End Group
# End Target
# End Project
