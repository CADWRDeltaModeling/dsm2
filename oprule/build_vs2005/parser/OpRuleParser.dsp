# Microsoft Developer Studio Project File - Name="OpRuleParser" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=OpRuleParser - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "OpRuleParser.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "OpRuleParser.mak" CFG="OpRuleParser - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "OpRuleParser - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "OpRuleParser - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "OpRuleParser - Win32 Release"

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
# ADD CPP /nologo /W3 /GR /GX /O2 /I ".\..\.." /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\..\lib\oprule_parser_msvc6_st.lib"
# Begin Special Build Tool
TargetDir=\Delta\models\oprule\lib
TargetPath=\Delta\models\oprule\lib\oprule_parser_msvc6_st.lib
SOURCE="$(InputPath)"
PostBuild_Desc=Copy to common library location
PostBuild_Cmds=IF EXIST $(TargetDir)\..\..\lib copy $(TargetPath) $(TargetDir)\..\..\lib\.	IF NOT EXIST $(TargetDir)\..\..\lib ECHO Common library location does not exist -- library must be copied manually.
# End Special Build Tool

!ELSEIF  "$(CFG)" == "OpRuleParser - Win32 Debug"

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
# ADD LIB32 /nologo /out:"..\..\lib\oprule_parser_msvc6_std.lib"
# Begin Special Build Tool
TargetDir=\Delta\models\oprule\lib
TargetPath=\Delta\models\oprule\lib\oprule_parser_msvc6_std.lib
SOURCE="$(InputPath)"
PostBuild_Desc=Copy to common library location
PostBuild_Cmds=IF EXIST $(TargetDir)\..\..\lib copy $(TargetPath) $(TargetDir)\..\..\lib\.	IF NOT EXIST $(TargetDir)\..\..\lib ECHO Common library location does not exist -- library must be copied manually.
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "OpRuleParser - Win32 Release"
# Name "OpRuleParser - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=..\..\lib\parser\ModelActionFactory.cpp
# End Source File
# Begin Source File

SOURCE=..\..\lib\parser\NamedValueLookup.cpp
# End Source File
# Begin Source File

SOURCE=..\..\lib\parser\op_rule.cpp
# End Source File
# Begin Source File

SOURCE=..\..\lib\parser\op_rule_tab.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=..\..\oprule\parser\ExpressionHandle.h
# End Source File
# Begin Source File

SOURCE=..\..\oprule\parser\ModelActionFactory.h
# End Source File
# Begin Source File

SOURCE=..\..\oprule\parser\ModelNameParseError.h
# End Source File
# Begin Source File

SOURCE=..\..\oprule\parser\ModelTimeNodeFactory.h
# End Source File
# Begin Source File

SOURCE=..\..\oprule\parser\NamedValueLookup.h
# End Source File
# Begin Source File

SOURCE=..\..\oprule\parser\NamedValueLookupImpl.h
# End Source File
# Begin Source File

SOURCE=..\..\oprule\parser\op_rule_tab.h
# End Source File
# Begin Source File

SOURCE=..\..\oprule\parser\ParseResultType.h
# End Source File
# Begin Source File

SOURCE=..\..\oprule\parser\Symbol.h
# End Source File
# Begin Source File

SOURCE=..\..\oprule\parser\unistd.h
# End Source File
# End Group
# Begin Source File

SOURCE=..\..\oprule\rule\ActionSet.h
# End Source File
# Begin Source File

SOURCE=..\..\lib\parser\op_rule.l

!IF  "$(CFG)" == "OpRuleParser - Win32 Release"

# Begin Custom Build
InputDir=\Delta\models\oprule\lib\parser
InputPath=..\..\lib\parser\op_rule.l

"$(InputDir)\op_rule.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	flex -L -Pop_rule -oop_rule.cpp $(InputPath) 
	copy .\op_rule.cpp $(InputDir)\op_rule.cpp 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "OpRuleParser - Win32 Debug"

# Begin Custom Build
InputDir=\Delta\models\oprule\lib\parser
InputPath=..\..\lib\parser\op_rule.l

"$(InputDir)\op_rule.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	flex -L -Pop_rule -oop_rule.cpp $(InputPath) 
	copy .\op_rule.cpp $(InputDir)\op_rule.cpp 
	
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\lib\parser\op_rule.y

!IF  "$(CFG)" == "OpRuleParser - Win32 Release"

# Begin Custom Build
InputDir=\Delta\models\oprule\lib\parser
InputPath=..\..\lib\parser\op_rule.y

BuildCmds= \
	bison -l -v -p op_rule -d  $(InputPath) \
	copy $(InputDir)\op_rule.tab.c $(InputDir)\op_rule_tab.cpp \
	copy $(InputDir)\op_rule.tab.h $(InputDir)\op_rule_tab.h \
	

"$(InputDir)\op_rule_tab.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"$(InputDir)\op_rule_tab.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ELSEIF  "$(CFG)" == "OpRuleParser - Win32 Debug"

# Begin Custom Build
InputDir=\Delta\models\oprule\lib\parser
InputPath=..\..\lib\parser\op_rule.y

BuildCmds= \
	bison -l -p op_rule -d  $(InputPath) \
	copy $(InputDir)\op_rule.tab.c $(InputDir)\op_rule_tab.cpp \
	copy $(InputDir)\op_rule.tab.h $(InputDir)\op_rule_tab.h \
	

"$(InputDir)\op_rule_tab.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"$(InputDir)\op_rule_tab.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ENDIF 

# End Source File
# End Target
# End Project
