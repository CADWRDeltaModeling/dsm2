' ReadSetPath.vbs
' Clean up a path that has multiple entries. Abstract: VBSript to read/Set PATH environment(s), both USER and SYSTEM.
' Version : 1.1 - Initial draft. 
'-----------------------------------------------------------
Function RegExist(RegData)
    Dim objShell
    Set objShell = CreateObject("wscript.shell")
        objShell.RegRead RegData

        Select Case Err
            Case 0:
            RegExist = True

            Case Else
            RegExist = False
        End Select
End Function

Sub DeleteFromPath(inputReg)
    On Error Resume Next
    Dim objShell, strSystemPath, strUserPath, rCode, strNewPath
    Set objShell = CreateObject("wscript.shell")

    SystemPathKey = "HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment\Path"
    UserPathKey= "HKCU\Environment\Path"

    'Read the path
    strSystemPath = objShell.RegRead(SystemPathKey)
    strUserPath   = objShell.RegRead(UserPathKey)

    ' Replace the string
    Set RegularExpressionObject = New RegExp
    With RegularExpressionObject
    .Pattern = inputReg
    .IgnoreCase = True
    .Global = True
    End With
    replaced = RegularExpressionObject.Replace(strSystemPath,"")
    rCode = objShell.RegWrite(SystemPathKey, replaced, "REG_EXPAND_SZ")
    replaced = RegularExpressionObject.Replace(strUserPath,"")
    rCode = objShell.RegWrite(UserPathKey, replaced, "REG_EXPAND_SZ")
End Sub

Sub DeleteFromSystemPythonPath(inputReg)
    On Error Resume Next
    Dim objShell, strSystemPythonPath, rCode, strNewPath
    Set objShell = CreateObject("wscript.shell")

    SystemPythonPathKey = "HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment\PythonPath"


	
    'if RegExist(SystemPythonPathKey) = TRUE then
	
	    'Read the path
        strSystemPythonPath = objShell.RegRead(SystemPythonPathKey)
			
		' Replace the string
		Set RegularExpressionObject = New RegExp
		With RegularExpressionObject
		.Pattern = inputReg
		.IgnoreCase = True
		.Global = True
		End With
		replaced = RegularExpressionObject.Replace(strSystemPythonPath,"")
		rCode = objShell.RegWrite(SystemPythonPathKey, replaced, "REG_EXPAND_SZ")
	
    'end if
	
	
End Sub

Sub DeleteFromUserPythonPath(inputReg)
    On Error Resume Next
    Dim objShell, strUserPythonPath, rCode, strNewPath
    Set objShell = CreateObject("wscript.shell")

    UserPythonPathKey= "HKCU\Environment\PythonPath"

    'Read the path
    strUserPythonPath   = objShell.RegRead(UserPythonPathKey)
		
    ' Replace the string
    Set RegularExpressionObject = New RegExp
    With RegularExpressionObject
    .Pattern = inputReg
    .IgnoreCase = True
    .Global = True
    End With
    replaced = RegularExpressionObject.Replace(strUserPythonPath,"")
    rCode = objShell.RegWrite(UserPythonPathKey, replaced, "REG_EXPAND_SZ")
End Sub

Sub StartUninstall

Dim WshShell
Dim CurrDir
Set WshShell = WScript.CreateObject("WScript.Shell")

'WScript.Echo WshShell.CurrentDirectory
WshShell.currentdirectory = wscript.scriptfullname & "\.."
'WScript.Echo WshShell.currentdirectory

UninsFile = WshShell.CurrentDirectory & "\unins000.exe"
'WScript.Echo CurrDir
WshShell.Run   UninsFile  

End Sub


call DeleteFromPath("%DSM2_HOME%\\bin[;$]")
call DeleteFromPath("%VISTA_HOME%\\bin[;$]")
call DeleteFromUserPythonPath("%SCRIPTS_HOME%[;$]")
call DeleteFromSystemPythonPath("%SCRIPTS_HOME%[;$]")
