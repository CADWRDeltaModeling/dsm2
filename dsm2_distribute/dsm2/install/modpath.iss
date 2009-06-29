// Heavily modified from a script by Jared Breland that had the following
// lines:
// ----------------------------------------------------------------------------
//
// Inno Setup Ver:  5.2.1
// Script Version:  1.3.1
// Author:          Jared Breland <jbreland@legroom.net>
// Homepage:		http://www.legroom.net/software
//
// Script Function:
//	Enable modification of system path directly from Inno Setup installers
//
// Instructions:
//	Copy modpath.iss to the same directory as your setup script
//
//	Add this statement to your [Setup] section
//		ChangesEnvironment=yes
//
//	Add this statement to your [Tasks] section
//	You can change the Description or Flags, but the Name must be modifypath
//		Name: modifypath; Description: &Add application directory to your system path; Flags: unchecked
//
//	Add the following to the end of your [Code] section
//	setArrayLength must specify the total number of dirs to be added
//	Dir[0] contains first directory, Dir[1] contains second, etc.
//		function ModPathDir(): TArrayOfString;
//		var
//			Dir:	TArrayOfString;
//		begin
//			setArrayLength(Dir, 1)
//			Dir[0] := ExpandConstant('{app}');
//			Result := Dir;
//		end;
//		#include "modpath.iss"
// ----------------------------------------------------------------------------

procedure ModPath();
var
	oldpath:	String;
	newpath:	String;
    item:       String;
	pathArr:	TArrayOfString;
	aExecFile:	String;
	aExecArr:	TArrayOfString;
	i, d:		Integer;
	pathdir:	TArrayOfString;
    addable:    Boolean;
begin
    
    // Get the User path
    RegQueryStringValue(HKEY_CURRENT_USER,'Environment', 'path', oldpath);
    
    // Add a semi-colon because the way we iterate items on path
    // requires token at end
    oldpath := oldpath + ';';
    pathdir := ModPathDir();

    i := 0;
    while (Pos(';', oldpath) > 0) do begin
        SetArrayLength(pathArr, i+1);
        item := Copy(oldpath, 0, Pos(';', oldpath)-1);
        oldpath := Copy(oldpath, Pos(';', oldpath)+1, Length(oldpath));
        addable := Length(Item)>1
        if addable then begin
            for d := 0 to GetArrayLength(pathdir)-1 do begin
                if pathdir[d] = item then begin
                    addable := false
                    break;  // only get items not on list
                end;
            end;
        end;

        if addable then begin
            if newpath = '' then begin
                newpath := item;
            end else begin
                newpath := newpath + ';' + item;
            end;
        end;
    end;
        
    // we have filtered everything -- now prepend       
    if IsUninstaller() = false then begin
        for d := 0 to GetArrayLength(pathdir)-1 do begin
            // Prepend thing being searched to path if not already included
            newpath := pathdir[d]+ ';' +newpath;
        end;
    end;

    // Eliminate leading semi-colons  
    while (Pos(';', newpath) = 0) do begin    
        newpath := Copy(newpath,2, Length(newpath));
    end;
        
    // Write new path
    RegWriteStringValue(HKEY_CURRENT_USER,'Environment', 'path', newpath);


    // Write file to flag modifypath was selected
    //   Workaround since IsTaskSelected() cannot be called 
    //   at uninstall and AppName and AppId cannot be "read" in Code section
    if IsUninstaller() = false then begin
        SaveStringToFile(ExpandConstant('{app}') + '\uninsTasks.txt', WizardSelectedTasks(False), False);
    end;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
	if CurStep = ssPostInstall then begin
		if IsTaskSelected('modifypath') then begin
			ModPath();
        end;
    end;
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
var
	appdir:			String;
	selectedTasks:	String;
begin
	appdir := ExpandConstant('{app}')
	if CurUninstallStep = usUninstall then begin
		if LoadStringFromFile(appdir + '\uninsTasks.txt', selectedTasks) then
			if Pos('modifypath', selectedTasks) > 0 then
				ModPath();
		DeleteFile(appdir + '\uninsTasks.txt')
	end;
end;

function NeedRestart(): Boolean;
begin
	if IsTaskSelected('modifypath') and not UsingWinNT() then begin
		Result := True;
	end else begin
		Result := False;
	end;
end;

