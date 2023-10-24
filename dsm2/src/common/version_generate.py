import os
import subprocess

VersionTemplate     = "      character*16 :: dsm2_version = '@{Version_GIT_DESCRIBE}', git_build = '@{Version_GIT}', git_uid = '@{GUID_GIT}'"
VersionFile_path    = "dsm2_version.f90"

try:
    p = subprocess.Popen("git describe", shell=True, stdout=subprocess.PIPE, universal_newlines=True)
    GITVersion_Describe = p.communicate()[0].strip()

    p = subprocess.Popen("git log --oneline -- | wc -l", shell=True, stdout=subprocess.PIPE, universal_newlines=True)
    GITVersion_SourceCode = p.communicate()[0].strip()

    p = subprocess.Popen("git show -s --pretty=format:%h", shell=True, stdout=subprocess.PIPE, universal_newlines=True)
    GITGUID_SourceCode = p.communicate()[0].strip()

    print(' GIT description of dsm2:     '+ GITVersion_Describe)
    print(' GIT version of dsm2:     '+ GITVersion_SourceCode)
    print(' GIT guid of dsm2:        '+ GITGUID_SourceCode)
    VersionTxt = VersionTemplate.replace("@{Version_GIT_DESCRIBE}", GITVersion_Describe)
    VersionTxt = VersionTxt.replace("@{Version_GIT}", GITVersion_SourceCode)
    VersionTxt = VersionTxt.replace("@{GUID_GIT}", GITGUID_SourceCode)
    if os.path.exists(VersionFile_path):
        with open(VersionFile_path, "r") as f:
            version_changed = False
            VersionFile_old = f.readline()
            if VersionFile_old == VersionTxt:
                print('No version changes.')
            else:
                print('The version has changed.')
                version_changed = True
        if version_changed:
            with open(VersionFile_path, "w") as f:
                f.write(VersionTxt)
    else:
        print('Create a new version file.')
        with open(VersionFile_path, "w") as f:
            f.write(VersionTxt)
except:
    if os.path.exists(VersionFile_path):
        os.remove(VersionFile_path)
    print('Abort.... possible error in file /dsm2/src/common/version_generate.py')
