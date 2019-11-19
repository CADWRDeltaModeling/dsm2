import os
import subprocess

VersionTemplate     = "      character*16 :: dsm2_version = '@{Version_GIT_DESCRIBE}', git_build = '@{Version_GIT}', git_uid = '@{GUID_GIT}'"
VersionFile_path    = os.path.split( __file__)[0]
VersionFile_path    = os.path.join(VersionFile_path,"version.fi")

VersionFile = open(VersionFile_path, "w")

try:
    p = subprocess.Popen("git describe", stdout=subprocess.PIPE, universal_newlines=True)
    GITVersion_Describe = p.communicate()[0].strip()

    p = subprocess.Popen("git log --oneline -- | wc -l", shell=True, stdout=subprocess.PIPE, universal_newlines=True)
    GITVersion_SourceCode = p.communicate()[0].strip()

    p = subprocess.Popen("git show -s --pretty=format:%h", stdout=subprocess.PIPE, universal_newlines=True)
    GITGUID_SourceCode = p.communicate()[0].strip()

    print(' GIT description of dsm2:     '+ GITVersion_Describe)
    print(' GIT version of dsm2:     '+ GITVersion_SourceCode)
    print(' GIT guid of dsm2:        '+ GITGUID_SourceCode)
    VersionTxt = VersionTemplate.replace("@{Version_GIT_DESCRIBE}", GITVersion_Describe)
    VersionTxt = VersionTxt.replace("@{Version_GIT}", GITVersion_SourceCode)
    VersionTxt = VersionTxt.replace("@{GUID_GIT}", GITGUID_SourceCode)
    VersionFile.write(VersionTxt)
    VersionFile.close()
except:
    VersionFile.close()
    os.remove(VersionFile_path)
    print('Abort.... possible error in file /dsm2/src/common/version_generate.py')
