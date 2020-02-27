import os
import subprocess

VersionTemplate     = "      character*16 :: dsm2_version = '@{Version_GIT_DESCRIBE}', git_build = '@{Version_GIT}', git_uid = '@{GUID_GIT}'"
VersionFile_path    = os.path.split( __file__)[0]
VersionFile_path    = os.path.join(VersionFile_path,"version.fi")

VersionFile = open(VersionFile_path, "w")

try:
#    (dummy, GITVersion_Describe) = os.popen4("git describe")
#    GITVersion_Describe = GITVersion_Describe.readlines()[0]
#    GITVersion_Describe = GITVersion_Describe.strip()

#    (dummy, GITVersion_SourceCode) = os.popen4("git log --oneline | wc -l")
#    GITVersion_SourceCode = GITVersion_SourceCode.readlines()[0]
#    GITVersion_SourceCode = GITVersion_SourceCode.strip()

#    (dummy, GITGUID_SourceCode) = os.popen4("git show -s --pretty=format:%h")
#    GITGUID_SourceCode = GITGUID_SourceCode.readlines()[0]
#    GITGUID_SourceCode = GITGUID_SourceCode.strip()

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
    VersionFile.write(VersionTxt)
    VersionFile.close()
except:
    VersionFile.close()
    os.remove(VersionFile_path)
    print('Abort.... possible error in file /dsm2/src/common/version_generate.py')
