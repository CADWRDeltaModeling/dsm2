import os

print __file__

VersionTemplate     = "      character*16 :: dsm2_version = '8.1.3', git_build = '@{Version_GIT}', git_uid = '@{GUID_GIT}'"
VersionFile_path    = os.path.split( __file__)[0]
VersionFile_path    = os.path.join(VersionFile_path,"version.fi")

VersionFile = open(VersionFile_path, "w")

try:
    (dummy, GITVersion_SourceCode) = os.popen4("git log --oneline | wc -l")
    GITVersion_SourceCode = GITVersion_SourceCode.readlines()[0]
    GITVersion_SourceCode = GITVersion_SourceCode.strip()

    (dummy, GITGUID_SourceCode) = os.popen4("git show -s --pretty=format:%h")
    GITGUID_SourceCode = GITGUID_SourceCode.readlines()[0]
    GITGUID_SourceCode = GITGUID_SourceCode.strip()

    print ' GIT version of dsm2:     '+ GITVersion_SourceCode
    print ' GIT guid of dsm2:        '+ GITGUID_SourceCode
    VersionTxt = VersionTemplate.replace("@{Version_GIT}", GITVersion_SourceCode)
    VersionTxt = VersionTxt.replace("@{GUID_GIT}", GITGUID_SourceCode)
    VersionFile.write(VersionTxt)
    VersionFile.close()
except:
    VersionFile.close()
    os.remove(VersionFile_path)
    print 'Abort.... possible error in file /dsm2/src/common/version_generate.py'
