import os

print __file__

VersionTemplate     = "      character*16 :: dsm2_version = '8.1.0', svn_build = '@{Version_SVN}' " 
VersionFile_path    = os.path.split( __file__)[0]
VersionFile_path    = os.path.join(VersionFile_path,"version.fi")

VersionFile = open(VersionFile_path, "w")

try:
    (dummy, SVNVersion_SourceCode) = os.popen4("svnversion ../.. ")
    SVNVersion_SourceCode = SVNVersion_SourceCode.readlines()[0]
    SVNVersion_SourceCode = SVNVersion_SourceCode.strip()

    print ' SVN version of dsm2:        '+ SVNVersion_SourceCode
    VersionTxt = VersionTemplate.replace("@{Version_SVN}", SVNVersion_SourceCode)
    VersionFile.write(VersionTxt)
    VersionFile.close()
   

except:
    VersionFile.close()
    os.remove(VersionFile_path) 
    print 'Abort.... possible error in file /dsm2/src/common/version_generate.py'    
