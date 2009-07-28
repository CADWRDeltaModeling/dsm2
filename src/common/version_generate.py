import os


VersionTemplate     = "      character*16 :: dsm2_version = '8.0b2', svn_build = '@{Version_SVN}' " 
VersionFile_path    = "./version.inc"





VersionFile = open(VersionFile_path, "w") 

try:
    (dummy, SVNVersion_SourceCode) = os.popen4("svnversion ../../../dsm2 ")
    SVNVersion_SourceCode = SVNVersion_SourceCode.readlines()[0]
    SVNVersion_SourceCode = SVNVersion_SourceCode.strip()

    print ' SVN version of ../../../dsm2:        '+ SVNVersion_SourceCode
    VersionTxt = VersionTemplate.replace("@{Version_SVN}", SVNVersion_SourceCode)
    VersionFile.write(VersionTxt)
    VersionFile.close()
   

except:
    VersionFile.close()
    os.remove(VersionFile_path) 
    print 'Abort.... possible error in file /delta/models/dsm2/src/common/verion_generate.py'    
