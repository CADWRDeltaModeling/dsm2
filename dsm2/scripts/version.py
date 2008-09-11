##!d:/vista/bin/vscript.bat 

def version():
    f = open("version.txt",'r')
    version = version.readline()
    return version

if __name__ == "__main__":
    print version()




