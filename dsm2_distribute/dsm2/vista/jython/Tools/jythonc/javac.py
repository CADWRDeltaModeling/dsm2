# Copyright � Corporation for National Research Initiatives
import java, jarray, os, time, sys, thread
from java.lang import System

runtime = java.lang.Runtime.getRuntime()



def dumpStream(stream, txtarr):
    array = jarray.zeros( 1024, 'b' )
    while 1:
        len = stream.read(array)
        if len < 0:
            break
        txtarr.append(array[:len].tostring())



def findDefaultJavac():
    jhome = System.getProperty("java.home")
    if jhome is None:
        return None
    root, dir = os.path.split(jhome)
    if dir.lower() == "jre":
        jhome = root
    javac = os.path.join(os.path.join(jhome, "bin"), "javac")
    return javac

def getClasspath():
    cpath = System.getProperty("java.class.path")
    return cpath



import sys
def compile(files, javac=None, cpathopt="-classpath",
            cpath=None, options=None, sourcedir=None):
    cmd = []
    # Search order for a Java compiler:
    #   1. -C/--compiler command line option
    #   2. python.jythonc.compiler property (see registry)
    #   3. guess a path to javac
    if javac is None:
        javac = sys.registry.getProperty("python.jythonc.compiler")
    if javac is None:
        javac = findDefaultJavac()
    cmd.append(javac)
    # Extra options
    #   1. -J/--compileropts command line option (passed in options)
    #   2. python.jythonc.compileropts property
    if options is None:
        options = sys.registry.getProperty("python.jythonc.compileropts")
        if options:
            options = options.split()
    if options is None:
        options = []
    cmd.extend(options)
    # new:
    # Classpath:
    #   1. python.jythonc.classpath property
    #   +
    #   2. java.class.path property
    #   +
    #   3. sourcedir
    #   +
    #   4. sys.path
    if cpath is None:
        sep = java.io.File.pathSeparator
        cpath = []
        part = sys.registry.getProperty("python.jythonc.classpath")
        if part != None:
            cpath.extend(part.split(sep))
        part = getClasspath()
        if part != None:
            cpath.extend(part.split(sep))
        if sourcedir:
            cpath.append(sourcedir)
        cpath.extend(sys.path)
        cpath = sep.join(cpath)
    cmd.extend([cpathopt, cpath])
    cmd.extend(files)
    print 'Compiling with args:', cmd

    try:
        proc = runtime.exec(cmd)
    except IOError, e:
        msg = '''%s

Consider using the -C/--compiler command line switch, or setting
the property python.jythonc.compiler in the registry.''' % e
        return 1, '', msg
    done = None
    procout = []
    procerr = []
    thread.start_new_thread(dumpStream, (proc.inputStream, procout))
    thread.start_new_thread(dumpStream, (proc.errorStream, procerr))
    while not done:
        proc.waitFor()
	try:
	    proc.exitValue()
	    done = 1
	except java.lang.IllegalThreadStateException:
	    pass
    return (proc.exitValue(), "".join(procout), "".join(procerr))



if __name__ == '__main__':
    files = ["c:\\jpython\\tools\\jpythonc2\\test\\ButtonDemo.java",
            "c:\\jpython\\tools\\jpythonc2\\test\\pawt.java",]

    print compile(files)
    print compile(files, ["-foo", "bar"])
    print 'done'
