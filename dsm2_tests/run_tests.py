import sys, os
sys.path.append('scripts')
#set executable directory that contains a hydro.exe and qual.exe here
os.environ['EXE_DIR']='d:/models/tests/exe/r8.1808'
# make this 'false' if you don't want to run the tear down scripts at the package level
os.environ['TEARDOWN']='true'
import nose
sys.argv=['dsm2_testing_nose','tests']
nose.run()
