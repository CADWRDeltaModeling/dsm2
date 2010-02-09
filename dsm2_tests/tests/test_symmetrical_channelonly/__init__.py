import os, os.path
import dsm2test
def setup():
    old_directory = os.getcwd()
    os.chdir(os.path.split(__file__)[0])
    os.environ['OUTPUTDIR']='test_output'
    print 'Setting up'
    dsm2test.cleanup(os.environ['OUTPUTDIR'])
    os.environ['DSM2MODIFIER']='lo_hi'
    os.environ['UP_NODE']='1'
    os.environ['DOWN_NODE']='7'
    dsm2test.run_module('all')
    os.environ['DSM2MODIFIER']='hi_lo'
    os.environ['UP_NODE']='7'
    os.environ['DOWN_NODE']='1'
    dsm2test.run_module('all')
    os.chdir(old_directory)
def teardown():
    if os.environ['TEARDOWN'] != 'false':
        old_directory = os.getcwd()
        os.chdir(os.path.split(__file__)[0])
        dsm2test.cleanup(os.environ['OUTPUTDIR'])
        os.chdir(old_directory)
    