import os, os.path
import dsm2test
def setup():
    if os.environ['SETUP'] != 'false':
        old_directory = os.getcwd()
        os.chdir(os.path.split(__file__)[0])
        os.environ['OUTPUTDIR']='output'
        print 'Setting up'
        dsm2test.cleanup(os.environ['OUTPUTDIR'])
        os.environ['DSM2MODIFIER']='lo_hi'
        os.environ['UP_NODE']='1'
        os.environ['DOWN_NODE']='7'
        os.environ['NEXT_TO_UP_NODE']='2'
        os.environ['NEXT_TO_DOWN_NODE']='5'
        dsm2test.run_module('all')
        os.environ['DSM2MODIFIER']='hi_lo'
        os.environ['UP_NODE']='7'
        os.environ['DOWN_NODE']='1'
        os.environ['NEXT_TO_UP_NODE']='5'
        os.environ['NEXT_TO_DOWN_NODE']='2'
        dsm2test.run_module('all')
        os.chdir(old_directory)
def teardown():
    if os.environ['TEARDOWN'] != 'false':
        old_directory = os.getcwd()
        os.chdir(os.path.split(__file__)[0])
        dsm2test.cleanup(os.environ['OUTPUTDIR'])
        os.chdir(old_directory)
