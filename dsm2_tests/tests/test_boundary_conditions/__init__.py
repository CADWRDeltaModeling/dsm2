import os, os.path
import dsm2test
def setup():
    if os.environ['SETUP'] != 'false':
        old_directory = os.getcwd()
        os.chdir(os.path.split(__file__)[0])
        os.environ['OUTPUTDIR']='output'
        os.environ['BC_CASE_NUMBER']='1'
        os.environ['NODE2_BC']='200'
        os.environ['NODE6_BC']='-200'
        os.environ['QUAL_BC_NODE']='node2'
        os.environ['QUAL_BC_NODE_NUMBER']='2'
        dsm2test.cleanup(os.environ['OUTPUTDIR'])
        dsm2test.run_module('all')
        os.environ['BC_CASE_NUMBER']='2'
        os.environ['NODE2_BC']='-200'
        os.environ['NODE6_BC']='200'
        os.environ['QUAL_BC_NODE']='node6'
        os.environ['QUAL_BC_NODE_NUMBER']='6'
        dsm2test.run_module('all')
        os.chdir(old_directory)
def teardown():
    if os.environ['TEARDOWN'] != 'false':
        old_directory = os.getcwd()
        os.chdir(os.path.split(__file__)[0])
        dsm2test.cleanup(os.environ['OUTPUTDIR'])
        os.chdir(old_directory)
    