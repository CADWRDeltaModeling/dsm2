import os, os.path, subprocess, glob
import unittest
from vtools.data import *
from vtools.data.api import *
from vtools.datastore.dss.api import *
from vtools.functions.api import *
import numpy
import datetime
# DSM2 is tested at the executable (system) level using nose. These are some
# of the common functions that enable/help with that.
class DSM2BaseTestCase(unittest.TestCase):

    def location(self):
        print self
        raise NotImplementedError("location() must be implemented by subclass")

    def tolerance(self):
        """
        the tolerance in comparision of expected & actual values. override this
        method in your test case if you want to change this tolerance
        """
        return 3

    def get_rts(self, file, path):
        """
        Retrieves a regular time series from the given file and path reference

        The file selector is relative to the location as defined by location() method
        of this test class, e.g. data1/test.dss

        The pathname selector in similar to the dss pathname with * for wildcard matches, e.g. /*/CHAN3/FLOW//15MIN/*/
        """
        if ( not os.path.isabs(file)):
            loc = os.path.dirname(self.location())
            abs_file = os.path.join(loc,file)
        else:
            abs_file = file
        rts = dss_retrieve_ts(abs_file, path)
        return rts
    def get_timewindow_ignoring_start_days(self, rts, ignore_start_days=0):
        """
        Returns a timewindow for the given regular time series, ignoring
        the given number of days from the start
        """
        window = (rts.start+datetime.timedelta(ignore_start_days), rts.end)
        return window
    def get_rts_for(self, file, b_part, c_part):
        """
        Retrieves the time series for the given B & C part
        """
        path="/*/%s/%s//*/*/"%(b_part,c_part)
        rts = self.get_rts(file, path)
        return rts
    def assert_equals_rts(self, ts_expected, ts_actual, compare_func=norm_diff_linf, time_window=None):
        """
        asserts the expected matches actual within tolerance() limit
        """
        diff = numpy.abs(compare_func(ts_expected, ts_actual, time_window))
        self.assertAlmostEqual(0,diff,self.tolerance(),"Difference %f more than expected"%diff)
# end of class DSM2BaseTestCase
def cleanup(dir='.'):
    """
    cleansup the directory, looking for dss files & h5 files
    """
    patterns = ['*.ds?', '*.h5', '*.out', '*.?rf', '*_echo.inp']
    print 'Cleaning up directory %s'%dir
    if not os.path.exists(dir):
        os.makedirs(dir)
    for pattern in patterns:
        for file in glob.glob(dir + '/' + pattern):
            print 'Removing %s'%file
            os.remove(file)
def run_command(command):
    retcode = subprocess.call(command, shell=True)
    if retcode < 0:
        print >>sys.stderr, "%s was terminated by signal"%command, -retcode
    else:
        print >>sys.stderr, "%s completed successfully"%command, retcode
def build_dsm2_command(module='hydro'):
    return "%s/%s %s.inp"%(os.environ['EXE_DIR'], module, module)
def run_module(module='all'):
    if (module=='all'):
        run_command(build_dsm2_command('hydro'))
        run_command(build_dsm2_command('qual'))
    elif (module=='hydro' or module=='qual'):
        run_command(build_dsm2_command(module))
    else:
        print 'run_module expects all or hydro or qual as argument'
#
