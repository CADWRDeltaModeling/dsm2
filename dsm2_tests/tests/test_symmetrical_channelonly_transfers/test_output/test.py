import unittest
import datetime
import dsm2test

class TestSimulationCompare(dsm2test.DSM2BaseTestCase):

    def location(self):
        return __file__

    def get_file_for_run1(self):
        return "lo_hi_out.dss"

    def get_file_for_run2(self):
        return "hi_lo_out.dss"

    def test_flow(self):
        b_parts=["BND_6","CHAN4","BND_1"]
        for b_part in b_parts:
            rts1 = self.get_rts_for(self.get_file_for_run1(), b_part, "FLOW")
            rts2 = self.get_rts_for(self.get_file_for_run2(), b_part, "FLOW")
            window = self.get_timewindow_ignoring_start_days(rts1, 10)
            self.assert_equals_rts(rts1, -rts2, time_window=window)
    def test_stage(self):
        rts1 = self.get_rts_for(self.get_file_for_run1(), "BND_6", "STAGE")
        rts2 = self.get_rts(self.get_file_for_run2(), "BND_1", "STAGE")
        window = self.get_timewindow_ignoring_start_days(rts1, 10)
        self.assert_equals_rts(rts1, rts2, time_window=window)
    def test_ec(self):
        b_parts=["BND_6","CHAN4","BND_1"]
        for b_part in b_parts:
            rts1 = self.get_rts_for(self.get_file_for_run1(), b_part, "EC")
            rts2 = self.get_rts_for(self.get_file_for_run2(), b_part, "EC")
            window = self.get_timewindow_ignoring_start_days(rts1, 10)
            self.assert_equals_rts(rts1, rts2, time_window=window)
        
if __name__ == '__main__':
    unittest.main()

    
