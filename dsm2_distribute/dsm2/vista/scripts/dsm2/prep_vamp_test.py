import sys
import calendar
import prep_vamp
sys.version_info = (3,3)
import unittest
from vista.set import RegularTimeSeries
from vtimeseries import interpolate, timewindow

unittest.False=0
unittest.True=1

class VampTestCase(unittest.TestCase):
  def setUp(self):
    self.ts_1 = RegularTimeSeries("///////","31MAR1980 2400",
                                         "1MON",[6000,8000,4000,2000])
    self.ts_2 = RegularTimeSeries("///////","31MAR1980 2400",
                                         "1MON",[10000,10000,10000,10000])
    self.ts_3 = RegularTimeSeries("///////","31MAR1980 2400",
                                         "1MON",[1000,1000,1000,1000])

    self.list_1 = [6000 for i in range(31)] 
    self.list_2 = [8000 for i in range(31)] 
    self.list_3 = [10000 for i in range(31)] 
    self.list_4 = [4000 for i in range(31)]
    self.list_5 = [2000 for i in range(31)]
    self.list_6 = [1000 for i in range(31)]    

    
    
  def tearDown(self):
    self.ts_1 = None
    self.ts_2 = None    
    self.ts_3 = None
    self.ts_4 = None

    self.list_1 = None
    self.list_2 = None
    self.list_3 = None
    self.list_4 = None
    self.list_5 = None
    self.list_6 = None
    

    
  # test monthly time series input with pulse flow greater than non-pulse flow and no shoulder
  def test_replace_vamp_pulse_above_no_shoulder(self):
    
    print ""
    print ""
    print "test_place_vamp_pulse_above_no_shoulder..."
  
    self.list_1.extend(self.list_2[:14])
    self.list_1.extend(self.list_3)
    self.list_1.extend(self.list_4[:16])
    self.list_1.extend(self.list_5[:30])

    out_array = prep_vamp.replace_vamp(self.ts_1, self.ts_2).getYArray()    
    for i in range(0, len(self.list_1)):
      self.assertEqual(self.list_1[i], round(out_array[i], 0)), 'replace_vamp function returned wrong data'
      
  # test monthly time series input with pulse flow greater than non-pulse flow and shoulders
  def test_replace_vamp_pulse_above_shoulder(self):
    print ""
    print ""
    print "test_replace_vamp_pulse_above_shoulder..."
    
    self.list_1.extend(self.list_3[:30])
    self.list_1.extend(self.list_3)
    self.list_1.extend(self.list_5[:30])

    out_array = prep_vamp.replace_vamp(self.ts_1, self.ts_2, 1).getYArray() 
    
    for i in range(0, len(self.list_1)):
      self.assertEqual(self.list_1[i], round(out_array[i], 0)), 'replace_vamp function returned wrong data'
   
  # test monthly time series input with pulse flow greater than non-pulse flow and no shoulder
  def test_replace_vamp_pulse_below_no_shoulder(self):
    print ""
    print ""
    print "test_replace_vamp_pulse_below_no_shoulder..."
    
    self.list_1.extend(self.list_2[:14])
    self.list_1.extend(self.list_6)
    self.list_1.extend(self.list_4[:16])
    self.list_1.extend(self.list_5[:30])

    out_array = prep_vamp.replace_vamp(self.ts_1, self.ts_3).getYArray()    
    for i in range(0, len(self.list_1)):
      self.assertEqual(self.list_1[i], round(out_array[i], 0)), 'replace_vamp function returned wrong data'

  # test monthly time series input with pulse flow greater than non-pulse flow and no shoulder
  def test_replace_vamp_pulse_below_shoulder(self):
    print ""
    print ""
    print "test_replace_vamp_pulse_below_shoulder..."

    self.list_1.extend(self.list_6[:30])
    self.list_1.extend(self.list_6)
    self.list_1.extend(self.list_5[:30])

    out_array = prep_vamp.replace_vamp(self.ts_1, self.ts_3, 1).getYArray()    
    for i in range(0, len(self.list_1)):
      self.assertEqual(self.list_1[i], round(out_array[i], 0)), 'replace_vamp function returned wrong data'
            
  # test monthly time series input with pulse flow greater than non-pulse flow and no shoulder
  def test_replace_vamp_pulse_same_no_shoulder(self):
  
    print ""
    print ""
    print "test_replace_vamp_pulse_same_no_shoulder..."  	
  

    self.list_1.extend(self.list_2[:30])
    self.list_1.extend(self.list_4)
    self.list_1.extend(self.list_5[:30])

    out_array = prep_vamp.replace_vamp(self.ts_1, self.ts_1).getYArray()    
    for i in range(0, len(self.list_1)):
      self.assertEqual(self.list_1[i], round(out_array[i], 0)), 'replace_vamp function returned wrong data'
              
            
  # test monthly time series input with pulse flow greater than non-pulse flow and no shoulder
  def test_replace_vamp_pulse_same_shoulder(self):
  
    print ""
    print ""
    print "test_replace_vamp_pulse_same_shoulder..."
  
    self.list_1.extend(self.list_2[:30])
    self.list_1.extend(self.list_4)
    self.list_1.extend(self.list_5[:30])

    out_array = prep_vamp.replace_vamp(self.ts_1, self.ts_1, 1).getYArray()    
    for i in range(0, len(self.list_1)):
      self.assertEqual(self.list_1[i], round(out_array[i], 0)), 'replace_vamp function returned wrong data'
      
  # 
  def test_calculate_exports_average_above_limit(self):
  
    print ""
    print ""
    print "test_calculate_exports_average_above_limit..."
    
    pulse_limit = RegularTimeSeries("///////","31MAR1980 2400",
                                          "1MON",[6000,4000,2000,8000])
    average_export = RegularTimeSeries("///////","31MAR1980 2400",
                                          "1MON",[5000,5000,5000,5000])
                                          
 
    export_limit = [5000 for i in range(0,31)]
    export_limit.extend([6143 for i in range(14)])
    export_limit.extend([4000 for i in range(16)])
    export_limit.extend([2000 for i in range(15)])
    export_limit.extend([7813 for i in range(16)])
    export_limit.extend([5000 for i in range(30)])
  
    export_limit_from_function = prep_vamp.calculate_exports(pulse_limit, average_export)
    export_limit_array = export_limit_from_function.getYArray()
    for i in range(0, len(export_limit_array)):
      self.assertEqual(export_limit[i], round(export_limit_array[i], 0)), 'calculate_exports function returned wrong value'
      
  # 
  def test_calculate_exports_average_below_limit(self):
  
    print ""
    print ""
    print "test_calculate_exports_average_below_limit..."
    
    pulse_limit = RegularTimeSeries("///////","31MAR1980 2400",
                                          "1MON",[6000,4000,2000,8000])
    average_export = RegularTimeSeries("///////","31MAR1980 2400",
                                          "1MON",[3000,3000,3000,3000])
                                          
 
    export_limit = [3000 for i in range(0,31)]
    export_limit.extend([3000 for i in range(14)])
    export_limit.extend([3000 for i in range(16)])
    export_limit.extend([2000 for i in range(15)])
    export_limit.extend([3938 for i in range(16)])
    export_limit.extend([3000 for i in range(30)])
  
    export_limit_from_function = prep_vamp.calculate_exports(pulse_limit, average_export)
    export_limit_array = export_limit_from_function.getYArray()
    for i in range(0, len(export_limit_array)):
      self.assertEqual(export_limit[i], round(export_limit_array[i], 0)), 'calculate_exports function returned wrong value'
                   
  # 
  def test_project_export_only_apr_may(self):
  
    print ""
    print ""
    print "test_project_export_only_apr_may..."
    pulse_limit = RegularTimeSeries("///////","31MAR1980 2400",
                                         "1MON",[0,2500,2000,0])
    delta_inflow = RegularTimeSeries("///////","31MAR1980 2400",
                                         "1MON",[20000,10000,8000,8000])
    ei_ratio = RegularTimeSeries("///////","31MAR1980 2400",
                                         "1MON",[0.35,0.35,0.35,0.35])
    #delta_inflow*ei_ratio = [7000, 3500, 2800, 2800]

    cvp_out = [3500, 1250, 1000, 1400]
    swp_out = [3500, 1250, 1000, 1400]
 
    swp_out_from_function, cvp_out_from_function = prep_vamp.project_export_limits(pulse_limit, ei_ratio, delta_inflow)
    swp_array = swp_out_from_function.getYArray()
    cvp_array = cvp_out_from_function.getYArray()
    for i in range(0, len(cvp_array)):
      self.assertEqual(cvp_out[i], round(cvp_array[i], 0)), 'project_export function returned wrong cvp_limit value'
      self.assertEqual(swp_out[i], round(swp_array[i], 0)), 'project_export function returned wrong swp_limit value'
      
  # 
  def test_project_export(self):
  
    print ""
    print ""
    print "test_project_export..."
    
    pulse_limit = RegularTimeSeries("///////","31MAR1980 2400",
                                             "1MON",[2500,2500,2000,2000])
    delta_inflow = RegularTimeSeries("///////","31MAR1980 2400",
                                             "1MON",[6000,8000,2286,4000])
    ei_ratio = RegularTimeSeries("///////","31MAR1980 2400",
                                             "1MON",[0.35,0.35,0.35,0.35])
    #delta_inflow*ei_ratio = [2100, 2800, 800, 1400]
  
    cvp_out = [1050, 1250, 800, 800]
    swp_out = [1050, 1250, 0, 600]
    

    swp_out_from_function, cvp_out_from_function = prep_vamp.project_export_limits(pulse_limit, ei_ratio, delta_inflow)
    swp_array = swp_out_from_function.getYArray()
    cvp_array = cvp_out_from_function.getYArray()
    for i in range(0, len(cvp_array)):
      self.assertEqual(cvp_out[i], round(cvp_array[i], 0)), 'project_export function returned wrong cvp_limit value'
      self.assertEqual(swp_out[i], round(swp_array[i], 0)), 'project_export function returned wrong swp_limit value'

  def read_spreadsheet(self, file):
    exports_pre_vamp = []
    exports_vamp_apr = []
    exports_vamp_may = []
    exports_post_vamp = []
    start_year = None
    end_year = None

    lines = file.readlines()
    
    start_year = int(lines[3].split()[0])
    end_year = int(lines[len(lines)-1].split()[0])
    
    for line in lines[3:]:
      items = line.split()
      pre_vamp = int(items[1])
      vamp_apr = int(items[2])
      vamp_may = int(items[3])
      post_vamp = int(items[4])
      exports_pre_vamp.append(pre_vamp)
      exports_vamp_apr.append(vamp_apr)
      exports_vamp_may.append(vamp_may)
      exports_post_vamp.append(post_vamp)
      
    return start_year, end_year, exports_pre_vamp, exports_vamp_apr, exports_vamp_may, exports_post_vamp

  # compare output with spreadsheet
  def test_compare_with_spreadsheet(self):
  
    print ""
    print ""
    print "test_compare_with_spreadsheet..."
    
    cvp_file_name = "cvp_exports.txt"
    swp_file_name = "swp_exports.txt"
    fpart = "2020D09D"
    calsimfile = "2020D09DDV.dss"
  
    #read in spreadsheet data    
    f_cvp = open(cvp_file_name)
    cvp_start_year_ss, cvp_end_year_ss, cvp_pre_vamp_ss, cvp_vamp_apr_ss, cvp_vamp_may_ss, \
    		cvp_post_vamp_ss = self.read_spreadsheet(f_cvp)
    f_swp = open(swp_file_name)
    swp_start_year_ss, swp_end_year_ss, swp_pre_vamp_ss, swp_vamp_apr_ss, swp_vamp_may_ss, \
    		swp_post_vamp_ss = self.read_spreadsheet(f_swp)
    if cvp_start_year_ss != swp_start_year_ss:
      raise ValueError("cvp and swp exports have different start time from the spreadsheet.")
    
    #read in dss data and do the calculation using the script    
    path="/CALSIM/D419/FLOW-DELIVERY//1MON/fpart/".replace("fpart",fpart)
    swp_average_exports=prep_vamp.dss_retrieve_ts(calsimfile,path)
    path="/CALSIM/D418/FLOW-DELIVERY//1MON/fpart/".replace("fpart",fpart)
    cvp_average_exports=prep_vamp.dss_retrieve_ts(calsimfile,path)
    path="/CALSIM/EXPRATIO_/EI-RATIO-STD//1MON/fpart/".replace("fpart",fpart)
    ei_ratio=prep_vamp.dss_retrieve_ts(calsimfile,path)
    path="/CALSIM/DINFLOW/INFLOW-PULSE//1MON/fpart/".replace("fpart",fpart)
    delta_inflow=prep_vamp.dss_retrieve_ts(calsimfile,path)
    path="/CALSIM/PULSEVAMPEXP/EXPORT//1MON/fpart/".replace("fpart",fpart)
    total_export_limit=prep_vamp.dss_retrieve_ts(calsimfile,path)
   
    swp_limit,cvp_limit=prep_vamp.project_export_limits(
                    total_export_limit,ei_ratio,delta_inflow)
    swp_from_script=prep_vamp.calculate_exports(swp_limit,swp_average_exports)
    cvp_from_script=prep_vamp.calculate_exports(cvp_limit,cvp_average_exports)
    
    tsmonth_cvp=prep_vamp.month_numbers(cvp_from_script)
    tsmonth_swp=prep_vamp.month_numbers(swp_from_script)
    is_april_cvp=(tsmonth_cvp==4)
    is_april_swp=(tsmonth_swp==4)
    is_may_cvp=(tsmonth_cvp==5)
    is_may_swp=(tsmonth_swp==5)
    cvp_apr_script = is_april_cvp*cvp_from_script
    swp_apr_script = is_april_swp*swp_from_script
    cvp_may_script = is_may_cvp*cvp_from_script
    swp_may_script = is_may_swp*swp_from_script
    
    
    cvp_start_year_script = int(cvp_apr_script.getStartTime().toString()[5:9])
    swp_start_year_script = int(swp_apr_script.getStartTime().toString()[5:9])
    if cvp_start_year_script != swp_start_year_script:
      raise ValueError("cvp and swp exports have different start time from the script.")
    if cvp_start_year_ss < cvp_start_year_script:
      raise ValueError("start time of cvp exports from the spreadsheet is ahead ofthe start time from the script.")   
    cvp_array_apr_script = cvp_apr_script.getYArray()
    cvp_array_may_script = cvp_may_script.getYArray()
    swp_array_apr_script = swp_apr_script.getYArray()
    swp_array_may_script = swp_may_script.getYArray()
    
    # get rid of days that are not in the spreadsheet (1920,1921) 
    # also get rid of days that is not Apr or May
    n = 0
    cvp_start_year_script_mod = cvp_start_year_script
    while cvp_start_year_script_mod < cvp_start_year_ss:
      if calendar.isleap(cvp_start_year_script_mod):
        n += 366
      else:
        n += 365
      cvp_start_year_script_mod += 1
    #print "cvp_start_year_script_mod:%s, n:%s" % (cvp_start_year_script_mod, n)
    
    cvp_pre_vamp_script, cvp_vamp_apr_script = self.get_vamp(cvp_array_apr_script) 
    cvp_vamp_may_script, cvp_post_vamp_script = self.get_vamp(cvp_array_may_script, 1) 
    swp_pre_vamp_script, swp_vamp_apr_script = self.get_vamp(swp_array_apr_script) 
    swp_vamp_may_script, swp_post_vamp_script = self.get_vamp(swp_array_may_script, 1) 
    
    f_out = open("spcript_out.txt", "w")
    
    f_out.write("Tracy Exports (cfs)\n")
    f_out.write(" \tpre_vamp\t \tvamp_apr\t \tvamp_may\t \tpost_vamp\n")
    f_out.write("year\tspreadsheet\tscript\tspreadsheet\tscript\tspreadsheet\tscript\tspreadsheet\tscript\n")
    year = cvp_start_year_ss
    for i in range(len(cvp_pre_vamp_script)):
    
      f_out.write("%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\n" % (year,cvp_pre_vamp_ss[i],cvp_pre_vamp_script[i],\
        		cvp_vamp_apr_ss[i],cvp_vamp_apr_script[i],cvp_vamp_may_ss[i],cvp_vamp_may_script[i], \
    		cvp_post_vamp_ss[i],cvp_post_vamp_script[i]))  
      year += 1
    f_out.write("\n")
    f_out.write("Banks Exports (cfs)\n")
    f_out.write(" \tpre_vamp\t \tvamp_apr\t \tvamp_may\t \tpost_vamp\n")
    f_out.write("year\tspreadsheet\tscript\tspreadsheet\tscript\tspreadsheet\tscript\tspreadsheet\tscript\n")
    year = cvp_start_year_ss
    for i in range(len(cvp_pre_vamp_script)):
      f_out.write("%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\n" % (year,swp_pre_vamp_ss[i],swp_pre_vamp_script[i],\
    		swp_vamp_apr_ss[i],swp_vamp_apr_script[i],swp_vamp_may_ss[i],swp_vamp_may_script[i], \
    		swp_post_vamp_ss[i],swp_post_vamp_script[i])) 
      year += 1
    f_out.close()


    for i in range(len(cvp_pre_vamp_ss)):
      self.assertEqual(round(cvp_pre_vamp_ss[i], 0), round(cvp_pre_vamp_script[i], 0)), \
      					'cvp pre_vamp export from script is different from spreadsheet'
      self.assertEqual(round(cvp_vamp_apr_ss[i], 0), round(cvp_vamp_apr_script[i], 0)), \
      						'cvp April vamp export from script is different from spreadsheet'
      self.assertEqual(round(cvp_vamp_may_ss[i], 0), round(cvp_vamp_may_script[i], 0)), \
      						'cvp May vamp export from script is different from spreadsheet'
      self.assertEqual(round(cvp_post_vamp_ss[i], 0), round(cvp_post_vamp_script[i], 0)), \
      						'cvp post vamp export from script is different from spreadsheet'
      self.assertEqual(round(swp_pre_vamp_ss[i], 0), round(swp_pre_vamp_script[i], 0)), \
      					'swp pre_vamp export from script is different from spreadsheet'
      self.assertEqual(round(swp_vamp_apr_ss[i], 0), round(swp_vamp_apr_script[i], 0)), \
      						'swp April vamp export from script is different from spreadsheet'
      self.assertEqual(round(swp_vamp_may_ss[i], 0), round(swp_vamp_may_script[i], 0)), \
      						'swp May vamp export from script is different from spreadsheet'
      self.assertEqual(round(swp_post_vamp_ss[i], 0), round(swp_post_vamp_script[i], 0)), \
      						'swp post vamp export from script is different from spreadsheet'
    
  def get_vamp(self, exports, is_may=0):
    exports_first_half = []
    exports_second_half = []
    exports_no_zeros = []
    i = 1
    last_day = 30
    mid_day = 14
    if is_may:
      last_day = 31
      mid_day = 15
    
    
    #get rid of zeros
    for export in exports:
      if export > 0:
        exports_no_zeros.append(export)
              
    pre_val = 0
    for export in exports_no_zeros:
      if (i != 1) and (i != (mid_day+1)) :
        if round(pre_val,0) != round(export,0):
          raise ValueError("the value should not be change during this period. previous value: (%d), current value: (%d)"\
          									% (pre_val, export)) 
      
      	if i == mid_day:
          exports_first_half.append(export)
        elif i == last_day:
          exports_second_half.append(export)
          i = 0
      i += 1
      pre_val = export
    return exports_first_half, exports_second_half
        
runner = unittest.TextTestRunner()
runner.run(VampTestCase("test_replace_vamp_pulse_above_no_shoulder"))
runner.run(VampTestCase("test_replace_vamp_pulse_above_shoulder"))
runner.run(VampTestCase("test_replace_vamp_pulse_below_no_shoulder"))
runner.run(VampTestCase("test_replace_vamp_pulse_below_shoulder"))
runner.run(VampTestCase("test_replace_vamp_pulse_same_no_shoulder"))
runner.run(VampTestCase("test_replace_vamp_pulse_same_shoulder"))
runner.run(VampTestCase("test_calculate_exports_average_above_limit"))
runner.run(VampTestCase("test_calculate_exports_average_below_limit"))
runner.run(VampTestCase("test_project_export_only_apr_may"))
runner.run(VampTestCase("test_compare_with_spreadsheet"))
runner.run(VampTestCase("test_project_export"))

sys.exit()
