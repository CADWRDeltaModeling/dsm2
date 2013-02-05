import unittest
from getmodel import *
from gov.ca.dsm2.input.parser import Parser

class TestGetModel(unittest.TestCase):
    ''' class TestGetModel(unittest.TestCase)
        This class is used to perform unittest for functions in getmodel.py file. 
    '''
    def setUp(self):
        echo_file1 = 'D:/workspace/vista-vdiff/scripts/compare_inp/hydro_echo_SC_ELT_MidRange.inp'
        echo_file2 = 'D:/workspace/vista-vdiff/scripts/compare_inp/hydro_echo_SC_ELT_SLR15_ROA25.inp'
        p = Parser()                                 
        tables1 = p.parseModel(echo_file1)
        tables2 = p.parseModel(echo_file2)
        table1 = tables1.getTableNamed('RESERVOIR_CONNECTION')
        table2 = tables2.getTableNamed('RESERVOIR_CONNECTION')
        a1 = GetModel(table1)
        a2 = GetModel(table2)
        self.a1key = a1.get_keys()
        self.block1=a1.get_block_by_key(str(self.a1key[0]))
        self.block2=a2.get_block_by_key(str(self.a1key[0]))
        res=CompareBlocks(self.block1,self.block2)
        self.chk = res.chk_len_block()
    
    def test_call(self):
    #     self.assertNotEqual(len(self.a1),0,' The returning table1 is empty!')
    #     self.assertNotEqual(len(self.a2),0,' The returning table2 is empty!')
         self.assertNotEqual(len(self.a1key),0,' The returning table2 is empty!') 
         self.assertNotEqual(len(self.block1),0,' The returning table2 is empty!')  
         
    #def test_metrics(self):
    #    self.assertNotEqual(len(self.ms),0,' The returning metric is empty! ')
        
    #def test_getxyz(self):
    #    hydro_echo = 'D:\delta\dsm2_v8\studies\Delta_Corridor_NT\output\hydro_echo.inp'
    #    gis_inp = 'D:\delta\dsm2_v8\studies\Delta_Corridor_NT\output\gis.inp'
    #    out_txt = 'D:\delta\dsm2_v8\studies\Delta_Corridor_NT\output\metric_xy.txt'
    #    chkarr = get_metric_xy(self.ms,hydro_echo,gis_inp,out_txt)
    #    self.assertNotEqual(len(chkarr),0,' The returning xy array is empty! ')
        
        
if __name__ == '__main__':
    unittest.main()