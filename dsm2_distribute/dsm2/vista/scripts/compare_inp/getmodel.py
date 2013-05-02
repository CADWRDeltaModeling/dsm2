"""
    Copyright (C) 2010 State of California, Department of 
    Water Resources.

    Comparison Tool: add-ons to VISTA 
    Version 1.0beta
    by En-Ching Hsu
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA 95814
    (916)-653-7552
    ehsu@water.ca.gov

    Send bug reports to ehsu@water.ca.gov

    This program is licensed to you under the terms of the GNU General
    Public License, version 2, as published by the Free Software
    Foundation.

    You should have received a copy of the GNU General Public License
    along with this program; if not, contact Dr. Francis Chung, below,
    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
    02139, USA.

    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
    DAMAGE.

    For more information about VISTA and Comparison Tool, contact:

    Dr. Francis Chung
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA  95814
    916-653-5601
    chung@water.ca.gov

    or see our home page: http://wwwdelmod.water.ca.gov/

    Send bug reports to ehsu@water.ca.gov or call (916) 654-8540

"""
from vutils import *

class GetModel:
    def __init__(self,table):
        self.data = table.getValues()
        
    def get_keys(self):
        col = self.get_col0()
        output_arr = []
        for x in col:
            if x not in output_arr:
                 output_arr.append(x)
        output_arr.sort()
        return output_arr
    
    def get_col0(self):
        try:
            return [int(row[0].encode('ascii')) for row in self.data]
        except:
            return [row[0].encode('ascii') for row in self.data]
        
    def get_block_by_key(self,inkey):
        # inkey is a string
        x = []
        y = ['','x[0]','[x[0],x[1]]','[x[0],x[1],x[2]]','[x[0],x[1],x[2],x[3]]','[x[0],x[1],x[2],x[3],x[4]]','[x[0],x[1],x[2],x[3],x[4],x[5]]','[x[0],x[1],x[2],x[3],x[4],x[5],x[6]]','[x[0],x[1],x[2],x[3],x[4],x[5],x[6],x[7]]','[x[0],x[1],x[2],x[3],x[4],x[5],x[6],x[7],x[8]]','[x[0],x[1],x[2],x[3],x[4],x[5],x[6],x[7],x[8],x[9]]']
        for ds in self.data:
            ncol = len(ds)
            if ds[0]==inkey:
                x.append(ds)
        x.sort(key=lambda x: y[ncol])
        return x

class CompareTables:
    def __init__(self,table1,table2,modifier1,modifier2):
        self.countdiff = 0
        self.countall = 0
        self.str_diff = ""
        self.str_all = ""
        self.remember_diff = 0
        self.notes = 0
        self.row_limit = 0
        p1 = GetModel(table1)
        p2 = GetModel(table2)
        self.p1_keys = p1.get_keys()
        self.p2_keys = p2.get_keys()
        merged_keys =self.merge_keys(p1.get_keys())
                                    # don't put self.p1_keys otherwise p1 will be overwritten
        self.get_extra_key = self.extra_key() 
        for pp1 in merged_keys:
            block1 = p1.get_block_by_key(str(pp1))
            block2 = p2.get_block_by_key(str(pp1)) 
            a = CompareBlocks(block1,block2,modifier1,modifier2,self.remember_diff,self.notes)
            diff, all = a.go()
            if self.countall <7000 and self.countall+a.get_max_row()>7000: 
                # to avoid the problem that rowspan exceeds the html limit
                self.row_limit = self.countall+a.get_max_row()
                self.notes = 1
            else:
                self.notes = 0
            if diff!="":
                self.remember_diff += 1
                self.countdiff += a.get_max_row()
                self.countall += a.get_max_row()
                self.str_diff += diff
                self.str_all += all
            else:
                self.countall += a.get_max_row()
                self.str_all += all
    def merge_keys(self,output_arr):
        for x in self.p2_keys:
            if x not in self.p1_keys:
                 output_arr.append(x)
        output_arr.sort()      
        return output_arr
    def extra_key(self):
        # return additional records in table 1 and table 2 respectively
        self.extra1 = []
        self.extra2 = []
        for x in self.p2_keys:
            if x not in self.p1_keys:
                 self.extra2.append(x)
        for y in self.p1_keys:
            if y not in self.p2_keys:
                 self.extra1.append(y)  
        return self.extra1,self.extra2
    def get_modified_block(self):
        return self.remember_diff
    def get_rows_diff(self):
        return self.countdiff
    def get_rows_all(self):
        return self.countall
    def get_str_diff(self):
        return self.str_diff
    def get_str_all(self):
        return self.str_all
    def get_limit_rowspan(self):
        return self.row_limit

class CompareBlocks:
    def __init__(self,block1,block2,modifier1,modifier2,remember_diff,notes):
        self.block1 = block1
        self.block2 = block2
        self.modifier1 = modifier1
        self.modifier2 = modifier2
        self.remember_diff = remember_diff
        self.notes = notes
    def go(self):
        ind = 1
        cnt = 1  #to fix the problem of rowspan
        if self.chk_len_block()==0:
            ind = 0
        if self.compare_rows()==0:
            ind = 0
        nrows = self.get_max_row()
        ncols = self.get_num_col()
        dtype = self.get_diff_type()
        diff=""
        all=""
        if ind == 0: # not identical block
            for i in range(nrows):
                if dtype==3:
                    diff += '<tr class="diff1">'
                else:
                    diff += '<tr>'
                for j in range(ncols):
                    try:  
                        if dtype==1:
                            diff += '<td class="add1">'+self.block1[i][j].replace(self.modifier1,"${DSM2MODIFIER}")+"</td>"
                        else:
                            diff += '<td>'+self.block1[i][j].replace(self.modifier1,"${DSM2MODIFIER}")+"</td>"
                    except:
                        diff += "<td></td>"
                for j in range(ncols):
                    try:
                        if dtype==2:
                            diff += '<td class="add2">'+self.block2[i][j].replace(self.modifier2,"${DSM2MODIFIER}")+"</td>"
                        else:
                            diff += '<td>'+self.block2[i][j].replace(self.modifier2,"${DSM2MODIFIER}")+"</td>" 
                    except:
                        diff += "<td></td>"
                diff += "</tr>"
            all += diff
        else:
            for i in range(nrows):
                all += '<tr>'
                for j in range(ncols):
                    try:  
                        all += "<td>"+self.block1[i][j].replace(self.modifier1,"${DSM2MODIFIER}")+"</td>"
                    except:
                        all += "<td></td>"   
                if self.notes!=0 and cnt==1:
                    all += "<td rowspan=7000 class='header'></td>"
                    cnt = 0            
                for j in range(ncols):
                    try:
                        all += "<td>"+self.block2[i][j].replace(self.modifier2,"${DSM2MODIFIER}")+"</td>"
                    except:
                        all += "<td></td>"
                all += "</tr>"          
        return diff, all
            
    def get_max_row(self):
        return max(len(self.block1),len(self.block2))
    
    def get_diff_type(self):
        if len(self.block1)==0:
            a=2
        elif len(self.block2)==0:
            a=1
        else:
            a=3
        return a
            
    def get_num_col(self):
        try:
            a=len(self.block1[0])
        except:
            a=len(self.block2[0])
        return a
        
    def compare_rows(self):
        #return false when the content of one of the rows is different
        i=0
        for ds in self.block1:
            try:
                compare_to = self.block2[i]
                for j in range(len(ds)):  
                    if ds[j].replace(self.modifier1,"${DSM2MODIFIER}")!=compare_to[j].replace(self.modifier2,"${DSM2MODIFIER}"):
                        return 0
            except:
                return 0
            i+=1
        return 1
    
    def chk_len_block(self):
        # return false when the number of rows are different
        if len(self.block1)==len(self.block2):
            return 1
        else:
            return 0
