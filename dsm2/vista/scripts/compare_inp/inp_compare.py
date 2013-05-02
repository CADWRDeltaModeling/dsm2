"""
    Copyright (C) 2010 State of California, Department of 
    Water Resources.

    Input Comparison Tool: VISTA Add-ons
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

import sys, os, shutil
import vdss, vutils, vdisplay
import logging
from gov.ca.dsm2.input.parser import Parser
import difflib # for SequenceMatcher function 
from difflib import *
from getmodel import *
from javax.swing import SwingUtilities
from compare_inp_GUI import *
from shutil import rmtree, copytree
true = 1
false = 0
tmp_filename = ""
class Diff:
    pass

def parse_template_file(template_file):
    p = Parser()                                 
    tables = p.parseModel(template_file)
    #load scalars into a map
    scalar_table = tables.getTableNamed("SCALAR")
    scalar_values = scalar_table.getValues()
    nscalars = scalar_values.size()
    scalars = {}
    for i in range(nscalars):
        name = scalar_table.getValue(i, "NAME").encode('ascii')
        value = scalar_table.getValue(i, "VALUE").encode('ascii')
        scalars[name] = value
    return scalars

def compare_echo_file(echo_file1,echo_file2):
    d = Diff() 
    p = Parser()
    scalar = {}
    header = {}
    tables1 = p.parseModel(echo_file1)
    tables2 = p.parseModel(echo_file2)
    tbl_list1 = list_tables(tables1)
    tbl_list2 = list_tables(tables2)
    ex_arr_tbls = chk_num_tbls(tbl_list1,tbl_list2)
    tbl_names = str_to_arr(ex_arr_tbls[0])
    for i in range(len(tbl_names)):
        print tbl_names[i]
        table1 = tables1.getTableNamed(tbl_names[i])
        table2 = tables2.getTableNamed(tbl_names[i])
        if (tbl_names[i]=='ENVVAR'):
            d.modifier1, d.modifier2 = get_modifier(table1,table2)
        try: 
            d.modifier1
            d.modifier2
        except:
            d.modifier1="nan"
            d.modifier2="nan"        
        change_tbl = compare_table(table1,table2,d.modifier1,d.modifier2)
        scalar[tbl_names[i]] = change_tbl
        header[tbl_names[i]] = table1.getHeaders()
    d.tbl_a = ex_arr_tbls[1]
    d.tbl_b = ex_arr_tbls[2]
    d.diff = scalar
    d.header = header
    #scalar: {'OPRULE_EXPRESSION': ([1, 2, 3], [], [[0],[0]])
    return d 
    
def chk_num_tbls(list1,list2):
    str0 = ''    # tables will be used
    str1 = ''    # additional table from file 1 
    str2 = ''    # additional table from file 2
    for i in range(len(list1)):
        try: 
            list2.index(list1[i])
            str0 += list1[i] + ";"
        except: str1 += list1[i] + ";"
    for i in range(len(list2)):
        try: list1.index(list2[i])
        except: str2 += list2[i] + ";"
    return [str0[:-1],str1[:-1],str2[:-1]]

def list_tables(tables):
    tbl_list = []
    a = tables.getTables()
    for i in range(len(a)):
        tbl_list.append(str(a[i]).replace('Input Table: ',''))
    return tbl_list

def compare_row(row1,row2,modifier1,modifier2):
    unmatched = 0
    for i in range(len(row1)):
        if str(row1[i]).replace(modifier1,"${DSM2MODIFIER}")!=str(row2[i]).replace(modifier2,"${DSM2MODIFIER}"):
            unmatched += 1
    return unmatched

def compare_table(table1,table2,modifier1,modifier2):
    values1 = table1.getValues()
    key = column(values1,0)
    if is_unique_key(key):
        change_tbl = chk_unique(table1,table2,modifier1,modifier2)
    else:
        change_tbl = chk_non_unique(table1,table2,modifier1,modifier2)
    return change_tbl 

def chk_unique(table1,table2,modifier1,modifier2):
    '''compare tables that have unique primary ID'''
    add_a = []
    add_b = []
    change1 = []
    change2 = []
    header = table1.getHeaders()
    values1 = table1.getValues()
    values2 = table2.getValues()
    nrows1 = len(values1)
    nrows2 = len(values2)
    ncols = len(header)
    for i in range(nrows1):
        try: 
            arr = column(values2,0)
            a = arr.index(values1[i][0])
            if compare_row(values1[i],values2[a],modifier1,modifier2) > 0:
                #if ncols==2:
                #    print values1[i],values2[a]
                change1.append(i)
                change2.append(a)
        except: 
            add_a.append(i)
    for i in range(nrows2):
        try: 
            arr = column(values1,0)
            a = arr.index(values2[i][0])
        except:
            add_b.append(i)
    return add_a, add_b, [change1,change2], 0

def chk_non_unique(table1,table2,modifier1,modifier2):
    '''compare tables that don't have unique primary ID'''
    comp = CompareTables(table1,table2,modifier1,modifier2)
    add_a, add_b = comp.get_extra_key
    comp.get_rows_diff()
    change = comp.get_modified_block() - len(add_a) - len(add_b)
    content_diff = comp.get_str_diff()
    content_all = comp.get_str_all()    
    html_str = [content_diff, content_all]
    # change are the numbers for summary table 
    # which is different to the output of chk_unique()
    #        a[0]        a[1]        a[2] a[3]  a[4]        a[5]                   a[6]               a[7]
    return len(add_a), len(add_b), change, 1, html_str, comp.get_rows_diff(), comp.get_rows_all(), comp.get_limit_rowspan() 

def retrieve_select_data(table1,table2,input_arr,d):
    out_arr = []
    header = table1.getHeaders()
    values1 = table1.getValues()
    values2 = table2.getValues()
    for i in range(len(input_arr[0])):
        out_arr.append([values1[input_arr[0][i]],[]])
    for i in range(len(input_arr[1])):
        out_arr.append([[],values2[input_arr[1][i]]])
    for i in range(len(input_arr[2][0])):
        res_arr1,res_arr2 = highlight_str_by_diff(values1[input_arr[2][0][i]],values2[input_arr[2][1][i]],d.modifier1,d.modifier2)
        out_arr.append([res_arr1,res_arr2])            
    return out_arr  

def retrieve_full_data(table1,table2,input_arr,d):
    out_arr = []
    header = table1.getHeaders()
    values1 = table1.getValues()
    values2 = table2.getValues()
    size = max(values1.size(),values2.size())
    if size==values1.size(): findex=1
    else: findex=2
    for i in range(size):
        type = find_comparison_type(input_arr,i,findex)
        if type==0:
            if findex==1:
                arr = column(values2,0)
                a = arr.index(values1[i][0])
                out_arr.append([values1[i],values2[a]])
            else:
                arr = column(values1,0)
                a = arr.index(values2[i][0])                            
                out_arr.append([values1[a],values2[i]])
        if type==1:                    
            out_arr.append([values1[i],[]])
        if type==2:
            out_arr.append([[],values2[i]])
        if type==3:
            try:
                a = input_arr[2][0].index(i)
                res_arr1,res_arr2 = highlight_str_by_diff(values1[i],values2[input_arr[2][1][a]],d.modifier1,d.modifier2)
            except:
                a = input_arr[2][1].index(i)  
                res_arr1,res_arr2 = highlight_str_by_diff(values1[input_arr[2][0][a]],values2[i],d.modifier1,d.modifier2)
            out_arr.append([res_arr1,res_arr2])
        if findex==1:
            if find_comparison_type(input_arr,i,2)==2:
                out_arr.append([[],values2[i]])
        else:
            if find_comparison_type(input_arr,i,1)==1:
                out_arr.append([values1[i],[]])        
    return out_arr   

def highlight_str_by_diff(arr1,arr2,modifier1,modifier2):
    res_arr1=[]
    res_arr2=[]
    for i in range(len(arr1)):
        str1=arr1[i].encode('ascii').replace(modifier1,"${DSM2MODIFIER}")
        str2=arr2[i].encode('ascii').replace(modifier2,"${DSM2MODIFIER}")
        s = SequenceMatcher(None, str1, str2)
        a = s.get_opcodes()
        if a[0][1]!=0: res1 = str1[0:a[0][1]-1]
        else: res1 = str1[0:0]
        if a[0][3]!=0: res2 = str2[0:a[0][3]-1]
        else: res2 = str2[0:0]        
        for i in range(len(a)):
            if a[i][0]!='equal' and a[i][1]!=a[i][2]:
                res1 = res1 + '<r>' + str1[a[i][1]:a[i][2]] +'</r>'
            else:
                res1 = res1 + str1[a[i][1]:a[i][2]]
            if a[i][0]!='equal' and a[i][3]!=a[i][4]:
                res2 = res2 + '<r>' + str2[a[i][3]:a[i][4]] +'</r>'
            else:
                res2 = res2 + str2[a[i][3]:a[i][4]]
        res_arr1.append(res1)
        res_arr2.append(res2)
    return res_arr1,res_arr2                      


def write_diff_table(echo_file1,echo_file2,fh,d):
    p = Parser()
    tables1 = p.parseModel(echo_file1)
    tables2 = p.parseModel(echo_file2)
    keys = d.diff.keys()
    keys.sort()
    for i in range(len(keys)):
        a = d.diff[keys[i]]
        h = d.header[keys[i]]
        num_headers = len(h)*2+1
        print >> fh, """<table width="200px" id="h%s" class="sample"><tr><th colspan=%s align=left>
        <img src="js/open.JPG" name="img_%s" onClick="hideDiv('%s');">  
        %s</th></tr></table>""" %(i,num_headers,i,i,keys[i]) 
        print >> fh, "<table id='%s' width='1200px' class='sample'>"%(i)
        print >> fh, """<tr><td class='header' colspan=%s><center>%s</center></td><td class='header'></td><td class='header' colspan=%s><center>%s</center></td></tr>
           """%(len(h),get_pathandname(echo_file1)[1],len(h),get_pathandname(echo_file2)[1])
        print >> fh, "<tr>"
        for c in h:
            print >> fh, "<td class='header'>"+c.encode('ascii')+"</td>"
        if a[3] == 0:  # unique key
            out_arr = retrieve_select_data(tables1.getTableNamed(keys[i]),tables2.getTableNamed(keys[i]),d.diff[keys[i]],d)
            full_arr = retrieve_full_data(tables1.getTableNamed(keys[i]),tables2.getTableNamed(keys[i]),d.diff[keys[i]],d)           
            #write diff table
            print >> fh, """<td rowspan=%s width="5px" class='header'></td>"""%(len(out_arr)+1)
            for c in h:
                print >> fh, "<td class='header'>"+c.encode('ascii')+"</td>"
            print >> fh, "</tr>"                
            for j in range(len(out_arr)):
                print >> fh, "<tr>"
                maxlen = max(len(out_arr[j][0]),len(out_arr[j][1]))
                if len(out_arr[j][0])>0 and len(out_arr[j][1])>0:
                    for k in range(maxlen):
                        print >> fh, "<td>"+out_arr[j][0][k]+"</td>"
                    for k in range(maxlen):
                        print >> fh, "<td>"+out_arr[j][1][k]+"</td>"
                if len(out_arr[j][0])>0 and len(out_arr[j][1])==0:    
                    for k in range(maxlen):
                        print >> fh, "<td class='add1'>"+out_arr[j][0][k]+"</td>"
                    for k in range(maxlen):
                        print >> fh,"<td></td>"
                if len(out_arr[j][0])==0 and len(out_arr[j][1])>0:
                    for k in range(maxlen):
                        print >> fh, "<td></td>"
                    for k in range(maxlen):
                        print >> fh, "<td class='add2'>"+out_arr[j][1][k]+"</td>"
                print >> fh, "</tr>" 
            print >> fh, """</table>"""
            #write full table
            print >> fh, "<table id='f%s' width='1200px' class='sample' style='display:none'><tr>"%(i)
            for c in h:
                print >> fh, "<td class='header'>"+c.encode('ascii')+"</td>"
            print >> fh, """<td rowspan=%s width="5px" class='header'></td>"""%(len(full_arr)+1)
            for c in h:
                print >> fh, "<td class='header'>"+c.encode('ascii')+"</td>"
            print >> fh, "</tr>"                
            for j in range(len(full_arr)):
                print >> fh, "<tr>"
                maxlen = max(len(full_arr[j][0]),len(full_arr[j][1]))
                if len(full_arr[j][0])>0 and len(full_arr[j][1])>0: #modified
                    for k in range(maxlen):
                        print >> fh, "<td>"+full_arr[j][0][k]+"</td>"
                    for k in range(maxlen):
                        print >> fh, "<td>"+full_arr[j][1][k]+"</td>"
                if len(full_arr[j][0])>0 and len(full_arr[j][1])==0: #addition in senario 1   
                    for k in range(maxlen):
                        print >> fh, "<td class='add1'>"+full_arr[j][0][k]+"</td>"
                    for k in range(maxlen):
                        print >> fh,"<td></td>"
                if len(full_arr[j][0])==0 and len(full_arr[j][1])>0: #addition in senario 2
                    for k in range(maxlen):
                        print >> fh, "<td></td>"
                    for k in range(maxlen):
                        print >> fh, "<td class='add2'>"+full_arr[j][1][k]+"</td>"
                print >> fh, "</tr>" 
            print >> fh, """</table>"""
            print >> fh, "<br>"
        else:   #non-unique key
            a = d.diff[keys[i]]
            #comp = CompareTables(tables1.getTableNamed(keys[i]),tables2.getTableNamed(keys[i]),d.modifier1,d.modifier2)
            #content_diff = comp.get_str_diff()
            #content_all = comp.get_str_all()
            #write diff table
            print >> fh, """<td rowspan=%s width="5px" class='header'></td>"""%(a[5]+1)
            for c in h:
                print >> fh, "<td class='header'>"+c.encode('ascii')+"</td>"
            print >> fh, "</tr>"               
            print >> fh, a[4][0]+"</table>"
            #write full table
            print >> fh, "<table id='f%s' width='1200px' class='sample' style='display:none'><tr>"%(i)
            for c in h:
                print >> fh, "<td class='header'>"+c.encode('ascii')+"</td>"
            if a[6]<7000:
                print >> fh, """<td rowspan=%s width="5px" class='header'></td>"""%(a[6]+1)
            else:
                print >> fh, """<td rowspan=%s width="5px" class='header'></td>"""%(a[7]+1)
            for c in h:
                print >> fh, "<td class='header'>"+c.encode('ascii')+"</td>"
            print >> fh, "</tr>"                
            print >> fh, a[4][1]+ "</table>"
            print >> fh, "<br>"
                     

def write_summary_table(echo_file1,echo_file2,fh,d):
    p = Parser()
    tables1 = p.parseModel(echo_file1)
    tables2 = p.parseModel(echo_file2)
    keys = d.diff.keys()
    keys.sort()
    head1=get_pathandname(echo_file1)[0]+"<br><k>"+get_pathandname(echo_file1)[1]+"</k>"
    head2=get_pathandname(echo_file2)[0]+"<br><k>"+get_pathandname(echo_file2)[1]+"</k>"
    print >> fh, "<h2><center>DSM2 Input Comparison Report</center></h2>"
    print >> fh, """<table class="summary" align=center><tr><th>Table Name</th>
    <th>Extra record in <br>%s</th><th>Extra record in <br>%s</th>
    <th>Modified records</th></tr>""" %(head1,head2)
    for i in range(len(keys)):
        a = d.diff[keys[i]]
        if a[3] == 0:
            print >> fh, "<tr><td><a href='#h%s'>%s</a></td>"%(i,keys[i])
            if len(a[0])>0:
                print >> fh, '<td class="red"><center>%d</center></td>'%(len(a[0]))
            else:
                print >> fh, '<td><center>%d</center></td>'%(len(a[0]))
            if len(a[1])>0:
                print >> fh, '<td class="red"><center>%d</center></td>'%(len(a[1]))
            else:
                print >> fh, '<td><center>%d</center></td>'%(len(a[1]))
            if len(a[2][0])>0:
                print >> fh, '<td class="red"><center>%d</center></td></tr>'%(len(a[2][0]))
            else:
                print >> fh, '<td><center>%d</center></td></tr>'%(len(a[2][0]))
        else:
            print >> fh, """<tr><td><a href='#h%s'>%s</a></td>"""%(i,keys[i])
            if a[0]>0:
                print >> fh, '<td class="red"><center>%d</center></td>'%(a[0])
            else:
                print >> fh, '<td><center>%d</center></td>'%(a[0])
            if a[1]>0:
                print >> fh, '<td class="red"><center>%d</center></td>'%(a[1])
            else:
                print >> fh, '<td><center>%d</center></td>'%(a[1])
            if a[2]>0:
                print >> fh, '<td class="red"><center>%d</center></td></tr>'%(a[2])
            else:
                print >> fh, '<td><center>%d</center></td></tr>'%(a[2])       
    print >> fh, "</table><br>"
    print >> fh, '<center><span>Color Styles: </span><span style="background-color:#F6CCDA" title="extra record in study 1"> &emsp;&emsp;Extra record in Study 1&emsp;</span>&emsp;'
    print >> fh, '<span style="background-color:#DAF4F0" title="extra record in study 2"> &emsp;&emsp;Extra record in Study 2&emsp;</span>&emsp;'
    print >> fh, '<span style="background-color:#FFFACD" title="modification made for tables with NON-UNIQUE lookup name at the first column"> &emsp;&emsp;Modified block&emsp;</span>&emsp;'
    print >> fh, '<span style="background-color:yellow;color:red" title="modification made for tables with UNIQUE lookup name at the first column">&emsp;Difference&emsp;</span></center><br>'
           
def get_pathandname(fullname):
    a = fullname.split("\\")
    return fullname.replace(a[len(a)-1],""),a[len(a)-1]  
def do_processing(echo_file1,echo_file2,output_path,output_file):
    # open files 1 and file 2 and loop over to plot
    from java.util import Date
    copy_basic_files(output_path)
    d = compare_echo_file(echo_file1,echo_file2) 
    fh = open(output_path+"/"+output_file,'w')
    print >> fh, """
<html>
<head>   
<script type="text/javascript" src="js/jquery-1.4.2.min.js"></script>
<script type="text/javascript">
function mOver(e,d) { e.style.backgroundColor = d;}
function mOut(e) {e.style.backgroundColor='';}
function hideDiv(div_name) {
  imgsrc = document.images['img_'+div_name].src;
  if (imgsrc.substr(-9)=="/open.JPG") {
     $("#f"+div_name).show("slow");
     $("#"+div_name).hide("slow"); 
     document.images['img_'+div_name].src = "js/close.JPG";
  }else{  $("#f"+div_name).hide("slow");
          $("#"+div_name).show("slow");
          document.images['img_'+div_name].src ="js/open.JPG";
  }
}
</script>
<link rel="stylesheet" type="text/css" href="js/style.css" /> 
<style>tr:hover{background: #DFFFA5;}
</style>
</head> 
<body>
    """
    write_summary_table(echo_file1,echo_file2,fh,d)
    write_diff_table(echo_file1,echo_file2,fh,d)
    print >> fh, """<br><center><font style="font-family:arial,verdana,sans-serif;font-size:80%">Copyright &copy; 2010 State of California, Department of Water Resources<br>Last Modified: 11/05/2010</font></center>
</body>
</html>    
    """
    fh.close()

def copy_basic_files(output_path):
    if output_path[-19:-1]=='scripts\\compare_in':
        print "Please select the other directory"
    else:
        if os.path.isdir(output_path+"//js"):
            shutil.rmtree(output_path+"//js")
        copytree(str(os.getenv('SCRIPT_HOME'))+"/js",str(output_path)+"/js",[])
        #os.system('cmd /c xcopy "'+str(os.getenv('SCRIPT_HOME'))+'/js" "'+output_path+'js"') 
def rm_mk_dir(out):
    out=str(out)
    if not os.path.isdir(out): 
        os.mkdir(out)
    else:
        shutil.rmtree(out)
        os.mkdir(out)     
def show_gui():
    #SwingUtilities.invokeLater(Invoker())
    #UIManager.put("swing.boldMetal", Boolean.FALSE)
    UIManager.setLookAndFeel("com.sun.java.swing.plaf.windows.WindowsLookAndFeel")
    sample = OpenFile(create_report)
    sample.createUI()

def select_distinct(input_arr):
  output_arr = []
  for x in input_arr:
    if x not in output_arr:
      output_arr.append(x)
  output_arr.sort()
  return output_arr
    
def str_to_arr(str):
    return str.split(";")

def is_unique_key(list):
    group = set(list)
    if len(group)==len(list):
        return true
    else:
        return false

def column(matrix, i):
    return [row[i].encode('ascii') for row in matrix]
def row(matrix,i):
    return matrix[i]

def find_comparison_type(input_arr,elem,findex):
    #find out what type of comparison group for this row - 
    # return 0:no change,1:additional,2:not exist,3:modified
    if findex==1:
        try:
            a = input_arr[0].index(elem)
            return 1
        except:
            try:
                a = input_arr[2][0].index(elem)                
                return 3
            except:
                return 0
    else:
        try:
            a = input_arr[1].index(elem)
            return 2 
        except:
            try:
                a = input_arr[2][1].index(elem)
                return 3
            except:
                return 0        

def get_modifier(table1,table2):
    values1 = table1.getValues()
    values2 = table2.getValues()
    for i in range(len(values1)):
        if table1.getValue(i,"NAME")=='DSM2MODIFIER':
            modifier1 = table1.getValue(i,"VALUE")
    for i in range(len(values2)):
        if table2.getValue(i,"NAME")=='DSM2MODIFIER':
            modifier2 = table2.getValue(i,"VALUE")
    return modifier1, modifier2 

def create_report(echo1,echo2,outpath,outhtml):
    logging.basicConfig(level=logging.DEBUG)    
    logging.debug('Parsing input echo file 1 %s'%echo1)
    logging.debug('Parsing input echo file 2 %s'%echo2)
    if not os.path.isdir(outpath):
        os.mkdir(outpath)
    #scalars = parse_template_file(filename)
    do_processing(echo1,echo2,outpath, outhtml)
    logging.debug('Done processing. The end!')    
        
if __name__ == '__main__':
    show_gui()