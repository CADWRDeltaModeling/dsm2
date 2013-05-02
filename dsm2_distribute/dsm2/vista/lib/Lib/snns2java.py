#!/site/bin/python
__author__ = 'Nicky Sandhu'
__version__= '1.0'
import os, sys, string
from string import *
file_ext=".java"
_debug = 0
#
def dump_ut(fh):
    fh.write('''//    Copyright (C) 2000 State of California, Department of Water
//    Resources.
//
//    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
//    numerical model.  No protection claimed in original FOURPT and
//    Branched Lagrangian Transport Model (BLTM) code written by the
//    United States Geological Survey.  Protection claimed in the
//    routines and files listed in the accompanying file "Protect.txt".
//    If you did not receive a copy of this file contact Dr. Francis
//    Chung, below.
//
//    This program is licensed to you under the terms of the GNU General
//    Public License, version 2, as published by the Free Software
//    Foundation.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, contact Dr. Francis Chung, below,
//    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
//    02139, USA.
//
//    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
//    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
//    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
//    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
//    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
//    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
//    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
//    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
//    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
//    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
//    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
//    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
//    DAMAGE.
//
//    For more information about DSM2, contact:
//
//    Dr. Francis Chung
//    California Dept. of Water Resources
//    Division of Planning, Delta Modeling Section
//    1416 Ninth Street
//    Sacramento, CA  95814
//    916-653-5601
//    chung@water.ca.gov
//
//    or see our home page: http://wwwdelmod.water.ca.gov/

package ANN;

import java.lang.*;

public class UnitType extends Object{
	public float act; //Activation
	public float bias; //Bias of the Unit
	public int no_of_sources; // Number of predecessor units
	public int[] sources; //predecessor units
	public float[] weights; // weights from predecessor units
	UnitType(){}
}''')
    
def convert(srcfile,outfile,modulename=None):
    inf = open(srcfile)
    lines = map(strip,inf.readlines())
    nlines = len(lines)
    uline = []
    wline = []
    sline = []
    for i in range(nlines):
	line = lines[i]
	if find(line,"static UnitType Units") >=0 :
	    if find(line,"=") >= 0:
		# loop to end of declartion
		while find(line,"};") < 0:
		    #accumulate units
		    uline.append(line)
		    i=i+1
		    line = lines[i]
	elif find(line,"static pUnit Sources") >= 0 :
	    #loop to end of declaration
	    while find(line,"};") < 0:
		#accumulate sources
		sline.append(line)
		i=i+1
		line = lines[i]
	elif find(line,"static float Weights") >=0:
	    #loop to end of declaration
	    while find(line,"};") < 0:
		# accumulate weights
		wline.append(line)
		i=i+1
		line = lines[i]
	elif find(line,"float *in,") >= 0:
	    if not modulename:
		modulename = strip(line[find(line,"int")+3:find(line,"(")])
	    break
	else:
	    continue
    #
    #
    defsarray = []
    for j in range(i,nlines):
	line = lines[j]
	if find(line,"static pUnit") >=0 or find(line,"static int") >= 0:
	    var_name = split(split(line)[2],"[")[0]
	    var_def = line
	    while find(line,"};") < 0:
		var_def = var_def + line
		j=j+1
		line = lines[j]
	    defsarray.append((var_name, var_def))
	elif find(line,"for") >= 0:
	    break
	else:
	    continue
    # get all the for loops
    exec_lines = []
    for k in range(j,nlines):
	line = lines[k]
	exec_lines.append(line)
	if find(line,'return(OK)') >= 0: break
    inf.close()
    outfile = strip(modulename) + file_ext
    dumpjava(outfile,modulename,uline,sline,wline,defsarray,exec_lines)
#    
def java_repr(arry,type=None):
    yline = repr(arry)
    arepr = yline[find(yline,"[")+1:find(yline,"]")]
    if type==None:
	return '{'+arepr+'}'
    else:
	return '{'+string.join(string.split(arepr,','),'%s,'%type)+type+'}'
#
def dumpjava(outfile,modulename, uline, sline, wline,defsarray,exec_lines):
    if _debug: print 'Opening %s for writing'%outfile
    outf = open(outfile,"w")
    #
    print 'Creating java for module: ', modulename
    outf.write('package ANN;\n')
    outf.write('import java.lang.*;\n')
    outf.write('public class %s extends Object {\n'%modulename)
    outf.write('''
  public float Act_Logistic(float s, float b){
    if((s+b) < 10000.0) {
      return(float)(1.0/(1.0+Math.exp(-s-b)));
    }
    else {
      return 0.0f;
    }
  }
    ''')
    outf.write('''
    UnitType [] units;
    int member,unitnr,source;
    float sum;
    ''')
    # create src array
    xline = ''
    for line in sline[1:]: xline = xline + ' ' + line
    xline = xline[1:rfind(xline,',')]
    srcs = split(xline[find(xline,'Units + ')+8:],', Units + ')
    srcs = map(atoi,srcs)
    if _debug: print 'Source array: %s'%srcs
    # create weights array
    xline = ''
    for line in wline[1:] : xline = xline + ' ' + line
    wts = map(atof,split(xline[0:rfind(xline,',')],','))
    if _debug: print 'Weights array: %s'%wts
    # 
    line = uline[0]
    no_units = line[find(line,"[")+1:find(line,"]")]
    if _debug: print 'No. of units: %s'%no_units
    #
    for xdef in defsarray:
	name = xdef[0]
	xline = xdef[1]
	if find(xline,"Units +") >= 0 :
	    xline = xline[find(xline,"Units +")+8:rfind(xline,"};")]
	    array = map(string.atoi,split(xline,', Units +'))
	else:
	    xline = xline[find(xline,"{")+1:rfind(xline,"};")]
	    array = [string.atoi(xline)]
	dim= len(array)
	outf.write('  int [] %s = {%s};\n'%(name,string.join(string.split(xline,', Units + '),',')))
    #
    outf.write('  public %s(){\n'%modulename)
    outf.write('    units = new UnitType[%s];\n'%no_units)
    ulen = len(uline)
    clen = 3
    nunit = 0
    while clen < ulen:
	clen = clen +1
	if clen >= ulen: break;
	act,bias,nsrcs,dummy = split(uline[clen],",")
	clen = clen +1
	line = uline[clen]
	src_offset = atoi(line[find(line,"[")+1:find(line,"]")])
	clen = clen +3; line = uline[clen]
	#
	nunit = nunit + 1
	outf.write('    units[%d] = new UnitType();\n'%nunit)
	outf.write('    units[%d].act=%sf;\n'%(nunit,act))
	outf.write('    units[%d].bias=%sf;\n'%(nunit,bias))
	outf.write('    units[%d].no_of_sources=%s;\n'%(nunit,nsrcs))
	if atoi(nsrcs) > 0:
	    src_array = srcs[src_offset:src_offset+atoi(nsrcs)]
	    outf.write('    units[%d].sources = new int [] %s;\n'%(nunit,java_repr(src_array)))
	    wt_array = wts[src_offset:src_offset+atoi(nsrcs)]
	    outf.write('    units[%d].weights = new float [] %s;\n'%(nunit,java_repr(wt_array,'f')))
	else:
	    pass
    outf.write('  }\n')
    outf.write('  public void engine(float[] inarray, float[] outarray, int init){\n')
    in_source = 0
    first_member = 1
    for line in exec_lines:
	if find(line,"for") >=0 and find(line,"member") >=0 : # for(member = init_val
	    loop_var = strip(line[find(line,"(")+1:find(line,"=")])
	    init_val = "0"
	    index_lessthan = find(line,"<")+1
	    final_val = strip(line[index_lessthan:index_lessthan+find(line[index_lessthan:],";")])
	    outf.write('    for(int %s=%s; %s<%s; %s++){\n'%(loop_var,init_val,loop_var,final_val,loop_var))
	elif find(line,"for") >=0 and find(line,"source") >=0 : # for(member = init_val
	    loop_var = 'source'
	    init_val = '0'
	    final_val = 'units[unitnr].no_of_sources'
	    in_source = 1; xline = ''
	    outf.write('    for(int %s=%s; %s<%s; %s++){\n'%(loop_var,init_val,loop_var,final_val,loop_var))
	elif find(line,"}") >= 0:
	    if in_source:
		outf.write('    '+xline); outf.write('\n')
		in_source = 0
	    outf.write('    }\n')
	elif find(line,"return") >= 0:
	    outf.write('    return;\n}\n')
	    break
	else: # substitute -> with % & [] with ()
	    line = strip(line)
	    if len(line) == 0: continue
	    if in_source:
		line = replace(line,"unit->sources[source]->act","units[units[unitnr].sources[source]].act")
		line = replace(line,"unit->weights[source]","units[unitnr].weights[source]")
	    line = replace(line,"unit->act","units[unitnr].act")
	    line = replace(line,"unit->Bias","units[unitnr].bias")
	    line = replace(line,".act",".act")
	    line = replace(line,"out[member] = Units[Output[member]].act","outarray[member] = units[Output[member]].act")
	    if find(line,"->") >=0 :
		index_pointer = find(line,"->")
		line = "units["+line
	    if find(line,"unit =") >=0:
		index_pointer = find(line,"unit =")
		line = " unitnr = " +line[index_pointer+7:]
	    line = replace(line,"->","].")
	    line = replace(line,"[","[")
	    line = replace(line,"]","]")
	    line = replace(line,";",";")
	    line = replace(line,'0.0;','0.0f;')
	    line = replace(line,"in[","inarray[")
	    line = replace(line,"out[","outarray[")
	    line = replace(line,"sum +=","sum +=")
	    if in_source:
		xline = xline + strip(line)
	    else:
		outf.write('    '+line); outf.write('\n')
    #
    #
    outf.write('}\n')
    outf.close()
#
def snns2java(infile,modulename=None):
    ci = find(infile,'.c')
    if ci < 0: raise "Only .c files accepted"
    outfile = infile[:ci]+".java"
    if modulename == None:
	convert(infile,outfile)
    else:
	convert(infile,outfile,modulename)
    fh=open('UnitType.java','w'); dump_ut(fh); fh.close()
#
def domain(arg_array):
    if len(arg_array) < 2:
	print "Usage: snns2java fnet.c [modulename]"
	raise SystemExit
    infile = strip(arg_array[1])
    if ( len(arg_array) == 3 ):
	snns2java(infile,strip(arg_array[2]))
    else:
	snns2java(infile)
#
if __name__ == '__main__' or __name__ == 'main':
    domain(sys.argv)
#
