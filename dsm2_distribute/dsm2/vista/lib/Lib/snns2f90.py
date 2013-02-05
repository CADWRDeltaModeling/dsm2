#!/site/scrip
import os, sys, string
from string import *
#
def dump_ut(fh):
    fh.write('''module unittype
  type UT
     real :: act
     real :: bias
     integer :: no_of_sources
     integer, pointer, dimension(:) :: sources
     real, pointer, dimension(:) :: weights
  end type UT
contains
function act_logistic(s,b)
  real :: act_logistic
  real, intent(in) :: s
  real, intent(in) :: b
  if ( (s+b) < 10000.0 ) then
     act_logistic = 1.0/(1.0+exp(-s-b))
  else
     act_logistic = 0.0
  end if
end function act_logistic
end module unittype''')
    
#srcfile = "fnet.c"
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
	    if len(modulename) > 32:
		raise "Module name is more than 32 characters: "+ modulename
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
	    #print 'array name is ', var_name
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
    outfile = strip(modulename) + ".f90"
    dumpf90(outfile,modulename,uline,sline,wline,defsarray,exec_lines)
#    
#
def fortran_repr(arry):
    yline = repr(arry)
    arepr = yline[find(yline,"[")+1:find(yline,"]")]
    ci = 0
    rl = len(arepr)
    max_line_len = 70
    while rl > max_line_len:
	ri = rfind(arepr[ci:ci+max_line_len],",")+1
	if ri >= 0:
	    arepr = arepr[:ci+ri] + '&'+ os.linesep + '&' +arepr[ci+ri:]
	ci = ci + max_line_len + 3
	rl = len(arepr[ci:])
    return '(/'+arepr+'/)'
#
def dumpf90(outfile,modulename, uline, sline, wline,defsarray,exec_lines):
    eol = os.linesep
    outf = open(outfile,"w")
    #
    print 'Creating f90 for module: ', modulename
    outf.write('module '+modulename)
    outf.write(eol)
    outf.write('use unittype')
    outf.write(eol)
    # create src array
    xline = ''
    for line in sline[1:]: xline = xline + ' ' + line
    xline = xline[1:rfind(xline,',')]
    srcs = split(xline[find(xline,'Units + ')+8:],', Units + ')
    srcs = map(atoi,srcs)
    # create weights array
    xline = ''
    for line in wline[1:] : xline = xline + ' ' + line
    wts = map(atof,split(xline[0:rfind(xline,',')],','))
    # 
    line = uline[0]
    no_units = line[find(line,"[")+1:find(line,"]")]
    outf.write('type(ut), dimension('+str(no_units)+') :: units'+eol)
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
	outf.write('integer, dimension('+repr(dim)+') :: '+lower(name))
	outf.write(eol)
    #
    outf.write('contains'); outf.write(eol)
    outf.write('subroutine '+modulename+'_initall()'); outf.write(eol)
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
	outf.write('units('+repr(nunit)+')%act='+act)
	outf.write(eol)
	outf.write('units('+repr(nunit)+')%bias='+bias)
	outf.write(eol)
	outf.write('units('+repr(nunit)+')%no_of_sources='+nsrcs)
	outf.write(eol)
	if atoi(nsrcs) > 0:
	    outf.write('allocate(units('+repr(nunit)+')%sources(units('+repr(nunit)+')%no_of_sources))')
	    outf.write(eol)
	    src_array = srcs[src_offset:src_offset+atoi(nsrcs)]
	    outf.write('units('+repr(nunit)+')%sources = '+fortran_repr(src_array))
	    outf.write(eol)
	    outf.write('allocate(units('+repr(nunit)+')%weights(units('+repr(nunit)+')%no_of_sources))')
	    outf.write(eol)
	    wt_array = wts[src_offset:src_offset+atoi(nsrcs)]
	    outf.write('units('+repr(nunit)+')%weights = ' +fortran_repr(wt_array))
	    outf.write(eol)
	else:
	    pass
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
	outf.write(lower(name) + ' = ' + fortran_repr(array))
	outf.write(eol)
    # 
    outf.write('end subroutine '+modulename+'_initall'); outf.write(eol)
    #
    outf.write('subroutine '+modulename+'_engine(inarray, outarray, init)'); outf.write(eol)
    outf.write('  real , dimension(:), intent(in) :: inarray'); outf.write(eol)
    outf.write('  real , dimension(:), intent(inout) :: outarray'); outf.write(eol)
    outf.write('  integer , intent(inout) :: init'); outf.write(eol)
    outf.write('  integer member,source'); outf.write(eol)
    #    outf.write('  type(ut) :: unit'); outf.write(eol)
    outf.write('  integer :: unitnr'); outf.write(eol)
    outf.write('  real sum'); outf.write(eol)
    in_source = 0
    first_member = 1
    for line in exec_lines:
	if find(line,"for") >=0 and find(line,"member") >=0 : # for(member = init_val
	    loop_var = strip(line[find(line,"(")+1:find(line,"=")])
	    init_val = "1" #strip(line[find(line,"=")+1:find(line,";")])
	    index_lessthan = find(line,"<")+1
	    final_val = strip(line[index_lessthan:index_lessthan+find(line[index_lessthan:],";")])
	    outf.write("do "+loop_var+"= "+init_val+", "+final_val); outf.write(eol)
	elif find(line,"for") >=0 and find(line,"source") >=0 : # for(member = init_val
	    loop_var = "source"
	    init_val = "1"
	    final_val = "units(unitnr)%no_of_sources"
	    in_source = 1; xline = ''
	    outf.write("do "+loop_var+"= "+init_val+", "+final_val); outf.write(eol)
	elif find(line,"}") >= 0:
	    if in_source:
		outf.write(xline); outf.write(eol)
		in_source = 0
	    outf.write("end do"); outf.write(eol)
	elif find(line,"return") >= 0:
	    break
	else: # substitute -> with % & [] with ()
	    line = strip(line)
	    #print line
	    if len(line) == 0: continue
	    if in_source:
		line = replace(line,"unit->sources[source]->act","units(units(unitnr)%sources(source))%act")
		line = replace(line,"unit->weights[source]","units(unitnr)%weights(source)")
	    line = replace(line,"unit->act","units(unitnr)%act")
	    line = replace(line,"unit->Bias","units(unitnr)%bias")
	    line = replace(line,".act","%act")
	    if find(line,"->") >=0 :
		index_pointer = find(line,"->")
		line = "units("+line
	    if find(line,"unit =") >=0:
		index_pointer = find(line,"unit =")
		#line = line[:index_pointer+7] + "units(" +line[index_pointer+7:]+")"
		line = " unitnr = " +line[index_pointer+7:]
	    line = replace(line,"->",")%")
	    line = replace(line,"[","(")
	    line = replace(line,"]",")")
	    line = replace(line,";","")
	    line = replace(line,"in(","inarray(")
	    line = replace(line,"out(","outarray(")
	    line = replace(line,"sum +=","sum = sum +")
	    if in_source:
		xline = xline + strip(line)
	    else:
		outf.write(line); outf.write(eol)
    #
    #
    outf.write('end subroutine '+modulename+'_engine'); outf.write(eol)
    #
    outf.write('end module '+modulename); outf.write(eol)
    outf.close()
#
def snns2f90(infile,modulename=None):
    ci = find(infile,'.c')
    if ci < 0: raise "Only .c files accepted"
    outfile = infile[:ci]+".f90"
    if modulename == None:
	convert(infile,outfile)
    else:
	convert(infile,outfile,modulename)
    fh=open('ut.f90','w'); dump_ut(fh); fh.close()
#
def domain(arg_array):
    if len(arg_array) < 2:
	print "Usage: snns2f90 fnet.c [modulename]"
	raise SystemExit
    print arg_array
    infile = string.strip(arg_array[1])
    if ( len(arg_array) == 3 ):
	snns2f90(infile,strip(arg_array[2]))
    else:
	snns2f90(infile)
#
if __name__ == '__main__' or __name__ == 'main':
    domain(sys.argv)
#
