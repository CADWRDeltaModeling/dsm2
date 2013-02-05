#!/usr/bin/env /site/scripts/vscript
import sys, os, vutils
def display_missing(ds):
    dsi = ds.getIterator()
    while not dsi.atEnd():
	el = dsi.getElement()
	begin_date = None
	while not Constants.DEFAULT_FLAG_FILTER.isAcceptable(el) \
	      and not dsi.atEnd():
	    if begin_date == None: begin_date = el.getXString()
	    end_date = el.getXString()
	    dsi.advance()
	    el = dsi.getElement()
	    #print el
	if begin_date != None:
	    print 'Missing for %s to %s'%(begin_date,end_date)
	if dsi.atEnd(): break
	dsi.advance()
if len(sys.argv) !=3 :
    raise "Usage: showmissing dssfile pathname"
dssfile = sys.argv[1]
pathname = sys.argv[2]
g=opendss(dssfile)
refs = vutils.findpath(g,pathname)
if refs == None or len(refs) == 0: raise "No data found for %s & %s"%(dssfile,pathname)
for ref in refs:
    display_missing(ref.getData())
#
