import vutils
from vista.set import PathPartPredicate, Pathname
def rename(file,oldf,newfile,newf):
    g=vutils.opendss(file)
    g.filterBy(PathPartPredicate(oldf,Pathname.F_PART),1)
    if len(g) == 0:
	raise SystemExit,'No FPART: %s in DSS FILE: %s'%(oldf,file)
    for ref in g:
	path = ref.getPathname()
	ds = ref.getData()
	path.setPart(Pathname.F_PART,newf)
	print 'OLD: %s -> NEW: %s'%(str(ref.getPathname()),str(path))
	vutils.writedss(newfile,str(path),ds)
	g.removeDataReference(ref)
#
if __name__ == '__main__' or __name__ == 'main':
    rename('sim809anndv.dss','2020D09C-809-ANN','sim809dxc898dv.dss','809-ANN-DXC898')
    rename('sim809annsv.dss','2020D09C-809-ANN','sim809dxc898sv.dss','809-ANN-DXC898')
    sys.exit(0)
