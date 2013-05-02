import vutils
from vutils import *
g=opendss('../testdata/ex3-base.dss')
ref = g[0]
refma = vutils.mov_avg(ref,20,20)
reftd = vutils.tidal_avg(ref)
ref_hour = vutils.per_avg(ref,'1hour')
ref_day = vutils.per_avg(ref,'1day')
plot(ref,refma)
plot(ref,reftd)
plot(ref,ref_hour,ref_day)
vutils.read_dssts('test.dssts')
vutils.read_dssits('test.dssits')
