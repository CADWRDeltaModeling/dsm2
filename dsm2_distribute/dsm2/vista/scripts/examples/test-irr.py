from vutils import *
x=['01jan1990 0112', '01jan1990 0155', '01jan1990 0333']
y=[2,3.5,5.4]
tmx = []
for i in x:
    tmx.append(time(i))
its = IrregularTimeSeries('',tmx,y)
plot(its)
writedss('junk.dss','/A/B/C//IR-YEAR/F/',its)
writedss('junk.dss','/A/B/C//IR-YEAR/F/',its)
writedss('junk.dss','/A/B/C//IR-YEAR/F/',its)
writedss('junk.dss','/A/B/C//IR-YEAR/F/',its)
