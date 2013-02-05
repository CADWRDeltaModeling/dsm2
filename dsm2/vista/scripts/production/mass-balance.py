# THIS SCRIPT READS A CONNECTIVITY FILE, AND COMPUTES THE MASS BALANCE FOR EACH NODE.
# THE NODE PASSED IN THIS FUNCTION MUST BE A STRING

from java.util import StringTokenizer
from java.lang import String
# gets a reference with exact b part from filename
def getReference(filename, bpart):
 g=opendss(filename)
 g.filterBy(PathPartPredicate(('^'+bpart+'$'),Pathname.B_PART),1)
 return g[0]
#
#
def getForwardDifference(ref,tw):
 ds = ref.getData()
 y1 = []
 filter = Constants.DEFAULT_FILTER
 # skip the leading missing values
 for k in range(len(ds)):
  y1.append(ds.getElementAt(k).getY())
  if filter.isAcceptable(ds.getElementAt(k)) :
   break; # got the first y that contains data
  y1[k]= -902          
 # now subtract the storage from the previous one
 for n in range(k+1, len(ds)):
  element = ds[n]
  y = element.y
  if filter.isAcceptable(element):
   dif = y - ds[n-1].y
   y1.append(dif)                    
  else:
   y1.append(y)
 # end of for            
 attr = ds.getAttributes()
 stime = ref.getTimeWindow().getStartTime().toString()
 ti = ref.getTimeInterval().toString()
 ts=RegularTimeSeries('FORWARD-DIFFERENCE', stime, ti, y1, attr)
 ts = ts.createSlice(tw)
 server = ref.getServername()
 file = ref.getFilename()
 path = ref.getPathname()
 path = Pathname.createPathname(path)
 path.setPart(Pathname.B_PART,'DEL'+path.getPart(Pathname.B_PART))
 path.setPart(Pathname.C_PART,path.getPart(Pathname.C_PART) + '-CHANGE')
 return DSSUtil.createDataReference(server,file,path.toString(),ts)
#                    
def massBalance(inflow,outflow,storage,indss,outdss,tw):
 #tw = timeWindow('31oct1921 2400 - 31jan1950 2400')
 #file1 = 'sim514.dss'
 #file2 = 'sim514dv.dss'
 inref = []
 for bpart in inflow:
  inref.append(DataReference.create(getReference(indss,bpart),tw))
 #
 outref = []
 for bpart in outflow:
  outref.append(DataReference.create(getReference(outdss,bpart),tw))
 #
 storage_ref = []
 storage_change_ref = []
 for bpart in storage:
  storage_ref.append(DataReference.create(getReference(outdss,bpart),tw))
  ref = DataReference.create(getReference(outdss,bpart),tw)
  storage_change_ref.append(getForwardDifference(ref,tw))
 #
 if len(inref) > 1:
  for ref in inref:
   sum = sum + ref
 elif len(inref) == 1:
  sum = inref[0]
 else :
  throw, 'No inflows specified'
 #
 for ref in outref:
  sum = sum - ref
 #for ref in storage_ref:
 # sum = sum - ref
 for ref in storage_change_ref:
  sum = sum - ref
 return sum
# end of def massBalance()  
#
def test():
 inflow = ['I1']
 outflow = ['E1','F1','S1']
 storage = ['S1']
 indss = '../testdata/sim514.dss'
 outdss = '../testdata/sim514dv.dss'
 tw = timeWindow('31oct1921 2400 - 31jan1950 2400')
 balance = massBalance(inflow,outflow,storage,indss,outdss,tw)
 #tabulate(balance)
#
