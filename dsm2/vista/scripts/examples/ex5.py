from vutils import *
#
from vista.set import PartNamePredicate, SortMechanism, ChainedComparators
#
g=opendss('../testdata/qual-2b.dss')
# Let us first sort by the b part in increasing order and if b part is 
# the same order them in decreasing order with respect to the c part
p1 = PartNamePredicate(Pathname.C_PART, SortMechanism.DECREASING);
p2 = PartNamePredicate(Pathname.B_PART, SortMechanism.INCREASING);
# create a sorter
# sort the group using this sorter
g.sortBy(p1)
g.sortBy(p2)
g.sortBy(ChainedComparators(p1,p2));
