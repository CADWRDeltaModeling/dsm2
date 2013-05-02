import string, re, os, posixpath, sys, math, glob, csv, cPickle
class SenseCompare:
    """
    Read in base and perturbed results from ascii files in Dir.
    Compute and return the sensitivity of a perturbation:
    at location Loc (station name);
    for parameter Param (EC, FLOW, STAGE);
    perturbing input Perturb (e.g. MANN, DISP, CHLEN);
    for object Obj (CH or RES);
    for object number or name ID.
    """
    def __init__(self, Dir):
#fpickle = open(senseDir+'pickle','w')
#cPickle.dump(x,fpickle,cPickle.HIGHEST_PROTOCOL)
        # Read reduced base and perturbed EC, Flow ampl, Stage ampl values
        print 'Reading...'
        self.BaseDict = dict()
        for file in glob.glob(Dir+'Results-Base*[!~]'):
            print file
            reader = csv.reader(open(file,'r'), \
                                    delimiter='\t', \
                                    quoting=csv.QUOTE_NONE)
            for row in reader:
                key = ':'.join(row[1:])
                val = float(row[0])
                self.BaseDict[key] = val
                #print len(self.BaseDict),key,self.BaseDict[key]
        self.PerturbDict = dict()
        for file in glob.glob(Dir+'Results-[!B]*[!~]'):
            print file
            reader = csv.reader(open(file,'r'), \
                                    delimiter='\t', \
                                    quoting=csv.QUOTE_NONE)
            for row in reader:
                key = ':'.join(row[1:])
                val = float(row[0])
                self.PerturbDict[key] = val
                #print len(self.PerturbDict),key,self.PerturbDict[key]
        ##
        # Read lists of EC, Flow, Stage locations (stations)
        self.paramDict = dict()
        for file in glob.glob(Dir+'*.lst'):
            param = os.path.basename(file).replace('.lst','')
            f=open(file,'r')
            self.paramDict[param] = list(f.readlines())
            f.close()
    #
    def Sense(self,Loc,Param,Perturb,Obj,ID):
        keyB = ':'.join([Loc.strip(),Param,'BASE'])
        keyP = ':'.join([Loc.strip(),Param,Perturb,Obj,ID])
        B = self.BaseDict.get(keyB)
        P = self.PerturbDict.get(keyP)
        if B and P: 
            return (P - B) / B
        else:
            return None
    #
#
senseDir = 'd:/delta/models/studies/2010-Calibration/SensitivityTests/'
SC = SenseCompare(senseDir)
f = open(senseDir+'Results', 'w')
perturb = 'DISP'
#for param in ['EC','FLOW','STAGE']:
for param in ['EC']:
    for loc in SC.paramDict[param]:
        for chan in range(1,575+1):
            chanS = '%03d' % chan
            x = SC.Sense(Loc=loc,Param=param,Perturb=perturb, Obj='CH',ID=chanS)
            if x: f.write('%-5s %-15s %-10s %03d %15.8e \n' % \
                          (param, loc.strip(), perturb, chan, x))
f.close()
print 'Finished.'
