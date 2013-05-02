"""
Script to check PTM mass balance.
"""
import sys
from vdss import opendss,findpath

total_pct = 95.0
nodes = ['7', '353', '350', '321', '335', '45', '272', '341', '249', '21', '355']
#nodes = ['7']
first_4days = ["01", "02","03", "04"]

def dss_retrieve_ts(file, path):
    f=opendss(file)
    g=findpath(f,path)
    if g==None or len(g) != 1:
        raise ValueError("Path %s in file s% does not exist or is not unique" % (path,file))
    return g[0].getData()



def check_ptm(calsimfile, node, f_out, f_out_day):
	boundary_path="/PTM7.2/ALL_BOUNDARY_node/PTM_FLUX//15MIN//".replace("node", node)
	print boundary_path
	f_out.write("date and time"+"\t"+"total pct particles (full_domain + boundary flux)"+"\n")
	f_out_day.write("date and time"+"\t"+"total pct particles (full_domain + boundary flux)"+"\n")
	p_out=dss_retrieve_ts(calsimfile, boundary_path)
	full_domain_path="/PTM7.2/FULL_DOMAIN_node/PTM_GROUP//15MIN//".replace("node", node)
	p_in_domain=dss_retrieve_ts(calsimfile, full_domain_path)
	p_total = p_in_domain + p_out
	for p in p_total:
		t = p.getXString()
		y = p.getY()
		if (y<total_pct and y>0 and (t[0:2] not in first_4days)):
			f_out.write(t+"\t"+str(y)+"\n")
			if t.find("0015")>0:
				f_out_day.write(t+"\t"+str(y)+"\n")
            
	
	
	
use = '''
Usage:
vscript check_ptm.py ptm_out_dss study_number
'''
def main():
    print sys.argv
    ptm_out_file = sys.argv[1]
    study = sys.argv[2]
	
    for node in nodes:
        f_out=open(study+'_ptm_errors_at_node'+node+'.txt','w')
        f_out_day=open(study+'_ptm_errors_daily_at_node'+node+'.txt','w')
        check_ptm(ptm_out_file, node, f_out, f_out_day)
        f_out.close()
        f_out_day.close()
    sys.exit()
        
if __name__ == '__main__':
    main()
