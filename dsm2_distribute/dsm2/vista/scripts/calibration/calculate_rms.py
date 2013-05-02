from gov.ca.dsm2.input.parser import Parser
if __name__=='__main__':
    base_dir="z:/calibration/"
    setup_file='setup.inp'
    p=Parser()
    inputTables=p.parseModel(base_dir+setup_file)
    table = inputTables.getTableNamed("CHANNEL_GROUPS");
    nrows=len(table.getValues())
    groups={}
    for i in range(nrows):
        gname=table.getValue(i,"GROUP_NAME")
        groups[gname]=gname
    print groups
