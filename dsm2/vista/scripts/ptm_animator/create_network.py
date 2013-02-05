import sys
from gov.ca.dsm2.input.parser import Parser
from com.ibm.util import CoordinateConversion
def latLngToUTM(lat,lng):
  cc=CoordinateConversion()
  str=cc.latLon2UTM(lat,lng)
  fields=str.split()
  return int(fields[2]),int(fields[3])
def parseInput(file):
  p=Parser()
  return p.parseModel(file)
def parseAndAddInput(file,tables):
  p=Parser()
  from java.io import FileInputStream
  fis = FileInputStream(file)
  p.parseAndAddToModel(tables,fis)
def loadChannelExt2IntMap(table):
  map={};
  vals=table.getValues();
  for i in range(len(vals)):
    map[table.getValue(i,'EXT')]=table.getValue(i,'INT')
  return map
def loadNodeExt2IntMap(table):
  map={};
  vals=table.getValues();
  for i in range(len(vals)):
    map[table.getValue(i,'EXT')]=table.getValue(i,'INT')
  return map
def writeNetwork(channels,nodes,reservoirs,chan_map,node_map):
  fh=open('network.dat','w')
  print >> fh, 'NODE_LIST'
  print >> fh, 'ID\tX\tY'
  for node in nodes.getNodes():
    id=node_map[node.id]
    x,y=latLngToUTM(node.latitude,node.longitude)
    print >> fh, '%s\t%f\t%f'%(id,x,y)
  #add the nodes for labels and flux element displays
  labelNodes=[(9001,37.8228,-121.7189),
              (9002,37.7973,-121.5713),
              (9003,38.020,-122.217),
              (9004,38.258,-121.893),
              (9005,38.561,-122.097),
              (9006,38.557,-121.965),
              (9010,38.540,-121.569),
              (9011,37.614,-121.436),
              (9012,38.005,-121.847),
              (9013,38.005,-122.138),
              (9014,38.314,-121.844),
              (9015,38.498,-121.735)
              ];
  for node in labelNodes:
      id=node[0]
      x,y=latLngToUTM(node[1],node[2])
      print >> fh, '%d\t%f\t%f'%(id,x,y)
  print >> fh, 'END NODE_LIST'
  print >> fh, 'CHANNEL_LIST'
  print >> fh, 'ID\tUP\tDOWN\tLENGTH'
  for channel in channels.getChannels():
    id=chan_map[channel.id]
    uid=node_map[channel.upNodeId]
    did=node_map[channel.downNodeId]
    l=channel.length
    print >> fh, '%s\t%s\t%s\t%s'%(id,uid,did,l)
  print >> fh, 'END CHANNEL_LIST'
  print >> fh, 'RESERVOIR_LIST'
  print >> fh, 'ID\tNODES'
  print >> fh, 'END RESERVOIR_LIST'
  fh.close()
def convertToNetwork(echo_file, gis_file, hydro_out_file):
  tables = parseInput(hydro_out_file)
  chan_geom_info=tables.getTableNamed('CHANNEL_GEOMETRY_INFO')
  chan_map=loadChannelExt2IntMap(chan_geom_info)
  node_info=tables.getTableNamed('NODES')
  node_map=loadNodeExt2IntMap(node_info)
  tables_hydro=parseInput(echo_file)
  parseAndAddInput(gis_file,tables_hydro)
  model=tables_hydro.toDSM2Model()
  channels = model.getChannels()
  nodes = model.getNodes()
  reservoirs=model.getReservoirs()
  writeNetwork(channels,nodes,reservoirs,chan_map,node_map)
if __name__=='__main__':
  if len(sys.argv) != 4:
      print """Usage: ptm_gen_network echo.inp gis.inp hydro.hof.inp
      echo.inp: hydro echo file
      gis.inp: gis input file from dsm2-grid-map
      hydro.hof.inp: edited hydro hof output file with node and channel info in .inp format
      """
      exit(1)
  echo_file=sys.argv[1]
  gis_file=sys.argv[2]
  hydro_out_file=sys.argv[3]
  convertToNetwork(echo_file,gis_file,hydro_out_file)
  exit(0)
#
