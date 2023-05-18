## modified by Hao Xie 10/24/2008: EC@48km = 5%(EC@0km-EC@54km)+EC@54km. Interpolation between EC@48 and EC@54 
## both daily X2 and monthly X2 are generated.


import sys,pdb
from vutils import *
from vista.set import MultiIterator, DataReference
from jarray import zeros
from vdss import RegularTimeSeries,opendss,findpath,writedss
from vmath import per_avg
from vtimeseries import timewindow

def loadNodeData(inputdss, tw, reach):

  # The nodeData array contains node data for nodes from downstream (ocean side) to upstream:
  #   node number, channel_length (to present node from previous node) in feet,
  #   node location in miles from Golden Gate, and dss path name.

  sjrNodeData=[
[361		,	0	,	33.56	,	    '//361/EC//1day///']	,
    [360	,	5398	,	34.58	,	    '//360/EC//1day///']	,
[440-13120	,	1680	,	34.90	,	    '//440_13120/EC//1day///']	,
[440-11480	,	1640	,	35.21	,	    '//440_11480/EC//1day///']	,
[440-9840	,	1640	,	35.52	,	    '//440_9840/EC//1day///']	,
[440-8200	,	1640	,	35.83	,	    '//440_8200/EC//1day///']	,
[440-6560	,	1640	,	36.14	,	    '//440_6560/EC//1day///']	,
[440-4920	,	1640	,	36.45	,	    '//440_4920/EC//1day///']	,
[440-3280	,	1640	,	36.76	,	    '//440_3280/EC//1day///']	,
[440-1640	,	1640	,	37.07	,	    '//440_1640/EC//1day///']	,
    [359	,	1640	,	37.39	,	    '//359/EC//1day///']	,
[439-9840	,	360	,	37.45	,	    '//439_9840/EC//1day///']	,
[439-8200	,	1640	,	37.76	,	    '//439_8200/EC//1day///']	,
[439-6560	,	1640	,	38.07	,	    '//439_6560/EC//1day///']	,
[439-4920	,	1640	,	38.39	,	    '//439_4920/EC//1day///']	,
[439-3280	,	1640	,	38.70	,	    '//439_3280/EC//1day///']	,
[439-1640	,	1640	,	39.01	,	    '//439_1640/EC//1day///']	,
    [358	,	1640	,	39.32	,	    '//358/EC//1day///']	,
[438-9840	,	2260	,	39.75	,	    '//438_9840/EC//1day///']	,
[438-8200	,	1640	,	40.06	,	    '//438_8200/EC//1day///']	,
[438-6560	,	1640	,	40.37	,	    '//438_6560/EC//1day///']	,
[438-4920	,	1640	,	40.68	,	    '//438_4920/EC//1day///']	,
[438-3280	,	1640	,	40.99	,	    '//438_3280/EC//1day///']	,
[438-1640	,	1640	,	41.30	,	    '//438_1640/EC//1day///']	,
    [362	,	1640	,	41.61	,	    '//362/EC//1day///']	,
[443-9840	,	1660	,	41.92	,	    '//443_9840/EC//1day///']	,
[443-8200	,	1640	,	42.23	,	    '//443_8200/EC//1day///']	,
[443-6560	,	1640	,	42.54	,	    '//443_6560/EC//1day///']	,
[443-4920	,	1640	,	42.86	,	    '//443_4920/EC//1day///']	,
[443-3280	,	1640	,	43.17	,	    '//443_3280/EC//1day///']	,
[443-1640	,	1640	,	43.48	,	    '//443_1640/EC//1day///']	,
    [357	,	1640	,	43.79	,	    '//357/EC//1day///']	,
[437-19680	,	520	,	43.89	,	    '//437_19680/EC//1day///']	,
[437-18040	,	1640	,	44.20	,	    '//437_18040/EC//1day///']	,
[437-16400	,	1640	,	44.51	,	    '//437_16400/EC//1day///']	,
[437-14760	,	1640	,	44.82	,	    '//437_14760/EC//1day///']	,
[437-13120	,	1640	,	45.13	,	    '//437_13120/EC//1day///']	,
[437-11480	,	1640	,	45.44	,	    '//437_11480/EC//1day///']	,
[437-9840	,	1640	,	45.75	,	    '//437_9840/EC//1day///']	,
[437-8200	,	1640	,	46.06	,	    '//437_8200/EC//1day///']	,
[437-6560	,	1640	,	46.37	,	    '//437_6560/EC//1day///']	,
[437-4920	,	1640	,	46.68	,	    '//437_4920/EC//1day///']	,
[437-3280	,	1640	,	46.99	,	    '//437_3280/EC//1day///']	,
[437-1640	,	1640	,	47.30	,	    '//437_1640/EC//1day///']	,
    [356        ,       1640    ,       47.61   ,           '//356/EC//1day///']        , # beginning of SJR channels
[288-9840       ,       483     ,       47.70   ,           '//288_9840/EC//1day///']   ,
[288-8200       ,       1640    ,       48.01   ,           '//288_8200/EC//1day///']   ,
[288-6560       ,       1640    ,       48.32   ,           '//288_6560/EC//1day///']   ,
[288-4920       ,       1640    ,       48.63   ,           '//288_4920/EC//1day///']   ,
[288-3280       ,       1640    ,       48.94   ,           '//288_3280/EC//1day///']   ,
[288-1640       ,       1640    ,       49.25   ,           '//288_1640/EC//1day///']   ,
    [463        ,       1640    ,       49.57   ,           '//463/EC//1day///']        ,
[287-4920       ,       1203    ,       49.79   ,           '//287_4920/EC//1day///']   ,
[287-3280       ,       1640    ,       50.10   ,           '//287_3280/EC//1day///']   ,
[287-1640       ,       1640    ,       50.41   ,           '//287_1640/EC//1day///']   ,
    [47         ,       1640    ,       50.72   ,           '//47/EC//1day///']         ,
[52-8200        ,       583     ,       50.84   ,           '//52_8200/EC//1day///']       ,
[52-6560        ,       1640    ,       51.15   ,           '//52_6560/EC//1day///']       ,
[52-4920        ,       1640    ,       51.46   ,           '//52_4920/EC//1day///']       ,
[52-3280        ,       1640    ,       51.77   ,           '//52_3280/EC//1day///']       ,
[52-1640        ,       1640    ,       52.08   ,           '//52_1640/EC//1day///']       ,
    [463        ,       1640    ,       52.39   ,           '//463/EC//1day///']           ,
[51-11480       ,       59      ,       52.40   ,           '//51_11480/EC//1day///']      ,
[51-9840        ,       1640    ,       52.71   ,           '//51_9840/EC//1day///']       ,
[51-8200        ,       1640    ,       53.02   ,           '//51_8200/EC//1day///']       ,
[51-6560        ,       1640    ,       53.33   ,           '//51_6560/EC//1day///']       ,
[51-4920        ,       1640    ,       53.64   ,           '//51_4920/EC//1day///']       ,
[51-3280        ,       1640    ,       53.95   ,           '//51_3280/EC//1day///']       ,
[51-1640        ,       1640    ,       54.26   ,           '//51_1640/EC//1day///']       ,    
    [461        ,       1640    ,       54.57   ,           '//461/EC//1day///']           ,
[50-8200        ,       1521    ,       54.86   ,           '//50_8200/EC//1day///']       ,
[50-6560        ,       1640    ,       55.17   ,           '//50_6560/EC//1day///']       ,
[50-4920        ,       1640    ,       55.48   ,           '//50_4920/EC//1day///']       ,
[50-3280        ,       1640    ,       55.79   ,           '//50_3280/EC//1day///']       ,
[50-1640        ,       1640    ,       56.10   ,           '//50_1640/EC//1day///']       ,
    [45         ,       1640    ,       56.41   ,           '//45/EC//1day///']            ,
[49-11480       ,       1468    ,       56.69   ,           '//49_11480/EC//1day///']      ,
[49-9840        ,       1640    ,       57.00   ,           '//49_9840/EC//1day///']       ,    
[49-8200        ,       1640    ,       57.31   ,           '//49_8200/EC//1day///']       ,
[49-6560        ,       1640    ,       57.62   ,           '//49_6560/EC//1day///']       ,
[49-4920        ,       1640    ,       57.94   ,           '//49_4920/EC//1day///']       ,
[49-3280        ,       1640    ,       58.25   ,           '//49_3280/EC//1day///']       ,
[49-1640        ,       1640    ,       58.56   ,           '//49_1640/EC//1day///']       ,
    [469        ,       1640    ,       58.87   ,           '//469/EC//1day///']           ,
[83-4920        ,       530     ,       58.97   ,           '//83_4920/EC//1day///']       ,
[83-3280        ,       1640    ,       59.28   ,           '//83_3280/EC//1day///']       ,
[83-1640        ,       1640    ,       59.59   ,           '//83_1640/EC//1day///']       ,
    [44         ,       1640    ,       59.90   ,           '//44/EC//1day///']            ,
[48-8200        ,       815     ,       60.05   ,           '//48_8200/EC//1day///']       ,
[48-6560        ,       1640    ,       60.36   ,           '//48_6560/EC//1day///']       ,
[48-4920        ,       1640    ,       60.67   ,           '//48_4920/EC//1day///']       ,
[48-3280        ,       1640    ,       61.99   ,           '//48_3280/EC//1day///']       ,
[48-1640        ,       1640    ,       61.30   ,           '//48_1640/EC//1day///']       ,
    [43         ,       1640    ,       61.61   ,           '//43/EC//1day///']            ,
[47-13120       ,       1128    ,       61.82   ,           '//47_13120/EC//1day///']      ,
[47-11480       ,       1640    ,       62.13   ,           '//47_11480/EC//1day///']      ,
[47-9840        ,       1640    ,       62.44   ,           '//47_9840/EC//1day///']       ,
[47-8200        ,       1640    ,       62.75   ,           '//47_8200/EC//1day///']       ,
[47-6560        ,       1640    ,       63.06   ,           '//47_6560/EC//1day///']       ,
[47-4920        ,       1640    ,       63.37   ,           '//47_4920/EC//1day///']       ,
[47-3280        ,       1640    ,       63.68   ,           '//47_3280/EC//1day///']       ,
[47-1640        ,       1640    ,       63.99   ,           '//47_1640/EC//1day///']       ,
    [42         ,       1640    ,       64.31   ,           '//42/EC//1day///']            ,
[46-11480       ,       300     ,       64.36   ,           '//46_11480/EC//1day///']      ,
[46-9840        ,       1640    ,       64.67   ,           '//46_9840/EC//1day///']       ,
[46-8200        ,       1640    ,       64.98   ,           '//46_8200/EC//1day///']       ,
[46-6560        ,       1640    ,       65.29   ,           '//46_6560/EC//1day///']       ,
[46-4920        ,       1640    ,       65.60   ,           '//46_4920/EC//1day///']       ,
[46-3280        ,       1640    ,       65.91   ,           '//46_3280/EC//1day///']       ,
[46-1640        ,       1640    ,       66.23   ,           '//46_1640/EC//1day///']       ,
    [41         ,       1640    ,       66.53   ,           '//41/EC//1day///']            ,
[45-11480       ,       941     ,       66.71   ,           '//45_11480/EC//1day///']      ,
[45-9840        ,       1640    ,       67.02   ,           '//45_9840/EC//1day///']       ,
[45-8200        ,       1640    ,       67.34   ,           '//45_8200/EC//1day///']       ,
[45-6560        ,       1640    ,       67.65   ,           '//45_6560/EC//1day///']       ,    
[45-4920        ,       1640    ,       67.96   ,           '//45_4920/EC//1day///']       ,
[45-3280        ,       1640    ,       68.27   ,           '//45_3280/EC//1day///']       ,
[45-1640        ,       1640    ,       68.58   ,           '//45_1640/EC//1day///']       ,    
    [40         ,       1640    ,       68.89   ,           '//40/EC//1day///']            ,
  ]
  
  sacNodeData=[
[361		,	0	,	33.56	,	    '//361/EC//1day///']	,
    [360	,	5398	,	34.58	,	    '//360/EC//1day///']	,
[440-13120	,	1680	,	34.90	,	    '//440_13120/EC//1day///']	,
[440-11480	,	1640	,	35.21	,	    '//440_11480/EC//1day///']	,
[440-9840	,	1640	,	35.52	,	    '//440_9840/EC//1day///']	,
[440-8200	,	1640	,	35.83	,	    '//440_8200/EC//1day///']	,
[440-6560	,	1640	,	36.14	,	    '//440_6560/EC//1day///']	,
[440-4920	,	1640	,	36.45	,	    '//440_4920/EC//1day///']	,
[440-3280	,	1640	,	36.76	,	    '//440_3280/EC//1day///']	,
[440-1640	,	1640	,	37.07	,	    '//440_1640/EC//1day///']	,
    [359	,	1640	,	37.39	,	    '//359/EC//1day///']	,
[439-9840	,	360	,	37.45	,	    '//439_9840/EC//1day///']	,
[439-8200	,	1640	,	37.76	,	    '//439_8200/EC//1day///']	,
[439-6560	,	1640	,	38.07	,	    '//439_6560/EC//1day///']	,
[439-4920	,	1640	,	38.39	,	    '//439_4920/EC//1day///']	,
[439-3280	,	1640	,	38.70	,	    '//439_3280/EC//1day///']	,
[439-1640	,	1640	,	39.01	,	    '//439_1640/EC//1day///']	,
    [358	,	1640	,	39.32	,	    '//358/EC//1day///']	,
[438-9840	,	2260	,	39.75	,	    '//438_9840/EC//1day///']	,
[438-8200	,	1640	,	40.06	,	    '//438_8200/EC//1day///']	,
[438-6560	,	1640	,	40.37	,	    '//438_6560/EC//1day///']	,
[438-4920	,	1640	,	40.68	,	    '//438_4920/EC//1day///']	,
[438-3280	,	1640	,	40.99	,	    '//438_3280/EC//1day///']	,
[438-1640	,	1640	,	41.30	,	    '//438_1640/EC//1day///']	,
    [362	,	1640	,	41.61	,	    '//362/EC//1day///']	,
[443-9840	,	1660	,	41.92	,	    '//443_9840/EC//1day///']	,
[443-8200	,	1640	,	42.23	,	    '//443_8200/EC//1day///']	,
[443-6560	,	1640	,	42.54	,	    '//443_6560/EC//1day///']	,
[443-4920	,	1640	,	42.86	,	    '//443_4920/EC//1day///']	,
[443-3280	,	1640	,	43.17	,	    '//443_3280/EC//1day///']	,
[443-1640	,	1640	,	43.48	,	    '//443_1640/EC//1day///']	,
    [357	,	1640	,	43.79	,	    '//357/EC//1day///']	,
[437-19680	,	520	,	43.89	,	    '//437_19680/EC//1day///']	,
[437-18040	,	1640	,	44.20	,	    '//437_18040/EC//1day///']	,
[437-16400	,	1640	,	44.51	,	    '//437_16400/EC//1day///']	,
[437-14760	,	1640	,	44.82	,	    '//437_14760/EC//1day///']	,
[437-13120	,	1640	,	45.13	,	    '//437_13120/EC//1day///']	,
[437-11480	,	1640	,	45.44	,	    '//437_11480/EC//1day///']	,
[437-9840	,	1640	,	45.75	,	    '//437_9840/EC//1day///']	,
[437-8200	,	1640	,	46.06	,	    '//437_8200/EC//1day///']	,
[437-6560	,	1640	,	46.37	,	    '//437_6560/EC//1day///']	,
[437-4920	,	1640	,	46.68	,	    '//437_4920/EC//1day///']	,
[437-3280	,	1640	,	46.99	,	    '//437_3280/EC//1day///']	,
[437-1640	,	1640	,	47.30	,	    '//437_1640/EC//1day///']	,
    [356	,	1640	,	47.61	,	    '//356/EC//1day///']	,
[291-3280	,	1377	,	47.87	,	    '//291_3280/EC//1day///']	,
[291-1640	,	1640	,	48.18	,	    '//291_1640/EC//1day///']	,
    [465	,	1640	,	48.49	,	    '//465/EC//1day///']	,
[290-4920	,	1306	,	48.74	,	    '//290_4920/EC//1day///']	,
[290-3280	,	1640	,	49.05	,	    '//290_3280/EC//1day///']	,
[290-1640	,	1640	,	49.36	,	    '//290_1640/EC//1day///']	,
    [464	,	1640	,	49.67	,	    '//464/EC//1day///']	,
[436-6560	,	534	,	49.77	,	    '//436_6560/EC//1day///']	,
[436-4920	,	1640	,	50.09	,	    '//436_4920/EC//1day///']	,
[436-3280	,	1640	,	50.40	,	    '//436_3280/EC//1day///']	,
[436-1640	,	1640	,	50.71	,	    '//436_1640/EC//1day///']	,
    [355	,	1640	,	51.02	,	    '//355/EC//1day///']	,
[435-9840	,	987	,	51.20	,	    '//435_9840/EC//1day///']	,
[435-8200	,	1640	,	51.51	,	    '//435_8200/EC//1day///']	,
[435-6560	,	1640	,	51.83	,	    '//435_6560/EC//1day///']	,
[435-4920	,	1640	,	52.14	,	    '//435_4920/EC//1day///']	,
[435-3280	,	1640	,	52.45	,	    '//435_3280/EC//1day///']	,
[435-1640	,	1640	,	52.76	,	    '//435_1640/EC//1day///']	,
    [354	,	1640	,	53.07	,	    '//354/EC//1day///']	,
[434-18040	,	2101	,	53.47	,	    '//434_18040/EC//1day///']	,
[434-16400	,	1640	,	53.78	,	    '//434_16400/EC//1day///']	,
[434-14760	,	1640	,	54.09	,	    '//434_14760/EC//1day///']	,
[434-13120	,	1640	,	54.40	,	    '//434_13120/EC//1day///']	,
[434-11480	,	1640	,	54.71	,	    '//434_11480/EC//1day///']	,
[434-9840	,	1640	,	55.02	,	    '//434_9840/EC//1day///']	,
[434-8200	,	1640	,	55.33	,	    '//434_8200/EC//1day///']	,
[434-6560	,	1640	,	55.64	,	    '//434_6560/EC//1day///']	,
[434-4920	,	1640	,	55.95	,	    '//434_4920/EC//1day///']	,
[434-3280	,	1640	,	56.26	,	    '//434_3280/EC//1day///']	,
[434-1640	,	1640	,	56.57	,	    '//434_1640/EC//1day///']	,
    [353	,	1640	,	56.88	,	    '//353/EC//1day///']	,
[433-11480	,	654	,	57.01	,	    '//433_11480/EC//1day///']	,
[433-9840	,	1640	,	57.32	,	    '//433_9840/EC//1day///']	,
[433-8200	,	1640	,	57.63	,	    '//433_8200/EC//1day///']	,
[433-6560	,	1640	,	57.94	,	    '//433_6560/EC//1day///']	,
[433-4920	,	1640	,	58.25	,	    '//433_4920/EC//1day///']	,
[433-3280	,	1640	,	58.56	,	    '//433_3280/EC//1day///']	,
[433-1640	,	1640	,	58.87	,	    '//433_1640/EC//1day///']	,
    [352	,	1640	,	59.18	,	    '//352/EC//1day///']	,
[431-11480	,	2303	,	59.62	,	    '//431_11480/EC//1day///']	,
[431-9840	,	1640	,	59.93	,	    '//431_9840/EC//1day///']	,
[431-8200	,	1640	,	60.24	,	    '//431_8200/EC//1day///']	,
[431-6560	,	1640	,	60.55	,	    '//431_6560/EC//1day///']	,
[431-4920	,	1640	,	60.86	,	    '//431_4920/EC//1day///']	,
[431-3280	,	1640	,	61.17	,	    '//431_3280/EC//1day///']	,
[431-1640	,	1640	,	61.48	,	    '//431_1640/EC//1day///']	,
    [351	,	1640	,	61.79	,	    '//351/EC//1day///']	,
[430-13120	,	811	,	61.94	,	    '//430_13120/EC//1day///']	,
[430-11480	,	1640	,	62.26	,	    '//430_11480/EC//1day///']	,
[430-9840	,	1640	,	62.57	,	    '//430_9840/EC//1day///']	,
[430-8200	,	1640	,	62.88	,	    '//430_8200/EC//1day///']	,
[430-6560	,	1640	,	63.19	,	    '//430_6560/EC//1day///']	,
[430-4920	,	1640	,	63.50	,	    '//430_4920/EC//1day///']	,
[430-3280	,	1640	,	63.81	,	    '//430_3280/EC//1day///']	,
[430-1640	,	1640	,	64.12	,	    '//430_1640/EC//1day///']	,
    [350	,	1640	,	64.43	,	'//350/EC//1day///'] 
  ]


  nodeData = None
  if reach.lower() in 'sac'>0:
    nodeData = sacNodeData
  elif reach.lower() in 'sjr':
    nodeData = sjrNodeData
  else:
    print('error: reach is not sac or sjr. exiting.')
    print('reach='+reach)
    exit(0)
    
  #tw=timewindow('01NOV1922 0000 - 30SEP2015 2400')
  indss=opendss(inputdss)
  #NDOpath = '//441_5398PER-AVG/FLOW//1DAY//' #+FPARTre+'/'  
  #print 'NDOpath:', NDOpath
  #NDOrts=DataReference.create(findpath(indss,NDOpath)[0],tw).getData()
  nnode=len(nodeData)
  rtsarray=[]
  chanLength=zeros(nnode,'d')
  nodeDistance=zeros(nnode,'d')
  for nn in range(nnode):
    nodeSpecs=nodeData[nn]
    chanLength[nn]=nodeSpecs[1]
    nodeDistance[nn]=nodeSpecs[2]
    dsspath=nodeSpecs[3]
    #if(nn ==1):    dsspath='//430_11480/EC//1day///'    
    print dsspath    
    #    pdb.set_trace()
    print('about to get data for file, pathname, tw='+str(indss)+','+str(dsspath)+','+str(tw))
    print('findpath(indss,dsspath)='+str(findpath(indss,dsspath)))

    rts=DataReference.create(findpath(indss,dsspath)[0],tw).getData()
    if rts is None:
      raise 'Path not found: ' + dsspath
    #rtsarray.append(findpath(indss,dsspath)[0].getData())
    rtsarray.append(rts)
  return[ chanLength, nodeDistance, rtsarray]
  #
#

def calcX2(inputdss,outputdss,FPART, tw, reach):
  """
  """
  print "Loading EC data..."
  [chanLength, nodeDistance, rtsarray] = loadNodeData(inputdss, tw, reach)
  print "Calculating X2..."
  x2Value = 2640.                       # Value of salinity representing X2
  feetMile = 5280.                      # Number of feet per mile
  kmMile = 1.609                        # Number of km per mile
  oceansalt = 43000.
  mi = MultiIterator(rtsarray)
  nnode=len(nodeDistance)
  x2=zeros( rtsarray[0].size() , 'd')
  t=0
  while not mi.atEnd():
    #
    # Salt is a tuple of dimension nn+1 with first dimension = time and
    # next nn dimensions equal to the values in the time series.
    salt = mi.getElement()
    backsalt = 0.
    x2[t]=-901.
    
    gotX2=0
    tempX2=0.
    
    for nn in range(nnode):
      thissalt = salt.getY(nn)
      
      if (thissalt >= 0. and backsalt > x2Value and thissalt <= x2Value): 
        # if x2 is between this node and previous node        
        ratio = (x2Value - thissalt)/(backsalt - thissalt)
        
        ##x=chanLength[nn]*(1.-ratio)  shengjun comment out

        if (gotX2 >0):
           # average the two X2 location
           
           tempX2=nodeDistance[nn] - ratio*chanLength[nn]/feetMile           
           tempX2=tempX2*kmMile
           print "-- X2 found again at Node# with backsalt and thissalt:", nn, tempX2, backsalt, thissalt
           x2[t]=(x2[t]+tempX2)/2.0
           
        else:
        
           x2[t]=nodeDistance[nn] - ratio*chanLength[nn]/feetMile
           
           x2[t]=x2[t]*kmMile
           
           print "-- X2 found at Node# with backsalt and thissalt:", nn, x2[t], backsalt, thissalt                                                                               
        
	gotX2=gotX2+1 # count how manytime X2 is found in one loop                               


      elif (backsalt > 0. and backsalt < x2Value and thissalt >= x2Value): #shengjun add to address salt reversal condition
        # if x2 is between this node and previous node        
        ratio = (x2Value - thissalt)/(backsalt - thissalt)
        
        if (gotX2 >0):
           # average the two X2 location
           
           tempX2=nodeDistance[nn] - ratio*chanLength[nn]/feetMile           
           tempX2=tempX2*kmMile
           print "** X2 found again in EC reversal range at Node# and distance, with backsalt and thissalt:", nn, tempX2, backsalt, thissalt
           x2[t]=(x2[t]+tempX2)/2.0
           
        else:
        
           x2[t]=nodeDistance[nn] - ratio*chanLength[nn]/feetMile
           
           x2[t]=x2[t]*kmMile
           
           print "** X2 found at Node# and distance with backsalt and thissalt:", nn, x2[t], backsalt, thissalt          
        
	gotX2=gotX2+1 # count how manytime X2 is found in one loop                               
                                                              
      backsalt = thissalt
    
    if (gotX2 >1): print "***** No of times X2 found and average X2:", gotX2, x2[t] #shengjun
    
    ###
    if (x2[t] == -901.): # interpolate between Mtz and ocean if X2 line does not exist between node 350 and 361
      EC48=(oceansalt-salt.getY(0))*0.05+salt.getY(0)
      
      # if (EC48>=x2Value):
      x2[t]=(EC48-x2Value)/(EC48-salt.getY(0)) * (nodeDistance[0]-48.0/kmMile)+48.0/kmMile
      x2[t]=x2[t]*kmMile
      
      # else:
      #     x2[t]=(oceansalt-x2Value)/(oceansalt-EC48) * 48.0/kmMile    
      #     x2[t]=x2[t]*kmMile
      
      print '******oceansalt used. at step t, x2: ',t,x2[t] 
    ##
        
    mi.advance()
    t=t+1
  ###
  x2rts = RegularTimeSeries( "X2 position", rtsarray[0].getStartTime().toString(),
                             rtsarray[0].getTimeInterval().toString(), x2)    
  x2rts_perave = per_avg(x2rts,'1MONTH')
  #NDOrts_perave = per_avg(NDOrts,'1MONTH')

  writedss(outputdss, '/DELTA/X2-'+reach+'/POSITION///'+FPART+'/', x2rts)  
  writedss(outputdss, '/DELTA/X2-'+reach+'/POSITION///'+FPART+'/', x2rts_perave)
  #writedss(outputdss, '/DELTA/NDO/CFS///'+FPART+'/', NDOrts_perave)

###
if __name__ == '__main__':
  print('num args = '+str(len(sys.argv)))
  if len(sys.argv) != 6:
    raise SystemExit("""
    Usage: vscript.bat x2.py dssinfile dssoutfile F_PART timewindow reach
    where dssinfile is the input file with DSM2 output
    and dssoutfile is the output file for X2 info
    and F_PART is the dss f part for the output dss file
    and timewindow is the timewindow
    and reach is either 'sac' or 'sjr'
    Also, check the FPart and DSS pathnames inside x2.py
    """)
  else:
    FPART=sys.argv[3]
    #Fpart_suffix = '+from-all'
    # escape r.e. special characters
    #FPARTre=re.sub('\+','\+',FPART)
    #FPARTre=re.sub('\/\/','/.*/',FPARTre)

    #tw=timewindow('01NOV1922 0000 - 30SEP2015 2400')
    tw=timewindow(sys.argv[4])
    reach=sys.argv[5]

    print('reach='+reach)
    calcX2(sys.argv[1],sys.argv[2],FPART,tw,reach)
  sys.exit()        
