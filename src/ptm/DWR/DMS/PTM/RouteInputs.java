/**
 * 
 */
package DWR.DMS.PTM;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Calendar;
import java.util.Iterator;
/**
 * @author xwang
 *
 */
public class RouteInputs {

	/**
	 * 
	 */
	public RouteInputs() {
		// TODO Auto-generated constructor stub
	}
	public RouteInputs(ArrayList<String> inText) {
		_barriers = new  HashMap<String, NonPhysicalBarrier>();
		//name vs node is unique.  but different name can return the same nodeId
		//name vs. wb is unique, but different name could return the same wbId
		// because nodeId WbId pair is unique and has a uique name
		_nameNodeLookup = new HashMap<String, Integer>();
		_nameWbLookup = new HashMap<String, Integer>();
		setBarriers(PTMUtil.getInputBlock(inText, "BARRIERS", "END_BARRIERS"));

	}
	public void addSpecialBehaviors(RouteHelper rh, String particleType){
		boolean containGSJ = false;
		if (_nameNodeLookup == null)
			System.err.println("WARNING: Could not find any special behavior!!!");
		else if (particleType.equalsIgnoreCase("SALMON")){
			Iterator<String> it = _nameNodeLookup.keySet().iterator();
			while (it.hasNext()){
				String key = it.next();
				// do switch according to key.  for now only one behavior for GSJunction involved
				if (key.equalsIgnoreCase("GSJUNCTION")){
					int nId = _nameNodeLookup.get(key);
					((SalmonRouteHelper)rh).addSpecialBehavior(nId, new SalmonGSJRouteBehavior(nId,_nameWbLookup.get(key)));
					containGSJ = true; 
				}
			}
			if (!containGSJ)
					System.err.println("WARNING: Special route behavior for Georgian Slough Junction not found!!!");
		}
		// here you can add more special behaviors for different species.
		
	}
	public void setNodeInfo(Node[] allNodes, int nodeNum){
	    // like wbArray, nodeArray starts from 1 PTMFixedInput.java line 287
		for(int i=1; i < nodeNum+1; i++){
	        if ( allNodes[i] == null ){
	        	System.err.println("Node,"+ i + "is null!");
	        	continue;
	        }
	        for(int j=0; j < allNodes[i].getNumberOfWaterbodies(); j++){
	      	  int wbId = allNodes[i].getWaterbodyEnvIndex(j);
	      	  if (_barriers != null && _barriers.get(Integer.toString(i)+"_"+Integer.toString(wbId)) != null)
	      		allNodes[i].installBarrier();
	        }
		}
	}
	public void setChannelInfo(Waterbody[] allChans, int chanNum){
		//wbArray start from 1 see PTMFixedInput.java line 180
		//Channels are first filled in wbArray
		for (int i = 1; i<=chanNum; i++){
		  Channel aChan = (Channel) allChans[i];
		  if ( aChan == null ){
	        	System.err.println("Channel,"+ i + "is null!");
	        	continue;
	      }
		  int nodeNum = aChan.getNumberOfNodes();
		  for(int j=0; j< nodeNum; j++){
			  int nodeId = aChan.getNodeEnvIndex(j);				  
			  if (_barriers != null && _barriers.get(Integer.toString(nodeId)+"_"+Integer.toString(i)) != null){
				  if (nodeId == aChan.getUpNodeId())
					  aChan.installBarrierAtUpNode();
				  if (nodeId == aChan.getDownNodeId())
					  aChan.installBarrierAtDownNode();  
			  }
		  }
		}
	}
	public void updateCurrentInfo(Node[] allNodes, int nodeNum, Waterbody[] allChans, int chanNum, int currentTime){
		//update barrier op info
		for(int channelNumber=1; channelNumber <= chanNum; channelNumber++){
			Channel chan = ((Channel) allChans[channelNumber]);
			int chanEnvId = chan.getEnvIndex();
    		if (chan.isBarrierAtUpNodeInstalled())
    			chan.setBarrierAtUpNodeOp(getBarrier(chan.getUpNodeId(), 
    					chanEnvId).getBarrierOp(PTMUtil.convertHecTime(currentTime)));
    		else if (chan.isBarrierAtDownNodeInstalled())
    			chan.setBarrierAtDownNodeOp(getBarrier(chan.getDownNodeId(), 
    					chanEnvId).getBarrierOp(PTMUtil.convertHecTime(currentTime)));
		}
	}
	public HashMap<String, NonPhysicalBarrier> getBarriers(){ return _barriers;}
	public HashMap<String, Integer> getNameNodeLookup() { return _nameNodeLookup; }
	private NonPhysicalBarrier getBarrier(int nodeId, int chanId){
		NonPhysicalBarrier npb = _barriers.get(PTMUtil.concatNodeWbIds(nodeId, chanId));
		if (npb == null)
			PTMUtil.systemExit("Barrier is not setup, exit from PTMHydroInput line 232.");
		return npb;
	  }
	private void setBarriers(ArrayList<String> inText){
		// first line of inText is number_of_barriers: number
		int numberOfBarriers = getNumberOfBarriers(inText.get(0).trim());
		for (int i = 1; i<numberOfBarriers+1; i++){
			setBarrier(PTMUtil.getInputBlock(inText, "BARRIER"+i, "END_BARRIER"+i));
		}	
	}
	private int getNumberOfBarriers(String numberLine){
		int number = 0;
		try{
			String[] items = numberLine.split("[,:\\s\\t]+");
			number = Integer.parseInt(items[1]);
		}catch (NumberFormatException e){
			e.printStackTrace();
			PTMUtil.systemExit("Couldn't find number of barriers in the behavior input file!");	
		}
		return number;
	}
	private void setBarrier(ArrayList<String> barrierText){
		String[] items = barrierText.get(0).trim().split("[,\\s\\t]+");
		// first line has to be name nodeID, wbID
		if (!items[0].trim().equalsIgnoreCase("NAME")&&!items[1].trim().equalsIgnoreCase("NODEID")&&items[2].trim().equalsIgnoreCase("CHANNELID")){
			PTMUtil.systemExit("behavior route input block has wrong format:"+items[0]+" "+items[1]+" "+items[2]+" should be Name NodeID ChannelID");
		}
		items = barrierText.get(1).trim().split("[,\\s\\t]+");
		int nodeId=-99, wbId=-99;		
		try{
			if (items.length<3)
				throw new NumberFormatException("Barrier Name, Node ID and WaterBady Id have wrong format!");
			
			// convert from external node channel number to internal node channel numbers
			nodeId = PTMHydroInput.getIntFromExtNode(Integer.parseInt(items[1]));
			wbId = PTMHydroInput.getIntFromExtChan(Integer.parseInt(items[2]));
		}catch(NumberFormatException e){
			e.printStackTrace();
			PTMUtil.systemExit("behavior route input block has wrong node and water body id numbers: " + barrierText.get(1));
		}
		_nameNodeLookup.put(items[0], nodeId);
		_nameWbLookup.put(items[0], wbId);
		addBarrier(barrierText, nodeId, wbId);
	}
	private void addBarrier(ArrayList<String> barrierBlock, int nodeId, int wbId){
		  HashMap<BarrierOpPeriod, Integer> bOpTS = new HashMap<BarrierOpPeriod, Integer>();
		  Calendar s_time = null, e_time = null;
		  int op = -99;
		  
		  // first 2 lines are barrier location info: name nodeid wbid
		  // the third line is operation schedule title.  operation schedule data start from forth line
		  // and the data end before line: end_barrieri
		  for (int i = 3; i<barrierBlock.size();i++){
			  String inLine = barrierBlock.get(i).trim();
			  String[] items = inLine.split("[,\\s\\t]+");
			  String[] dateStr = items[0].trim().split("/"), timeStr = items[1].trim().split(":");
			  int optemp = -99, year = -99, month = -99, day = -99, hour = -99, minute = -99;
			  try{
				  if (items.length<3 || dateStr.length<3 || timeStr.length<2)
					  throw new NumberFormatException();
				  optemp = Integer.parseInt(items[2].trim());
				  year = Integer.parseInt(dateStr[2]);
				  // java month start from 0
				  month = Integer.parseInt(dateStr[0])-1;
				  day = Integer.parseInt(dateStr[1]);
				  hour = Integer.parseInt(timeStr[0]);
				  minute = Integer.parseInt(timeStr[1]);
			  }catch (NumberFormatException e){
					e.printStackTrace();
					PTMUtil.systemExit("operation schecule line has wrong format: "+inLine);	
			  }
					  
			  e_time = Calendar.getInstance();
			  e_time.clear();
			  if (year == -99||month == -99|| day== -99|| hour== -99|| minute== -99||optemp == -99 )
				  PTMUtil.systemExit("wrong format for barrier operation schedule data: "+inLine);
			  e_time.set(year, month, day, hour, minute);
			  /*
			   * schedule example:
			   * Date		Time		On/Off
			   * 2/1/2011	0:00		0
			   * 3/17/2011	18:45		1
			   * 6/30/2011	0:00		0
			   * 
			   * from startTime(2/1/2011 0:00) - EndTime(3/17/2011 18:45) barrier off
			   * from startTime(3/17/2011 18:45) - EndTime(6/30/2011 0:00) barrier on
			   * operation at endTime takes the next time interval value, e.g. op at 3/17/2011 18:45 is on
			   * see BarrierOpPeriod for period treatment
			   */
			  if (s_time != null){
				  bOpTS.put(new BarrierOpPeriod(s_time, e_time), op);
				  //System.out.println(s_time.getTime()+"--"+e_time.getTime()+"  op:"+op);
			  }
			  op = optemp;
			  s_time = e_time;
		  }
		  _barriers.put(Integer.toString(nodeId)+"_"+Integer.toString(wbId), new NonPhysicalBarrier(nodeId, wbId, bOpTS));			  
	}
	// map key is name
	private HashMap<String, NonPhysicalBarrier> _barriers;
	private HashMap<String, Integer> _nameNodeLookup;
	private HashMap<String, Integer> _nameWbLookup;

}
