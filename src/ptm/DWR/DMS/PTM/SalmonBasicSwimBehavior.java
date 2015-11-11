/**
 * 
 */
package DWR.DMS.PTM;
import java.nio.IntBuffer;

/**
 * @author xwang
 *
 */
public class SalmonBasicSwimBehavior implements SalmonSwimBehavior {
	//TODO need to be very careful about class variables, they are visible to ALL particles!!!
	SalmonConfusionFactorCalculator _confusionCalc;
	SalmonSwimmingVelocityCalculator _swimCalc;
	SalmonHoldingTimeCalculator _holdingTimeCalc;
	BasicHydroCalculator _hydroCalc;
	private float _floodHoldVel = -999999.0f;
	private TravelTimeOutput _travelTimeOut;
	private float MISSING = -9999999999.0f;

	/**
	 * 
	 */
	public SalmonBasicSwimBehavior(SwimInputs si) { //TODO consider to pass in SalmonSwimHelper?  Helper as a mediator
		_confusionCalc = new SalmonConfusionFactorCalculator(si); //TODO consider to pass in Behavior? Behavior as a mediator
		_swimCalc = new SalmonSwimmingVelocityCalculator(si);
		_holdingTimeCalc = new SalmonHoldingTimeCalculator(si);
		_hydroCalc = new BasicHydroCalculator();
		_travelTimeOut = Globals.Environment.getBehaviorInputs().getTravelTimeOutput();
		_floodHoldVel = si.getFloodHoldingThreshold();
	}
	public void updateCurrentInfo(Waterbody[] allWaterbody){ _confusionCalc.updateConfusionConstsChanDirs(allWaterbody);}
	public int getConfusionFactor(int chanId){ return _confusionCalc.getConfusionFactor(chanId);}
	// add a current time to particle???
	public void setMeanSwimmingVelocity(int pId, int chanId){_swimCalc.setMeanSwimmingVelocity(pId, chanId);}
	public void setSwimmingTime(Particle p, int chanId){_holdingTimeCalc.setSwimTime(p, chanId);} 
	public long getSwimmingTime(int pId, int chanId){ return _holdingTimeCalc.getSwimTime(pId, chanId);}
	// careful swimming velocity here does not include confusion factor
	public float getSwimmingVelocity(int pId, int chanId){ 
		return _swimCalc.getSwimmingVelocity(pId, chanId);
	}
	
	/**
	  *  check if a node is reached
	  *  if reached return the new node else return null
	  */ 
	private final boolean isNodeReached(Particle p, float xpos){
		if (p.particleWait) return false; // false if the particle is asked to wait
		Channel ch = (Channel) p.wb;
	    if (xpos < 0.0f) {// crossed starting Node of Channel
	    	p.nd = ch.getNode(Channel.UPNODE);
	    	return true;
	    }
	    else if (xpos > ch.getLength()) {// crossed ending Node of Channel
	    	p.nd = ch.getNode(Channel.DOWNNODE);
	    	return true;
	    }
	    else return false;
	}
	//private void addSpecialBehaviors(SwimHelper sh, String particleType){}
	/**
	 *  x,y,z positioning
	 *  called after Particle insertion or returned from Reservoir/Conveyor
	 */
	public void setXYZLocationInChannel(Particle p){
		if (p.wb.getPTMType() == Waterbody.CHANNEL) {
			p.x = _hydroCalc.getXLocationInChannel(p);
			if (p.first) _hydroCalc.updateChannelParameters(p);
			_hydroCalc.setYZLocationInChannel(p);
		}else{ 
			p.x=MISSING; p.y=MISSING; p.z=MISSING;
		}
	}	  
	/**
	  *  insert particle in the system
	  */
	public void insert(Particle p){
	    p.observer.observeChange(ParticleObserver.INSERT,p);
	    p.inserted = true;
	    // insert to a node
	    if (p.wb == null){
	    	//makeNodeDecision will set mean and instantaneous swimming velocities, confusion factor and swimming time
	    	//swimming time is only set at insert() and makeNodeDecision()
	    	p.makeNodeDecision();
	    	setXYZLocationInChannel(p);
	    }
	    // insert to a channel (and distance known), p.x already set
	    else{
	    	 if (p.wb.getPTMType() == Waterbody.CHANNEL){
	    		 int chId = p.wb.getEnvIndex();
	    		 setMeanSwimmingVelocity(p.Id, chId);
	    		 setSwimmingTime(p, chId);
	    		 p.setSwimmingTime(getSwimmingTime(p.Id, chId));
	    		 //don't need to set confusion factor or swimming velocity here
	    		 //it'll be set at the beginning of the first time step
	 	    	 if (p.first) _hydroCalc.updateChannelParameters(p);
	 	    	 _hydroCalc.setYZLocationInChannel(p);
	    	 }else{ 
	    		 p.x=MISSING; p.y=MISSING; p.z=MISSING;
	    	 }
	    }
	}
	public void updatePosition(Particle p, float delT){
		boolean holding = _holdingTimeCalc.daytimeHolding();
		if (holding){
			p.age += delT;
			p.addTimeUsed(delT);
			//TODO clean up	
			
			if (p.wb.getType() == Channel.CHANNEL && p.wb.getEnvIndex()<801)
				System.err.println(p.Id + " " +(p.getCurrentParticleTime()-56300000)+" " + PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex())+" "
					 +PTMHydroInput.getExtFromIntChan(p.wb.getEnvIndex())
					 + " " +"start" + " " + "day time holding");
			else
				System.err.println(p.Id + " " +(p.getCurrentParticleTime()-56300000)+" " + PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex())+" "
						 +p.wb.getEnvIndex()
						 + " " +"start" + " " + "day time holding no channel");
						 
			return;
		}
		float tmLeft = delT;
		while (tmLeft>0 && !p.isDead){
			//TODO clean up
			
			if (p.wb.getType() == Channel.CHANNEL && p.wb.getEnvIndex() < 801)
				System.err.println(p.Id + " " +(p.getCurrentParticleTime()-56300000)+" " + PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex())+" "
					 +PTMHydroInput.getExtFromIntChan(p.wb.getEnvIndex())
					 + " " +"start" + " " + tmLeft);
			else
				System.err.println(p.Id + " " +(p.getCurrentParticleTime()-56300000)+" " + PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex())+" "
						 +p.wb.getEnvIndex()
						 + " " +"start" + " " + "day time holding no channel");
						 
			// Channel	
			if (p.wb.getPTMType() ==  Waterbody.CHANNEL) {
				 Channel ch = (Channel)p.wb;
				 int cId = ch.getEnvIndex();				 
				 // confusion and swimming velocity are sampled in two places
				 // 1) at the beginning of a new time step (here)
				 // 2) when entering a new channel (in SalmonBasicRouteBehavior.makeRouteDecision)
				 float sv = getSwimmingVelocity(p.Id, cId);
				 int cf = getConfusionFactor(cId);
				 p.setSwimmingVelocity(sv*cf);
				 p.setConfusionFactor(cf);
				 // update sub-time step due to y & z mixing
				 int numOfSubTimeSteps = _hydroCalc.getSubTimeSteps(tmLeft, p);
				 // sub-time step in seconds
				 float tmstep = tmLeft/numOfSubTimeSteps;
				 // PTM internal calculation time step
				 float tmToAdv = 0.0f;
				 //y, z set up for particles which are just out of reservoir or conveyor or inserted
				 //it is not necessary to set x because makeNodeDecision or setInsertInfo will be called and x will be set then
				 if (PTMUtil.floatNearlyEqual(p.y, MISSING) || PTMUtil.floatNearlyEqual(p.z,MISSING)) 
					 _hydroCalc.setYZLocationInChannel(p);
					 
				 // update particle's x,y,z position every sub-time step
				 while (tmLeft > 0 && !p.isDead){
					 if (tmLeft >= tmstep) // for all sub-time steps except the last
						 tmToAdv = tmstep;
					 else // for the last sub-time step; deal with division precision & truncation
						 tmToAdv = tmLeft;
					 //System.out.println(tmToAdv + " "+tmLeft);
					 //TODO work on this later
					 if (!p.checkSurvival(tmToAdv)) {
						 // count for last sub time step
						 p.age += tmToAdv;
						 p.addTimeUsed(tmToAdv);
						 //TODO clean up
						 System.err.println(p.Id + " " +(p.getCurrentParticleTime()-56300000)+" " + PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex())+" "
								 +PTMHydroInput.getExtFromIntChan(p.wb.getEnvIndex())
								 + " " +"End survival");
								 
						 return;
					 }
					 _hydroCalc.updateChannelParameters(p);
					 _hydroCalc.updateDiffusion(p);
					 float [] cInfo = _hydroCalc.getChannelInfo(p.Id);
					 // if an average cross section velocity (channelVave) is less than a user specified threshold, make the particle hold for one time step 
					 // because channelVave will not change in that time step.
					 // confusion may be reset after makeNodeDecision, so use p.getConfusionFactor(), instead cf.  					  
					 if (cInfo[3]*p.getConfusionFactor() < _floodHoldVel){
						 // wait time is the time left for the time step.
						 p.age += tmLeft;
						 p.addTimeUsed(tmLeft);
						 //TODO clean up
						 
						 if (p.wb.getEnvIndex() < 801)
							 System.err.println(p.Id + " " +(p.getCurrentParticleTime()-56300000)+" " + PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex())+" "
								 +PTMHydroInput.getExtFromIntChan(p.wb.getEnvIndex())
								 + " " +"End flood");
						 else
								System.err.println(p.Id + " " +(p.getCurrentParticleTime()-56300000)+" " + PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex())+" "
										 +p.wb.getEnvIndex()
										 + " " +"end flood");
										 
						 return;
					 }
					 if (!p.particleWait){
						 //Calculate X direction movement
						 float advVel = _hydroCalc.calcXAdvectionVelocity(p);
						 float advDeltaX = _hydroCalc.calcXAdvection(advVel, tmToAdv);
						 float swimV = p.getSwimmingVelocity();
						 float swimDeltaX = _swimCalc.CalcXSwimm(swimV, tmToAdv);
						 float deltaX = advDeltaX + swimDeltaX;
						 float xPos = p.x + deltaX;
						 IntBuffer currNdWb = IntBuffer.wrap(new int[] {p.nd.getEnvIndex(), cId});						 
						 if (isNodeReached(p, xPos)){
							 tmToAdv = _hydroCalc.calcTimeToNode(p, advVel+swimV, xPos);
							 //TODO clean up
							 //System.out.println("in NodeReached:" + tmToAdv+"\n");
							 // makeNodeDecision updates wb and sets x for new wb, x = 0 or channel length
							 p.makeNodeDecision();
							 // water body is updated in makeNodeDecision.  Now check if the new water body is a channel
							 // if not, exit the while loop and find a block that deal with the waterbody type
							 //wbId = p.wb.getEnvIndex();
							 if (p.wb.getPTMType() != Waterbody.CHANNEL){
								 tmLeft -= tmToAdv;
								 p.age += tmToAdv;
								 p.addTimeUsed(tmToAdv);
								 // before exit the loop, check if time should be recorded
								 _travelTimeOut.recordTravelTime(p, currNdWb, xPos, deltaX);
								 break;
							 }
							 // if channel, check to see if stay in the same node, 
							 // if yes, wait until next time step
							 // if not, continue on the code that calc y, z
							 else{
								 // if particle stays in the same node, wait for a time step (exit both while loops).  
								 if (tmToAdv < Float.MIN_VALUE && p.wb.getEnvIndex() == currNdWb.get(1) && p.nd.getEnvIndex() == currNdWb.get(0) 
										 && advVel*p.wb.getInflowWSV(p.nd.getEnvIndex(), p.getSwimmingVelocity())<0){
									 p.age += tmLeft;
									 p.addTimeUsed(tmLeft);
									 //TODO clean up
									 
									 if (p.wb.getType() == Channel.CHANNEL && p.wb.getEnvIndex() < 801)
										 System.err.println(p.Id + " " +(p.getCurrentParticleTime()-56300000)+" " + PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex())+" "
											 +PTMHydroInput.getExtFromIntChan(p.wb.getEnvIndex())
											 + " " +"End same node");
											 
									 return;
								 }
							 }
						 } //end if (isNodeReached(xPos) == true) 
						 else
							 p.x = xPos;
						 /* 	
						  * Careful!!! wb, nd are updated in makeNodeDecision!!!
						  * but channel parameters (channel width, etc.) are not (!!!) 
						  * because updateAllParameters is not called 
						  * y, z calculation are based on previous wb, nd,
						  * and will be updated for the new wb and nd at the beginning of the function
						  */
						 if (!p.isDead) {// save time if particle's dead
							 p.y = _hydroCalc.getYPosition(p,tmToAdv);
							 p.z = _hydroCalc.getZPosition(p,tmToAdv);
						 }
						 // check if need record before going to next sub-timestep
						 _travelTimeOut.recordTravelTime(p, currNdWb, xPos, deltaX);
						 //tmLeft -= tmToAdv;
						 // age in seconds
						 //p.age += tmToAdv;
						 //p.addTimeToParticle(tmToAdv/60);
						 //TODO clean up
						 /*
						 if (p.Id == 1)
							 System.err.println(p.Id + " " + PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex())+" "+PTMHydroInput.getExtFromIntChan(p.wb.getEnvIndex())
									 + " " + p.getSwimmingVelocity()+ " " + p.getConfusionFactor());
									 */
					 }//end if(!particleWait)
					 // don't need to record travel time because no x advanced
					 tmLeft -= tmToAdv;
					 // age in seconds
					 p.age += tmToAdv;
					 p.addTimeUsed(tmToAdv);
					 //TODO clean up
					 /*
					 if (tmLeft < 0.0f)
						 System.out.println(tmToAdv + " "+tmstep+" "+tmLeft);
						 */
					 
				 }// end the while in Channel
			}// end if(CHANNEL)
		    
			else if (p.wb.getPTMType() ==  Waterbody.RESERVOIR){
				 //TODO clean up later unnecessary function call, commented out
				 //tryCrossReservoir(tmLeft); 
				 
			    p.nd = p.makeReservoirDecision(tmLeft);
			  
			    if (p.nd != null){
			    	// zero time delay
			    	//makes decision of which Waterbody to go into
			    	p.makeNodeDecision();
			    	// set previous depth and width to current depth and width
			    	p.first = true;
			    	//? what should be new x,y,z for the pParticle in the Waterbody?
			    	setXYZLocationInChannel(p);
			    }
			    else{
			    	// if no node found the particle will still in the reservoir until next time step
			    	p.age += tmLeft;
			    	p.addTimeUsed(tmLeft);
			    	tmLeft = 0.0f;
			    }
			    //TODO only record travel time when particle is in a channel.
			    //it doesn't record travel time when a particle exits a reservoir.
			}
	    
			else if (p.wb.getPTMType() == Waterbody.CONVEYOR){
				 // zero time delay
				 p.moveInConveyor(tmLeft);
				 //TODO only record travel time when particle is in a channel.
				 //it doesn't record travel time when a particle exits a conveyor.				 
			 }

			 else if (p.wb.getPTMType() ==  Waterbody.BOUNDARY) {
				 //TODO only record travel time when particle is in a channel.
				 //it doesn't record travel time when a particle at a boundary.
				 p.setParticleDead();
				 //TODO clean up
				 
				System.err.println(p.Id + " " +(p.getCurrentParticleTime()-56300000)+" " + PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex())+" "
						 +p.wb.getEnvIndex()
						 + " " +"End boundary");
						 
				 break;
			 }
			//TODO clean up
			
			if (p.wb.getType() == Channel.CHANNEL && p.wb.getEnvIndex() < 801)
				System.err.println(p.Id + " " +(p.getCurrentParticleTime()-56300000)+" " + PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex())+" "
					 +PTMHydroInput.getExtFromIntChan(p.wb.getEnvIndex())
					 + " " +"End" + " "+tmLeft);
			else{
				System.err.println(p.Id + " " +(p.getCurrentParticleTime()-56300000)+" " + PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex())+" "
						 +p.wb.getEnvIndex()
						 + " " +"End non channel" + " " + tmLeft);
						 
			}
			
		 } // end first while 
	}
}
	  
	  
	  
	  
	  
	  
	  
	/*  
	protected final void updateXYZPosition(Particle p, float delT){
		float tmLeft = delT;
		while (tmLeft>0 && !p.isDead){
			int wbId = p.wb.getEnvIndex();
			// Channel
			if (p.wb.getPTMType() ==  Waterbody.CHANNEL) {
				 if (p.DEBUG) System.out.println("Particle " + this + " in channel " + wbId);
				 Channel ch = (Channel)p.wb;	 
				 float swimmingVelocity = getSwimmingVelocity(p.Id, wbId);
				 // update sub-time step due to y & z mixing
				 int numOfSubTimeSteps = getSubTimeSteps(tmLeft, p);
				 // sub-time step in seconds
				 float tmstep = tmLeft/numOfSubTimeSteps;
				 // PTM internal calculation time step
				 float tmToAdv = 0.0f;
				 //y, z set up for particles which are just out of reservoir or conveyor or inserted
				 //it is not necessary to set x because makeNodeDecision or setInsertInfo will be called and x will be set then
				 if (PTMUtil.floatNearlyEqual(p.y, p.MISSING) || PTMUtil.floatNearlyEqual(p.z,p.MISSING)) {
					 p.setYZLocationInChannel(ch);
				 }
				 
				 // update particle's x,y,z position every sub-time step
				 while (tmLeft > 0 && !p.isDead){
					 if (tmLeft >= tmstep) // for all sub-time steps except the last
						 tmToAdv = tmstep;
					 else // for the last sub-time step; deal with division precision & truncation
						 tmToAdv = tmLeft;
					 //TODO work on this later
					 if (!p.checkSurvival(tmToAdv)) {
						 p.age += tmToAdv;
						 return;
					 }
					 updateChannelParameters(p);
					 updateDiffusion(p);
					 // if an average cross section velocity (channelVave) is less than a user specified threshold, make the particle hold for one time step 
					 // because channelVave will not change in that time step.
					 // for a particular channel, confusion is calculated from channel hydrodynamics at the moment
					 // and is independent from any particle.
					 int confusionFactor = _confusionCalc.getConfusionFactor(wbId);
					 if (_pChanInfo.get(p.Id)[3]*confusionFactor < _floodHoldVel){
						 // wait time is the time left for the time step.
						 p.age += tmLeft;
						 return;
					 }
					 //TODO clean up
					 //boolean needToBeRecorded = false;
					 //int n = -999999, w= -999999, d = 0;
					 if (!p.particleWait){
						 //Calculate X direction movement
						 float xPos = p.x + calcXDisplacement(tmToAdv);
						 
						 /**
						  * wb and nd will be updated after isNodeReached and makeNodeDecision calls, 
						  * so test if the travel time needs to be recorded before those calls
						  * only record the travel time for particles from upstream because the output node is an upstream node
						  * xPos - x is always positive because the particle is from the upstream  
						  */
						 //TODO clean up
						 /* will be recorded in TravelTimeOutput
						 // n and w will be used to check if the particle stay in the same node
						 n = nd.getEnvIndex();
						 w = wb.getEnvIndex();
						 if (!_travelTimeRecorded && wb.isOutputWb() && nd.isOutputNode() && 
								 ((xPos > wb.getOutputDistance()) || PTMUtil.floatNearlyEqual(xPos, wb.getOutputDistance()))){
							 d = wb.getOutputDistance();
							 needToBeRecorded = true;
							 //TODO record age when particle encounter start location!!! use age as travel time, because when calibrate for travel time, particles will be inserted 
							 //!!! in the upstream end of the river reach.  May consider modify later.
						 }
						 */
	/*
						 // return null if a new node is not Reached
						 p.newNode = isNodeReached(p, ch, xPos);
						 if ( p.newNode != null){
							 float totalVelocity = calcXVelocityExtDeterministic() + calcXVelocityExtRandom()
					  			 	 			 + calcXVelocityIntDeterministic() + calcXVelocityIntRandom();
							 if(Math.abs(totalVelocity)<Float.MIN_VALUE) //xPos == x
								 PTMUtil.systemExit("when calculate x position, encountered a 0 velocity which is impossible");
							 else{ 
								 //if isNodeReached(xPos) == true, xPos could only > length or < 0
								 if (xPos < 0)
									 // now the entire tmToAdv is not totally used up.  Reset tmToAdv to the time only used 
									 tmToAdv = Math.abs(p.x/totalVelocity); // tmToAdv is less than tmstep
								 else if (xPos > p.channelLength)
									 tmToAdv = (p.channelLength-p.x)/totalVelocity; 
								 else
									 PTMUtil.systemExit("when a node is reached xPos can be only < 0 or > length, but xPos ="+xPos
											 + " in channel:"+PTMHydroInput.getExtFromIntChan(wb.getEnvIndex()));	
							 }
							 // makeNodeDecision updates wb and sets x for new wb, x = 0 or channel length
							 p.makeNodeDecision();
							 // water body is updated in makeNodeDecision.  Now check if the new water body is a channel
							 // if not, exit the while loop and find a block that deal with the waterbody type
							 wbId = p.wb.getEnvIndex();
							 if (wb.getPTMType() != Waterbody.CHANNEL){
								 tmLeft -= tmToAdv;
								 age += tmToAdv;
								 // before exit the loop, check if time should be recorded
								 if (needToBeRecorded){
									 _travelTimeRecorded = true;
									 needToBeRecorded = false;
									 _tto.setTravelTime(n,w,d, _insertStationName, PTMUtil.modelTimeToCalendar(insertionTime), Id, age/60);
								 } 
								 break;
							 }
							 // if channel, check to see if stay in the same node, 
							 // if yes, wait until next time step
							 // if not, continue on the code that calc y, z
							 else{
								 // if particle stays in the same node, wait for a time step (exit both while loops).  
								 if (tmToAdv < Float.MIN_VALUE && wb.getEnvIndex() == w && nd.getEnvIndex() == n 
										 && totalVelocity*wb.getInflowWSV(nd.getEnvIndex(), _swimmingVelocity)<0){
									 age += tmLeft;
									 return;
								 }
							 }
						 } //end if (isNodeReached(xPos) == true) 
						 else
							 x = xPos;
					 
						 /* 	
						  * Careful!!! wb, nd are updated in makeNodeDecision!!!
						  * but channel parameters (channel width, etc.) are not (!!!) 
						  * because updateAllParameters is not called 
						  * y, z calculation are based on previous wb, nd,
						  * and will be updated for the new wb and nd at the beginning of the function
						  */
	/*
						 if (!isDead) {// save time if particle's dead
							 y = calcYPosition(tmToAdv);
							 z = calcZPosition(tmToAdv);
						 }
					 }//end if(!particleWait)
					 tmLeft -= tmToAdv;
					// age in seconds
					 age += tmToAdv;
					// check if need record before going to next sub-timestep	 					 
					 if (needToBeRecorded){
						 _travelTimeRecorded = true;
						 needToBeRecorded = false;
						 _tto.setTravelTime(n,w,d,_insertStationName, PTMUtil.modelTimeToCalendar(insertionTime), Id, age/60);
					 } 
				 }// end the while in Channel
	      
			 }// end if(CHANNEL)
	    
			 else if (wb.getPTMType() ==  Waterbody.RESERVOIR){
				 if (DEBUG) System.out.println("Particle " + this + " in reservoir " + wb.getEnvIndex() );
				 //TODO clean up later unnecessary function call, commented out
				 //tryCrossReservoir(tmLeft); 
				 
			    nd = makeReservoirDecision(tmLeft);
			  
			    if ( nd != null){
			    // zero time delay
			      //makes decision of which Waterbody to go into
			      makeNodeDecision();
			      // set previous depth and width to current depth and width
			      first = true;
			      //? what should be new x,y,z for the pParticle in the Waterbody?
			      setXYZLocationInChannel(true);
			    }
			    else{
			    	// if no node found the particle will still in the reservior until next time step
			    	age += tmLeft;
			    	tmLeft = 0.0f;
			    }
			    if (!_travelTimeRecorded && wb.isOutputWb() && nd.isOutputNode()){
					 _travelTimeRecorded = true;
					 _tto.setTravelTime(nd.getEnvIndex(), wb.getEnvIndex(), 0,_insertStationName, PTMUtil.modelTimeToCalendar(insertionTime), Id, age/60); 
			    }
			 }
	    
			 else if (wb.getPTMType() == Waterbody.CONVEYOR){
				 if (DEBUG) System.out.println("Particle " + this + " in conveyor " + wb.getEnvIndex() );
				 // zero time delay
				 moveInConveyor(tmLeft);
				 if (!_travelTimeRecorded && wb.isOutputWb() && nd.isOutputNode()){
					 _travelTimeRecorded = true;
					 _tto.setTravelTime(nd.getEnvIndex(), wb.getEnvIndex(), 0,_insertStationName, PTMUtil.modelTimeToCalendar(insertionTime), Id, age/60); 
			    }
			 }

			 else if (wb.getPTMType() ==  Waterbody.BOUNDARY) {
				 if (DEBUG) System.out.println("Particle " + this + " in boundary " + wb.getEnvIndex() );
				 if (!_travelTimeRecorded && wb.isOutputWb() && nd.isOutputNode()){
					 _travelTimeRecorded = true;
					 _tto.setTravelTime(nd.getEnvIndex(), wb.getEnvIndex(), 0,_insertStationName, PTMUtil.modelTimeToCalendar(insertionTime), Id, age/60); 
			    }
				 setParticleDead();
				 break;
			 }
		 } // end first while
	    
	  }

}




/*

public void setConfusionFactor(Particle p, int chanId){
	//TODO make sure _chanDirs _confusionConst not return null
	if (_randomAccess && (PTMUtil.getRandomNumber() < _accessProb)
			 && (PTMUtil.getRandomNumber() < _confusionConsts.get(chanId)))
	    			p.setConfusionFactor(-1*_chanDirs.get(chanId));
	else
		p.setConfusionFactor(_chanDirs.get(chanId));
	
}

public HashMap<Integer, Double> getConfusionConsts() {return _confusionConsts;}
public HashMap<Integer, Integer> getChannelDir(){return _chanDirs;};

private boolean _randomAccess = false;
private float _accessProb;
_confusionConsts = new HashMap<Integer, Double> ();
_chanDirs = new HashMap<Integer, Integer> ();

public ArrayList<String> getGroupNames() {return _groupNames;}
public float getDaytimeNotSwimPercent() {return _daytimeNotSwimPercent;}
public Pair<Integer, Integer> getSunrise() {return _sunrise;}
public Pair<Integer, Integer> getSunset() {return _sunset;}
public float getFloodVelThrshold() {return _floodHoldVel;}
public boolean getRandomAccess() {return _randomAccess;}


	// pid vs. channel info (0: length, 1: width, 2: depth, 3: velocity, 4: area)
	private ConcurrentHashMap<Integer, float[]> _pChanInfo;
	// pid vs. previous Width Depth (0: width, 1:depth)
	private ConcurrentHashMap<Integer, float[]> _prePWidthDepth;
	// pid vs. vertical diffusion coefficient
	private ConcurrentHashMap<Integer, Float> _pVertD;
	private float EtToEvConst;
	private float EvConst;
	
		
	private static final float EMIN=0.0001f;
	
			_pChanInfo = new ConcurrentHashMap<Integer, float []>();
		_prePWidthDepth = new ConcurrentHashMap<Integer, float[]>();
		_pVertD = new ConcurrentHashMap<Integer, Float>();
		
		ParticleFixedInfo pFI = Globals.Environment.getParticleFixedInfo();
	    EvConst = pFI.getVerticalConstant();
	    if (PTMUtil.floatNearlyEqual(EvConst, 0.0f))
	    	PTMUtil.systemExit("vertical diffusion coefficient cannot be 0, system exit");
	    EtToEvConst = Math.abs(pFI.getTransverseConstant()/pFI.getVerticalConstant());
	    if (PTMUtil.floatNearlyEqual(EtToEvConst, 0.0f))
	    	PTMUtil.systemExit("lateral diffusion coefficient cannot be 0, system exit");
	    	
	    _transMove = pFI.moveLaterally();
	    _vertMove = pFI.moveVertically();
	    
	    private boolean _transMove, _vertMove;
	    
	    	private final void updateDiffusion(Particle p){
		float [] chanInfo = getChannelInfo(p);
		//diffusion coefficients are from Fisher Mixing in Inland and Coastal Waters p106 Eqs. (5.3) (5.6)
		//shear velocity is assumed 1/10 of the channel average velocity (see Fisher p138 Example 5.5 assumptions)
		//TODO may consider change
		// trans diffusion coefficient can be calculated using (EtConst/EvConst)*Ev so only need to store Ev
		_pVertD.put(p.Id, Math.max(Math.abs(EvConst*chanInfo[2]*chanInfo[3]*0.1f),EMIN));
		
	}
	private final void updateChannelParameters(Particle p){
		float [] cL={},cW={},cD={},cV={},cA={};
	    ((Channel)p.wb).updateChannelParameters(p.x,cL,cW,cD,cV,cA);
	    //_pChanInfo and _prePChanInfo will be initialized in the constructor no need to check null
	    float [] chanInfo = _pChanInfo.get(p.Id);
	    if (chanInfo == null){
	    	chanInfo = new float[5];
	    	_pChanInfo.put(p.Id, chanInfo);
	    }
	    chanInfo[0] = cL[0];
	    chanInfo[1]  = cW[0];
	    chanInfo[2]  = cD[0];
	    chanInfo[3]   = cV[0];
	    chanInfo[4]   = cA[0];
	    float [] preWD = _prePWidthDepth.get(p.Id);
	    if (preWD == null) {
	    	//previous=current, if transfer from reservoir/conveyer to channel
	    	preWD = new float[2];
	    	_prePWidthDepth.put(p.Id, preWD);
	    	preWD[0] = chanInfo[1];
	    	preWD[1] = chanInfo[2];
	    	p.first=false;
	    }
	}
	
		private float[] getChannelInfo(Particle p){
		float [] chanInfo = _pChanInfo.get(p.Id);
		// this method should be call after call update Channel parameters
		// therefore current info should not be null
		if (chanInfo == null){
			PTMUtil.systemExit("Particle hydrodynamic info in a channel was not properly updated, system exit.");
			return null;
		}
		return chanInfo;
	}
	
		/**
	 *  updates particle y, z position, Ev, Evdt, Etdt
	 */ 
/*
	private final void updateYZPosition(Particle p, float timeStep){
		// 
		float [] chanInfo = getChannelInfo(p);
		float [] preDW = _prePWidthDepth.get(p.Id);
		// previous channel info should not be null and previous width and depth should not be 0
		if (preDW == null || PTMUtil.floatNearlyEqual(preDW[0], 0.0f)
									|| PTMUtil.floatNearlyEqual(preDW[1], 0.0f))
			PTMUtil.systemExit("Particle previous width and depth were not properly updated, system exit.");
		//map y & z in new xsection over the node
		p.y = p.y*chanInfo[1]/preDW[0];
		p.z = p.z*chanInfo[2]/preDW[1];
  
		preDW[0] = chanInfo[1];
		preDW[1] = chanInfo[2];
	}
	
		private final int getSubTimeSteps(float timeStep, Particle p){
	    // the function will be called at the beginning of the time step so that parameters needs to be updated as hydrodynamic condition changes then.
		//TODO should move updates at the beginning of a time step? if not doing vertMove or transMove, channel parameters should still be updated?
	    updateChannelParameters(p); // necessary here??????
	    updateDiffusion(p);
	    
	    float minTimeStep = timeStep;
	  
	    if ((_vertMove) || (_transMove))
	    	minTimeStep = getMinTimeStep(timeStep, p);
	  
	    int numOfSubTimeSteps = 1;
	    if (minTimeStep < timeStep) numOfSubTimeSteps=(int) (timeStep/minTimeStep+1);

	    if (numOfSubTimeSteps > MAX_NUM_OF_SUB_TIME_STEPS){
	    	System.err.println("Warning: Number Of Sub Time Steps exceeds a maximum of "+MAX_NUM_OF_SUB_TIME_STEPS);
	    	return MAX_NUM_OF_SUB_TIME_STEPS;
	    }
	    else 
	      return numOfSubTimeSteps;
	}
	
	private final float getMinTimeStep(float timeStep, Particle p){
	    float terminalVelocity = getTerminalVelocity();
	    float [] chanInfo = getChannelInfo(p);
	    Float Ev = _pVertD.get(p.Id);
	    if (Ev == null)
	    	PTMUtil.systemExit("Particle vertical diffusion coefficient were not properly updated, system exit.");
	    float dymax = DFAC*chanInfo[1];
	    float dzmax = DFAC*chanInfo[2];
	    float dtz = Math.min(dzmax/terminalVelocity,dzmax*dzmax/Ev);
	    float dty = (dymax*dymax)/(EtToEvConst*Ev);

	    if (_vertMove && _transMove) return Math.min(dty,dtz);
	    else if (_vertMove && !_transMove) return dtz;
	    else if (!_vertMove && _transMove) return dty;
	    PTMUtil.systemExit("lateral or vertical move was not set properly. One of them has to be true, but not, system exit.");
	    return timeStep;
	}
		  /**
	    *  factor used for calculating minimum time step
	    *  set outside of method to allow overwriting to include fall velocity
	    */  
	/*
	  private float getTerminalVelocity(){
	    return 1.0e-10f;
	  }
	
*/
