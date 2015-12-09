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
			return;
		}
		float tmLeft = delT;
		while (tmLeft>0 && !p.isDead){		 
			// Channel	
			if (p.wb.getPTMType() ==  Waterbody.CHANNEL) {
				 Channel ch = (Channel)p.wb;
				 int cId = ch.getEnvIndex();				 
				 // confusion and swimming velocity are sampled in two places
				 // 1) at the beginning of a new time step (here)
				 // 2) when entering a new channel (in SalmonBasicRouteBehavior.makeRouteDecision)
				 
				 // this check is to avoid swimming velocity to be reset immediately after exiting from a junction
				 if (!p.isSwimVelSetInJunction()){
					 float sv = getSwimmingVelocity(p.Id, cId);
					 int cf = getConfusionFactor(cId);
					 p.setSwimmingVelocity(sv*cf);
					 p.setConfusionFactor(cf);
				 }
				 else
					 p.swimVelSetInJunction(false);
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
				 int timesLooped = 0;	 
				 // update particle's x,y,z position every sub-time step
				 while (tmLeft > 0 && !p.isDead){
					 if (tmLeft >= tmstep) // for all sub-time steps except the last
						 tmToAdv = tmstep;
					 else // for the last sub-time step; deal with division precision & truncation
						 tmToAdv = tmLeft;
					 
					 //TODO work on this later
					 if (!p.checkSurvival(tmToAdv)) {
						 // count for last sub time step
						 p.age += tmToAdv;
						 p.addTimeUsed(tmToAdv); 
						 return;
					 }
					 
					 if (p.particleWait){
						 p.age += tmLeft;
						 p.addTimeUsed(tmLeft);
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
						 return;
					 }
					 //if (!p.particleWait){
					 //Calculate X direction movement
					 float advVel = _hydroCalc.calcXAdvectionVelocity(p);
					 float advDeltaX = _hydroCalc.calcXAdvection(advVel, tmToAdv);
					 float swimV = p.getSwimmingVelocity();
					 float swimDeltaX = _swimCalc.CalcXSwim(swimV, tmToAdv);
					 float deltaX = advDeltaX + swimDeltaX;
					 float xPos = p.x + deltaX;
					 IntBuffer currNdWb = IntBuffer.wrap(new int[] {p.nd.getEnvIndex(), p.wb.getEnvIndex()});
					// this is to avoid swimming velocity to be reset immediately after exiting from a junction
					// after a couple of sub-time step it is OK to reset
					 p.swimVelSetInJunction(false);
					 
					 //TODO clean up
					 /*
					 System.err.println((p.getCurrentParticleTimeExact()-56300000)+" " + PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex())+" "
							 +PTMHydroInput.getExtFromIntChan(p.wb.getEnvIndex())+ " "+tmToAdv
							 + " "+tmLeft+" "+p.x+" "+p.y+" "+p.z + " "+xPos+" "+p.getConfusionFactor()+" "+p.age/3600.0f+" "+p.getSwimmingVelocity()
							 + " "+ _swimCalc.getMeanSwimmingVelocity(p.Id, p.wb.getEnvIndex())+" " + _confusionCalc.getChanDir(p.wb.getEnvIndex())
							 + " "+advVel + " "+advDeltaX + " "+swimDeltaX+" "+deltaX);
					 */
					 if (isNodeReached(p, xPos)){
						 //Calculate actual tmToAdv 
						 tmToAdv = _hydroCalc.calcTimeToNode(p, advVel+swimV, xPos);
						 // makeNodeDecision updates wb and sets x for new wb, x = 0 or channel length
						 p.makeNodeDecision();
						 // wait for a time step
						 if (p.particleWait){
							 p.age += tmLeft;
							 p.addTimeUsed(tmLeft);
							 return;
						 }
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
						 
						 //this block is for preventing the same particle stays in the same sub time step, node and waterbody too many times
						 else{
							 // if particle stays in the same sub for 20 , wait for a time step (exit both while loops).
							 
							 if (tmToAdv < Float.MIN_VALUE && p.nd.getEnvIndex() == currNdWb.get(0) && p.wb.getEnvIndex() == currNdWb.get(1)){
								 timesLooped++;
								 if (timesLooped > 20){
									System.err.println("Warning: the particle "+p.Id+" looped more than 20 times at the same time step at the same node. It will continue on next time step.");
									p.age += tmLeft;
									p.addTimeUsed(tmLeft);
									return;
								 }
								 
								 /*
								 //TODO clean up	
								 if (p.wb.getType() == Channel.CHANNEL && p.wb.getEnvIndex() < 801)
									 System.err.println(p.Id + " " +(p.getCurrentParticleTimeExact()-56300000)+" " + PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex())+" "
										 +PTMHydroInput.getExtFromIntChan(p.wb.getEnvIndex())
										 + " " +"End same node"+ " "+p.wb.getInflowWSV(p.nd.getEnvIndex(), p.getSwimmingVelocity())
										 +" "+p.getSwimmingVelocity()+" "+_hydroCalc.calcXAdvectionVelocity(p));
								 */ 
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
					 //}//end if(!particleWait)
					 // don't need to record travel time because no x advanced
					 tmLeft -= tmToAdv;
					 // age in seconds
					 p.age += tmToAdv;
					 p.addTimeUsed(tmToAdv);					 
				 }// end the while in Channel
			}// end if(CHANNEL)
		    
			else if (p.wb.getPTMType() ==  Waterbody.RESERVOIR){				 
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
				 break;
			 }
			
		 } // end first while 
	}
}
