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
	public boolean DEBUG_SWIM = false;
	public boolean DEBUG_SURVIVAL = false;
	//these variables are visible to ALL particles.  The containers in the classes are mapped with pid as the key.
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
	public SalmonBasicSwimBehavior(SwimInputs si) { 
		_confusionCalc = new SalmonConfusionFactorCalculator(si); 
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

	private final boolean isNodeReached(Channel ch, float xpos){
	    if ((xpos < 0.0f) || (xpos > ch.getLength())) // crossed starting/ending Node of Channel 
	    	return true;
	    else return false;
	}
	private final Node getNewNode(Channel ch, float xpos){
	    if (xpos < 0.0f) // crossed starting Node of Channel
	    	return ch.getNode(Channel.UPNODE);
	    else if (xpos > ch.getLength()) // crossed ending Node of Channel
	    	return ch.getNode(Channel.DOWNNODE);
	    else 
	    	PTMUtil.systemExit("getNewNode method should not be called here, system exit");
	    return null;
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
		if ((p.wb.getPTMType() ==  Waterbody.CHANNEL) 
				&& (_holdingTimeCalc.daytimeHolding(p.Id, ((Channel)p.wb).getEnvIndex()))) {
			p.age += delT;
			p.addTimeUsed(delT);
			return;
		}		
		float tmLeft = delT;
		while (tmLeft>0){
			if (p.particleWait){
				 p.age += tmLeft;
				 p.addTimeUsed(tmLeft);
				 if (p.Id == 1 && DEBUG_SWIM)
					 System.err.println("Warning: At the beginning of the loop, the particle "+p.Id+" will wait until next time step.");
				 return;
			}
			// Channel	
			if (p.wb.getPTMType() ==  Waterbody.CHANNEL) {
				 int cId = ((Channel)p.wb).getEnvIndex();		
				 
				 // confusion and swimming velocity are sampled in two places
				 // 1) at the beginning of a new time step (here)
				 // 2) when entering a new channel (in SalmonBasicRouteBehavior.makeRouteDecision)
				 // this check is to avoid swimming velocity to be reset immediately after exiting from a junction
				 // update swimming related parameters
				 if (!p.isSwimVelSetInJunction()){
					 float sv = getSwimmingVelocity(p.Id, cId);
					 int cf = getConfusionFactor(cId);
					 p.setSwimmingVelocity(sv*cf);
					 p.setConfusionFactor(cf);
				 }
				 else
					 p.swimVelSetInJunction(false);
				 
				 //y, z set up for particles which are just out of reservoir or conveyor or inserted
				 //it is not necessary to set x because makeNodeDecision or setInsertInfo will be called and x will be set then
				 if (PTMUtil.floatNearlyEqual(p.y, MISSING) || PTMUtil.floatNearlyEqual(p.z,MISSING)) 
					 _hydroCalc.setYZLocationInChannel(p);
				 
				 //one time step won't make a particle too far (e.g., from one node to another).  So timesLooped will not have memory 
				 int timesLooped = 0;	 
				 // update particle's x,y,z position every sub-time step
				 while (tmLeft > 0 && !p.isDead){
					 _hydroCalc.updateChannelParameters(p);
					 _hydroCalc.updateDiffusion(p);	
					 /*
					  * check survival
					  * to make sure a survival station is not skipped, will be checked again in the node reached loop 
					  * because node and channel will be reset when a node is reached
					  * 
					  * only calculate survival when a particle is in a channel
					  */
					 p.checkSurvival();
					 if (p.isDead) return;
					 float [] cInfo = _hydroCalc.getChannelInfo(p.Id);
					 
					 // if an average cross section velocity (channelVave) is less than a user specified threshold, make the particle hold for one time step 
					 // because channelVave will not change in that time step.
					 // confusion may be reset after makeNodeDecision, so use p.getConfusionFactor(), instead cf.  					  
					 if (cInfo[3]*p.getConfusionFactor() < _floodHoldVel){
						 // wait time is the time left for the time step.
						 p.age += tmLeft;
						 p.addTimeUsed(tmLeft);						 
						 if (p.Id == 1 && DEBUG_SWIM)
							 System.err.println("Warning: particle "+p.Id+" velocity < flood hold velocity, velocity:"
							 + cInfo[3]*p.getConfusionFactor()+", will wait until next time step.");							 
						 return;
					 } 
					 _hydroCalc.mapYZ(p);
					 // update sub-time step to avoid y & z direction boundary bouncing
					 int numOfSubTimeSteps = _hydroCalc.getSubTimeSteps(tmLeft, p);
					 // sub-time step in seconds
					 float tmstep = tmLeft/numOfSubTimeSteps;
					// PTM internal calculation time step
					 float tmToAdv = tmLeft; // for the last sub-time step; deal with division precision & truncation
					 if (tmLeft >= tmstep) // for all sub-time steps except the last
						 tmToAdv = tmstep;
					 float advVel = _hydroCalc.calcXAdvectionVelocity(p.Id, p.x, p.y, p.z, (Channel)p.wb);
					 float advDeltaX = _hydroCalc.calcXAdvection(advVel, tmToAdv);
					 float swimV = p.getSwimmingVelocity();
					 float swimDeltaX = _swimCalc.CalcXSwim(swimV, tmToAdv);
					 float deltaX = advDeltaX + swimDeltaX;
					 float xPos = p.x + deltaX;
					 IntBuffer ndWb = IntBuffer.wrap(new int[] {p.nd.getEnvIndex(), p.wb.getEnvIndex()});
					 
					// this is to avoid swimming velocity to be reset immediately after exiting from a junction
					// after a couple of sub-time step it is OK to reset
					 p.swimVelSetInJunction(false);
					 
					 if (DEBUG_SWIM && p.Id == 1 && p.wb.getEnvIndex()<800){
						  System.out.println("node:"+PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex()) 
								  +"  wb:" + PTMHydroInput.getExtFromIntChan(p.wb.getEnvIndex())
								  + "  p.x:" +p.x +"  xPos:" +xPos +" p.age:" + p.age
								  +"  advVel:" + advVel + "  swimV:" + swimV + "  tmToAdv:" + tmToAdv 
								  + "  tmLeft:" +tmLeft +"  currTime:" + p.getCurrentParticleTimeExact()
								  + "  confusion:" + p.getConfusionFactor());
					  }
					 
					 
					 // isNodeReached now keeps the old node. the node will be changed to the new node just reached when getNewNode(...) is called	
					 if (isNodeReached((Channel) p.wb, xPos)){
						 //tmToAdv could be less than tmToAdv passed on 
						 tmToAdv = _hydroCalc.calcTimeToNode((Channel)p.wb, advVel, swimV, p.x, xPos); 						 
						 p.x += _hydroCalc.calcDistanceToNode((Channel)p.wb, p.x, xPos);
						 p.y = _hydroCalc.getYPosition(p.Id, p.y,tmToAdv);
						 p.z = _hydroCalc.getZPosition(p.Id, p.z,tmToAdv);
						 p.age += tmToAdv;
						 tmLeft -= tmToAdv;
						 _travelTimeOut.recordTravelTime(p.Id, p.getInsertionStation(), p.getInsertionTime(), p.age, ndWb, advVel+swimV, p.x, deltaX);
						 p.addTimeUsed(tmToAdv);
						 //here node and channel hasn't been changed yet because the survival calc needs to do with current ones
						 p.checkSurvival();
						 if(p.isDead){
							 if (DEBUG_SURVIVAL && p.Id == 1 && p.wb.getEnvIndex()<800){
								  System.err.println("node:"+PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex()) 
										  +"  wb:" + PTMHydroInput.getExtFromIntChan(p.wb.getEnvIndex())
										  + "  p.x:" +p.x +" p.age:" + p.age + "  tmToAdv:" + tmToAdv 
										  + "  tmLeft:" +tmLeft
										  + "  p.isDead:" + p.isDead);
							 }
							 return;
						 }
						 p.nd = getNewNode((Channel) p.wb, xPos);
						 //set new node so make node decision can calculate total node inflow
						 p.makeNodeDecision();
						 // now p.wb is the new water body just selected
						 // and p.x is set either 0 or channel length if p.wb is a channel
						 
						 if (DEBUG_SWIM && p.Id == 1 && p.wb.getEnvIndex()<800){
							  System.err.println("Reach Node, node:"+PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex()) 
									  +"  wb:" + PTMHydroInput.getExtFromIntChan(p.wb.getEnvIndex())
									  + "  p.x:" +p.x +" p.age:" + p.age + "  tmToAdv:" + tmToAdv 
									  + "  tmLeft:" +tmLeft
									  );
						  }
						 
						 if (DEBUG_SURVIVAL && p.Id == 1 && p.wb.getEnvIndex()<800){
							  System.err.println("node:"+PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex()) 
									  +"  wb:" + PTMHydroInput.getExtFromIntChan(p.wb.getEnvIndex())
									  + "  p.x:" +p.x +" p.age:" + p.age + "  tmToAdv:" + tmToAdv 
									  + "  tmLeft:" +tmLeft
									  + "  p.isDead:" + p.isDead);
						  }
						 
						 // wait for a time step
						 // don't need to check survival because nothing has been changed since the last time step
						 if (p.particleWait){
							 p.age += tmLeft;
							 p.addTimeUsed(tmLeft);
							 if (DEBUG_SWIM && p.Id == 1)
								 System.err.println("Warning: the particle "+p.Id
										 +" is set to wait by makeNodeDecision, will wait until next time step.");
							 return;
						 }
						 // check if the new water body is a channel
						 // if NOT, exit the while loop and find a block that deal with the waterbody type
						// don't need to check survival because survival stations are only installed in channels
						 if (p.wb.getPTMType() != Waterbody.CHANNEL)							 
							 break;
						 else{
							 //check survival when arrive a new channel
							 p.checkSurvival();
							 if(p.isDead)
								 return;
							 // if channel, check to see if stay in the same node. if yes, wait until next time step
							 // if not, continue on the code that calc y, z
							 // this block is for preventing the same particle from staying in the same sub time step, node and waterbody too many times
							 if (tmToAdv < Float.MIN_VALUE && p.nd.getEnvIndex() == ndWb.get(0) && p.wb.getEnvIndex() == ndWb.get(1)){
								 timesLooped++;
								 if (timesLooped > 20){
									System.err.println("Warning: the particle "+p.Id+" looped more than 20 times at the same time step at the same node. It will continue on next time step.");
									p.age += tmLeft;
									p.addTimeUsed(tmLeft);
									return;
								 }
							 }
						 }
					 } //end if (isNodeReached(xPos) == true) 
					 else{
						 p.x = xPos;
						 /*
						  * y, z are calculated according to current xsection info (channel parameters hasn't been updated yet)
						  * they'll be mapped to new xsection at the beginning of the loop 
						  */
						 p.y = _hydroCalc.getYPosition(p.Id, p.y,tmToAdv);
						 p.z = _hydroCalc.getZPosition(p.Id, p.z,tmToAdv);
						 p.age += tmToAdv;
						 tmLeft -= tmToAdv;
						 _travelTimeOut.recordTravelTime(p.Id, p.getInsertionStation(), p.getInsertionTime(), p.age, ndWb, advVel+swimV, p.x, deltaX);
						 p.addTimeUsed(tmToAdv);
					 }
				 }// end the while in Channel
			}// end if(CHANNEL)
		    
			//TODO travel time is not recorded for other water body types because the travel time will be recorded 
			//when the particle enters the channel as long as it is from upstream.
			else if (p.wb.getPTMType() ==  Waterbody.RESERVOIR){				 
			    p.nd = p.makeReservoirDecision(tmLeft);
			  
			    if (p.nd != null){
			    	// zero time delay
			    	//makes decision of which Waterbody to go into
			    	p.makeNodeDecision();
			    	// set previous depth and width to current depth and width
			    	p.first = true;
			    	//TODO what should be new x,y,z for the pParticle in the Waterbody?
			    	setXYZLocationInChannel(p);
			    }
			    else{
			    	// if no node found the particle will still in the reservoir until next time step
			    	p.age += tmLeft;
			    	p.addTimeUsed(tmLeft);
			    	tmLeft = 0.0f;
			    }
			}
	    
			else if (p.wb.getPTMType() == Waterbody.CONVEYOR){
				 // zero time delay
				 p.moveInConveyor(tmLeft);				 
			 }

			 else if (p.wb.getPTMType() ==  Waterbody.BOUNDARY) {
				 p.setParticleDead();	 
				 break;
			 }
			
		 } // end first while 
	}
}


//TODO clean up: very confusing when changing node here, write another method to change the node
/**
  *  check if a node is reached
  *  if reached return the new node else return null
  */ 
/*
private final boolean isNodeReached(Particle p, float xpos){
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
*/
//TODO clean up moved to survival behavior
/*
private void setMaxDistanceFromEntrance(Particle p){
	//set the maximum distance a particle traveled from the entrance node
	 float currDist = p.x * p.getDistSignForXT() + p.getDistOverheadForXT();
	 if (p.getMaxDistFrmEntrance() < currDist)
		 p.setMaxDistFrmEntrance(currDist);
	
}
*/

/*
//TODO clean up	
if (p.wb.getType() == Channel.CHANNEL && p.wb.getEnvIndex() < 801)
	 System.err.println(p.Id + " " +(p.getCurrentParticleTimeExact()-56300000)+" " + PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex())+" "
		 +PTMHydroInput.getExtFromIntChan(p.wb.getEnvIndex())
		 + " " +"End same node"+ " "+p.wb.getInflowWSV(p.nd.getEnvIndex(), p.getSwimmingVelocity())
		 +" "+p.getSwimmingVelocity()+" "+_hydroCalc.calcXAdvectionVelocity(p));
*/ 

/*
System.err.println((p.getCurrentParticleTimeExact()-56300000)+" " + PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex())+" "
		 +PTMHydroInput.getExtFromIntChan(p.wb.getEnvIndex())+ " "+tmToAdv
		 + " "+tmLeft+" "+p.x+" "+p.y+" "+p.z + " "+xPos+" "+p.getConfusionFactor()+" "+p.age/3600.0f+" "+p.getSwimmingVelocity()
		 + " "+ _swimCalc.getMeanSwimmingVelocity(p.Id, p.wb.getEnvIndex())+" " + _confusionCalc.getChanDir(p.wb.getEnvIndex())
		 + " "+advVel + " "+advDeltaX + " "+swimDeltaX+" "+deltaX);
*/


//}//end if(!particleWait)
//tmLeft -= tmToAdv;
// age in seconds
//p.age += tmToAdv;
//p.addTimeUsed(tmToAdv);
// check if need record travel time before going to next sub-timestep
//_travelTimeOut.recordTravelTime(p, currNdWb, advVel+swimV, deltaX);

// from line 147

/* this is the old code. Now only check survival when encounters a node.  So commented this out

if (!p.checkSurvival(tmToAdv)) {
	 // count for last sub time step
	 p.age += tmToAdv;
	 p.addTimeUsed(tmToAdv); 
	 return;
}
*/

// need to update every sub time step
// update sub-time step at the beginning of a time step to avoid y & z direction boundary bouncing
// channel parameters (e.g. width and depth) are updated in the getSubTimeSteps(...) call
//int numOfSubTimeSteps = _hydroCalc.getSubTimeSteps(tmLeft, p);
// sub-time step in seconds
//float tmstep = tmLeft/numOfSubTimeSteps;
// PTM internal calculation time step
//float tmToAdv = 0.0f;


/*
if (p.particleWait){
	 p.age += tmLeft;
	 p.addTimeUsed(tmLeft);
	 return;
}
*/