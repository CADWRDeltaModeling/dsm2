package DWR.DMS.PTM;


//TODO this is swimming module for particles without any behavior. need to do more work.
public class BasicSwimBehavior implements SwimBehavior {
	private SwimInputs _si;
	protected float tmLeft;
	private BasicHydroCalculator _hydroCalc;
	private float MISSING = -9999999999.0f;
	
	public BasicSwimBehavior(SwimInputs si) {
		_si = si;	
		_hydroCalc = new BasicHydroCalculator();
	}
	public SwimInputs getSwimInputs(){return _si;}
	final boolean isNodeReached(Channel ch, float xpos){
	    if ((xpos < 0.0f) || (xpos > ch.getLength())) // crossed starting/ending Node of Channel 
	    	return true;
	    else return false;
	}
	final Node getNewNode(Channel ch, float xpos){
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
	 	    	 if (p.first) _hydroCalc.updateChannelParameters(p);
	 	    	 _hydroCalc.setYZLocationInChannel(p);
	    	 }
	    	 else{ 
	    		 p.x=MISSING; p.y=MISSING; p.z=MISSING;
	    	 }
	    }
	}
	/**
	  *  updates the position and parameters of Particle.
	  */
	public final void updatePosition(Particle p, float delT){	
		float tmLeft = delT;
		//if(p.Id == 1)
			//System.err.println(p.wb.getEnvIndex());
		while (tmLeft>0){
			if (p.particleWait){
				 p.age += tmLeft;
				 p.addTimeUsed(tmLeft);
				 return;
			}
			// Channel	
			if (p.wb.getPTMType() ==  Waterbody.CHANNEL) {
				 if (p.isDead) return;		
				 //y, z set up for particles which are just out of reservoir or conveyor or inserted
				 //it is not necessary to set x because makeNodeDecision or setInsertInfo will be called and x will be set then
				 if (PTMUtil.floatNearlyEqual(p.y, MISSING) || PTMUtil.floatNearlyEqual(p.z,MISSING)) 
					 _hydroCalc.setYZLocationInChannel(p);
				  
				 // update particle's x,y,z position every sub-time step
				 while (tmLeft > 0 && !p.isDead){
					 _hydroCalc.updateChannelParameters(p);
					 _hydroCalc.updateDiffusion(p);				 
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
					 float deltaX = advDeltaX;
					 float xPos = p.x + deltaX;				 
					 
					 if (Math.abs(advVel) < 0.00001f){
						 // wait time is the time left for the time step.
						 p.age += tmLeft;
						 p.addTimeUsed(tmLeft);						 
						 return;
					 } 
					 					 
					 // isNodeReached now keeps the old node. the node will be changed to the new node just reached when getNewNode(...) is called
					 if (isNodeReached((Channel) p.wb, xPos)){
						 //tmToAdv could be less than tmToAdv passed on 
						 tmToAdv = _hydroCalc.calcTimeToNode((Channel)p.wb, advVel, 0, p.x, xPos); 						 
						 p.x += _hydroCalc.calcDistanceToNode((Channel)p.wb, p.x, xPos);
						 p.y = _hydroCalc.getYPosition(p.Id, p.y,tmToAdv);
						 p.z = _hydroCalc.getZPosition(p.Id, p.z,tmToAdv);
						 p.age += tmToAdv;
						 tmLeft -= tmToAdv;
						 p.addTimeUsed(tmToAdv);
						 p.nd = getNewNode((Channel) p.wb, xPos);
						 //set new node so make node decision can calculate total node inflow and call a special behavior if necessary
						 p.makeNodeDecision();
						 // now p.wb is the new water body just selected
						 // and p.x is set either 0 or channel length if p.wb is a channel
						 
						 // wait for a time step
						 if (p.particleWait){
							 p.age += tmLeft;
							 p.addTimeUsed(tmLeft);
							 return;
						 }
						 // check if the new water body is a channel
						 // if NOT, exit the while loop and find a block that deal with the waterbody type
						 if (p.wb.getPTMType() != Waterbody.CHANNEL)							 
							 break;
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
						 p.addTimeUsed(tmToAdv);
					 }
				 }// while (tmLeft > 0 && !p.isDead)
			}// end if(CHANNEL)
		     
			else if (p.wb.getPTMType() ==  Waterbody.RESERVOIR){				 
			    p.nd = p.makeReservoirDecision(tmLeft);
			  
			    if (p.nd != null){
			    	// zero time delay
			    	//makes decision of which Waterbody to go into
			    	p.makeNodeDecision();
			    	// set previous depth and width to current depth and width
			    	// initializing a particle's position in a channel
			    	p.first = true;
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

/*		
		
		recursionCounter=0;
	    if (p.wb.getPTMType() ==  Waterbody.CHANNEL) {
	    	if (DEBUG) System.out.println("Particle " + this + " in channel " + p.wb.getEnvIndex());
		    tmLeft = delT;
		    // update sub-time step due to y & z mixing
		    int numOfSubTimeSteps = _hydroCalc.getSubTimeSteps(delT, p);
		    float tmstep = delT/numOfSubTimeSteps;
		    // PTM internal calculation time step
		    float tmToAdv = 0;
		    //y, z set up for particles which are just out of reservoir or conveyor or inserted
		    //it is not necessary to set x because makeNodeDecision or setInsertInfo will be called and x will be set then
			if (PTMUtil.floatNearlyEqual(p.y, MISSING) || PTMUtil.floatNearlyEqual(p.z,MISSING)) 
				 _hydroCalc.setYZLocationInChannel(p);		      
		    // update particle's x,y,z position every sub-time step
		    while (tmLeft > 0 && isDead == false){
		        if (tmLeft >= tmstep) {// for all sub-time steps except the last
		        	tmToAdv = tmstep;
		        } else {// for the last sub-time step; deal with division precision & truncation
		        	tmToAdv = tmLeft;
		        }
		        p.age += tmToAdv;
		        _hydroCalc.updateChannelParameters(p);
				_hydroCalc.updateDiffusion(p);	
				float [] cInfo = _hydroCalc.getChannelInfo(p.Id);
				
		    }
	    }
	}
	*/			/*
				
		        updateAllParameters(tmToAdv);
		        if (particleWait == false){
		          x = calcXPosition(tmToAdv);
		          // particle into reservoir/conveyor, out of the whole function
		          if (wb.getPTMType() != Waterbody.CHANNEL) return;
		          if (isDead == false) {// save time if particle's dead
		            y = calcYPosition(tmToAdv);
		            z = calcZPosition(tmToAdv);
		          }
		        }//end if(particleWait)
		        tmLeft -= tmToAdv;
		      }// end while
		    }// end if(CHANNEL)
	
		    else if (wb.getPTMType() ==  Waterbody.RESERVOIR){
		      if (DEBUG) System.out.println("Particle " + this + " in reservoir " + wb.getEnvIndex() );
		      tryCrossReservoir(delT); 
		    }
		    
		    else if (wb.getPTMType() == Waterbody.CONVEYOR){
		      if (DEBUG) System.out.println("Particle " + this + " in conveyor " + wb.getEnvIndex() );
		      // zero time delay
		      moveInConveyor(delT);
		    }
		    
		    else if (wb.getPTMType() ==  Waterbody.BOUNDARY) {
		      if (DEBUG) System.out.println("Particle " + this + " in boundary " + wb.getEnvIndex() );
		      isDead=true;
		    }
	      updateOtherParameters(delT);
      //      System.out.println("update "+Id);
      if(! isDead) checkHealth();
      //      if (Id == 1) System.out.println(Id+" "+age+" "+getFallVel());
    }
	    else if (!inserted && Globals.currentModelTime >= insertionTime) {//when current time reach insertion time
	      if ((Globals.currentModelTime - insertionTime)/60.0 > delT)//insertion time may set as way before PTM start time 
	        warning("Particle insertion time specification may be incorrect");//may include particles 1 time step before the 1st insertion
	      insert();
	      recursionCounter=0;
	      updateXYZPosition(delT);
		  updateOtherParameters(delT);
	    }
	  }
	  */
	  /**
	    *  updates the Particle position for the given time step;
	    *  input time step is usually divided into small sub-time step to complete the calculation; 
	    *  The Particle is moved for the time step given; 
	    *  The new position of the Particle is available as
	    *  Particle.x, Particle.y and Particle.z
	    */
				/*
	  protected final void updateXYZPosition(float delT){
	    
	  }
	  */
	  

