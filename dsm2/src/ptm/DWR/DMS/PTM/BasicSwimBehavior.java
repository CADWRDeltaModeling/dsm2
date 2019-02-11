package DWR.DMS.PTM;
//TODO this is swimming module for particles without any behavior. need to do more work.
public class BasicSwimBehavior implements SwimBehavior {
	private SwimInputs _si;
	private static int recursionCounter;
	protected float tmLeft;
	private boolean DEBUG;
	private BasicHydroCalculator _hydroCalc;
	private float MISSING = -9999999999.0f;
	private boolean isDead = false;
	
	public BasicSwimBehavior(SwimInputs si) {
		_si = si;	
		_hydroCalc = new BasicHydroCalculator();
	}
	public SwimInputs getSwimInputs(){return _si;}
	/**
	  *  updates the position and parameters of Particle.
	  */
	public final void updatePosition(Particle p, float delT){
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
				/*
				
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
	  
}
