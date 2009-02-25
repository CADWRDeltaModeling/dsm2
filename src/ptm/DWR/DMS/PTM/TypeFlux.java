/*<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public !<license as published by
C!    the Free Software Foundation, either version 3 of the !<license, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public !<license for more details.

C!    You should have received a copy of the GNU General Public !<license
C!    along with DSM2.  If not, see <http://www.gnu.org/!<licenses/>.
</license>*/

package DWR.DMS.PTM;

/**
 * Calculates the Flux of particles based on the types of waterbodies optionally
 * excluding specific waterbodies of those types
 * 
 * @author Nicky Sandhu
 * @version $Id: TypeFlux.java,v 1.2.6.4 2007/07/31 18:30:40 eli2 Exp $
 */
public class TypeFlux extends Flux {
    /**
     *  
     */
    public TypeFlux(FluxFixedData info) {
	fluxType = TYPE_FLUX;
	this.info = info;
    }

 /**
     *  
     */
    public void calculateFlux(ParticleTrace[] traceArray,
			      int numberOfTraceParticles, int sTime, int eTime, int tStep,
			      int nParticles) {
	if (initialized == false) {
	    super.calculateFlux(traceArray, numberOfTraceParticles, sTime,
				eTime, tStep, nParticles);
	}

	numberOfParticles = numberOfTraceParticles;

	for (int pNum = 0; pNum < numberOfParticles; pNum++) {
	    int traceNum = 1;
	    int particleFlux = 0;
	    int maxTraces = traceArray[pNum].getNumberOfTraces();
	    //System.out.println("Particle #: " + pNum);
	    try {
		for (int index = 0; index < numberOfTimeSteps; index++) {
		    while (traceNum <= maxTraces
			&& traceArray[pNum].getTime(traceNum) == index
			* timeStep + startTime) {

			Waterbody wbIn = Globals.Environment
			    .getWaterbody(traceArray[pNum]
					  .getWaterbodyId(traceNum - 1));
			Waterbody wbOut = Globals.Environment
			    .getWaterbody(traceArray[pNum]
					  .getWaterbodyId(traceNum));
                                                
			//System.out.println("Out WB type" + wbOut.getType());
			/* if (wbOut.getType() !=100){
			   System.out.println("Index : " + index);
			   System.out.println("In : " + wbIn);
			   System.out.println("Out : " + wbOut);
			   System.out.println("Flux check");
			   System.out.println("Out Flux check: " + info.getOutGroup().containsWaterbody(wbOut));
			   System.out.println("Out Flux check2: " + info.getOutGroup().containsWaterbody(wbIn));
			   System.out.println("In Flux check: " + info.getInGroup().containsWaterbody(wbOut));
			   System.out.println("In Flux check2: " + info.getInGroup().containsWaterbody(wbIn));

			   }*/
			//System.out.println("Out : " + wbOut);
			if (info.getInGroup().containsWaterbody(wbIn)
			    && info.getOutGroup().containsWaterbody(wbOut)) {
			    //System.out.println("Flux recognized");
			    particleFlux++;
			}
			if( info.getOutGroup().containsWaterbody(wbIn)
			    && info.getInGroup().containsWaterbody(wbOut)){
			    //System.out.println("Reverse Flux recognized");
			    particleFlux--;
			}
			traceNum++;
		    }//if (traceArray[pNum].getTime(traceNum) == index*
		    flux[index] += particleFlux;
		    //System.out.println("particleFlux["+index+"]="+Flux[index]);
		}//for(index
	    } catch (java.lang.ArrayIndexOutOfBoundsException e) {
		e.printStackTrace();
	    }
	}//for(pNum
    }

    /**
     *  
     */
    boolean isCorrectType(Waterbody wbIn, Waterbody wbOut) {
	return info.getInGroup().containsWaterbody(wbIn)
	    && info.getOutGroup().containsWaterbody(wbOut);
    }

    protected FluxFixedData info;
}
