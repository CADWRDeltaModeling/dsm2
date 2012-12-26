//    Copyright (C) 1996, 2009 State of California, Department of Water
//    Resources.
//
//    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
//    numerical model.  No protection claimed in original FOURPT and
//    Branched Lagrangian Transport Model (BLTM) code written by the
//    United States Geological Survey.  Protection claimed in the
//    routines and files listed in the accompanying file "Protect.txt".
//    If you did not receive a copy of this file contact 
//    Tara Smith, below.
//
//    This program is licensed to you under the terms of the GNU General
//    Public License, version 2, as published by the Free Software
//    Foundation.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, contact Tara Smith, below,
//    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
//    02139, USA.
//
//    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
//    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
//    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
//    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
//    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
//    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
//    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
//    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
//    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
//    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
//    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
//    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
//    DAMAGE.
//
//    For more information about DSM2, contact:
//
//    Tara Smith
//    California Dept. of Water Resources
//    Division of Planning, Delta Modeling Section
//    1416 Ninth Street
//    Sacramento, CA  95814
//    916-653-9885
//    tara@water.ca.gov
//
//    or see our home page: http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/

//$Id: ParticleFixedInfo.java,v 1.7.6.1 2006/04/04 18:16:25 eli2 Exp $
package DWR.DMS.PTM;
/**
 *  ParticleFixedInfo
 * 
 *  This class encapsulates the fixed input information for the
 *  particle object.
 *  <p>
 */
class ParticleFixedInfo{
  

  /**
   *  constructor
   */
public  ParticleFixedInfo(){
}
  
  /**
   *  sets the logical variable values
   */
  public final void setBehavior(ParticleBehavior behave){
    useBehaviorFile = true;
    behavior = behave;
  }
  /**
   *  sets the logical variable values
   */
public final void setVariables(boolean ivert,boolean itrans,
			       boolean iey,boolean iez,
			       boolean iprof,boolean igroup,
			       boolean fluxPercent, boolean groupPercent,
			       boolean fluxCumulative){
  useVerticalProfile = ivert;
  useTransverseProfile = itrans;
  useTransverseMixing = iey;
  useVerticalMixing = iez;
  doProfileOutput = iprof;
  doGroupOutput = igroup;
  doFluxPercentage = fluxPercent;
  doGroupPercentage = groupPercent;
  doFluxCumulative = fluxCumulative;
}

  /**
   *  sets the float variable values
   */
public final void setVariables(int random_seed,
			       float trans_constant,float vert_constant,
			       float trans_a_coef, float trans_b_coef,
			       float trans_c_coef, int animated_particles){
  randomSeed = random_seed;
  Ct = trans_constant;
  Cv = vert_constant;
  Aq = trans_a_coef;
  Bq = trans_b_coef;
  Cq = trans_c_coef;
  animatedParticles = animated_particles;
}

  /**
   *  sets the insertion variable values
   */
public final void setVariables(int nInjections, 
                               int[] nNode, 
                               int[] nInjected, 
                               int[] startJulmin, 
                               int[] lengthJulmin){
  
  numberOfInjections = nInjections;

  particleInjectionNode = nNode;
  numberOfParticlesInjected = nInjected;
  particleInjectionStartJulmin = startJulmin;
  particleInjectionLengthJulmin = lengthJulmin;

}

  /**
   *  sets the number of groups and Qual binary file info
   */
  public final void setVariables(int nGroups, boolean qBinary, String[] qNames){
    numberOfGroups = nGroups;
    qualityBinary = qBinary;
    qualityNames = qNames;
  }

  /**
   *  return random seed
   */
public final ParticleBehavior getBehavior(){
  return behavior;
}

  /**
   *  return the existance of a behavior file
   */
public final boolean getBehaviorExists(){
  return useBehaviorFile;
}

  /**
   *  return random seed
   */
public final int getRandomSeed(){
  return randomSeed;
}

  /**
   *  return number of desired animated particles
   */
public final int getAnimatedParticles(){
  return animatedParticles;
}

  /**
   *  return the vertical constant
   */
public final float getVerticalConstant(){
  return Cv;
}



  /**
   *  return the transverse constant
   */
public final float getTransverseConstant(){
  return Ct;
}    


  /**
   *  return the transverse A coefficient
   */
public final float getTransverseACoef(){
  return Aq;
}    


  /**
   *  return the transverse B coefficient
   */
public final float getTransverseBCoef(){
  return Bq;
}    


  /**
   *  return the transverse C coefficient
   */
public final float getTransverseCCoef(){
  return Cq;
}    


  /**
   *  allowed to move vertically
   */
public final boolean moveVertically(){
  return useVerticalMixing;
}


  /**
   *  allowed to move laterally
   */
public final boolean moveLaterally(){
  return useTransverseMixing;
}


  /**
   *  OK to use transverse profile
   */
public final boolean doTransverseProfile(){
  return useTransverseProfile;
}


  /**
   *  OK to use vertical profile
   */
public final boolean doVerticalProfile(){
  return useVerticalProfile;
}

  /**
   *  OK to use percentage for flux output
   */
public final boolean doFluxPercentage(){
  return doFluxPercentage;
}
  /**
   *  OK to use percentage for group output
   */
public final boolean doGroupPercentage(){
  return doGroupPercentage;
}
  /**
   *  OK to use cumulative flux output
   */
public final boolean doFluxCumulative(){
  return doFluxCumulative;
}
  /**
   *  return the number of channel groups
   */
public int getNumberOfGroups(){
  return numberOfGroups;
}
  /**
   *  returns true if a qual binary is available
   */
public boolean getBinaryExistance(){
  return qualityBinary;
}
  /**
   *  return the number of channel groups
   */
public String[] getQualityNames(){
  return qualityNames;
}

  /**
   *  return the total number of injected particles
   */
public final int getTotalNumberOfInjectedParticles(){
  int numberOfInjectedParticles=0;
  for(int i=0; i< numberOfInjections; i++)
    numberOfInjectedParticles += numberOfParticlesInjected[i];
  return numberOfInjectedParticles;
}


  /**
   *  return the total number of injections
   */
public final int getNumberOfInjections(){
  return numberOfInjections;
}


  /**
   *  return the number of particles injected for specified injection id
   */
public final int getNumberOfParticlesInjected(int injectionId){
  return numberOfParticlesInjected[injectionId-1];
}


  /**
   *  Return the node EnvIndex for the specified injection id
   */
public final int getLocationOfParticlesInjected(int injectionId){
  return particleInjectionNode[injectionId-1];
}


  /**
   *  return the start of injection for specified injection id
   */
public final int getInjectionStartJulmin(int injectionId){
  return particleInjectionStartJulmin[injectionId-1];
}


  /**
   *  return the length of injection for specified injection id
   */
public final int getInjectionLengthJulmin(int injectionId){
  return particleInjectionLengthJulmin[injectionId-1];
}

public String toString(){
  String rep = "Particle Fixed Information "+ "\n" 
    + "useVerticalProfile "+ useVerticalProfile+ "\n" 
    + "useTransverseProfile "+ useTransverseProfile+ "\n" 
    + "useTransverseMixing "+ useTransverseMixing+ "\n" 
    + "useVerticalMixing "+ useVerticalMixing+ "\n" 
    + "randomSeed "+ randomSeed+ "\n"
    + "animatedParticles "+ animatedParticles+ "\n"
    + "Aq "+ Aq+ "\n"
    + "Bq "+ Bq+ "\n"
    + "Cq "+ Cq+ "\n"
    + "numberOfInjections "+ numberOfInjections+ "\n" 
    + "particleInjectionNode[0] "+ particleInjectionNode[0]+ "\n" 
    + "numberOfParticlesInjected[0] "+ numberOfParticlesInjected[0]+ "\n" 
    + "particleInjectionStartJulmin[0] "+ particleInjectionStartJulmin[0]+ "\n" 
    + "particleInjectionLengthJulmin[0] "+ particleInjectionLengthJulmin[0]+ "\n";
  return rep;
}

protected ParticleBehavior behavior;
  /**
   *  true if behaviors exist
   */
protected boolean useBehaviorFile;

  /**
   *  true to use a vertical velocity profile
   */
protected boolean useVerticalProfile;

  /**
   *  true to use transverse velocity profile
   */
protected boolean useTransverseProfile;

  /**
   *  true to use transverse mixing
   */
protected boolean useTransverseMixing;

  /**
   *  true to use vertical mixing
   */
protected boolean useVerticalMixing;

  /**
   *  true to output profiling
   */
protected boolean doProfileOutput;

  /**
   *  true reads from a file groups of chnls & res and writes # of parts in each group
   */
protected boolean doGroupOutput;

  /**
   *  true reads from a file groups of chnls & res and writes # of parts in each group
   */
protected boolean doFluxPercentage;

  /**
   *  true reads from a file groups of chnls & res and writes # of parts in each group
   */
protected boolean doGroupPercentage;

  /**
   *  true reads from a file groups of chnls & res and writes # of parts in each group
   */
protected boolean doFluxCumulative;

  /**
   *  initalize random number generator with this seed
   */
protected int randomSeed;

  /**
   *  number of particles to be included in animation
   */
protected int animatedParticles;

  /**
   *  Transverse constant Ct
   */
protected float Ct;

  /**
   *  Vert Constant
   */
protected float Cv;

  /**
   *  Transverse velocity A coefficient
   */
protected float Aq;

  /**
   *  Transverse velocity B coefficient
   */
protected float Bq;

  /**
   *  Transverse velocity C coefficient
   */
protected float Cq;

  /**
   *  number of injections of particles
   */
protected int numberOfInjections;

  /**
   *  node at which to inject the particle
   */
protected int[] particleInjectionNode;

  /**
   *  number released at said node.
   */
protected int[] numberOfParticlesInjected;

  /**
   *  starting injection time
   */
protected int[] particleInjectionStartJulmin;

  /**
   *  length of injection
   */
protected int[] particleInjectionLengthJulmin;
  /**
   *  number of channel groups
   */
protected int numberOfGroups;
  /**
   *  binary file existance
   */
protected boolean qualityBinary;
  /**
   *  array of quality constituent names
   */
protected String[] qualityNames;

}

