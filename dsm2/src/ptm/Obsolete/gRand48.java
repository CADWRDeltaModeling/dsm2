//    Copyright (C) 1996 State of California, Department of Water
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

//    Copyright (C) 1996 State of California, Department of Water
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

//$Id: gRand48.java,v 1.2 2000/08/07 17:00:32 miller Exp $

// MT-LEVEL: Safe
// returns a gaussian random number generated from a uniformly
// distributed pseudo-random number generation
// Generate Gaussian R.V. (mean=0, variance=1)
//? should all particles share a common random number generator?
//? search for better random number generators
public class gRand48{

 boolean haveNextNextGaussian;
 double nextNextGaussian;



 final void setSeed(long seed){ srand48(seed); }

 final float uniFDraw() { return (float) drand48();}

 final double uniDdraw() { return drand48();}

 final float fdraw(){
  // generate uniform random numbers
  float rN1 = uniFDraw();
  if (rN1 == 1.0) rN1=uniFDraw();
  float rN2 = rN1;
  if (rN1 > 0.5) rN2 = 1 - rN1;

  // transform uniform random numbers to gaussian distribution
  float v = sqrt(-2.0*log(rN2));
  float v2 = v*v;
  float v3 = v2*v;

  float rNumber=v-(2.51557+0.802853*v+0.010328*v2)/
    (1.0+1.432788*v+0.189269*v2+0.001308*v3);

  if(rN1 < 0.5)  
    return -rNumber;
  else 
    return rNumber;
}

 final double ddraw(){
  double rN1 = uniDdraw();
  if (rN1 == 1.0) rN1=uniDdraw();
  double rN2 = rN1;
  if (rN1 > 0.5) rN2 = 1 - rN1;
  double v = sqrt(-2.0*log(rN2));
  double v2 = v*v;
  double v3 = v*v*v;
  double rNumber=v-(2.51557+0.802853*v+0.010328*v2)/
    (1.0+1.432788*v+0.189269*v2+0.001308*v3);
  if(rN1 < 0.5)  
    return -rNumber;
  else 
    return rNumber;
}

 final double nextGaussian(){
  // See Knuth, ACP, Section 3.4.1 Algorithm C.
  if (haveNextNextGaussian) {
    haveNextNextGaussian = false;
    return nextNextGaussian;
  } else {
    double v1, v2, s;
    do { 
      v1 = 2 * uniDdraw() - 1; // between -1 and 1
      v2 = 2 * uniDdraw() - 1; // between -1 and 1 
      s = v1 * v1 + v2 * v2;
    } while (s >= 1);
    double multiplier = sqrt(-2.0 * log(s)/s);
    nextNextGaussian = v2 * multiplier;
    haveNextNextGaussian = true;
    return v1 * multiplier;
  }
}



