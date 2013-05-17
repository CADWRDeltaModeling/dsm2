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
package DWR.DMS.PTM;
/**
 *
 */
public class PTMFluxOutput extends PTMOutput{
	
  /**
   *  constructor
   */
  public PTMFluxOutput(int startTime){
    super();
    initializeFluxOutput(startTime);
  }
  
  /**
   * 
   */
  public void GroupOutput(Flux [] groupAt, boolean percent){
    groupPercent = percent;

    if (groupAt.length < MAX_GROUP_OUTPUT){
      nGroup = groupAt.length;
    }else {
      System.out.println("Too many group values required");
      nGroup = MAX_GROUP_OUTPUT;
    }
    
    groupAtNode = groupAt;
  }

  /**
   * 
   */
  public void FluxOutput(Flux [] fluxAt, boolean percent){
    fluxPercent = percent;
    
    if (fluxAt.length < MAX_FLUX_OUTPUT)
      nFlux = fluxAt.length;
    else {
      System.out.println("Too many flux values required");
      nFlux = MAX_FLUX_OUTPUT;
    }
    
    fluxAtNode = fluxAt;
  }
  
  /**
   * 
   */
  public void output() {
  	   
    if (fluxAtNode != null)
      genericFlux = fluxAtNode;
    else if (groupAtNode != null)
      genericFlux = groupAtNode;
    else
      return;
    if (genericFlux[0] == null)
      return;
    
    float fluxOut;
    //
    for (int cTime = genericFlux[0].getStartTime(); cTime < genericFlux[0]
      	 .getEndTime(); cTime += genericFlux[0].getPTMTimeStep()) {
         
      //Node
      for (int i = 0; i < nFlux; i++) {
        if (fluxPercent) {
      	  fluxOut = (fluxAtNode[i].getFlux(cTime) * 100.0f)
                   / fluxAtNode[i].getNumberOfParticles();
      	} else {
          fluxOut = (fluxAtNode[i].getFlux(cTime));
      	}
      	setFlux(i + 1, fluxOut);
      }
      //Group
      for (int i = 0; i < nGroup; i++) {
      	if (groupPercent) {
          fluxOut = (groupAtNode[i].getFlux(cTime) * 100.0f)
                  / groupAtNode[i].getNumberOfParticles();
      	} else {
          fluxOut = (groupAtNode[i].getFlux(cTime));
      	}
      	setGroup(i + 1, fluxOut);
      }
      
      writeFluxOutput();
    }// end for(cTime)
  }
  
  /**
   * 
   */
  public void closeFile(){
    closeFluxOutput();
  }

  protected native void initializeFluxOutput(int startTime);
  protected native void writeFluxOutput();
  protected native void closeFluxOutput();
  protected native void setFlux(int fluxId, float fluxValue);
  protected native void setGroup(int groupId, float groupValue);
  protected final int MAX_FLUX_OUTPUT=50;
  protected final int MAX_GROUP_OUTPUT=50;
  protected boolean groupPercent;
  protected boolean fluxPercent;
  protected int nFlux;
  protected int nGroup;
  protected Flux [] fluxAtNode;
  protected Flux [] groupAtNode;
  protected Flux [] genericFlux;
}

