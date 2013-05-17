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

//$Id: PTMRestartInput.java,v 1.2 2000/08/07 17:00:29 miller Exp $
/**
 *  This class handles output from PTM for animation purposes. It records at a
 *  given instant of time the Particle Id and the normalized x,y and z location
 *  of the Particle.<br>
 *  <p>
 * 
 */
package DWR.DMS.PTM;
import java.io.*;
import java.util.StringTokenizer;

public class PTMRestartInput extends PTMInput{

  /**
   * Constructor
   */
  public PTMRestartInput(String filename, 
                         int type, Particle [] particles) throws IOException{
    super(filename, type);
    this.particles = particles;
  }

  /**
   *  input function
   */
  public void input() throws IOException{
    if(getInputType() == Globals.BINARY)
      inputBinary();
    else if (getInputType() == Globals.ASCII) 
      inputAscii();
  }

  /**
   *  input ascii
   */
  protected void inputAscii() throws IOException{
    boolean insertUninserted = false;
    String line = null;
    line = inputReader.readLine();
    StringTokenizer sToken = new StringTokenizer(line);
    String modelDate = sToken.nextToken();
    String modelTime = sToken.nextToken();
    
    int julianTime = Globals.getTimeInJulianMins(modelDate, modelTime);
    if(julianTime != Globals.currentModelTime)
      insertUninserted = true;
    
    line = inputReader.readLine();
    int numberOfParticles = (new Integer(line)).intValue();
    particles = new Particle[numberOfParticles];
    
    for(int pNum = 0; pNum < particles.length; pNum++){
      //? This should really be fixed. This information should be in the restart file.
      particles[pNum] = new Particle(Globals.Environment.getParticleFixedInfo());
      line = inputReader.readLine();
      particles[pNum].fromString(line);
      if (insertUninserted == true){
        if (particles[pNum].inserted == false){
          Node nd = particles[pNum].getRecentNode();
          particles[pNum].setInsertionInfo(Globals.currentModelTime, nd);
        }
      }
    }//end for
  }

  /**
   *  input binary
   */
  protected final void inputBinary() throws IOException{
  }

  /**
   *  Particle pointer array
   */
  protected Particle [] particles;
}


