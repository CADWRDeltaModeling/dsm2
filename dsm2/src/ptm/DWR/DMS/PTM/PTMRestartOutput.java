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

//$Id: PTMRestartOutput.java,v 1.2 2000/08/07 17:00:29 miller Exp $
package DWR.DMS.PTM;
import java.io.*;
/**
 *  This class handles output from PTM for animation purposes. It records at a
 *  given instant of time the Particle Id and the normalized x,y and z location
 *  of the Particle.<br>
 *  <p>
 * 
 */
public class PTMRestartOutput extends PTMOutput{

  /**
   * constructor
   */
  public PTMRestartOutput(String filename, int type, 
                          int outInterval,
                          Particle [] particles) throws IOException{
    super(filename, type);
    outputInterval = outInterval;
    previousOutputTime = Globals.currentModelTime - outputInterval;
    this.particles = particles;
    this.output();
  }

  /**
   *  output function
   */
  public void output() throws IOException{
    if(Globals.currentModelTime >= previousOutputTime+outputInterval){
      previousOutputTime = Globals.currentModelTime;
      int outputType = getOutputType();
      if(outputType == Globals.BINARY) {
        initialize(outputFilename, outputType);
        outputBinary();
        outputStream.close();
      }
      else if (outputType == Globals.ASCII) {
        initialize(outputFilename, outputType);
        outputAscii();
        outputWriter.close();
      }
    }
  }

  /**
   *  output ascii
   */
  private final void outputAscii() throws IOException{
    String modelDate = Globals.getModelDate(Globals.currentModelTime);
    String modelTime = Globals.getModelTime(Globals.currentModelTime);
    String line = modelDate + " " + modelTime;
    outputWriter.write(line, 0, line.length());
    outputWriter.newLine();
    line = particles.length + " ";
    outputWriter.write(line, 0, line.length());
    outputWriter.newLine();
    
    for (int i=0; i< particles.length; i++){
      if(particles[i] != null){
        line = particles[i] + " ";
        outputWriter.write(line, 0, line.length());
        outputWriter.newLine();
      }
    }
  }

  /**
   *  output binary
   */
  private final void outputBinary() throws IOException{
    int nParticles = particles.length;
    outputStream.writeInt(Globals.currentModelTime);
    outputStream.writeInt(nParticles);
    for (int i=0; i< particles.length; i++){
      if(particles[i] != null){
        String line = particles[i].toString();
        outputStream.writeUTF(line);
      }
    }
  }
  
  protected int outputInterval, previousOutputTime;
  protected Particle [] particles;
}


