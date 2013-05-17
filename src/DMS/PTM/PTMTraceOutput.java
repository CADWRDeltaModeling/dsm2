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

//$Id: PTMTraceOutput.java,v 1.2.6.1 2006/04/04 18:16:24 eli2 Exp $
package DWR.DMS.PTM;
import java.io.*;
import java.lang.*;
/**
 *  Outputs trace information to a file in binary or ascii mode.
 *  <p>
 */
public class PTMTraceOutput extends PTMOutput{

  /**
   *  Constructor for ascii or binary
   */
  public PTMTraceOutput(String filename, int type, 
                        int startTime, int endTime, int PTMTimeStep, 
                        int nParticles) throws IOException{
    super(filename, type);
    if (getOutputType() == Globals.ASCII) 
      writeHeaderAscii(startTime, endTime, PTMTimeStep, nParticles);
    else if (getOutputType() == Globals.BINARY) 
      writeHeaderBinary(startTime, endTime, PTMTimeStep, nParticles);
  }

  /**
   *  output function
   */
  public void output(int tmStamp, int particleNum, int nodeNum, int wbNum){
    if (getOutputType() == Globals.ASCII) 
      writeOutputAscii(tmStamp, particleNum, nodeNum, wbNum);
    else if (getOutputType() == Globals.BINARY) 
      writeOutputBinary(tmStamp, particleNum, nodeNum, wbNum);
  }

  /**
   *  write output in ascii format
   */
  protected final void writeOutputAscii(int tmStamp, 
                                        int particleNum, 
                                        int nodeNum, 
                                        int wbNum){
    try{
      String line = tmStamp 
                  + " " +  particleNum 
                  + " " +  nodeNum
                  + " " +  wbNum;
      
      outputWriter.write(line,0,line.length());
      outputWriter.newLine();
      outputWriter.flush();
    }
    catch ( IOException ioe){
      System.out.println("Exception occurred in PTMTraceOutput.writeOutputAscii");
      System.out.println("IOException : " + ioe);
    }
  }

  /**
   *  write output in binary format
   */
  protected final void writeOutputBinary(int tmStamp, 
                                         int particleNum, 
                                         int nodeNum, 
                                         int wbNum){
    try{
      outputStream.writeInt(tmStamp); 
      outputStream.writeInt(particleNum);
      outputStream.writeShort(nodeNum);
      outputStream.writeShort(wbNum);
      outputStream.flush();
    }
    catch (IOException ioe){
      System.out.println("Exception occurred in PTMTraceOutput.writeOutputBinary");
      System.out.println("IOException : " + ioe);
    }
  }

  /**
   *  write header in ascii format
   */
  protected final void writeHeaderAscii(int startTime, int endTime, int PTMTimeStep, 
                                        int nParticles){
    try{
      String line = startTime 
                  + " " +  endTime  
                  + " " +  PTMTimeStep  
                  + " " +  nParticles;
  
      outputWriter.write(line, 0, line.length());
      outputWriter.newLine();
      outputWriter.flush();
    }catch(IOException ioe){
      System.out.println("Exception occurred in PTMTraceOutput.writeOutputAscii");
      System.out.println("IOException : " + ioe);
    }
  }

  /**
   *  write header in binary format
   */
  protected final void writeHeaderBinary(int startTime, int endTime, int PTMTimeStep, 
                                         int nParticles){
    try { 
      outputStream.writeInt(startTime);
      outputStream.writeInt(endTime);
      outputStream.writeInt(PTMTimeStep);
      outputStream.writeInt(nParticles);
    } catch (IOException ioe){
      System.out.println("Exception occurred in PTMTraceOutput.writeHeaderBinary");
      System.out.println("IOException : " + ioe);
    }
  }

}

