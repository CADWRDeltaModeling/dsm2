//    Copyright (C) 1996 State of California, Department of Water
//    Resources.
//
//    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
//    numerical model.  No protection claimed in original FOURPT and
//    Branched Lagrangian Transport Model (BLTM) code written by the
//    United States Geological Survey.  Protection claimed in the
//    routines and files listed in the accompanying file "Protect.txt".
//    If you did not receive a copy of this file contact Dr. Paul
//    Hutton, below.
//
//    This program is licensed to you under the terms of the GNU General
//    Public License, version 2, as published by the Free Software
//    Foundation.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, contact Dr. Paul Hutton, below,
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
//    Dr. Paul Hutton
//    California Dept. of Water Resources
//    Division of Planning, Delta Modeling Section
//    1416 Ninth Street
//    Sacramento, CA  95814
//    916-653-5601
//    hutton@water.ca.gov
//
//    or see our home page: http://wwwdelmod.water.ca.gov/

package DWR.DMS.PTM;
import java.io.*;
/**
 * This class outputs information for animation. At any 
 * given instant of time the particle Id 
 * and the normalized x,y and z location are output.
 * of the particle.<br>
 * 
 * @author Nicky Sandhu
 * @version $Id: PTMAnimationOutput.java,v 1.7 2000/08/07 17:00:27 miller Exp $
 */
public class PTMAnimationOutput extends PTMOutput{
  /**
   * 
   * @param filename Name of animation output file
   * @param type format of output: ascii/binary
   * @param interval The number of minutes between animation information
   * @param numberOfParticles The number of particles for animation output
   * @param particleArray The array of particles 
   */
public PTMAnimationOutput(String filename,
			  int type,
			  int interval, 
			  int numberOfParticles, 
			  int requestedNumberOfParticles,
			  particle [] particleArray) throws IOException{
  super(filename, type);
  outputInterval = interval;
  setOutputParameters(numberOfParticles, requestedNumberOfParticles, particleArray);
}
  /**
   *  particle array and size
   */
private final void setOutputParameters(int numParts, 
				       int requestedNumParts,
				      particle [] pArray){
  if (requestedNumParts <= MAX_NUMBER_OF_PARTICLES && requestedNumParts <= numParts)
    numberOfParticles = requestedNumParts;
  else if (numParts <= MAX_NUMBER_OF_PARTICLES)
    numberOfParticles = numParts;
  else
    numberOfParticles = MAX_NUMBER_OF_PARTICLES;
  particlePtrArray = new particle[numberOfParticles];
  System.arraycopy(pArray,0,particlePtrArray,0,particlePtrArray.length);
  previousOutputTime = Globals.currentModelTime - outputInterval;
}

  /**
   *  output function
   */
public void output() throws IOException{
  instantaneousOutput [] outputData = new instantaneousOutput[MAX_NUMBER_OF_PARTICLES];
  int julianMin = Globals.currentModelTime;
  if(Globals.currentModelTime >= previousOutputTime+outputInterval){
    previousOutputTime = Globals.currentModelTime;
    updateOutputStructure(outputData);
    int outputType = getOutputType();
    if (outputType == Globals.ASCII) {
      // write out into ascii file
      writeOutputAscii(outputData);
    }
    else if (outputType == Globals.BINARY) {
      // write out into binary file
      writeBinary(outputData);
    }
  }
}

public void writeBinary(instantaneousOutput [] outputData) throws IOException{
 int julianMin = Globals.currentModelTime;
  String modelDate, modelTime;
  modelDate = Globals.getModelDate(julianMin);
  modelTime = Globals.getModelTime(julianMin);
  String line = modelDate; 
  outputStream.writeUTF(line);
  outputStream.writeShort(new Short(modelTime).shortValue());
  outputStream.writeShort(numberOfParticles);
  for (int i=0; i< numberOfParticles; i++){
    outputStream.writeShort(outputData[i].particleNumber);
    outputStream.writeShort(outputData[i].channelNumber);
    outputStream.writeShort(outputData[i].normXDistance);
    outputStream.writeShort(outputData[i].normYDistance);
    outputStream.writeShort(outputData[i].normZDistance);
    outputStream.writeShort(outputData[i].value);
  }
}
  
  /**
   *  output ascii
   */
private final void writeOutputAscii( instantaneousOutput [] outputData ) throws IOException{
  int julianMin = Globals.currentModelTime;
  String modelDate, modelTime;
  modelDate = Globals.getModelDate(julianMin);
  modelTime = Globals.getModelTime(julianMin);

  String line = modelDate + " " + modelTime;
  outputWriter.write(line, 0, line.length());
  outputWriter.newLine();
  line = numberOfParticles + " ";
  outputWriter.write(line, 0, line.length());
  outputWriter.newLine();

  for (int i=0; i< numberOfParticles; i++){
    line = "  " + outputData[i].particleNumber 
      + "  " + outputData[i].channelNumber
      + "  " + outputData[i].normXDistance
      + "  " + outputData[i].normYDistance
      + "  " + outputData[i].normZDistance
      + "  " + outputData[i].value;
    outputWriter.write(line, 0, line.length());
    outputWriter.newLine();
  }
}

  /**
   *  fill up output data structure with current information
   */
private final void updateOutputStructure(instantaneousOutput [] outputData){
  waterbody wb=null;
  float []  x = new float [1],y = new float [1],z = new float [1];
  for(int i=0; i<numberOfParticles; i++) {

    outputData[i] = new instantaneousOutput();
    outputData[i].particleNumber = (short) particlePtrArray[i].getId();
    
    wb = particlePtrArray[i].getLocation(x, y, z);
    if ( wb != null ) {
      if( wb.getPTMType() ==  waterbody.CHANNEL) {
	outputData[i].channelNumber = (short) wb.getEnvIndex();
	outputData[i].normXDistance = 
	  (short) ((1.0f-x[0]/((channel )wb).getLength())*100);
	outputData[i].normYDistance = 
	  (short) (y[0]/((channel )wb).getWidth(x[0])*100);
	outputData[i].normZDistance = 
	  (short) (z[0]/((channel )wb).getDepth(x[0])*100);
	outputData[i].value = (short) (1);
      }
      else {
	outputData[i].channelNumber = (short) -1;
	outputData[i].normXDistance = (short)-1;
	outputData[i].normYDistance = (short)-1;
	outputData[i].normZDistance = (short)-1;
	outputData[i].value = (short)0;
      }
    }
    else {
      outputData[i].channelNumber = (short) -1;
      outputData[i].normXDistance = (short)-1;
      outputData[i].normYDistance = (short)-1;
      outputData[i].normZDistance = (short)-1;
      outputData[i].value = (short)0; 
    }
  } 
}

  public void FlushAndClose() {

    int outputType = getOutputType();
    try {
      if (outputType == Globals.ASCII) {
	outputWriter.write(endOfFile, 0, endOfFile.length());
	outputWriter.newLine();
	outputWriter.flush();
	outputWriter.close();
      }
      else if (outputType == Globals.BINARY) {
	outputStream.writeUTF(endOfFile);
	outputStream.flush();
	outputStream.close();
      }
    } catch(IOException e){System.out.println(e); }
  }
  
private final int DEFAULT_OUTPUT_INTERVAL = 15;

private final int MAX_NUMBER_OF_PARTICLES=20000;

  /**
   *  number of outputs for instantaneous
   */
private int numberOfParticles;

  /**
   *  particle pointer array
   */
private particle [] particlePtrArray;

  /**
   *  keeps track of last output time
   */
private int previousOutputTime;

  /**
   *  output interval
   */
private int outputInterval;

  /**
    *  End of File tag
    */
private String endOfFile = "EOF";

}
