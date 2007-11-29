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

//$Id: PTMTraceInput.java,v 1.2.8.1 2003/04/08 21:21:15 miller Exp $
/**
 *  CLASS
 * 
 *  PTMTraceInput
 * 
 *  Reads particle trace one particle, one trace at a time. This trace can then
 *  be stored in a particleTrace array.
 *  <p>
 */
package DWR.DMS.PTM;
import java.io.*;
import java.util.StringTokenizer;
public class PTMTraceInput extends PTMInput{
  /**
   *  constructor
   */
public PTMTraceInput(String filename, int type,
		     int []  startTime, int []  endTime, int []  PTMTimeStep, 
		     int []  nParticles) throws FileNotFoundException{
  super(filename, type);
  try {
    inputHeader(startTime, endTime, PTMTimeStep, nParticles);
  }catch(IOException e){
    System.out.println("PTMTraceInput: constructor" +
		       "Error reading trace file: " + filename);
  }
  trace = new traceData();
}

  /**
   *  reads trace file header information
   */
protected final void inputHeader(int []  startTime, 
				 int []  endTime, 
				 int []  PTMTimeStep, 
				 int []  nParticles) throws IOException {
  if (inputReader != null) 
    readHeaderAscii(startTime, endTime, PTMTimeStep, nParticles);
  else if (inputStream != null )
    readHeaderBinary(startTime, endTime, PTMTimeStep, nParticles);
  else 
    throw new IOException("PTMTraceInput: inputHeader(), No input stream initialized");
}

  /**
   *  reads one trace at a time.
   */
public final void input(int []  tmStamp, 
			int []  particleNum, 
			int []  nodeNum, 
			int []  wbNum) throws IOException{
  if (inputReader != null) 
    readInputAscii();
  else if (inputStream != null) 
    readInputBinary();
  else
    throw new IOException("PTMTraceInput: input(), No input stream initialized");
   
  tmStamp[0] = (trace.timeStamp);
  particleNum[0] = (trace.particleNumber);
  nodeNum[0] = (trace.nodeNumber);
  wbNum[0] = (trace.waterbodyNumber);
}

  /**
   *  trace input structure
   */
protected traceData trace;

  /**
   *  reads input from ascii trace file
   */
protected final void readInputAscii() throws IOException{
  try{
    String line = inputReader.readLine();
    if(line == null) {
      trace.timeStamp = -1;      
      trace.particleNumber = -1; 
      trace.nodeNumber = -1;     
      trace.waterbodyNumber = -1;
      return;
    }
    StringTokenizer sToken = new StringTokenizer(line);
    
    trace.timeStamp = (new Integer(sToken.nextToken())).intValue();
    trace.particleNumber = (new Integer(sToken.nextToken())).intValue();
    trace.nodeNumber = (new Integer(sToken.nextToken())).intValue();
    trace.waterbodyNumber = (new Integer(sToken.nextToken())).intValue();

  }
  catch (EOFException e){
    trace.timeStamp = -1;      
    trace.particleNumber = -1; 
    trace.nodeNumber = -1;     
    trace.waterbodyNumber = -1;
  }
}


  /**
   *  reads input from binary trace file
   */
protected final void readInputBinary() throws IOException{
 try{
    trace.timeStamp = inputStream.readInt();
//      trace.particleNumber = (int) inputStream.readShort();
    trace.particleNumber = inputStream.readInt();
    trace.nodeNumber = (int) inputStream.readShort();
    trace.waterbodyNumber = (int) inputStream.readShort();
  }
  catch (EOFException e){
    trace.timeStamp = -1;      
    trace.particleNumber = -1; 
    trace.nodeNumber = -1;     
    trace.waterbodyNumber = -1;
  }
}


  /**
   *  reads header from ascii trace file
   */
protected final void readHeaderAscii(int []  startTime, int []  endTime, 
				     int []  PTMTimeStep, int []  nParticles){
 try{
    String line = inputReader.readLine();
    StringTokenizer sToken = new StringTokenizer(line);
    
    startTime[0] = new Integer(sToken.nextToken()).intValue();
    endTime[0] =  new Integer(sToken.nextToken()).intValue();
    PTMTimeStep[0] =  new Integer(sToken.nextToken()).intValue();
    nParticles[0] =  new Integer(sToken.nextToken()).intValue();

  }
  catch (IOException e){
    System.out.println("PTMTraceInput: readHeaderAscii:" + 
		       " Incorrect format or corrupted trace file");
  }
}

  /**
   *  reads header from trace file.
   */
protected final void readHeaderBinary(int [] startTime, int [] endTime, 
				      int [] PTMTimeStep, int [] nParticles){
  try{
    startTime[0] =( inputStream.readInt());
    endTime[0] =( inputStream.readInt());
    PTMTimeStep[0] =( inputStream.readInt());
    nParticles[0] =( inputStream.readInt());
  }
  catch (IOException e){
    System.out.println("PTMTraceInput: readHeaderBinary:" + 
		       " Incorrect format or corrupted trace file");
  }
}
}

