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

//$Id: PTMOutput.java,v 1.2 2000/08/07 17:00:29 miller Exp $
package DWR.DMS.PTM;
import java.lang.*;
import java.io.*;
/**
 *  CLASS
 *  PTMOutput
 * 
 *  This class is responsible for output from the PTM model.<br>
 *  The types of output needed maybe<br>
 *  1. In ascii form for a certain # of particles @ certain time intervals<br>
 *  2. In binary form for a certain # of particles @ certain time intervals<br>
 *  3. In information enough to generate post processing questions such
 *  as residence time, pass node information, circling particles etceta.<br>
 *  <p>
 */

public class PTMOutput{
public PTMOutput() {
}
  /**
   *  constructor
   *  set filename for output in instanteous location form
   */
public PTMOutput(String filename, int type) throws IOException{
  initialize(filename, type);
}

  /**
   *
   */
public void initialize(String filename, int type) throws IOException{
  outputFilename = filename;
 if (type == Globals.ASCII)
    outputWriter = new BufferedWriter(new FileWriter(outputFilename));
  else if (type == Globals.BINARY)
    outputStream = new DataOutputStream((new FileOutputStream(outputFilename)));
}
  /*
   * returns type of output
   */
public int getOutputType(){
  if(outputWriter == null) 
    return Globals.BINARY;
  else 
    return Globals.ASCII;
}

  /**
   *  filename for output
   */
protected String outputFilename;

  /**
   *  stream for ASCII output
   */
protected BufferedWriter outputWriter;

  /**
   *  stream for BINARY output
   */
protected DataOutputStream outputStream;
}

