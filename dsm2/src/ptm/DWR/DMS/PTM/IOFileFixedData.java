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
 * Encapsulates input/output information for the particle tracking
 * model
 *
 * @author Nicky Sandhu
 * @version $Id: IOFileFixedData.java,v 1.1.1.1 1999/09/28 23:43:13 miller Exp $
 */
public class IOFileFixedData {

  /**
   * Constructor
   */
  public IOFileFixedData(String animationFileName, int animationOutputInterval,
                         String traceFileName, int traceOutputInterval,
                         String restartOutputFileName, int restartOutputInterval,
                         String restartInputFileName){

    this.animationFileName=animationFileName;
    this.animationOutputInterval=animationOutputInterval;
    this.traceFileName=traceFileName;
    this.traceOutputInterval=traceOutputInterval;
    this.restartOutputFileName=restartOutputFileName;
    this.restartOutputInterval=restartOutputInterval;
    this.restartInputFileName=restartInputFileName;
  }

  /**
   * String representation
   */
  public String toString(){
    String rep = "IO File Fixed Data" + "\n";
    rep += "Animation File Name: " + animationFileName + "\n";
    rep += "Trace File Name: " + traceFileName + "\n";
    rep += "RestartOutput File Name: " + restartOutputFileName + "\n";
    rep += "RestartInput File Name: " + restartInputFileName + "\n";
    return rep;
  }
  /**
   *
   */
  public String animationFileName;
  /**
   *
   */
  public int animationOutputInterval;
  /**
   *
   */
  public String traceFileName;
  /**
   *
   */
  public int traceOutputInterval;
  /**
   *
   */
  public String restartOutputFileName;
  /**
   *
   */
  public int restartOutputInterval;
  /**
   *
   */
  public String restartInputFileName;
}
