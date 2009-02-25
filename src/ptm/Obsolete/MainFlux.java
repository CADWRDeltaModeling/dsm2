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
//$Id: MainFlux.java,v 1.2 2000/08/07 17:00:33 miller Exp $
package DWR.DMS.PTM;
/**
  *
  */
public class MainFlux {

public static void main(String[] args) {
  try {
    //set version and module ids
    Globals.initialize();
//    int i;

    // Initialize environment
    String fixedInputFilename = "dsm2.inp";
    if(args.length > 0) fixedInputFilename = args[0];
    PTMEnv Environment = new PTMEnv(fixedInputFilename);
    
    if (DEBUG) System.out.println("Environment initialized");
    // set global environment pointer
    Globals.Environment = Environment;
    
    // get runtime parameters
//    int startTime = Environment.getStartTime();
//    int runTime = Environment.getRunLength();
//    int endTime = startTime + runTime;
//    int PTMTimeStep = Environment.getPTMTimeStep();
    if(DEBUG) System.out.println("Initialized model run times");

//    int timeStep = PTMTimeStep*60;
//    int displayInterval = Environment.getDisplayInterval();
    

    String traceFileName = Environment.getTraceFileName();
    // output flux calculations in dss format
    FluxInfo fluxFixedInfo = Environment.getFluxFixedInfo();
    
    GroupInfo groupFixedInfo = Environment.getGroupFixedInfo();

    FluxMonitor fluxCalculator = new FluxMonitor(traceFileName,
						   Environment.getFileType(traceFileName),
						   fluxFixedInfo,
						   groupFixedInfo);
    
    fluxCalculator.calculateFlux();
    
    fluxCalculator.writeOutput();

    System.out.println("");
  }catch(Exception e){
    e.printStackTrace();
    System.out.println("Exception " + e + " occurred");
    System.exit(-1);
  }
}

public static boolean DEBUG = false;  
protected static int previousDisplayTime = 0;
  //public native static void display(int displayInterval);
public static void display(int displayInterval) {
  if(previousDisplayTime == 0){ 
    previousDisplayTime = Globals.currentModelTime - displayInterval;
  }

  if(Globals.currentModelTime >= previousDisplayTime + displayInterval){
    // output model date and time
    String modelDate = Globals.getModelDate(Globals.currentModelTime);
    String modelTime = Globals.getModelTime(Globals.currentModelTime);
    System.out.println("Model date: " + modelDate + " time: " + modelTime);
    previousDisplayTime = Globals.currentModelTime;
  }
}


}
