/*
    Copyright (C) 1998 State of California, Department of Water
    Resources.

    This program is licensed to you under the terms of the GNU General
    Public License, version 2, as published by the Free Software
    Foundation.

    You should have received a copy of the GNU General Public License
    along with this program; if not, contact Dr. Francis Chung, below,
    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
    02139, USA.

    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
    DAMAGE.

    For more information, contact:

    Dr. Francis Chung
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA  95814
    916-653-5601
    chung@water.ca.gov

    or see our home page: http://wwwdelmod.water.ca.gov/
*/
package DWR.CSDP;
import java.io.*;
import java.util.*;

public class OpenWaterAreaStationAsciiInput extends OpenWaterAreaStationInput{

  LineNumberReader _asciiIn;

  /**
   * Open ascii file
   */
protected void open(){
  FileReader aInFile;
  try {
//      if((_directory.substring(_directory.length()-1,_directory.length())).
//         equals(File.separator) == false){
//  	_directory += File.separator;
//      }
    aInFile = new FileReader(_filename + "." + _filetype);
    _asciiIn = new LineNumberReader(aInFile);
    if (DEBUG) System.out.println("In ascii open " + _asciiIn);
  } catch(IOException e) {
    if (DEBUG) System.out.println("Directory + Filename: " +_filename);
    if (DEBUG) System.out.println("Filetype: " + _filetype);
    System.out.println
      ("Error ocurred while opening file "+_filename + _filetype + e.getMessage());
  } // catch()
}

  /**
   * Read ascii file
   */
protected void read(){
  int numLines=0;
  Centerline centerline = null;
  String line = null;
  try{
    line=_asciiIn.readLine();
  }catch(IOException e){
      System.out.println("error reading first line");
  }
    while(line.toLowerCase().indexOf("station") < 0){
	try{
	    line = _asciiIn.readLine();
	} catch(IOException e) {
	    System.out.println
		("Error ocurred while reading file " +_filename + ".prn:" + e.getMessage());
	}//catch
    }
    while(line.toLowerCase().indexOf("end") < 0){
	try{
	    line = _asciiIn.readLine();
	} catch(IOException e) {
	    System.out.println
		("Error ocurred while reading file " +_filename + ".prn:" + e.getMessage());
	}//catch
	if(line.toLowerCase().indexOf("end") < 0){
	    parseStationDistanceLine(line);
	}
    }

    while(line.toLowerCase().indexOf("date") < 0){
	try{
	    line = _asciiIn.readLine();
	} catch(IOException e) {
	    System.out.println
		("Error ocurred while reading file " +_filename + ".prn:" + e.getMessage());
	}//catch
    }

    parseDateHeaders(line);
    int dateIndex=0;
    String lastLine = null;
    while(line.toLowerCase().indexOf("end") < 0){
	try{
	    line = _asciiIn.readLine();
	} catch(IOException e) {
	    System.out.println
		("Error ocurred while reading file " +_filename + ".prn:" + e.getMessage());
	}//catch
	if(line.toLowerCase().indexOf("end") < 0){
	    parseDateLine(dateIndex,line, lastLine);
	}
	dateIndex++;
	lastLine = line;
    }
    
    //  } catch(IOException e) {
    //      System.out.println
    //  ("Error ocurred while reading file " +_directory+ 
    //   _filename + ".prn:" + e.getMessage());
    // } finally {
      close();
      //  } //finally
}//read()

  /**
   * Close open water area station data file
   */
protected void close(){
  try{
    _asciiIn.close();
  }catch(IOException e){
    System.out.println
      ("Error ocurred while closing file "+_filename+_filetype+":"+e.getMessage());
  }// catch
}

  /**
   * Parses a line from an ascii open water area data file (*.owa)
   */
protected void parseStationDistanceLine(String unparsedLine){
  StringTokenizer t = new StringTokenizer(unparsedLine, " ,\"\042\011");

  String stationName = new String(t.nextToken());
  //System.out.println("stationName="+stationName);
  String stationDistance = new String(t.nextToken());
  //System.out.println("stationDistance="+stationDistance);
  float distanceFloat = -CsdpFunctions.BIG_FLOAT;

  try{
    distanceFloat = Float.parseFloat(stationDistance);
  } catch(java.lang.NumberFormatException e){
    System.out.println("Error in OpenWaterAreaStationAsciiInput.parseStationDistanceLine");
    System.out.println("Unable to parse "+stationDistance+" as an int");
  }//try

  _stationData.addStation(stationName, distanceFloat);
} // parseStationDistanceLine
  
    /**
     * parses time varying data
     */
    public void parseDateHeaders(String unparsedLine){
	StringTokenizer t = new StringTokenizer(unparsedLine, " ,\042\011");
	String header = t.nextToken();
	if(header.toLowerCase().indexOf("date") < 0){
	    System.out.println("ERROR:  first header not 'date'");
	}
	int index=0;
	while(t.hasMoreTokens()){
	    header = t.nextToken();
	    if(header.toLowerCase().equals("flow")){
		_stationData.setFlowIndex(index);
	    }else{
		if(DEBUG)System.out.println("adding station header index.  index,header="+index+","+header);
		_stationData.addStationIndex(index, header);
	    }//if
	    index++;
	}//while
    }//parseDateHeaders

  /**
   * Parses a line from an ascii station data file which contains time series data
   */
    protected void parseDateLine(int dateIndex, String unparsedLine, String lastLine){
  StringTokenizer t = new StringTokenizer(unparsedLine, " ,\042\011");
  String date = null;

  try{
      date = t.nextToken();
  }catch(NoSuchElementException e){
      System.out.println("ERROR in OpenWaterAreaStationAsciiInput.java:");
      System.out.println("unable find a date in the string "+unparsedLine+";");
      System.out.println("previous line="+lastLine+"; dateIndex="+dateIndex);
  }
  String eToken = null;
  String name =null;
  float elevation = -CsdpFunctions.BIG_FLOAT;
  String fToken = null;
  float flow = -CsdpFunctions.BIG_FLOAT;
  float flowObject = -CsdpFunctions.BIG_FLOAT;
  float elevationObject = -CsdpFunctions.BIG_FLOAT;
  //  float stationDistance = -CsdpFunctions.BIG_FLOAT;
  String stationName = null;

  for(int i=0; i<=_stationData.getNumStations()-1; i++){
      eToken = t.nextToken();
      try{
	  elevationObject = Float.parseFloat(eToken);
      } catch (java.lang.NumberFormatException e){
	  System.out.println("Error in OpenWaterAreaStationAsciiInput.parseDataLine");
	  System.out.println("Unable to parse "+eToken+ " as a float");
      }//try
      elevation = elevationObject;

      if(DEBUG)System.out.println("about to get distance.  i,stationName="+i+","+_stationData.getStationName(i));
      stationName = _stationData.getStationName(i);

      //      stationDistance = _stationData.getDistance(_stationData.getStationName(i));
      _stationData.addElevation(date, stationName, elevation);
  }
  
  try{
      fToken = t.nextToken();
  }catch(java.util.NoSuchElementException e){
      System.out.println("nextToken not found. unparsedLine="+unparsedLine);
  }
  try{
      flowObject = Float.parseFloat(fToken);
  } catch (java.lang.NumberFormatException e){
      System.out.println("Error in OpenWaterAreaStationAsciiInput.parseDataLine");
      System.out.println("Unable to parse "+fToken+ " as a float");
  }//try
  flow = flowObject;
  
  _stationData.addDate(dateIndex, date);
  _stationData.addFlow(date, flow);
}//parseDateLine

} // class NetworkAsciiInput
