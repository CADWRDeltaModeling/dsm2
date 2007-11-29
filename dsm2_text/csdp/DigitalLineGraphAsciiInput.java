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

public class DigitalLineGraphAsciiInput extends DigitalLineGraphInput{

  LineNumberReader _asciiIn;

  /**
   * Open ascii file
   */
protected void open(){
  FileReader aInFile;
  try {
    if((_directory.substring(_directory.length()-1,_directory.length())).
       equals(File.separator) == false){
	_directory += File.separator;
    }
    aInFile = new FileReader(_directory + _filename + "." + _filetype);
    _asciiIn = new LineNumberReader(aInFile);
    if (DEBUG) System.out.println("In ascii open " + _asciiIn);
  } catch(IOException e) {
    if (DEBUG) System.out.println("Directory + Filename: "+ _directory + _filename);
    if (DEBUG) System.out.println("Directory + Filetype: "+ _directory + _filetype);
    System.out.println
      ("Error ocurred while opening file "+_directory + 
       _filename + _filetype + e.getMessage());
  } // catch()
}

    /**
     * Read ascii file
     */
    protected void read(){
	//	int numLines=0;
	try {
	    String line=null;
	    //these lines are for reading USGS digital line graph files
//  	    for(int i=0; i<=9; i++){
//  		line = _asciiIn.readLine();
//  	    }
//  	    parseCoefficientLine(line);
//  	    if(DEBUG)System.out.println("number of landmarks = "+_numLines);
//  	    //skip next 5 lines; read next line
//  	    for(int i=0; i<=5; i++){
//  		line = _asciiIn.readLine();
//  	    }
//  	    //skip area and node lines
//  	    while(line.indexOf("L") < 0){
//  		line = _asciiIn.readLine();
//  	    }
	    //start reading line lines
	    System.out.println("reading file...");
	    line = _asciiIn.readLine();
	    _numLines = parseFirstLine(line);
	    for(int i=0; i<=_numLines-1; i++){
		//finds _currentLineName
		line = _asciiIn.readLine();
		parseDigitalLineGraphLineHeader(i,line);
		addLine();
		_currentPointIndex = 0;
		while(line.indexOf("END") < 0){
		    line = _asciiIn.readLine();
		    if(line.indexOf("END") < 0){
			if(DEBUG)System.out.println("about to parse line "+line);
			parseAndStoreDigitalLineGraphPoint(line);
		    }
		}//while
	    } // for i

	    findMaxMin(_data.getNumLines());
	} catch(IOException e) {
	    System.out.println
		("Error ocurred while reading file "+ _directory + _filename + ".prn:" + 
		 e.getMessage());
	} finally {
	    close();
	} // catch()
    }

  /**
   * Close ascii Landmark data file
   */
protected void close(){
  try{
    _asciiIn.close();
  }catch(IOException e){
    System.out.println
      ("Error ocurred while closing file "+_directory + 
       _filename + _filetype+":"+e.getMessage());
  }// catch
}

    /**
     * parses line header.  Line headers begin with "L" and contain the 
     * number of coordinate pairs(points) in the line.
     */
    protected void parseDigitalLineGraphLineHeader(int index, String line){
	StringTokenizer t = new StringTokenizer(line, ",\042\011");
	//	int numPoints = -CsdpFunctions.BIG_INT;
	String lineName = null;
	String nextToken = null;

	try{
//  	    nextToken = t.nextToken();
  	    lineName = t.nextToken();
//  	    for(int i=0; i<=3; i++){
//  		t.nextToken();
//  	    }
//  	    nextToken = t.nextToken();
//  	    numPoints = Integer.parseInt(nextToken);
	}catch(java.lang.NumberFormatException e) {
	    System.out.println("Error in DigitalLineGraphAsciiInput.parseDigitalLineGraphLineHeader:");
	    System.out.println("Unable to parse "+nextToken);
	}
	_currentLineName = lineName;
	if(DEBUG)System.out.println("currentLineName="+_currentLineName);
    }//parseDigitalLineGraphLineHeader

    /**
     * parses first line to an int.  The first line should be 
     * the number of lines in the file
     */
    protected int parseFirstLine(String line){
	StringTokenizer t = new StringTokenizer(line, ",\042\011");
	//	int numPoints = -CsdpFunctions.BIG_INT;
	String nextToken = null;
	int returnValue = -CsdpFunctions.BIG_INT;

	try{
	    nextToken=t.nextToken();
  	    returnValue = Integer.parseInt(nextToken);
	}catch(java.lang.NumberFormatException e) {
	    System.out.println("Error in DigitalLineGraphAsciiInput.parseFirstLine:");
	    System.out.println("Unable to parse "+nextToken);
	}
	return returnValue;
    }//parseDigitalLineGraphLineHeader


  /**
   * Parses a line from an ascii DigitalLineGraph data file (*.dlg)
   * pLineNum is actually the line number-1.  Store values in the vector parsedData.
   */
protected void parseAndStoreDigitalLineGraphPoint(String unparsedLine){

    if(DEBUG)System.out.println("unparsedline = "+unparsedLine);

  StringTokenizer t = new StringTokenizer(unparsedLine, " ,\042\011");
  String pName = null;
  String nextToken = null;

  try{
      while(t.hasMoreTokens()){
	  nextToken = t.nextToken();
	  _x = Float.parseFloat(nextToken);
	  nextToken = t.nextToken();
	  _y = Float.parseFloat(nextToken);
	  addAndStorePoint();
      }
  } catch(java.lang.NumberFormatException e) {
    System.out.println
	("Error in DigitalLineGraphAsciiInput.parseDigitalLineGraphPoint");
    System.out.println("Unable to parse "+nextToken+" as a float");
  }//try

} // parseAndStoreDigitalLineGraphPoint
  
//    /**
//     * Parses the 10th line of the dlg file which contains the coefficients used
//     * for converting to the internal coordinate system.
//     */
//      protected void parseCoefficientLine(String theLine) {

//  	StringTokenizer t = new StringTokenizer(theLine, " ");
//  	String nextToken = t.nextToken();
//  	float a1 = -CsdpFunctions.BIG_FLOAT;
//  	float a2 = -CsdpFunctions.BIG_FLOAT;
//  	float a3 = -CsdpFunctions.BIG_FLOAT;
//  	float a4 = -CsdpFunctions.BIG_FLOAT;
//  	try{
//  	    a1 = Float.parseFloat(nextToken);
//  	    a2 = Float.parseFloat(t.nextToken());
//  	    a3 = Float.parseFloat(t.nextToken());
//  	    a4 = Float.parseFloat(t.nextToken());
//  	} catch(java.lang.NumberFormatException e) {
//  	    System.out.println("Error reading coefficient line: "+theLine);
//  	}
//  	_a1 = a1;
//  	_a2 = a2;
//  	_a3 = a3;
//  	_a4 = a4;
//      } // parseCoefficientLine

} // class DigitalLineGraphAsciiInput
