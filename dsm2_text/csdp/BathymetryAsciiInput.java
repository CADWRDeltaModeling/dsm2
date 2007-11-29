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
import DWR.CSDP.dialog.*;
import java.io.*;
import java.util.*;

public class BathymetryAsciiInput extends BathymetryInput{

    LineNumberReader _asciiIn;
    OkDialog _noMetadataDialog = new OkDialog(_gui, "This bathymetry file has no metadata. UTM zone 10 NAD 27, NGVD 1929 will be assumed.",true);
    OkDialog _errorDialog = new OkDialog(_gui, "error message", true);
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
  int numLines=0;
  
  try {
    String line=null;
    line = _asciiIn.readLine();
    if(line.indexOf("#")<0){
	_noMetadataDialog.show();
	parseFirstLine(line);
    }else{
	System.out.println("parsing bathymetry metadata");
	parseFirstLine(line);
	///require 5 lines of metadata
	for(int i=0; i<=4; i++){
	    line=_asciiIn.readLine();
	    if(line.indexOf("#")>=0){
		parseMetadata(line);
	    }else{
	      _errorDialog.setMessage("incomplete metadata! there should be 5 lines.");
	      _errorDialog.show();
	    }
	}
	System.out.println("done parsing metadata. hdatum, hzone, hunits,vdatum,vunits="+
			   CsdpFunctions.getHDatum()+","+CsdpFunctions.getHZone()+","+
			   CsdpFunctions.getHUnits()+","+CsdpFunctions.getVDatum()+"."+
			   CsdpFunctions.getVUnits());
    }


    System.out.println("number of pts in ascii file=" +  _data.getNumLines());
    for(int i=0; i<=_data.getNumLines()-1; i++){
      line = _asciiIn.readLine();
      ////      System.out.println("about to parse line: "+line);
      parseBathymetryData(i,line);
      storeData(i);
    } // for
    _data.findMaxMin(_data.getNumLines());      
  } catch(IOException e) {
    System.out.println
      ("Error ocurred while reading file "+ _directory + _filename + ".prn:" + 
       e.getMessage());
  } finally {
    close();
  } // catch()
}

  /**
   * Close ascii bathymetry data file
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
   * Parses a line from an ascii bathymetry data file (*.prn)
   * pLineNum is actually the line number-1.  Store values in the vector parsedData.
   */
protected void parseBathymetryData(int linenum, String unparsedLine){
  StringTokenizer t=null;
  try{
    t = new StringTokenizer(unparsedLine, " ,\042\011");
  } catch(java.lang.NullPointerException e){
    System.out.println("Error in BathymetryAsciiInput.parseBathymetryData");
    System.out.println("unable to create StringTokenizer for line # "+linenum);
    System.out.println("line: "+unparsedLine);
  }//catch
  float pX    = -CsdpFunctions.BIG_FLOAT;
  float pY    = -CsdpFunctions.BIG_FLOAT;
  float pZ    = -CsdpFunctions.BIG_FLOAT;
  short pYear = -CsdpFunctions.BIG_SHORT;
  String nextToken = t.nextToken();

  try{
    pX                  = Float.parseFloat(nextToken);
  } catch(java.lang.NumberFormatException e){
    System.out.println("Error in BathymetryAsciiInput.parseBathymetryData");
    System.out.println("Unable to parse "+nextToken+" as a float");
    System.out.println("line number="+linenum);
  }
  nextToken = t.nextToken();
  try{
    pY                  = Float.parseFloat(nextToken);
  } catch(java.lang.NumberFormatException e){
    System.out.println("Error in BathymetryAsciiInput.parseBathymetryData");
    System.out.println("Unable to parse "+nextToken+" as a float");
    System.out.println("line number="+linenum);
  }
  nextToken = t.nextToken();
  try{
    pZ                  = Float.parseFloat(nextToken);
  } catch(java.lang.NumberFormatException e){
    System.out.println("Error in BathymetryAsciiInput.parseBathymetryData");
    System.out.println("Unable to parse "+nextToken+" as a float");
  }
  nextToken = t.nextToken();
  try{
    pYear                  = Short.parseShort(nextToken);
  } catch(java.lang.NumberFormatException e){
    System.out.println("Error in BathymetryAsciiInput.parseBathymetryData");
    System.out.println("Unable to parse "+nextToken+" as a float");
    System.out.println("line number="+linenum);
  }

  try{
    _pd.x           = pX;
    _pd.y           = pY;
    _pd.z           = pZ;
    _pd.year        = pYear;
    _pd.source      = t.nextToken();
    if(t.hasMoreTokens()) _pd.description = t.nextToken();  
  } catch (java.lang.NullPointerException e){
    System.out.println("Error in BathymetryAsciiInput.parseBathymetryData");
    System.out.println("Unable to convert values to primitives.");
    System.out.println("line number="+ linenum);
    System.out.println("line="+unparsedLine);
  }//catch
} // parse
  
  /**
   * Parses 1st line of file which contains number of points in file
   */
protected void parseFirstLine(String firstLine) {

  StringTokenizer t = new StringTokenizer(firstLine, " ");
  String nextToken = t.nextToken();
  if(firstLine.indexOf("num")>=0){
    nextToken=t.nextToken();
  }
  int numLines = -CsdpFunctions.BIG_INT;
  
  try{
      //    numLines = new Integer(nextToken);
    numLines = Integer.parseInt(nextToken);
  } catch(java.lang.NumberFormatException e) {
    System.out.println("Error reading first line: "+nextToken);
    System.out.println("The first line must contain only one value, which");
    System.out.println("should be an integer which is the number of values");
    System.out.println("in the file.  Did you forget to do this?");
  }//try
  if(DEBUG) System.out.println(numLines);
  _data.putNumLines(numLines);
} // parseFirstLine

    protected void parseMetadata(String line){
	StringTokenizer t = new StringTokenizer(line, " ");
	String nextToken=t.nextToken();
	nextToken=t.nextToken();
	if(line.indexOf("HorizontalDatum")>=0){
	    if(nextToken.indexOf("UTMNAD27")>=0 || nextToken.indexOf("UTMNAD83")>=0){
		CsdpFunctions.setHDatum(nextToken);
	    }else{
		_errorDialog.setMessage("unknown h datum in bathymetry file: " + nextToken);
		_errorDialog.show();
	    }
	}else if(line.indexOf("HorizontalZone")>=0){
	    if(nextToken.indexOf("10")>=0){
		CsdpFunctions.setHZone(Integer.parseInt(nextToken));
	    }else{
		_errorDialog.setMessage("unknown h zone in bathymetry file: " + nextToken);
		_errorDialog.show();
	    }
	    
	}else if(line.indexOf("HorizontalUnits")>=0){
	    if(nextToken.indexOf("Meters")>=0){
		CsdpFunctions.setHUnits(nextToken);
	    }else{
		_errorDialog.setMessage("unknown h units in bathymetry file: " + nextToken);
		_errorDialog.show();
	    }

	}else if(line.indexOf("VerticalDatum")>=0){
	    if(nextToken.indexOf("NGVD29")>=0 || nextToken.indexOf("NAVD88")>=0){
		CsdpFunctions.setVDatum(nextToken);
	    }else{
		_errorDialog.setMessage("unknown v datum in bathymetry file: " + nextToken);
		_errorDialog.show();
	    }

	}else if(line.indexOf("VerticalUnits")>=0){
	    if(nextToken.indexOf("U.S.SurveyFeet")>=0){
		CsdpFunctions.setVUnits(nextToken);
	    }else{
		_errorDialog.setMessage("unknown v units in bathymetry file: " + nextToken);
		_errorDialog.show();
	    }
	}else{
	    _errorDialog.setMessage("unable to parse metadata line: " + line);
	    _errorDialog.show();
	}
    }

} // class BathymetryAsciiInput
