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

public class IrregularXsectsInpAsciiInput extends IrregularXsectsInpInput{

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
      boolean done = false;
      line = _asciiIn.readLine();//skip first line
      if(DEBUG)System.out.println("about to call firstToken. line="+line);
      while(firstToken(line).equalsIgnoreCase("IRREG_GEOM") == false){
	  line = _asciiIn.readLine();
      }
      line = _asciiIn.readLine();
      parseHeaderLine(line);
      //assume no blank lines.
      while(line == null || firstToken(line).equalsIgnoreCase("CHAN") == false){
	  line = _asciiIn.readLine();
      }
      while(done == false){
	  line = _asciiIn.readLine();
	  done = parseIrregularXsectsInpData(line);
	  storeData();
      }

    } catch(IOException e) {
      System.out.println
	("Error ocurred while reading file "+ _directory + _filename + ".inp:" + 
	 e.getMessage());
    } finally {
      close();
    } // catch()
  }

  /**
   * Close ascii IrregularXsectsInp data file
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
   * Parses a line from an ascii DSMChannels data file (channels.inp)
   * pLineNum is actually the line number-1.  Store values in the vector parsedData.
   */
  protected boolean parseIrregularXsectsInpData(String unparsedLine){
    
      if(DEBUG)System.out.println("unparsedline = "+unparsedLine);
    
    //delimeters: space, tab
    boolean done = false;
    StringTokenizer t = new StringTokenizer(unparsedLine, " ,\011\042");
    String firstToken = null;
    String nextToken = null;

    boolean foundChan = false;
    boolean foundDist = false;
    boolean foundFilename = false;

    try{
	firstToken = t.nextToken();
    }catch(NoSuchElementException e){
	System.out.println("error in parseIrregularXsectsInpData.  unparsedLine="+
			   unparsedLine);
    }

    if(firstToken.substring(0,1).equals("#")){
	if(DEBUG)System.out.println("comment line ignored. line=" + unparsedLine);
    } else {
      if(firstToken.equalsIgnoreCase(END_HEADER)){
	done = true;
	if(DEBUG)System.out.println("firstToken="+firstToken);
      }
      else{
	  if(_chanIndex == 0){
	      _chanNum = firstToken;
	      foundChan = true;
	  }
	  if(_distIndex == 0){
	      //	      _dist = (new Float(firstToken)).floatValue();
	      _dist = Float.parseFloat(firstToken);
	      foundDist = true;
	  }
	  if(_filenameIndex == 0){
	      _pathname = firstToken;
	      foundFilename = true;
	  }

	  for(int i=1; t.hasMoreTokens(); i++){
	      nextToken = t.nextToken();
	      if(DEBUG)System.out.println("i, nextToken="+i+","+nextToken);
	      if(_chanIndex == i){
		  _chanNum = nextToken; 
		  foundChan = true;
	      }
	      if(_distIndex == i){
		  if(DEBUG)System.out.println("distIndex, i="+_distIndex+","+i);
		  if(nextToken.equalsIgnoreCase("mid")){
		      _dist = _middleNormalizedDist;
		  }else if(nextToken.equalsIgnoreCase("length")){
		      _dist = _lengthNormalizedDist;
		  }else{
		      _dist = Float.parseFloat(nextToken);
		  }
		  foundDist = true;
	      }
	      if(_filenameIndex == i){
		  if(DEBUG)System.out.println("distIndex, i="+_distIndex+","+i);
		  _pathname = nextToken;
		  foundFilename = true;
	      }
	      if(foundChan == false && foundDist == false && foundFilename == false){
		  System.out.println("Error in IrregularXsectsInpAsciiInput:  ");
		  System.out.println("foundChan = "+foundChan);
		  System.out.println("foundDist = "+foundDist);
		  System.out.println("foundFilename = "+foundFilename);
	      }
	  }//while has more tokens
      }//else
    }//if the line is not commented out
    return done;
  } // parse
  
  /**
   * Parses second line of file which contains column headers
   */
    protected void parseHeaderLine(String unparsedLine){

	if(DEBUG)System.out.println("HEADER LINE = "+unparsedLine);
	
	StringTokenizer t = new StringTokenizer(unparsedLine, " ,\042\011");
	String tok = null;
	
	for(int i=0; t.hasMoreTokens(); i++){
	    tok = t.nextToken();
	    if(DEBUG)System.out.println("token "+i+"=["+tok+"]");
	    if(tok.equalsIgnoreCase(CHAN_HEADER)) {
		_chanIndex = i;
		if(DEBUG)System.out.println("chan_header="+i);
	    }
	    if(tok.equalsIgnoreCase(DIST_HEADER)){
		_distIndex = i;
		if(DEBUG)System.out.println("dist_header="+i);
	    }
	    if(tok.equalsIgnoreCase(FILENAME_HEADER)){
		_filenameIndex = i;
		if(DEBUG)System.out.println("filename_header="+i);
	    }
	}
	
	if(DEBUG)System.out.println("_chanIndex, _distIndex, _filenameIndex="+_chanIndex+","+_distIndex+","+_filenameIndex);
	
    } // parseSecondLine
    
    /**
     * returns first token from line
     */
    protected String firstToken(String unparsedLine){
	StringTokenizer t = new StringTokenizer(unparsedLine, " ,\011\042");
	String returnValue = null;
	try{
	    returnValue = t.nextToken();
	}catch(NoSuchElementException e){
	    System.out.println("ERROR in IrregularXsectsInpAsciiInput");
	}
	return returnValue;
    }

  LineNumberReader _asciiIn;

    protected int _chanIndex = -CsdpFunctions.BIG_INT;
    protected int _distIndex = -CsdpFunctions.BIG_INT;
    protected int _filenameIndex = -CsdpFunctions.BIG_INT;

    protected final String CHAN_HEADER = "chan";
    protected final String DIST_HEADER = "dist";
    protected final String FILENAME_HEADER = "filename";

    protected final String END_HEADER      = "end";
    protected String _firstToken = null;

    /**
     * If a distance of "MID" is read from the irregular_xsects_copy.inp file,
     * use this for the distance temporarily, until the actual distance from the
     * channels.inp file is found.
     */
    protected final float _middleNormalizedDist = 0.5f;
    /**
     * If a distance of "LENGTH" is read from the irregular_xsects_copy.inp file,
     * use this for the distance temporarily, until the actual distance from the
     * channels.inp file is found.
     */
    protected final float _lengthNormalizedDist = 1.0f;
} // class IrregularXsectsInpAsciiInput
