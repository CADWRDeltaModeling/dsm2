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

public class XsectsInpAsciiInput extends XsectsInpInput{

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
      while(line == null || firstToken(line).equalsIgnoreCase("XSECTS") == false){
	  line = _asciiIn.readLine();
      }
      line = _asciiIn.readLine();
      parseHeaderLine(line);
      //assume no blank lines.
      while(line == null || firstToken(line).equalsIgnoreCase("XSECT") == false){
	  line = _asciiIn.readLine();
      }
      while(done == false){
	  line = _asciiIn.readLine();
	  if(CsdpFunctions.lastNonblank(line) > 0){
	      done = parseXsectsInpData(line);
	      storeData();
	  }
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
   * Close ascii XsectsInp data file
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
   * Parses a line from an ascii rectangular cross-section data file (xsects.inp)
   * pLineNum is actually the line number-1.  Store values in the vector parsedData.
   */
  protected boolean parseXsectsInpData(String unparsedLine){
    
    if(DEBUG)System.out.println("unparsedline = "+unparsedLine);
    
    //delimeters: space, tab
    boolean done = false;
    StringTokenizer t = new StringTokenizer(unparsedLine, " ,\011\042");
    String firstToken = null;
    String nextToken = null;
    
    boolean foundXsect = false;
    boolean foundWidth = false;
    boolean foundBotelv = false;
    boolean foundInitStage = false;
    boolean foundInitFlow = false;

    boolean blankLine = false;

    try{
	firstToken = t.nextToken();
    }catch(NoSuchElementException e){
	System.out.println
	    ("error in XsectsInpAsciiInput.parseXsectsInpData.  unparsedLine="+
	     unparsedLine);
	blankLine = true;
    }

    if(blankLine == false){
	if(firstToken.substring(0,1).equals("#")){
	    if(DEBUG)System.out.println("comment line ignored. line=" + unparsedLine);
	} else {
	    if(firstToken.equalsIgnoreCase(END_HEADER)){
		done = true;
		if(DEBUG)System.out.println("firstToken="+firstToken);
	    }
	    else{
		if(_xsectIndex ==0){
		    _xsectNum = Integer.parseInt(firstToken);
		    foundXsect = true;
		}
		if(_widthIndex ==0){
		    _width = Float.parseFloat(firstToken);
		    foundWidth = true;
		}
		if(_botelvIndex ==0){
		    _botelv = Float.parseFloat(firstToken);
		    foundBotelv = true;
		}
		if(_initStageIndex ==0){
		    _initStage = Float.parseFloat(firstToken);
		    foundInitStage = true;
		}
		if(_initFlowIndex ==0){
		    _initFlow = Float.parseFloat(firstToken);
		    foundInitFlow = true;
		}
		
		for(int i=1; t.hasMoreTokens(); i++){
		    nextToken = t.nextToken();
		    if(DEBUG)System.out.println("i, nextToken="+i+","+nextToken);
		    if(_xsectIndex == i){
			_xsectNum = Integer.parseInt(nextToken);
			foundXsect = true;
		    }
		    if(_widthIndex == i){
			_width = Float.parseFloat(nextToken);
			foundWidth = true;
		    }
		    if(_botelvIndex == i){
			_botelv = Float.parseFloat(nextToken);
			foundBotelv = true;
		    }
		    if(_initStageIndex == i){
			_initStage = Float.parseFloat(nextToken);
			foundInitStage = true;
		    }
		    if(_initFlowIndex == i){
			_initFlow = Float.parseFloat(nextToken);
			foundInitFlow = true;
		    }
		    if(foundXsect == false && foundWidth == false && foundBotelv == false &&
		       foundInitStage == false && foundInitFlow == false){
			System.out.println("Error in XsectsInpAsciiInput: ");
			System.out.println("foundXsect = "+foundXsect);
			System.out.println("foundWidth = "+foundWidth);
			System.out.println("foundBotelv = "+foundBotelv);
			System.out.println("foundInitStage = "+foundInitStage);
			System.out.println("foundInitFlow = "+foundInitFlow);
		    }
		}//while has more tokens
	    }//else
	}//if the line is not commented out
    }//if the line isn't blank
    return done;
  } // parse
    
  /**
   * Parses second line of file which contains column headers
   */
  protected void parseHeaderLine(String firstLine){
    
    StringTokenizer t = new StringTokenizer(firstLine, " ");
    String tok = null;
    
    for(int i=0; t.hasMoreTokens(); i++){
      tok = t.nextToken();
      if(DEBUG)System.out.println("next token="+tok);
      if(tok.equalsIgnoreCase(XSECT_HEADER)) {
	_xsectIndex = i;
	if(DEBUG)System.out.println("xsect_header="+i);
      }
      if(tok.equalsIgnoreCase(WIDTH_HEADER)) {
	_widthIndex = i;
	if(DEBUG)System.out.println("width_header="+i);
      }
      if(tok.equalsIgnoreCase(BOTELV_HEADER)) {
	_botelvIndex = i;
	if(DEBUG)System.out.println("botelv_header="+i);
      }
      if(tok.equalsIgnoreCase(INITSTAGE_HEADER)) {
	_initStageIndex = i;
	if(DEBUG)System.out.println("initstage_header="+i);
      }
      if(tok.equalsIgnoreCase(INITFLOW_HEADER)) {
	_initFlowIndex = i;
	if(DEBUG)System.out.println("initflow_header="+i);
      }
    }
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
	    System.out.println("ERROR in XsectsInpAsciiInput");
	}
	return returnValue;
    }

  LineNumberReader _asciiIn;

    protected int _xsectIndex     = 0;
    protected int _widthIndex     = 0;
    protected int _botelvIndex    = 0;
    protected int _initStageIndex = 0;
    protected int _initFlowIndex  = 0;

    protected static final String XSECT_HEADER     = "xsect";
    protected static final String WIDTH_HEADER     = "width";
    protected static final String BOTELV_HEADER    = "botelv";
    protected static final String INITSTAGE_HEADER = "init-stage";
    protected static final String INITFLOW_HEADER  = "init-flow";

    protected static final String END_HEADER      = "end";
    protected String _firstToken = null;
} // class XsectsInpAsciiInput
