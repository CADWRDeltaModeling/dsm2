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

public class DSMChannelsAsciiInput extends DSMChannelsInput{

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
      line = _asciiIn.readLine();
      parseSecondLine(line);
      for(int i=0; done==false; i++){
	line = _asciiIn.readLine();
	done = parseDSMChannelsData(i,line);
	if(_firstToken.equals("#")) {
	  if(DEBUG)System.out.println("comment line read");
	} else {
	  storeData(i);
	}
      } // for
    } catch(IOException e) {
      System.out.println
	("Error ocurred while reading file "+ _directory + _filename + ".prn:" + 
	 e.getMessage());
    } finally {
      close();
    } // catch()
  }

  /**
   * Close ascii DSMChannels data file
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
  protected boolean parseDSMChannelsData(int linenum, String unparsedLine){
    
    if(DEBUG)System.out.println("unparsedline = "+unparsedLine);
    
    //delimeters: space, tab
    boolean done = false;
    StringTokenizer t = new StringTokenizer(unparsedLine, " ,\011\042");
    String firstToken = null;
    try{
	firstToken = t.nextToken();
    }catch(NoSuchElementException e){
	System.out.println("error in parseDSMChannelsData.  linenum, unparsedLine="+
			   linenum+","+unparsedLine);
    }
    _firstToken = firstToken;
    String nextToken = null;
    boolean foundChan     = false;
    boolean foundLength   = false;
    boolean foundUpnode   = false;
    boolean foundDownnode = false;
    boolean foundXsect1   = false;
    boolean foundDist1    = false;
    boolean foundXsect2   = false;
    boolean foundDist2    = false;
    
    if(firstToken.equals("#")){
      if(DEBUG)System.out.println("comment line ignored");
    } else {
      if(firstToken.equalsIgnoreCase(END_HEADER)){
	done = true;
	if(DEBUG)System.out.println("firstToken="+firstToken);
      }
      else{
	if(_chanIndex == 0){
	  _pd.chan = firstToken;
	  foundChan = true;
	}
	if(_lengthIndex == 0){
	  _pd.length = Integer.parseInt(firstToken);
	  foundLength = true;
	}
	else if(_upnodeIndex == 0){
	  _pd.upnode = Integer.parseInt(firstToken);
	  foundUpnode = true;
	}
	else if(_downnodeIndex == 0){
	  _pd.downnode = Integer.parseInt(firstToken);
	  foundDownnode = true;
	}
	else if(_xsect1Index == 0){
	    _pd.xsect1 = Integer.parseInt(firstToken);
	    foundXsect1 = true;
	}
	else if(_dist1Index == 0){
	    _pd.dist1 = Integer.parseInt(firstToken);
	    foundDist1 = true;
	}
	else if(_xsect2Index == 0){
	    _pd.xsect2 = Integer.parseInt(firstToken);
	    foundXsect2 = true;
	}
	else if(_dist2Index == 0){
	    _pd.dist2 = Integer.parseInt(firstToken);
	    foundDist2 = true;
	}

	for(int i=1; t.hasMoreTokens(); i++){
	    StringTokenizer commentRemover = null;
	    nextToken = t.nextToken();

	  if(nextToken.indexOf("#") > 0){
	      if(DEBUG)System.out.println("about to strip comment from end of line.");
	      if(DEBUG)System.out.println("nextToken before="+nextToken);
	      commentRemover = new StringTokenizer(nextToken,"#");
	      nextToken = commentRemover.nextToken();
	      if(DEBUG)System.out.println("nextToken after="+nextToken);
	  }

	  if(DEBUG)System.out.println("i, nextToken="+i+","+nextToken);
	  if(_chanIndex == i){
	    _pd.chan = nextToken; 
	    foundChan = true;
	  }
	  if(_lengthIndex == i){
	    if(DEBUG)System.out.println("lengthIndex, i="+_lengthIndex+","+i);
	    _pd.length = Integer.parseInt(nextToken); 
	    foundLength = true;
	  }
	  if(_upnodeIndex == i){
	    _pd.upnode = Integer.parseInt(nextToken);
	    foundUpnode = true;
	  }
	  if(_downnodeIndex == i){
	      _pd.downnode = Integer.parseInt(nextToken);
	      foundDownnode = true;
	  }
	  if(_xsect1Index == i){
	      _pd.xsect1 = Integer.parseInt(nextToken);
	      foundXsect1 = true;
	  }
	  if(_dist1Index == i){
	      _pd.dist1 = Integer.parseInt(nextToken);
	      foundDist1 = true;
	  }
	  if(_xsect2Index == i){
	      _pd.xsect2 = Integer.parseInt(nextToken);
	      foundXsect2 = true;
	  }
	  if(_dist2Index == i){
	      _pd.dist2 = Integer.parseInt(nextToken);
	      foundDist2 = true;
	  }

	  if(foundChan == false && foundLength == false && foundUpnode ==  false && 
	     foundDownnode == false && foundXsect1 == false && foundDist1 == false &&
	     foundXsect2 == false && foundDist2 == false){
	    System.out.println("Error in DSMChannelsAsciiInput: foundChan, foundLength, foundUpnode, foundDownnode, foundXsect1, foundDist1, foundXsect2, foundDist2="+foundChan+","+foundLength+","+foundUpnode+","+foundDownnode+","+foundXsect1+","+foundDist1+","+foundXsect2+","+foundDist2);
	  }
	}//while has more tokens
      }//else
    }//if the line is not commented out
    return done;
  } // parse
  
  /**
   * Parses second line of file which contains column headers
   */
  protected void parseSecondLine(String firstLine){
    
    StringTokenizer t = new StringTokenizer(firstLine, " ");
    String tok = null;
    
    for(int i=0; t.hasMoreTokens(); i++){
      tok = t.nextToken();
      if(DEBUG)System.out.println("next token="+tok);
      if(tok.equalsIgnoreCase(CHAN_HEADER)) {
	_chanIndex = i;
	if(DEBUG)System.out.println("chan_header="+i);
      }
      if(tok.equalsIgnoreCase(LENGTH_HEADER)){
	_lengthIndex = i;
	if(DEBUG)System.out.println("length_header="+i);
      }
      if(tok.equalsIgnoreCase(DOWNNODE_HEADER)){
	_downnodeIndex = i;
	if(DEBUG)System.out.println("downnode_header="+i);
      }
      if(tok.equalsIgnoreCase(UPNODE_HEADER)){
	_upnodeIndex = i;
	if(DEBUG)System.out.println("upnode_header="+i);
      }
      if(tok.equalsIgnoreCase(XSECT_HEADER)){
	  if(_xsect1Index <= 0){
	      _xsect1Index = i;
	  }else if(_xsect1Index > 0 && _xsect2Index <= 0){
	      _xsect2Index = i;
	  }
      }
      if(tok.equalsIgnoreCase(DIST_HEADER)){
	  if(_dist1Index <= 0){
	      _dist1Index = i;
	  }else if(_dist1Index > 0 && _dist2Index <= 0){
	      _dist2Index = i;
	  }
      }
    }
  } // parseSecondLine

  LineNumberReader _asciiIn;
  protected int _chanIndex     = 0;
  protected int _lengthIndex   = 0;
  protected int _upnodeIndex   = 0;
  protected int _downnodeIndex = 0;
    protected int _xsect1Index = 0;
    protected int _dist1Index  = 0;
    protected int _xsect2Index = 0;
    protected int _dist2Index  = 0;

  protected static final String CHAN_HEADER     = "chan";
  protected static final String LENGTH_HEADER   = "length";
  protected static final String UPNODE_HEADER   = "upnode";
  protected static final String DOWNNODE_HEADER = "downnode";
  protected static final String END_HEADER      = "end";
    protected static final String XSECT_HEADER  = "xsect";
    protected static final String DIST_HEADER   = "dist";
  protected String _firstToken = null;
} // class DSMChannelsAsciiInput
