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

public class XsectAsciiInput extends XsectInput{

  /**
   * Open ascii file
   */
protected void open(){
  FileReader aInFile;
  try {
      if(DEBUG)System.out.println("about to read file "+_fullPathname);
    aInFile = new FileReader(_fullPathname);
    _asciiIn = new LineNumberReader(aInFile);
  } catch(IOException e) {
    System.out.println
      ("Error ocurred while opening file "+_fullPathname + e.getMessage());
  } // catch()
}

    /**
     * Read ascii file
     */
    protected Xsect read(){
	Xsect xsect = null;
	int numLines=0;
	
	try {
	    String line=null;
	    line = _asciiIn.readLine();//skip first line
	    while(line.indexOf("station") < 0){
		line = _asciiIn.readLine();
	    }
	    parseStationLine(line);
	    while(line.indexOf("elevation") < 0){
		line = _asciiIn.readLine();
	    }
	    parseElevationLine(line);
	    
	    xsect = storeData();
	    
	} catch(IOException e) {
	    System.out.println
		("Error ocurred while reading file "+ _fullPathname + e.getMessage());
	} finally {
	    close();
	} // catch()
	return xsect;
    }//read

  /**
   * Close ascii IrregularXsectsInp data file
   */
protected void close(){
  try{
    _asciiIn.close();
  }catch(IOException e){
    System.out.println
      ("Error ocurred while closing file "+_fullPathname +":"+ e.getMessage());
  }// catch
}

  /**
   * parses line of station values
   */
    protected void parseStationLine(String unparsedLine){
	if(DEBUG)System.out.println("station line = "+unparsedLine);
	
	StringTokenizer t = new StringTokenizer(unparsedLine, " ,:");
	int numTokens = 0;
	String nextToken = t.nextToken();

	if(nextToken.indexOf("station") < 0){
	    System.out.println("ERROR in XsectAsciiInput.parseStationLine:  ");
	    System.out.println("First token should be 'station' but it's actually "+nextToken);
	}
	while(t.hasMoreTokens()){
	    _station.put(numTokens, Float.parseFloat(t.nextToken()));
	    numTokens++;
	}
	_numStationValues = numTokens;
    } // parseStationLine

  /**
   * parses line of elevation values
   */
    protected void parseElevationLine(String unparsedLine){

	if(DEBUG)System.out.println("elevation line = "+unparsedLine);
	
	StringTokenizer t = new StringTokenizer(unparsedLine, " ,:");
	int numTokens = 0;
	String nextToken = t.nextToken();

	if(nextToken.indexOf("elevation") < 0){
	    System.out.println("ERROR in XsectAsciiInput.parseElevationLine:  ");
	    System.out.println("First token should be 'elevation' but it's actually "+nextToken);
	}
	while(t.hasMoreTokens()){
	    _elevation.put(numTokens, Float.parseFloat(t.nextToken()));
	    numTokens++;
	}
	_numElevationValues = numTokens;
    } // parseElevationLine
    
    LineNumberReader _asciiIn;
} // class XsectAsciiInput
