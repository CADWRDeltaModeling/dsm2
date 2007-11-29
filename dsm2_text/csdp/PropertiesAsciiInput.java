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

public class PropertiesAsciiInput extends PropertiesInput{

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
  int numLines=0;
  
  try {
    String line=null;
    //    for(int i=0; i<=CsdpFunctions.getNumColors()-1; i++){
    for(int i=0; i<=_gui.getNumColors()-1; i++){
      line = _asciiIn.readLine();
      parsePropertiesData(i,line);
      storeData(i,_colorValue);
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
   * Close ascii properties data file
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
   * Parses a line from an ascii properties data file (*.prn)
   * pLineNum is actually the line number-1.  Store values in the vector parsedData.
   */
protected void parsePropertiesData(int linenum, String unparsedLine){
  StringTokenizer t = new StringTokenizer(unparsedLine, " ,\042\011");

  int red   = -CsdpFunctions.BIG_INT;
  int green = -CsdpFunctions.BIG_INT;
  int blue  = -CsdpFunctions.BIG_INT;
  String nextToken = t.nextToken();

  try{
    red   = Integer.parseInt(nextToken);
  } catch(java.lang.NumberFormatException e){
    System.out.println("Error in PropertiesAsciiInput.parsePropertiesData");
    System.out.println("Unable to parse "+nextToken+" as an int");
  }//try
  nextToken = t.nextToken();
  try{
    green   = Integer.parseInt(nextToken);
  } catch(java.lang.NumberFormatException e){
    System.out.println("Error in PropertiesAsciiInput.parsePropertiesData");
    System.out.println("Unable to parse "+nextToken+" as an int");
  }//try
  nextToken = t.nextToken();
  try{
    blue   = Integer.parseInt(nextToken);
  } catch(java.lang.NumberFormatException e){
    System.out.println("Error in PropertiesAsciiInput.parsePropertiesData");
    System.out.println("Unable to parse "+nextToken+" as an int");
  }//try

  _colorValue[0] = red;
  _colorValue[1] = green;
  _colorValue[2] = blue;

} // parse

  int[] _colorValue = new int[3];
  
} // class PropertiesAsciiInput
