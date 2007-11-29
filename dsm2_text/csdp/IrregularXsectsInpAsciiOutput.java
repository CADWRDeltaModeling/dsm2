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

/**
 * writes network data to ascii file
 */
public class IrregularXsectsInpAsciiOutput extends IrregularXsectsInpOutput{
  FileWriter _aOutFile     = null;                // ascii input file
  BufferedWriter _asciiOut = null;
  Network _net             = null;
  protected static final String SPACES = "          ";
  /**
   * assigns data storage object to class variable
   */
  public IrregularXsectsInpAsciiOutput(Network net){
    _net = net;
  }

  /**
   * Open ascii file for writing
   */
  protected void open(){
      try {
	  if((_directory.substring(_directory.length()-1,_directory.length())).
	     equals(File.separator) == false){
	      _directory += File.separator;
	  }
	  _aOutFile = new FileWriter(_directory + FILENAME);
	  _asciiOut = new BufferedWriter(_aOutFile);
      } catch(IOException e) {
	  System.out.println
	      ("Error ocurred while opening file "+ _directory + FILENAME + e.getMessage());
      } // catch()
  }
    
  /**
   * write ascii irregular_xsects.inp file
   */
  protected void write(){
    String line = null;
    Centerline centerline;
    CenterlinePoint cPoint;
    Xsect xsect;
    XsectPoint xPoint;
    String name = null;
    float dist = 0.0f;
    float length = 0.0f;
    float normalizedDist = 0.0f;
    try{
      _asciiOut.write(FIRST_LINE);
      _asciiOut.newLine();
      _asciiOut.write(SECOND_LINE);
      _asciiOut.newLine();
      for(int i=0; i<=_net.getNumCenterlines()-1; i++){
	name = _net.getCenterlineName(i);
	centerline = _net.getCenterline(name);
	for(int j=0; j<=centerline.getNumXsects()-1; j++){
	  if(name.length()<=4){
	    line = SPACES.substring(0,4-name.length())+name+SPACES.substring(0,5);
	  }else{
	    line = " "+name+" ";
	  }
	  xsect =centerline.getXsect(j);
	  length = centerline.getLength();
	  dist = xsect.getDistAlongCenterline();
	  normalizedDist = dist/length;
	  if(xsect.getNumPoints() > 0){
	      String s = Float.toString(normalizedDist);
	      if(s.indexOf(".",0) > 0) s = s+"0000000";
	      else s = s+".000000";
	      line += s.substring(0,7) + SPACES.substring(0,4);
	      line += "$IRREG/"+name+"_"+s.substring(0,7)+".txt";
	      _asciiOut.write(line);	
	      _asciiOut.newLine();
	  }else{
	      System.out.println("not writing xsect "+name+"_"+j+" to irregular_xsects.inp file because it has no points");

	  }
	  line = null;
	}//for j
	line = null;
      }//for i
      _asciiOut.write(LAST_LINE);
    } catch(IOException e) {
    System.out.println
      ("Error ocurred while writing file " + FILENAME + e.getMessage());
    } finally {
    } // catch()
  }//write

  /**
   * Close ascii bathymetry data file
   */
  protected void close(){
    try{
    _asciiOut.close();
    }catch(IOException e){
      System.out.println
	("Error ocurred while closing file "+ FILENAME+e.getMessage());
    }// catch
  }//close
  
}//class IrregularXsectsInpAsciiOutput
