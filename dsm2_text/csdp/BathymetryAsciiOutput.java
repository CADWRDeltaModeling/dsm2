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

public class BathymetryAsciiOutput extends BathymetryOutput{
  FileWriter _aOutFile     = null;
  BufferedWriter _asciiOut = null;
  BathymetryData _data     = null;

  /**
   * assigns data storage object to class variable
   */
  BathymetryAsciiOutput(BathymetryData data){
    _data = data;
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
    _aOutFile = new FileWriter(_directory + _filename+"."+ASCII_TYPE);
    _asciiOut = new BufferedWriter(_aOutFile);
  } catch(IOException e) {
    if (DEBUG) System.out.println("Directory + Filename: "+ _directory + _filename);
    if (DEBUG) System.out.println("Filetype: " + _filetype);
    System.out.println
      ("Error ocurred while opening file "+_directory + _filename + 
       _filetype + e.getMessage());
  } // catch()
}

  /**
   * write ascii bathymetry data file
   */
protected boolean write(){
  float[] point;
  String line = null;
  short year;
  String source = null;
  boolean success = false;

  try {
    String nl=null;
    Integer numLines=null;
    nl=numLines.toString(_data.getNumLines());
    _asciiOut.write("#numlines:  "+nl);
    _asciiOut.newLine();
    _asciiOut.write("#HorizontalDatum:  "+CsdpFunctions.getHDatum());
    _asciiOut.newLine();
    _asciiOut.write("#HorizontalZone:  "+CsdpFunctions.getHZone());
    _asciiOut.newLine();
    _asciiOut.write("#HorizontalUnits:  "+CsdpFunctions.getHUnits());
    _asciiOut.newLine();
    _asciiOut.write("#VerticalDatum:  " + CsdpFunctions.getVDatum());
    _asciiOut.newLine();
    _asciiOut.write("#VerticalUnits:  " + CsdpFunctions.getVUnits());
    _asciiOut.newLine();

    for(int i=0; i<=_data.getNumLines()-1; i++){
      point  = _data.getPoint(i);
      point[xIndex]=CsdpFunctions.feetToMeters(point[xIndex]);
      point[yIndex]=CsdpFunctions.feetToMeters(point[yIndex]);
      //to extract data
//        //boynton
//        if(point[xIndex]>581282.0f && point[xIndex]<584531.0f &&
//  	 point[yIndex]>4228512.0f && point[yIndex]<4230005.0f){

      //chadbourne
//        if(point[xIndex]>580246.0f && point[xIndex]<581546.0f &&
//  	 point[yIndex]>4225578.0f && point[yIndex]<4226614.0f){

      //marsh
//          if(point[xIndex] > 575523.0f && point[xIndex] < 601110.0f &&
//           	 point[yIndex] > 4214805.0f && point[yIndex] < 4234369.0f){
      //confluence
//          if(point[xIndex] > 596436.0f && point[xIndex] < 610796.0f &&
//           	 point[yIndex] > 4208011.0f && point[yIndex] < 4215480.0f){

//          if(point[xIndex] > 613202.0f && point[xIndex] < 624893.0f &&
//           	 point[yIndex] > 4218988.0f && point[yIndex] < 4226661.0f){
      //dxc
//        if(point[xIndex] > 610084.0f && point[xIndex] < 619301.0f &&
//  	 point[yIndex] > 4213746.0f && point[yIndex] < 4221911.0f){
	year   = _data.getYear(_data.getYearIndex(i));
	source = _data.getSource(_data.getSourceIndex(i));
	line=point[xIndex]+" "+point[yIndex]+" "+point[zIndex]+" "+year+" "+source;
	_asciiOut.write(line);
	_asciiOut.newLine();
	//  		}
    }
    success = true;
  } catch(IOException e) {
    System.out.println
      ("Error ocurred while writing file "+ _directory + _filename + "."+
       ASCII_TYPE + e.getMessage());
  } finally {
  } // catch()
  return success;
}

  /**
   * Close ascii bathymetry data file
   */
protected void close(){
  try{
    _asciiOut.close();
  }catch(IOException e){
    System.out.println
      ("Error ocurred while closing file "+_directory + _filename+"." + 
       _filetype+":"+e.getMessage());
  }// catch
}//close

} // class BathymetryAsciiInput
