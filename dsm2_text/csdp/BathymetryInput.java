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
import javax.swing.*;
/**
 * Read ascii and binary bathymetry data
 *
 * @author
 * @version $Id: BathymetryInput.java,v 1.2 2002/10/21 19:58:26 btom Exp $
 */
public abstract class BathymetryInput {

  /**
   * Make instance of subclass of BathymetryInput
   */
public static BathymetryInput getInstance(JFrame gui, String directory, String filename) {
    _gui = gui;
    _directory = directory;
    if((_directory.substring(_directory.length()-1,_directory.length())).
       equals(File.separator) == false){
	_directory += File.separator;
    }
    int dotIndex = filename.indexOf(".",0);
    _filename = filename.substring(0,dotIndex);
    _filetype = filename.substring(dotIndex+1);
    BathymetryInput input = null;
    if (_filetype.equals(ASCII_TYPE)) {
	input = new BathymetryAsciiInput();
    }
    else if (_filetype.equals(BINARY_TYPE)) {
	input = new BathymetryBinaryInput();
    }
    else if (_filetype.equals(GZIP_TYPE)) {
	input = new BathymetryBinaryGzipInput();
    }
    else {// throw new IllegalInputFileException(msg);
	System.out.println();
	_filetype = null;
    }
    return input;
} //getInstance
    
  /**
   * Calls appropriate read method to read bathymetry data
   */
public BathymetryData readData(){
  if(_data == null){
    _data = new BathymetryData();
    if(DEBUG)System.out.println("creating new BathymetryData object");
  }
  else{
      
    _data.initializeVariables();
    if(DEBUG)System.out.println("reinitializing BathymetryData object");
  }
  open();
  read();
  close();
  return _data;
}

  /**
   * Open file
   */
protected abstract void open();
  /**
   * Read file
   */
protected abstract void read();
  /**
   * Close file
   */
protected abstract void close();

  /**
   * Stores one line of parsed data in adjustable arrays.  Converts x&y to feet.
   */
protected void storeData(int dataNumber) {
  
  float x = metersToFeet(_pd.x);
  float y = metersToFeet(_pd.y);
  float z = _pd.z;
  _data.setPoint(dataNumber, x, y, z); 
  storeUnique(dataNumber);
}

  /**
   * Convert x and y values from meters(UTM) to feet
   */
protected float metersToFeet(float value){
  final float METERS_TO_FEET=3.28084f;
  return METERS_TO_FEET * value;
}

  /**
   * If year/source matches any previous years, don't save.  If it doesn't, save.
   * Save index of matching (or new) year.
   */
protected void storeUnique(int dataNumber){
  boolean pMatch=false;
  // YEAR
  for (short j=0; j<=_data.getNumYears()-1; j++) {
    if(_data.getYear(j)==_pd.year) {
      _data.putYearIndex(dataNumber,j);
      pMatch=true;
    }
  }
  if (pMatch==false) {
    _data.putYear(_data.getNumYears(),_pd.year);
    _data.putYearIndex(dataNumber, _data.getNumYears()-1);
  }

  // SOURCE
  pMatch=false;
  for(short j=0; j<=_data.getNumSources()-1; j++) {
    if(_data.getSource(j) != null && _data.getSource(j).equals(_pd.source)) {
      _data.putSourceIndex(dataNumber,j);
      pMatch=true;
    }
  }
  if(pMatch==false) {
    _data.putSource(_data.getNumSources(),_pd.source);
    _data.putSourceIndex(dataNumber,_data.getNumSources()-1);

    if(DEBUG)System.out.println("numSources="+_data.getNumSources()+" "+_pd.source);
  }
} // storeData()

  BathymetryData _data = null;
  BathymetryParsedData _pd = new BathymetryParsedData();  // vector-stores 6 values
  protected final int xIndex=0;
  protected final int yIndex=1;
  protected final int zIndex=2;
  public static final boolean DEBUG = false;
  
  protected static String _filename = null; // part of filename before the first dot
  protected static String _filetype = null; // filename extension (after first dot)
  protected static final String ASCII_TYPE  = "prn";
  protected static final String BINARY_TYPE = "cdp";
  protected static final String GZIP_TYPE   = "cdp.gz";
  protected static String _directory = null;
    protected static JFrame _gui;
} // class BathymetryInput
