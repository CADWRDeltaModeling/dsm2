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
import java.awt.*;
import java.util.*;

/**
 * Landmarks are symbols with labels.  Landmarks have x and y UTM coordinates
 *
 * @author
 * @version
 */
public class DigitalLineGraph {

  /**
   * Returns number of Landmarks
   */
  public int getNumLines(){
    return _numLines;
  }//getNumLines
  
  /**
   * Stores number of lines
   */
  public void putNumLines(int value){
    _numLines=value;
  }//putNumLines

  /**
   * adds a line
   */
  public void addLine(String name){
    DigitalLineGraphLine line = new DigitalLineGraphLine(name);
    _dlgLineTable.put(name, line);
    putDigitalLineGraphLineName(_numLines, name);
    _numLines++;
    if(DEBUG)System.out.println("adding line number "+_numLines);
  }//addLine

    private void putDigitalLineGraphLineName(int index, String name){
	_dlgLineNames.put(index, name);
    }

    public String getDigitalLineGraphLineName(int index){
	return _dlgLineNames.get(index);
    }

    public DigitalLineGraphLine getLine(String name){
	return (DigitalLineGraphLine)_dlgLineTable.get(name);
    }
    public DigitalLineGraphLine getLine(int index){
	return (DigitalLineGraphLine)
	    //	    _dlgLineTable.get((new Integer(index)).toString());
	    _dlgLineTable.get(getDigitalLineGraphLineName(index));
    }

  /**
   * removes DigitalLineGraphLine from hashtable
   */
  public void removeDigitalLineGraphLine(String name){
    Object value = _dlgLineTable.remove(name);
    _numLines--;
  }

//    /**
//     * returns x value
//     */
//    public float getX(String pointName){
//      DigitalLineGraphPoint dlgPoint = 
//  	(DigitalLineGraphPoint)(_dlgLineTable.get(pointName));
//      if(dlgPoint != null) return dlgPoint.x;
//      else return -CsdpFunctions.BIG_FLOAT;
//    }

//    /**
//     * returns y value
//     */
//    public float getY(String pointName){
//      DigitalLineGraphPoint dlgPoint = 
//  	(DigitalLineGraphPoint)(_dlgLineTable.get(pointName));
//      if(dlgPoint != null) return dlgPoint.y;
//      else return -CsdpFunctions.BIG_FLOAT;
//    }

    /**
     * stores value of minimum x coordinate.
     */
    public void putMinX(float value){
      _minX = value;
    }

    /**
     * stores value of maximum x coordinate.
     */
    public void putMaxX(float value){
      _maxX = value;
    }
    /**
     * stores value of minimum y coordinate.
     */
    public void putMinY(float value){
      _minY = value;
    }
    /**
     * stores value of maximum y coordinate.
     */
    public void putMaxY(float value){
      _maxY = value;
    }
    /**
     * returns value of minimum x coordinate
     */
    protected float getMinX(){
      return _minX;
    }
    /**
     * returns value of maximum x coordinate
     */
    protected float getMaxX(){
      return _maxX;
    }
    /**
     * returns value of minimum y coordinate
     */
    protected float getMinY(){
      return _minY;
    }
    /**
     * returns value of maximum y coordinate
     */
    protected float getMaxY(){
      return _maxY;
    }

  protected int _numLines=0;
  protected Hashtable _dlgLineTable = new Hashtable();
  protected ResizableStringArray _dlgLineNames = new ResizableStringArray();
    protected float _minX = 0.0f;
    protected float _maxX = 0.0f;
    protected float _minY = 0.0f;
    protected float _maxY = 0.0f;
    private final boolean DEBUG = false;
}//class DigitalLineGraph
