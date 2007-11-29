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
    /**
     * A line in a DigitalLineGraph
     */
    public class DigitalLineGraphLine{
	public DigitalLineGraphLine(String n){
	    _name = n;
	    _points  = new Vector();
	}

	/**
	 * add DigitalLineGraphPoint to hashtable
	 */
	public void addPoint(int index, float x, float y){
	    DigitalLineGraphPoint point = new DigitalLineGraphPoint(x,y);
	    _numPoints++;
	    _points.add(index, point);
	    if(DEBUG)System.out.println("added point:"+x+","+y);
	    if(index >= _numPoints){
  		System.out.println("Error in DigitalLineGraphLine.addPoint:");
  		System.out.println("Specified point index is greater than the");
  		System.out.println("specified number of points.");
  		System.out.println("_numPoints, index="+_numPoints+","+index);
		
	    }
	}

	public DigitalLineGraphPoint getPoint(int index){
	    return (DigitalLineGraphPoint)_points.elementAt(index);
	}

	/**
	 * Remove DigitalLineGraphPoint from hashtable
	 */
	public void removePoint(int index){
	    _points.setElementAt(null, index);
	}
	/**
	 * returns number of DigitalLineGraphPoints
	 */
	public int getNumPoints(){
	    return _numPoints;
	}
	/**
	 * returns x value of specified point name
	 */
	public float getX(int index){
	    DigitalLineGraphPoint dlgPoint = 
		(DigitalLineGraphPoint)(_points.elementAt(index));
	    if(dlgPoint != null) return dlgPoint.getX();
	    else return -CsdpFunctions.BIG_FLOAT;
	}
	/**
	 * returns y value of specified point name
	 */
	public float getY(int index){
	    DigitalLineGraphPoint dlgPoint = 
		(DigitalLineGraphPoint)(_points.elementAt(index));
	    if(dlgPoint != null) return dlgPoint.getY();
	    else return -CsdpFunctions.BIG_FLOAT;
	}
	private Vector _points;
	private String _name;
	private int _numPoints;
	private final boolean DEBUG=false;
    }//class DigitalLineGraphLine
