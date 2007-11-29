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
import vista.set.*;
////  import DWR.Graph.*;
////  import DWR.Graph.Canvas.*;
//import vista.graph.*;

/**
 * used for creating and modifying set of network data to plot in xsect view
 */
public class NetworkDataSet extends DefaultDataSet {
  
  protected static final boolean DEBUG = false;
  /**
   * constructor with name for data set
   */  
  public NetworkDataSet(String name, double [] xArray, double [] yArray){
    super(name, xArray, yArray);
  }

    public double getMaxX(){
	double[] xArray = getXArray();
	double returnValue = -CsdpFunctions.BIG_FLOAT;
	for(int i=0; i<=xArray.length-1; i++){
	    if(xArray[i] > returnValue){
		returnValue = xArray[i];
	    }
	}
	return returnValue;
    }

    public double getMaxY(){
	double[] yArray = getYArray();
	double returnValue = -CsdpFunctions.BIG_FLOAT;
	for(int i=0; i<=yArray.length-1; i++){
	    if(yArray[i] > returnValue){
		returnValue = yArray[i];
	    }
	}
	return returnValue;
    }

    public double getMinX(){
	double[] xArray = getXArray();
	double returnValue = CsdpFunctions.BIG_FLOAT;
	for(int i=0; i<=xArray.length-1; i++){
	    if(xArray[i] < returnValue){
		returnValue = xArray[i];
	    }
	}
	return returnValue;
    }

    public double getMinY(){
	double[] yArray = getYArray();
	double returnValue = CsdpFunctions.BIG_FLOAT;
	for(int i=0; i<=yArray.length-1; i++){
	    if(yArray[i] < returnValue){
		returnValue = yArray[i];
	    }
	}
	return returnValue;
    }

    public double[] getXArray(){
	double xArray[] = new double[size()];
	DataSetIterator dsi = getIterator();
	dsi.resetIterator();
	for(int i=0; i<=size()-1; i++){
	    xArray[i] = dsi.getElement().getX();
	    dsi.advance();
	}
	dsi.resetIterator();
	return xArray;
    }

    public double[] getYArray(){
	double yArray[] = new double[size()];
	DataSetIterator dsi = getIterator();
	dsi.resetIterator();
	for(int i=0; i<=size()-1; i++){
	    yArray[i] = dsi.getElement().getY();
	    dsi.advance();
	}
	dsi.resetIterator();
	return yArray;
    }

    
  
}//NetworkDataSet
