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
 * Read the DSM2 file channels.inp, which contains node/channel connectivity information
 *
 * @author
 * @version
 */
public abstract class XsectInput {

  /**
   * Make instance of subclass of IrregularXsectsInpInput
   */
public static XsectInput getInstance(String fullPathname) {
    _fullPathname = fullPathname;
    XsectInput input = null;
    input = new XsectAsciiInput();
    return input;
} //getInstance

  /**
   * Calls appropriate read method to read DSMChannels data
   */
public Xsect readData(){
    Xsect xsect = null;
    open();
    xsect = read();
    close();
    return xsect;
}

  /**
   * Open file
   */
protected abstract void open();
  /**
   * Read file
   */
protected abstract Xsect read();
  /**
   * Close file
   */
protected abstract void close();

  /**
   * Stores a Xsect point
   */
protected Xsect storeData() {
    Xsect xsect = new Xsect();

    if(_numStationValues == 0 && _numElevationValues == 0){
	System.out.println("ERROR in XsectInput.storeData while reading file "+
			   _fullPathname);
	System.out.println("numstationvalues, numelevationvalues="+_numStationValues+","+_numElevationValues);

    }

    if(_numStationValues == _numElevationValues){
	for(int i=0; i<=_numStationValues-1; i++){
	    xsect.addXsectPoint(_station.get(i),_elevation.get(i));

	    if(DEBUG)System.out.println("adding xsect point:  station, elevation="+_station.get(i)+","+_elevation.get(i));
	}
	//	xsect.setDistAlongCenterline(_distAlong);
    }else{
	System.out.println("ERROR in XsectInput.storeData:");
	System.out.println("numStationValues,numElevationValues="+
			   _numStationValues+","+_numElevationValues);
    }
    return xsect;
}//storeData

    IrregularXsectsInp _data = new IrregularXsectsInp();
    //parsed data

    //    protected float _distAlong = -CsdpFunctions.BIG_FLOAT;
    protected ResizableFloatArray _station = new ResizableFloatArray(20,10);
    protected ResizableFloatArray _elevation = new ResizableFloatArray(20,10);
    protected int _numStationValues = -CsdpFunctions.BIG_INT;
    protected int _numElevationValues = -CsdpFunctions.BIG_INT;

    public static final boolean DEBUG = false;
    
    protected static String _fullPathname = null;
} // class XsectInput
