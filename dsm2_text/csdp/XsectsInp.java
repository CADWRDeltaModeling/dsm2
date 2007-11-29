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
 * Stores data which are written to irregular_xsects.inp--used to assign cross-
 * sections to channels
 *
 * @author
 * @version
 */
public class XsectsInp {
  
  /**
   * Returns number of Centerlines
   */
  public int getNumAssignments(){
    return _numXsects;
  }
  
    public void addXsect(int xsectNum, float width, float botelv,
			 float initStage, float initFlow){
	//	String xn = (new Integer(xsectNum)).toString();
	String xn = Integer.toString(xsectNum);
	setXsectNum(_numXsects, xsectNum);
	setWidth(xn, width);
	setBotelv(xn, botelv);
	setInitStage(xn, initStage);
	setInitFlow(xn, initFlow);
	_numXsects++;
    }

    /**
     * Stores number of Centerlines
     */
    public void putNumXsects(int value){
	_numXsects=value;
    }
    public int getXsectNum(int index){
	return _xsectNum.get(index);
    }

    public float getWidth(String xsectNum){
	return ((Float)_width.get(xsectNum)).floatValue();
    }
    public float getBotelv(String xsectNum){
	return ((Float)(_botelv.get(xsectNum))).floatValue();
    }
    public float getInitStage(String xsectNum){
	return ((Float)(_initStage.get(xsectNum))).floatValue();
    }
    public float getInitFlow(String xsectNum){
	return ((Float)(_initFlow.get(xsectNum))).floatValue();
    }

    public void setXsectNum(int index, int xsectNum){
	_xsectNum.put(index, xsectNum);
    }
    public void setWidth(String xsectNum, float width){
	_width.put(xsectNum, new Float(width));
    }
    public void setBotelv(String xsectNum, float botelv){
	_botelv.put(xsectNum, new Float(botelv));
    }
    public void setInitStage(String xsectNum, float initStage){
	_initStage.put(xsectNum, new Float(initStage));
    }
    public void setInitFlow(String xsectNum, float initFlow){
	_initFlow.put(xsectNum, new Float(initFlow));
    }

    /**
     * the number of lines in the file
     */
    private int _numXsects = 0;
    private ResizableIntArray _xsectNum = new ResizableIntArray();
    private Hashtable _width = new Hashtable();
    private Hashtable _botelv = new Hashtable();
    private Hashtable _initStage = new Hashtable();
    private Hashtable _initFlow = new Hashtable();

} // class XsectsInp
