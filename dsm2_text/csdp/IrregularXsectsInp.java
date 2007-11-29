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
public class IrregularXsectsInp {
  
  /**
   * Returns number of Centerlines
   */
  public int getNumChan(){
    return _numChan;
  }

    public String getChanNum(int index){
	String chanNum = null;
	if(index <= _allIxiChan.size()-1){
	    Enumeration e = _allIxiChan.keys();
	    for(int i=0; i<=index; i++){
		chanNum = (String)e.nextElement();
	    }
	}else{
	    System.out.println("error in IrregularXsectsInp.getChanNum");
	    System.out.println("probably called by App.calcRect");
	    System.out.println("index, number of elements="+
			       index+","+_allIxiChan.size());
	}
	return chanNum;
    }

    /**
     *
     */
    public IXIChan getChan(String chanNum){
	return (IXIChan)(_allIxiChan.get(chanNum));
    }

    /**
     * chanNum can't be index because some lines will have duplicate channel
     * numbers.
     */
    public void addLine(String chanNum, float distance, String filename){
	IXIChan ixiChan = null;

	if(_allIxiChan.containsKey(chanNum)){
	    ixiChan = (IXIChan)(_allIxiChan.get(chanNum));
	}else{
	    ixiChan = new IXIChan(chanNum);
	    _allIxiChan.put(chanNum, ixiChan);
	    _numChan++;
	}
	ixiChan.addIXILine(distance, filename);
	_allIxiChan.put(chanNum, ixiChan);
    }

    public void removeLine(String chanNum, float distance, String filename){
	IXIChan ixiChan = (IXIChan)(_allIxiChan.get(chanNum));
	ixiChan.removeIXILine(distance, filename);
    }

    public boolean chanExists(String chanNum){
	boolean returnValue = false;
	if(_allIxiChan.containsKey(chanNum)){
	    returnValue = true;
	}


	//	Enumeration e = _allIxiChan.keys();
//  	while(e.hasMoreElements()){
//  	    System.out.println("element="+(String)e.nextElement());
//  	}

	return returnValue;
    }

    /**
     * stores IXILines
     */
    public class IXIChan{
	public IXIChan(String chanNum){
	    _chanNum = chanNum;
	}
	public int getNumLines(){
	    return _numLines;
	}
	public IXILine getLine(int index){
	    return (IXILine)(_ixiLines.elementAt(index));
	}
	public void addIXILine(float distance, String filename){
	    IXILine ixiLine = new IXILine(distance, filename);
	    _ixiLines.addElement(ixiLine);
	    _numLines++;
	}
	public void removeIXILine(float distance, String filename){
	    int ixiLineIndex = -CsdpFunctions.BIG_INT;
	    for(int i=0; i<=_numLines-1; i++){
		if( ((IXILine)_ixiLines.elementAt(i)).getDistance() == distance &&
		    ((IXILine)_ixiLines.elementAt(i)).getFilename() == filename){
		    ixiLineIndex = i;
		}
	    }
	    _ixiLines.removeElementAt(ixiLineIndex);
	    _numLines--;
	}
	private int _numLines = 0;
	private Vector _ixiLines = new Vector();
	private String _chanNum;
    }//class IXIChan

    public class IXILine{
	public IXILine(float distance, String filename){
	    _distance = distance;
	    _filename = filename;
	}
	public float getDistance(){
	    return _distance;
	}
	public String getFilename(){
	    return _filename;
	}
	public void setDistance(float dist){
	    _distance = dist;
	}
	public void setFilename(String filename){
	    _filename = filename;
	}
	private float _distance;
	private String _filename;
    }//class IXILine

    private int _numChan = 0;
    private Hashtable _allIxiChan = new Hashtable();
} // class IrregularXsectsInp
