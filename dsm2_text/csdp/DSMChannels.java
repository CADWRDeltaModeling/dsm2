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
 * Stores channel connectivity information
 */
public class DSMChannels{

  /**
   * add a DSM channel.
   */
  public void addDSMChannel(int index, String name, int length, int upnode, 
			    int downnode, int xsect1, int dist1, int xsect2,
			    int dist2){
    putChanNum(index, name);
    putLength(name, length);
    putUpnode(name, upnode);
    putDownnode(name, downnode);
    putXsect1(name, xsect1);
    putDist1(name, dist1);
    putXsect2(name, xsect2);
    putDist2(name, dist2);
  }

  /**
   * stores channel number
   */
  protected void putChanNum(int index, String value){
    _chanNum.put(index, value);
    _numChannels++;
  }

  /**
   * returns channel number
   */
  public String getChanNum(int index){
    return _chanNum.get(index);
  }

  /**
   * returns number of channels
   */
  public int getNumChannels(){
    return _numChannels;
  }
  /**
   * stores length
   */
  public void putLength(String chan, int value){
    Integer lengthObject = new Integer(value);
    _length.put(chan, lengthObject);
  }
  
  /**
   * returns length
   */
  public int getLength(String chan){
      int length = -CsdpFunctions.BIG_INT;
      if(_length.containsKey(chan)){
	  length = ((Integer)_length.get(chan)).intValue();
      } else {
	  System.out.println("Not writing output for channel "+chan+" because it's not in the DSM2 input file");
      }
      return length;
  }
  
  /**
   * stores downnode
   */
  public void putDownnode(String chan, int value){
    Integer downnodeObject = new Integer(value);
    _downnode.put(chan, downnodeObject);
  }
  
  /**
   * returns downnode
   */
  public int getDownnode(String chan){
      int returnValue = -CsdpFunctions.BIG_INT;
      if(_downnode.containsKey(chan)){
	  returnValue= ((Integer)_downnode.get(chan)).intValue();
      }
      return returnValue;
  }
  
  /**
   * stores upnode
   */
  public void putUpnode(String chan, int value){
    Integer upnodeObject = new Integer(value);
    _upnode.put(chan, upnodeObject);
  }

    /**
     * stores first rectangular xsect number
     */
    public void putXsect1(String chan, int value){
	Integer xsect1Object = new Integer(value);
	_xsect1.put(chan, xsect1Object);
    }
    /**
     * Stores distance of first rectangular cross-section
     */
    public void putDist1(String chan, int value){
	Integer dist1Object = new Integer(value);
	_dist1.put(chan, dist1Object);
    }
    /**
     * stores first rectangular xsect number
     */
    public void putXsect2(String chan, int value){
	Integer xsect2Object = new Integer(value);
	_xsect2.put(chan, xsect2Object);
    }
    /**
     * Stores distance of first rectangular cross-section
     */
    public void putDist2(String chan, int value){
	Integer dist2Object = new Integer(value);
	_dist2.put(chan, dist2Object);
    }

    /**
     * returns number of first rectangular cross-section
     */
    public int getXsect1(String chan){
	int returnValue = -CsdpFunctions.BIG_INT;
	if(_xsect1.containsKey(chan)){
	    returnValue = ((Integer)_xsect1.get(chan)).intValue();
	}
	return returnValue;
    }
    /**
     * returns distance of first rectangular cross-section
     */
    public int getDist1(String chan){
	int returnValue = -CsdpFunctions.BIG_INT;
	if(_dist1.containsKey(chan)){
	    returnValue = ((Integer)_dist1.get(chan)).intValue();
	}
	return returnValue;
    }
    /**
     * returns number of second rectangular cross-section
     */
    public int getXsect2(String chan){
	int returnValue = -CsdpFunctions.BIG_INT;
	if(_xsect2.containsKey(chan)){
	    returnValue = ((Integer)_xsect2.get(chan)).intValue();
	}
	return returnValue;
    }
    /**
     * returns distance of second rectangular cross-section
     */
    public int getDist2(String chan){
	int returnValue = -CsdpFunctions.BIG_INT;
	if(_dist2.containsKey(chan)){
	    returnValue = ((Integer)_dist2.get(chan)).intValue();
	}
	return returnValue;
    }

    /**
   * returns upnode
   */
  public int getUpnode(String chan){
    if(DEBUG)System.out.println("chan, _upnode="+chan+","+_upnode);
    if(DEBUG)System.out.println("upnode="+(Integer)_upnode.get(chan)); 
    int returnValue = -CsdpFunctions.BIG_INT;
    if(_upnode.containsKey(chan)){
	returnValue = ((Integer)_upnode.get(chan)).intValue();
    }
    return returnValue;
  }

    /**
     * To check if all information exists for the specified channel number
     */
    public boolean channelExists(String chan){
	boolean exists = false;
	if(_length.containsKey(chan) && _downnode.containsKey(chan) &&
	   _upnode.containsKey(chan)) exists = true;
	return exists;
    }
  
    protected final String CHAN_HEADER     = "CHAN";
    protected final String LENGTH_HEADER   = "LENGTH";
    protected final String DOWNNODE_HEADER = "DOWNNODE";
    protected final String UPNODE_HEADER   = "UPNODE";
    protected final String XSECT_HEADER = "XSECT";
    protected final String DIST_HEADER  = "DIST";
    protected int _numChannels=0;
  
  protected ResizableStringArray _chanNum  = new ResizableStringArray();
  protected Hashtable _length   = new Hashtable();
  protected Hashtable _downnode = new Hashtable();
  protected Hashtable _upnode   = new Hashtable();
    protected Hashtable _xsect1 = new Hashtable();
    protected Hashtable _dist1 = new Hashtable();
    protected Hashtable _xsect2 = new Hashtable();
    protected Hashtable _dist2 = new Hashtable();
  protected static final boolean DEBUG = false;
  
}//DSMChannels
