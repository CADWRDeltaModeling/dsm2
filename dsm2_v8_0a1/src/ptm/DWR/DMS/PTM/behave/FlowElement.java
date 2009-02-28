//    Copyright (C) 1996 State of California, Department of Water
//    Resources.
//
//    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
//    numerical model.  No protection claimed in original FOURPT and
//    Branched Lagrangian Transport Model (BLTM) code written by the
//    United States Geological Survey.  Protection claimed in the
//    routines and files listed in the accompanying file "Protect.txt".
//    If you did not receive a copy of this file contact 
//    Tara Smith, below.
//
//    This program is licensed to you under the terms of the GNU General
//    Public License, version 2, as published by the Free Software
//    Foundation.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, contact Tara Smith, below,
//    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
//    02139, USA.
//
//    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
//    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
//    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
//    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
//    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
//    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
//    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
//    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
//    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
//    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
//    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
//    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
//    DAMAGE.
//
//    For more information about DSM2, contact:
//
//    Tara Smith
//    California Dept. of Water Resources
//    Division of Planning, Delta Modeling Section
//    1416 Ninth Street
//    Sacramento, CA  95814
//    916-653-9885
//    tara@water.ca.gov
//
//    or see our home page: http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/

package DWR.DMS.PTM.behave;
import com.sun.xml.tree.XmlDocument;
import com.sun.xml.tree.TreeWalker;
import org.w3c.dom.Element;
import java.util.*;

/**
 * This class contains Flow Behavior Properties.
 * It can read and write this information to/from XML. <br>
 * 
 * @author Aaron Miller
 * @version $Id: FlowElement.java,v 1.2 2000/08/07 17:05:05 miller Exp $
 */

public class FlowElement extends Behavior {

  public FlowElement() {
    pVector = new Vector();
  }

  public void setVerticalPosition(Vector pData){
    pVector = pData;
  }

  public int [][][] getVerticalArray(){
    return PosData;
  }

  public Vector getVerticalPosition(){
    return pVector;
  }

  public void xmlToVector(Element element, Vector vector, String name){
    Vector tmp;
    TreeWalker walker = new TreeWalker(element);
    for (int j = 0; j < NUM_ROWS; j++){
      element = walker.getNextElement(name);
      tmp = new Vector();
      for (int i = 0; i < headerData.length; i++){
	if (element != null)
	  tmp.addElement(element.getAttribute(headerData[i]));
	else
	  tmp.addElement("");
      }
      vector.addElement(tmp);
    }
  }

  public void xmlToArray(Element element, int [][][] array, String name){
    Vector tmp;
    TreeWalker walker = new TreeWalker(element);
    int count=0;
    element = walker.getNextElement(name);

    while (element != null){
      element = walker.getNextElement(name);
      count++;
    }
    walker.reset();

    array [0] = new int [count][];
    for (int i = 0; i < count; i++){
      element = walker.getNextElement(name);
      array [0][i] = new int [headerData.length];
      for (int j = 0; j < headerData.length; j++){
       	array [0][i][j] = (new Integer (element.getAttribute(headerData[j]))).intValue();
      }
    }
  }

  public void dumpArray(){
//         System.out.println("zlen = "+PosData.length+" ylen = "+yPosData.length);
//         System.out.println("zlen = "+PosData[0].length+" ylen = "+yPosData[0].length);
//         System.out.println("zlen = "+PosData[0][0].length+" ylen = "+yPosData[0][0].length);    
    System.out.println("ll = "+PosData[0][0][0]+" ul = "+PosData[0][0][1]);    
    System.out.println("st = "+PosData[0][0][2]+" et = "+PosData[0][0][3]);    

    int test [][][] = new int[1][][];

    System.arraycopy(PosData, 0, test, 0, PosData.length);

    System.out.println("ll = "+test[0][0][0]+" ul = "+test[0][0][1]);    
    System.out.println("st = "+test[0][0][2]+" et = "+test[0][0][3]);    
//     System.out.println("tlen = "+test.length);
//     System.out.println("tlen = "+test[0].length);
//     System.out.println("tlen = "+test[0][0].length);


  }

  public void fromXml(Element element){
    PosData = new int [1][][];
    TreeWalker walker = new TreeWalker(element);
    Element thisElement = walker.getNextElement("FLOW");

    if (thisElement != null){
      xmlToVector(thisElement, pVector, "V-POSITION");
      xmlToArray(thisElement, PosData, "V-POSITION");
    }
  }

  public void toXml(XmlDocument doc, Element element){
    Element thisElement = doc.createElement("FLOW");

    vectorToXml(doc, thisElement, pVector, "V-POSITION");
    element.appendChild(thisElement);
  }

  public void vectorToXml(XmlDocument doc, Element parent, Vector vector, String name){
    Element element;
    String stmp;
    Vector vtmp;
    for (int i = 0; i < vector.size(); i++){
      vtmp = (Vector)vector.elementAt(i);
      if ( (((String)vtmp.elementAt(0)).trim()).length() != 0){
	element = doc.createElement(name);
	for (int j = 0; j < vtmp.size(); j++){
	  stmp = (String)vtmp.elementAt(j);
	  if(testCondition(stmp)){
	    element.setAttribute(headerData[j], stmp);
	  }
      
	}
	parent.appendChild(element);
      }
    }
  }

  public boolean testCondition(String subject){
    if (subject != null && subject.length() != 0) {
      return true;
    }
    else {
      return false;
    }
  }


  Vector pVector;
private int NUM_ROWS = 2;
  boolean DEBUG = false;
  String headerData [] = {"flow_direction","lower_limit","upper_limit"};
  int [][][] PosData;

}

