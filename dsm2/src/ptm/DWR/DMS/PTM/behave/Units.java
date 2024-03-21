//    Copyright (C) 1996, 2009 State of California, Department of Water
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

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import java.util.Vector;
import java.util.Map;
import java.util.HashMap;

public class Units {

  public static int TIME = 1;

  public static int VEL = 2;

  public static int MORT = 3;

  public static String time [] = {"sec","min","hour","day","week","year"};
  public static float timeConvert [] = {1.f, 60.f, 3600.f, 86400.f, 604800.f, 31556926.f};

  public static String velocity [] = {"ft/s","m/s","mm/s"};
  public static float velConvert [] = {1.f, 3.2808f, 0.0032808f};

  public static String mortality [] = {"dth/sec","dth/min","dth/hour","dth/day","dth/month","dth/year"};
  public static float mortalConvert [] = {1.f, 0.01667f, 2.7777e-4f, 1.157e-5f, 1.653e-6f, 3.169e-8f};
  //get elements from XML
  public static Vector<Element> getElements(Element element, String tagName) {
	    NodeList nodeList = element.getElementsByTagName(tagName);
	    Vector<Element> els = new Vector<Element>(); 
		for ( int i=0; i<nodeList.getLength(); i++){
			Node node = nodeList.item(i);
			if ((node != null) && (node.getNodeType() == Node.ELEMENT_NODE)) 
				els.add((Element) node);
		}
		return els;
  }

}
