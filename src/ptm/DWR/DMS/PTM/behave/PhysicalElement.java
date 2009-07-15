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
import com.sun.xml.tree.XmlDocument;
import com.sun.xml.tree.TreeWalker;
import org.w3c.dom.Element;

/**
 * This class contains Physical Behavior Properties.
 * It can read and write this information to/from XML. <br>
 * 
 * @author Aaron Miller
 * @version $Id:
 */

public class PhysicalElement extends Behavior {

  public PhysicalElement() {

  }
  /**
    * sets the fall velocity, units and the real value
    */
  public void setFallVel(String fallvel, String units) {
    float value, conversion;
    int unit;
    _fallvel = fallvel.trim();
    if(testCondition(_fallvel)) {
      _fallvelUnits = units.trim();
      unit = (int) new Integer(getFallVelUnits()).intValue();
      conversion = unitData.velConvert[unit];
      value = (float) new Float(getFallVel()).floatValue();
      _fallvelReal =  value*conversion;
    }
    if (DEBUG) System.out.println("Fall velocity (ft/sec) = "+_fallvelReal);
  }

  /**
    * gets the fall velocity
    */
   public String getFallVel() {
    return _fallvel;
  }

  /**
    * gets the fall velocity units
    */
   public String getFallVelUnits() {
    return _fallvelUnits;
  }

  /**
    * gets the real value of fall velocity 
    */
   public float getFallVelReal() {
    return _fallvelReal;
  }

  /**
    * sets the Development Time, units and the real value.
    * the Development Time amount of time for which a particle 
    * has these properties.
    */
  public void setDevelopTime(String time, String units) {
    float value, conversion;
    int unit;
    _developTime = time.trim();
    if(testCondition(_developTime)) {
      _developTimeUnits = units.trim();
      unit = (int) new Integer(getDevelopTimeUnits()).intValue();
      conversion = unitData.timeConvert[unit];
      value = (float) new Float(getDevelopTime()).floatValue();
      _developTimeReal =  value*conversion;
    }
    if (DEBUG) System.out.println("Development time (sec) = "+_developTimeReal);
  }

  /**
    * gets the Development Time
    */
  public String getDevelopTime() {
    return _developTime;
  }

  /**
    * gets the Development Time units
    */
  public String getDevelopTimeUnits() {
    return _developTimeUnits;
  }

  /**
    * gets the real value of Development Time 
    */
  public float getDevelopTimeReal() {
    return _developTimeReal;
  }

  /**
    * Sets the mortality rate for the particle given a probability
    * and unit of time.
    */
  public void setMortality(String mortality, String units) {
    float value, conversion;
    int unit;
    _mortality = mortality.trim();
    if(testCondition(_mortality)) {
      _mortalityUnits = units.trim();
      unit = (int) new Integer(getMortalityUnits()).intValue();
      conversion = unitData.mortalConvert[unit];
      value = (float) new Float(getMortality()).floatValue();
      _mortalityReal = (float) -(Math.log(1.0-value))*conversion; // calculates the alpha in exponential distribution
    }
    if (DEBUG) System.out.println("Mortality (1/sec) = "+_mortalityReal);
  }

  /**
    * gets the Mortality
    */
  public String getMortality() {
    return _mortality;
  }

  /**
    * gets the Mortality units
    */
  public String getMortalityUnits() {
    return _mortalityUnits;
  }

  /**
    * gets the real value of Mortality 
    */
  public float getMortalityReal() {
    return _mortalityReal;
  }


  /**
    * Gets parameter information from the XML file.
    */
  public void fromXml(Element element){
    TreeWalker walker = new TreeWalker(element);
    Element thisElement = walker.getNextElement("PHYSICAL");
    walker = new TreeWalker(thisElement); // new

    thisElement = walker.getNextElement("FALL_VELOCITY"); //new
    setFallVel(thisElement.getAttribute("value"), thisElement.getAttribute("units"));

    thisElement = walker.getNextElement("MORTALITY"); //new
    setMortality(thisElement.getAttribute("value"), thisElement.getAttribute("units"));

    thisElement = walker.getNextElement("DEVELOP_TIME"); //new
    setDevelopTime(thisElement.getAttribute("value"), thisElement.getAttribute("units"));
  }

  /**
    * Sets parameter information in the XML file.
    */
  public void toXml(XmlDocument doc, Element element){
    Element thisElement = doc.createElement("PHYSICAL");

    Element subElement = doc.createElement("FALL_VELOCITY");
    if(testCondition(_fallvel)) subElement.setAttribute("value",_fallvel);
    if(testCondition(_fallvelUnits)) subElement.setAttribute("units",_fallvelUnits);
    thisElement.appendChild(subElement);

    subElement = doc.createElement("MORTALITY");
    if(testCondition(_mortality)) subElement.setAttribute("value",_mortality);
    if(testCondition(_mortalityUnits)) subElement.setAttribute("units",_mortalityUnits);
    thisElement.appendChild(subElement);

    subElement = doc.createElement("DEVELOP_TIME");
    if(testCondition(_developTime)) subElement.setAttribute("value",_developTime);
    if(testCondition(_developTimeUnits)) subElement.setAttribute("units",_developTimeUnits);
    thisElement.appendChild(subElement);

    element.appendChild(thisElement);
  }

  /**
    * Tests the condition of the String. Returns true if String contains 
    * information.
    */
  public boolean testCondition(String subject){
    if (subject != null && subject.length() != 0) {
      return true;
    }
    else {
      return false;
    }
  }

  /**
    * 
    */
  boolean DEBUG = false;

  /**
    * Unit data and conversion tables.
    */
  static Units unitData;

  /**
    * String representation of fall velocity.
    */
  String _fallvel;

  /**
    * String representation of mortality.
    */
  String _mortality;

  /**
    * String representation of development time.
    */
  String _developTime;

  /**
    * String representation of fall velocity units.
    */
  String _fallvelUnits;

  /**
    * String representation of mortality units.
    */
  String _mortalityUnits;

  /**
    * String representation of development time units.
    */
  String _developTimeUnits;
    
  /**
    * Real value of the fall velocity and default value.
    */
  float _fallvelReal = 0.0f;

  /**
    * Real value of the mortality and default value.
    */
  float _mortalityReal = 0.0f;

  /**
    * Real value of the development time and default value.
    */
  float _developTimeReal = 0.0f;

}

