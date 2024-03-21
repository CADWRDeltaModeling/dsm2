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
import org.w3c.dom.Document;

/**
 * This class contains Physical Behavior Properties.
 * It can read and write this information to/from XML. <br>
 *
 * @author Aaron Miller
 * @version $Id:
 */

public class StageElement extends Behavior{

	public StageElement(){

	}

	public void setLowerHorizontalRising(String lower){
		_lowerHorzRising = lower;
	}

	public String getLowerHorizontalRising(){
		if(_lowerHorzRising == null)
			return "";
		return _lowerHorzRising;
	}

	public void setUpperHorizontalRising(String upper){
		_upperHorzRising = upper;
	}

	public String getUpperHorizontalRising(){
		if(_upperHorzRising == null)
			return "";
		return _upperHorzRising;
	}

	public void setLowerHorizontalFalling(String lower){
		_lowerHorzFalling = lower;
	}

	public String getLowerHorizontalFalling(){
		if(_lowerHorzFalling == null)
			return "";
		return _lowerHorzFalling;
	}

	public void setUpperHorizontalFalling(String upper){
		_upperHorzFalling = upper;
	}

	public String getUpperHorizontalFalling(){
		if(_upperHorzFalling == null)
			return "";
		return _upperHorzFalling;
	}

	public void setLowerVerticalRising(String lower){
		_lowerVertRising = lower;
	}

	public String getLowerVerticalRising(){
		if(_lowerVertRising == null)
			return "";
		return _lowerVertRising;
	}

	public void setUpperVerticalRising(String upper){
		_upperVertRising = upper;
	}

	public String getUpperVerticalRising(){
		if(_upperVertRising == null)
			return "";
		return _upperVertRising;
	}

	public void setLowerVerticalFalling(String lower){
		_lowerVertFalling = lower;
	}

	public String getLowerVerticalFalling(){
		if(_lowerVertFalling == null)
			return "";
		return _lowerVertFalling;
	}

	public void setUpperVerticalFalling(String upper){
		_upperVertFalling = upper;
	}

	public String getUpperVerticalFalling(){
		if(_upperVertFalling == null)
			return "";
		return _upperVertFalling;
	}

	public void setVelocityRising(String velocity, String units){
		_velocityRising = velocity;
		_velocityUnits = units;
		if(testCondition(velocity) && testCondition(units)){
			int unit = (int) new Integer(units).intValue();
			float conversion = Units.velConvert[unit];
			float value = (float) new Float(velocity).floatValue();
			_velocityRisingReal = (float) value*conversion;
		}
	}

	public String getVelocityRising(){
		if(_velocityRising == null)
			return "";
		return _velocityRising;
	}

	public void setVelocityFalling(String velocity, String units){
		_velocityFalling = velocity;
		_velocityUnits = units;
		if(testCondition(velocity) && testCondition(units)){
			int unit = (int) new Integer(units).intValue();
			float conversion = Units.velConvert[unit];
			float value = (float) new Float(velocity).floatValue();
			_velocityFallingReal = (float) value*conversion;
		}
	}

	public String getVelocityFalling(){
		if(_velocityFalling == null)
			return "";
		return _velocityFalling;
	}

	public String getVelocityUnits(){
		if(_velocityUnits == null)
			return "0";
		return _velocityUnits;
	}

	public float getVelocityRisingReal(){
		return _velocityRisingReal;
	}

	public float getVelocityFallingReal(){
		return _velocityFallingReal;
	}

	public int [][][] getVerticalArray(){
		return zPosData;
	}

	public int [][][] getTransverseArray(){
		return yPosData;
	}

	public void toArray(){
		// LL = 0
		// UL = 1
		//
		if(getLowerHorizontalRising() != "")
			yPosData[0][0][0] = Integer.parseInt(getLowerHorizontalRising());
		if(getUpperHorizontalRising() != "")
			yPosData[0][0][1] = Integer.parseInt(getUpperHorizontalRising());
		if(getLowerHorizontalFalling() != "")
			yPosData[0][1][0] = Integer.parseInt(getLowerHorizontalFalling());
		if(getUpperHorizontalFalling() != "")
			yPosData[0][1][1] = Integer.parseInt(getUpperHorizontalFalling());

		if(getLowerVerticalRising() != "")
			zPosData[0][0][0] = Integer.parseInt(getLowerVerticalRising());
		if(getUpperVerticalRising() != "")
			zPosData[0][0][1] = Integer.parseInt(getUpperVerticalRising());
		if(getLowerVerticalFalling() != "")
			zPosData[0][1][0] = Integer.parseInt(getLowerVerticalFalling());
		if(getUpperVerticalFalling() != "")
			zPosData[0][1][1] = Integer.parseInt(getUpperVerticalFalling());
	}

	public void fromXml(Element element){		
		Element sElement = Units.getElements(element,"STAGE").get(0);
		Element rElement = Units.getElements(sElement, "RISING").get(0);
		
		Element tElement = Units.getElements(rElement,"TRANSVERSE").get(0);
		setLowerHorizontalRising(tElement.getAttribute("lower_limit"));
		setUpperHorizontalRising(tElement.getAttribute("upper_limit"));
		
		Element vElement = Units.getElements(rElement,"VERTICAL").get(0);
		setLowerVerticalRising(vElement.getAttribute("lower_limit"));
		setUpperVerticalRising(vElement.getAttribute("upper_limit"));
		
		Element velElement = Units.getElements(rElement,"VELOCITY").get(0);
		setVelocityRising(velElement.getAttribute("value"),velElement.getAttribute("units"));
		
		Element fElement = Units.getElements(sElement, "FALLING").get(0);
		
		Element ftElement = Units.getElements(fElement,"TRANSVERSE").get(0);
		setLowerHorizontalFalling(ftElement.getAttribute("lower_limit"));
		setUpperHorizontalFalling(ftElement.getAttribute("upper_limit"));
		
		Element fvElement = Units.getElements(fElement,"VERTICAL").get(0);
		setLowerVerticalFalling(fvElement.getAttribute("lower_limit"));
		setUpperVerticalFalling(fvElement.getAttribute("upper_limit"));
		
		Element fvelElement = Units.getElements(fElement,"VELOCITY").get(0);
		setVelocityFalling(fvelElement.getAttribute("value"),fvelElement.getAttribute("units"));

		zPosData = new int [1][2][2];
		yPosData = new int [1][2][2];

		toArray();
	}

	/**
    * Sets parameter information in the XML file
    */
	public void toXml(Document doc, Element element){
		Element thisElement = doc.createElement("STAGE");
		Element subElement = doc.createElement("RISING");

		Element subSubElement = doc.createElement("TRANSVERSE");
		if(testCondition(_lowerHorzRising)) subSubElement.setAttribute("lower_limit",_lowerHorzRising);
		if(testCondition(_upperHorzRising)) subSubElement.setAttribute("upper_limit",_upperHorzRising);
		subElement.appendChild(subSubElement);

		subSubElement = doc.createElement("VERTICAL");
		if(testCondition(_lowerVertRising)) subSubElement.setAttribute("lower_limit",_lowerVertRising);
		if(testCondition(_upperVertRising)) subSubElement.setAttribute("upper_limit",_upperVertRising);
		subElement.appendChild(subSubElement);

		subSubElement = doc.createElement("VELOCITY");
		if(testCondition(_velocityRising)) subSubElement.setAttribute("value",_velocityRising);
		if(testCondition(_velocityUnits)) subSubElement.setAttribute("units",_velocityUnits);
		subElement.appendChild(subSubElement);
		thisElement.appendChild(subElement);


		subElement = doc.createElement("FALLING");
		subSubElement = doc.createElement("TRANSVERSE");
		if(testCondition(_lowerHorzFalling)) subSubElement.setAttribute("lower_limit",_lowerHorzFalling);
		if(testCondition(_upperHorzFalling)) subSubElement.setAttribute("upper_limit",_upperHorzFalling);
		subElement.appendChild(subSubElement);

		subSubElement = doc.createElement("VERTICAL");
		if(testCondition(_lowerVertFalling)) subSubElement.setAttribute("lower_limit",_lowerVertFalling);
		if(testCondition(_upperVertFalling)) subSubElement.setAttribute("upper_limit",_upperVertFalling);
		subElement.appendChild(subSubElement);

		subSubElement = doc.createElement("VELOCITY");
		if(testCondition(_velocityFalling)) subSubElement.setAttribute("value",_velocityFalling);
		if(testCondition(_velocityUnits)) subSubElement.setAttribute("units",_velocityUnits);
		subElement.appendChild(subSubElement);
		thisElement.appendChild(subElement);

		element.appendChild(thisElement);
	}


	public boolean testCondition(String subject){
		if (subject != null && subject.length() != 0) {
			return true;
		}
		else {
			return false;
		}
	}

	/**
	 * The horizontal lower limit for positioning on a rising stage.
	 */
	private String _lowerHorzRising;

	/**
	 * The horizontal upper limit for positioning on a rising stage.
	 */
	private String _upperHorzRising;

	/**
	 * The horizontal lower limit for positioning on a falling stage.
	 */
	private String _lowerHorzFalling;

	/**
	 * The horizontal upper limit for positioning on a falling stage.
	 */
	private String _upperHorzFalling;

	/**
	 * The vertical lower limit for positioning on a rising stage.
	 */
	private String _lowerVertRising;

	/**
	 * The vertical upper limit for positioning on a rising stage.
	 */
	private String _upperVertRising;

	/**
	 * The vertical lower limit for positioning on a falling stage.
	 */
	private String _lowerVertFalling;

	/**
	 * The vertical upper limit for positioning on a falling stage.
	 */
	private String _upperVertFalling;

	/**
	 * The additional velocity on a falling stage.
	 */
	private String _velocityFalling;

	/**
	 * The additional velocity on a rising stage.
	 */
	private String _velocityRising;

	/**
	 * The velocity units.
	 */
	private String _velocityUnits;

	/**
	 * The additional velocity on a falling stage adjusted to ft/s.
	 */
	private float _velocityFallingReal;

	/**
	 * The additional velocity on a rising stage adjusted to ft/s.
	 */
	private float _velocityRisingReal;

	int zPosData [][][];
	int yPosData [][][];

}



