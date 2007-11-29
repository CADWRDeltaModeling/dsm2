package DWR.DMS.PTM.behave;
import DWR.DMS.PTM.*;
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
		TreeWalker walker = new TreeWalker(element);
		Element thisElement = walker.getNextElement("STAGE");
		walker = new TreeWalker(thisElement); // new
		TreeWalker subWalker = new TreeWalker(walker.getNextElement("RISING"));

		thisElement = subWalker.getNextElement("TRANSVERSE");
		setLowerHorizontalRising(thisElement.getAttribute("lower_limit"));
		setUpperHorizontalRising(thisElement.getAttribute("upper_limit"));

		thisElement = subWalker.getNextElement("VERTICAL");
		setLowerVerticalRising(thisElement.getAttribute("lower_limit"));
		setUpperVerticalRising(thisElement.getAttribute("upper_limit"));

		thisElement = subWalker.getNextElement("VELOCITY");
		setVelocityRising(thisElement.getAttribute("value"),thisElement.getAttribute("units"));


		subWalker = new TreeWalker(walker.getNextElement("FALLING"));

		thisElement = subWalker.getNextElement("TRANSVERSE");
		setLowerHorizontalFalling(thisElement.getAttribute("lower_limit"));
		setUpperHorizontalFalling(thisElement.getAttribute("upper_limit"));

		thisElement = subWalker.getNextElement("VERTICAL");
		setLowerVerticalFalling(thisElement.getAttribute("lower_limit"));
		setUpperVerticalFalling(thisElement.getAttribute("upper_limit"));

		thisElement = subWalker.getNextElement("VELOCITY");
		setVelocityFalling(thisElement.getAttribute("value"),thisElement.getAttribute("units"));

		zPosData = new int [1][2][2];
		yPosData = new int [1][2][2];
		
		toArray();
	}

	/**
    * Sets parameter information in the XML file.
    */
	public void toXml(XmlDocument doc, Element element){
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



