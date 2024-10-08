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

package DWR.DMS.PTM;
import DWR.DMS.PTM.behave.*;
import javax.xml.parsers.*;
//import java.awt.event.*;
//TODO clean up
//import com.sun.xml.tree.XmlDocument;
//import com.sun.xml.tree.TreeWalker;
import org.w3c.dom.Element;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;
import java.io.*;
import java.util.*;

import javax.xml.parsers.DocumentBuilderFactory;

/**
 * @author Aaron Miller
 * @version $Id: ParticleBehavior.java,v 1.5.6.1 2006/04/04 18:16:24 eli2 Exp $
 */

public class ParticleBehavior {

  Hashtable phases;
  Vector phaseId;
  int _id;
  float _age;
  String file;
  Phase thisPhase;
  static Units unitData;
  float [] ageId;
  float _baseAge;
  boolean DEBUG = false;
  int [][][] zTimePosData;
  int [][][] yTimePosData;
	// [phase no][row no][start/end/index]
	int [][][] zStagePosData;
	int [][][] yStagePosData;
	// [phase no][tidephase][index]
  int _currentAgeId;
  public static int LL = 0;
  public static int UL = 1;
  public static int ST = 2;
  public static int ET = 3;

  public ParticleBehavior (String filename) throws IOException {
    phases = new Hashtable();
    phaseId = new Vector();
    load(filename);
    initDevelopTime();
  }

  public void setPhase() {
    thisPhase = (Phase) phases.get(phaseId.elementAt(_currentAgeId));
  }

  public void setPhaseAge(int i){
    if(i != 0) {
      _baseAge = ageId [i-1];
    }
    else _baseAge = 0.0f;
  }

  public float getPhaseAge(){
    return _baseAge;
  }

	public Phase getPhase(){
		return thisPhase;
	}

  public void setId(int id){
    _id = id;
  }

  public String getId(){
    return new String().valueOf(_id);
  }

  public void setCurrentAgeId(double age){
    int i = 0;
    while (i <= ageId.length - 1 && ageId [i] <= age) i++;
    _currentAgeId = i;
    setPhaseAge(i);
  }

  public int getCurrentAgeId(){
    return _currentAgeId;
  }

  public int getNumberPhases(){
    return phases.size();
  }
  //**************** Physical Information ********************************************

  public void initDevelopTime() {
    Phase tmpPhase;
    ageId = new float [phaseId.size()];

	// initialize time position
    zTimePosData = new int [phaseId.size()][][];
    yTimePosData = new int [phaseId.size()][][];

	// initialize stage position
	zStagePosData = new int [phaseId.size()][][];
	yStagePosData = new int [phaseId.size()][][];

    tmpPhase = (Phase) phases.get(phaseId.elementAt(0));
    ageId [0] = tmpPhase.getDevelopTime();

	dump3DArray(tmpPhase.getTimeVerticalPosition());

    System.arraycopy(tmpPhase.getTimeVerticalPosition(), 0, zTimePosData, 0, 1);
    System.arraycopy(tmpPhase.getTimeTransversePosition(), 0, yTimePosData, 0, 1);

    System.arraycopy(tmpPhase.getStageVerticalPosition(), 0, zStagePosData, 0, 1);
    System.arraycopy(tmpPhase.getStageTransversePosition(), 0, yStagePosData, 0, 1);

    for ( int i = 1; i < phaseId.size(); i++){
      tmpPhase = (Phase) phases.get(phaseId.elementAt(i));
      ageId [i] = ageId [i-1] + tmpPhase.getDevelopTime();

      System.arraycopy(tmpPhase.getTimeVerticalPosition(), 0, zTimePosData, i, 1);
      System.arraycopy(tmpPhase.getTimeTransversePosition(), 0, yTimePosData, i, 1);

      System.arraycopy(tmpPhase.getStageVerticalPosition(), 0, zStagePosData, i, 1);
      System.arraycopy(tmpPhase.getStageTransversePosition(), 0, yStagePosData, i, 1);

      if (DEBUG) System.out.println("age "+i+" "+ageId[i]);
    }

  }

	public void addIndexData(int data[]){

	}

  public float getFallVel(double age){
    setCurrentAgeId(age);
    setPhase();
    return thisPhase.getFallVel();
  }

  public float getMortality(double age){
    setCurrentAgeId(age);
    setPhase();
    return thisPhase.getMortality();
  }

  public float getDevelopTime(double age){
    setCurrentAgeId(age);
    setPhase();
    return thisPhase.getDevelopTime();
  }
  //**********************************************************************************

  //**************** Time Position Information ********************************************

  public void initPositioning(){

  }

  public int getTimeZLowerLimit(double age, int time){
    setCurrentAgeId(age);
    return extractTimeData(zTimePosData,this.LL,time,0);
  }

  public int getTimeZUpperLimit(double age, int time){
    setCurrentAgeId(age);
    return extractTimeData(zTimePosData,this.UL,time,100);
  }

  public int getYLowerLimit(double age, int time){
    setCurrentAgeId(age);
    return extractTimeData(yTimePosData,this.LL,time,0);
  }

  public int getYUpperLimit(double age, int time){
    setCurrentAgeId(age);
    return extractTimeData(yTimePosData,this.UL,time,50);
  }

  private int extractTimeData(int [][][] dataArray, int index, int time, int defaultVal){
    int answer = defaultVal;

    if (dataArray[_currentAgeId] != null)
      for (int i = 0; i < dataArray[_currentAgeId].length; i++){
	if (dataArray[_currentAgeId][i][ST] < dataArray[_currentAgeId][i][ET] &&
	    dataArray[_currentAgeId][i][ST] <= time &&
	    dataArray[_currentAgeId][i][ET] > time){
	  answer = dataArray[_currentAgeId][i][index];
	  //	System.out.println("regular index "+ answer);
	}
	else if (dataArray[_currentAgeId][i][ST] > dataArray[_currentAgeId][i][ET] &&
		 (dataArray[_currentAgeId][i][ST] <= time ||
		  dataArray[_currentAgeId][i][ET] > time)){
	  answer = dataArray[_currentAgeId][i][index];
	  //	System.out.println("reversed index "+ answer);
	}
      }
    return answer;
  }

  //**************** Stage Position Information ********************************************

	public int getStageZLowerLimit(double age, int tidalPhase){
		setCurrentAgeId(age);
		return extractStageData(zStagePosData,this.LL,tidalPhase,0);
	}

	public int getStageZUpperLimit(double age, int tidalPhase){
		setCurrentAgeId(age);
		return extractStageData(zStagePosData,this.UL,tidalPhase,100);
	}

	private int extractStageData(int [][][] dataArray, int index, int tidalPhase, int defaultVal){
		int answer = defaultVal;
		if (dataArray[_currentAgeId] != null)
			answer = dataArray[_currentAgeId][tidalPhase][index];

//  		System.out.println("index="+index+" tidalPhase="+tidalPhase+" ans="+answer);
//  		dump3DArray(dataArray);
		return answer;

	}

	public static void dump3DArray(int [][][] dataArray){
//  		System.out.println("Array Dimensions "+dataArray.length+"X"+dataArray[0]+"X"+dataArray[0][0]);
		for(int i=0; i<dataArray.length; i++){
			for(int j=0; j<dataArray[i].length; j++){
				for(int k=0; k<dataArray[i][j].length; k++){
					System.out.println("i="+i+" j="+j+" k="+k+" val="+dataArray[i][j][k]);
				}
			}
		}
	}

  //**********************************************************************************

  public void load(String filename) throws IOException{
    try {
      //XmlDocument doc = XmlDocument.createXmlDocument(new FileInputStream(filename),false);
      Document doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new FileInputStream(filename));
      this.fromXml(doc.getDocumentElement());
    }catch(Exception e){ System.out.println(e); }
  }

  public void fromXml(Element element){  
    setId (new Integer (element.getAttribute("id")).intValue());
    int numBehaviors = new Integer (element.getAttribute("num_behaviors")).intValue();    
    //walker.reset();
    Phase newPhase;
    String phaseName;
    Vector<Element> elements = Units.getElements(element,"PHASE");
    for ( int i=0; i<numBehaviors; i++){
		Element phaseElement = elements.get(i); 
		phaseName = phaseElement.getAttribute("name");
		if ( phaseElement == null ) //break;
			//TODO need to raise an exception?
			System.out.println("Error in fromXml");
		newPhase = new Phase(phaseName);
		// create vector name lookup
		phaseId.insertElementAt(phaseName,i);
		// fill new Phase will values
		newPhase.fromXml(phaseElement);
		// store in a Hashtable
		phases.put(phaseId.elementAt(i),newPhase);    	
    }
  }
}
