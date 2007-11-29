//    Copyright (C) 1996 State of California, Department of Water
//    Resources.
//
//    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
//    numerical model.  No protection claimed in original FOURPT and
//    Branched Lagrangian Transport Model (BLTM) code written by the
//    United States Geological Survey.  Protection claimed in the
//    routines and files listed in the accompanying file "Protect.txt".
//    If you did not receive a copy of this file contact Dr. Paul
//    Hutton, below.
//
//    This program is licensed to you under the terms of the GNU General
//    Public License, version 2, as published by the Free Software
//    Foundation.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, contact Dr. Paul Hutton, below,
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
//    Dr. Paul Hutton
//    California Dept. of Water Resources
//    Division of Planning, Delta Modeling Section
//    1416 Ninth Street
//    Sacramento, CA  95814
//    916-653-5601
//    hutton@water.ca.gov
//
//    or see our home page: http://wwwdelmod.water.ca.gov/

package DWR.DMS.PTM;
import DWR.DMS.PTM.behave.*;
import java.awt.event.*;
import com.sun.xml.tree.XmlDocument;
import com.sun.xml.tree.TreeWalker;
import org.w3c.dom.Element;
import java.io.*;
import java.util.*;

/**
 * @author Aaron Miller
 * @version $Id: ParticleBehavior.java,v 1.5 2000/08/07 17:00:30 miller Exp $
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
  int [][][] zPosData;
  int [][][] yPosData;
  int _currentAgeId;
  int LL = 0;
  int UL = 1;
  int ST = 2;
  int ET = 3;

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

  public void setId(int id){
    _id = id;
  }

  public String getId(){
    return new String().valueOf(_id);
  }

  public void setCurrentAgeId(float age){
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

    zPosData = new int [phaseId.size()][][];
    yPosData = new int [phaseId.size()][][];

    tmpPhase = (Phase) phases.get(phaseId.elementAt(0));
    ageId [0] = tmpPhase.getDevelopTime();

    System.arraycopy(tmpPhase.getVerticalPosition(), 0, zPosData, 0, 1);
    System.arraycopy(tmpPhase.getTransversePosition(), 0, yPosData, 0, 1);

    for ( int i = 1; i < phaseId.size(); i++){
      tmpPhase = (Phase) phases.get(phaseId.elementAt(i));
      ageId [i] = ageId [i-1] + tmpPhase.getDevelopTime();

      System.arraycopy(tmpPhase.getVerticalPosition(), 0, zPosData, i, 1);
      System.arraycopy(tmpPhase.getTransversePosition(), 0, yPosData, i, 1);

      if (DEBUG) System.out.println("age "+i+" "+ageId[i]);
    }
  }

  public float getFallVel(float age){
    setCurrentAgeId(age);
    setPhase();
    return thisPhase.getFallVel();
  }

  public float getMortality(float age){
    setCurrentAgeId(age);
    setPhase();
    return thisPhase.getMortality();
  }

  public float getDevelopTime(float age){
    setCurrentAgeId(age);
    setPhase();
    return thisPhase.getDevelopTime();
  }
  //**********************************************************************************

  //**************** Position Information ********************************************

  public void initPositioning(){

  }

  public int getZLowerLimit(float age, int time){
    setCurrentAgeId(age);
    return extractData(zPosData,this.LL,time,0);
  }

  public int getZUpperLimit(float age, int time){
    setCurrentAgeId(age);
    return extractData(zPosData,this.UL,time,100);
  }

  public int getYLowerLimit(float age, int time){
    setCurrentAgeId(age);
    return extractData(yPosData,this.LL,time,0);
  }

  public int getYUpperLimit(float age, int time){
    setCurrentAgeId(age);
    return extractData(yPosData,this.UL,time,50);
  }

  private int extractData(int [][][] dataArray, int index, int time, int defaultVal){
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


  //**********************************************************************************

  public void load(String filename) throws IOException{
    try {
      XmlDocument doc = XmlDocument.createXmlDocument(new FileInputStream(filename),false);
      this.fromXml(doc.getDocumentElement());
    }catch(Exception e){ System.out.println(e); }
  }

  public void fromXml(Element element){
    TreeWalker walker = new TreeWalker(element);
    setId (new Integer (element.getAttribute("id")).intValue());
    int numBehaviors = new Integer (element.getAttribute("num_behaviors")).intValue();
    walker.reset();
    Phase newPhase;
    String phaseName;
    for ( int i=0; i<numBehaviors; i++){
      Element phaseElement = walker.getNextElement("PHASE");
      phaseName = phaseElement.getAttribute("name");
      if ( phaseElement == null ) //break;
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
