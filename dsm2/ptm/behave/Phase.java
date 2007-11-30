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

package DWR.DMS.PTM.behave;
import com.sun.xml.tree.XmlDocument;
import com.sun.xml.tree.TreeWalker;
import org.w3c.dom.Element;
import java.util.*;

/**
 * This class creates a field that contains 
 * a label, text field and a combo box. <br>
 * 
 * @author Aaron Miller
 * @version $Id: Phase.java,v 1.5.6.1 2006/04/04 18:16:26 eli2 Exp $
 */

public class Phase {

  /**
    *  Name of this Phase
    */
  String _name;

  /**
    *  Physical behaviors
    */
  private PhysicalElement physical;

  /**
    *  Time related behaviors
    */
  //  private TimeElement time;

  /**
    *  Position related behaviors
    */
  private PositionElement position;

  /**
    *  Flow affect on behaviors
    */
  private FlowElement flow;

  /**    
    *  Stage related behaviors
    */
  private StageElement stage;

  /**
    *  Quality related behaviors
    */
  //  private QualityElement quality;

  /**
   * A Phase is a theoretical period in which a particle may have certain
   * traits or behaviors. It contains specific behavior properties such as
   * a PhysicalElement and TimeElement.
   *
   * @param name Name of the this Phase.
   *
   */
  public Phase(String name) {
    _name = name;
    physical = new PhysicalElement();
    //    time = new TimeElement();
    position = new PositionElement();
    flow = new FlowElement();
    stage = new StageElement();
    //    quality = new QualityElement();
  }

  //****************************************************************

  /**
    *  returns the PhysicalElement object
    */
  public PhysicalElement getPhysical(){
    return physical;
  }

  /**
    *  gets the fall velocity from PhysicalElement
    */
  public float getFallVel(){
    return physical.getFallVelReal();
  }

  /**
    *  gets the mortality from PhysicalElement
    */
  public float getMortality(){
    return physical.getMortalityReal();
  }

  /**
    *  gets the development time from PhysicalElement
    */
  public float getDevelopTime(){
    return physical.getDevelopTimeReal();
  }

  //****************************************************************

  /**
    *  returns the PhysicalElement object
    */
  public PositionElement getPosition(){
    return position;
  }

  /**
    *  gets the vertical positions from PositionElement
    */
  public int [][][] getTimeVerticalPosition(){
    return position.getVerticalArray();
  }

  /**
    *  gets the transverse positions from PositionElement
    */
  public int [][][] getTimeTransversePosition(){
    return position.getVerticalArray();
  }

  //****************************************************************

  /**
    *  returns the FlowElement object
    */
  public FlowElement getFlow(){
    return flow;
  }

  /**
    *  gets the vertical positions from FlowElement
    */
  public int [][][] getFlowVertPosition(){
    return flow.getVerticalArray();
  }

  //****************************************************************

  /**
    *  returns the StageElement object
    */
  public StageElement getStage(){
    return stage;
  }

  /**
    *  gets the vertical positions from PositionElement
    */
  public int [][][] getStageVerticalPosition(){
    return stage.getVerticalArray();
  }

  /**
    *  gets the transverse positions from PositionElement
    */
  public int [][][] getStageTransversePosition(){
    return stage.getVerticalArray();
  }


  //****************************************************************


  /** 
    *  passes this Element to the specific behaviors
    */
  public void fromXml(Element phaseElement){
    TreeWalker walker = new TreeWalker(phaseElement);
    _name = phaseElement.getAttribute("name");

    physical.fromXml(phaseElement);
    //    time.fromXml(phaseElement);
    position.fromXml(phaseElement);
    flow.fromXml(phaseElement);
    stage.fromXml(phaseElement);
    //    quality.fromXml(phaseElement);
    //    System.out.println(physical.getFallVel());
    //    System.out.println(physical.getMortality());
  }
  

  /**
    *
    */
  public void toXml(XmlDocument doc, Element element){
    Element phaseElement = doc.createElement("PHASE");
    phaseElement.setAttribute("name",_name);

    physical.toXml(doc,phaseElement);
    //    time.toXml(doc,phaseElement);
    position.toXml(doc,phaseElement);
    flow.toXml(doc,phaseElement);
    stage.toXml(doc,phaseElement);    
    //    quality.toXml(doc,phaseElement);

    element.appendChild(phaseElement);
  }

  void setPhysicalElement (PhysicalElement value){
    physical = value;
  }

  public String getName() {
    return _name;
  }

}
