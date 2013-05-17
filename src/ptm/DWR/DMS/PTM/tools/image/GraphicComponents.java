/*
    Copyright (C) 1996-2000 State of California, Department of 
    Water Resources.

    DSM2-PTM : Delta Simulation Model 2 - Particle Tracking Model module.
        Maintained by: Aaron Miller
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA 95814
    (916)-653-4603
    knam@water.ca.gov

    Send bug reports to knam@water.ca.gov

    This program is licensed to you under the terms of the GNU General
    Public License, version 2, as published by the Free Software
    Foundation.

    You should have received a copy of the GNU General Public License
    along with this program; if not, contact Tara Smith, below,
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

    For more information about PTM, contact:

    Tara Smith
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA  95814
    916-653-9885
    tara@water.ca.gov

    or see our home page: http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/

    Send bug reports to knam@water.ca.gov or call (916)-653-7552

*/
package DWR.DMS.PTM.tools.image;

import java.awt.*;
import java.io.*;
import java.util.*;
/**
 * @author Aaron Miller
 * @version $Id: GraphicComponents.java,v 1.3 2000/08/07 17:15:19 miller Exp $
 * 
 */
public class GraphicComponents {
  PlotBarGraph graph;
  PlotLabel label;
  PlotGraphic graphic;
  Vector info;
  int [] values2 = {10,15,25,50};
  float [] values;
  public GraphicComponents(BufferedReader input){
    info = new Vector();
    createPlots(input);
  }

  private void createPlots (BufferedReader in){
    String line;
    try {
      line = in.readLine();
      while(true){
	if (line == null) break;
	info.addElement(line);
	line = in.readLine();
      }
      in.close();
    }catch (IOException ex){System.out.println(ex);}
  }

  public void drawComponents(Graphics g){
    String line;
    String str;
    String labelStr;
    int x,y,type,rotation;
    int num,fcol,bcol,bar;
    graph = new PlotBarGraph(g);
    label = new PlotLabel(g);
    graphic = new PlotGraphic(g);
    for (int i = 0; i < info.size(); i++){
      line = (String) info.elementAt(i);
      StringTokenizer st = new StringTokenizer(line,",");
      str = st.nextToken();
      if (str.equals("plot:")) {
	x = (new Integer(st.nextToken())).intValue();
	y = (new Integer(st.nextToken())).intValue();
	values = new float[st.countTokens()];
	for (int j = 0; j < values.length; j++){
	  values [j] = (new Float(st.nextToken())).floatValue();
	}
	graph.drawGraph(x,y,values);
      }
      else if (str.equals("label:")){
	x = (new Integer(st.nextToken())).intValue();
	y = (new Integer(st.nextToken())).intValue();
	type = (new Integer(st.nextToken())).intValue();
	labelStr = st.nextToken();
	label.drawLabel(x,y,type,labelStr);
      }
      else if (str.equals("barrier:")){
	x = (new Integer(st.nextToken())).intValue();
	y = (new Integer(st.nextToken())).intValue();
	type = (new Integer(st.nextToken())).intValue();
	rotation = (new Integer(st.nextToken())).intValue();
	graphic.drawGraphic(x,y,type,rotation);
      }
      else if (str.equals("plotpref:")){
	num = (new Integer(st.nextToken())).intValue();
	fcol = (new Integer(st.nextToken())).intValue();
	bcol = (new Integer(st.nextToken())).intValue();
	bar = (new Integer(st.nextToken())).intValue();
	graph.setPrefs(num,fcol,bcol,bar);
      }
    }
  }

}
