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
    miller@water.ca.gov

    Send bug reports to miller@water.ca.gov

    This program is licensed to you under the terms of the GNU General
    Public License, version 2, as published by the Free Software
    Foundation.

    You should have received a copy of the GNU General Public License
    along with this program; if not, contact Dr. Paul Hutton, below,
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

    Dr. Paul Hutton
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA  95814
    916-653-5601
    hutton@water.ca.gov

    or see our home page: http://wwwdelmod.water.ca.gov/

    Send bug reports to miller@water.ca.gov or call (916)-653-7552

*/
package DWR.DMS.PTM.tools.image;

import java.awt.*;
/**
 * @author Aaron Miller
 * @version $Id: PlotBarGraph.java,v 1.3 2000/08/07 17:15:20 miller Exp $
 * 
 */

public class PlotBarGraph{
  Graphics g;
  int pHeight = 100;
  int pWidth = 7;
  Bar bar;
  Color [] color = {Color.black, Color.white, Color.darkGray, Color.gray, Color.lightGray,
		    Color.blue, Color.cyan, Color.green, Color.yellow, 
		    Color.orange, Color.red, Color.pink, Color.magenta};

  Bar [] barType = {new SolidBar(),new VertLineBar(),new CrossHatchBar(),
		    new RightDiagBar(),new LeftDiagBar(),new SquareHatchBar()};

  int [][] prefs = {{6,2,2},{2,2,1},{8,2,3},{9,10,4},{10,1,5},{11,2,6},{3,3,1},{5,5,1},{1,1,1},{13,13,1}};

  public PlotBarGraph(Graphics g){
    this.g = g;
  }

  /**
   * determines the height of this plot element
   */
  public void drawGraph (int xpos, int ypos, float [] values){
    int y;
    y = ypos;
    for(int i = 0; i < values.length; i++){
      bar = barType[prefs[i][2]-1];
      paintBlock(xpos, y - pHeight, (int)(pHeight*values[i])/100, 
		 color[prefs[i][0]-1], color[prefs[i][1]-1]);
      y = y + (int)values[i];
    }
  }
  
  private void paintBlock(int xpos, int ypos, int height, Color frColor, Color bkColor){
    bar.setForeGroundColor(frColor);
    bar.setBackGroundColor(bkColor);
    bar.drawBar(g,xpos,ypos,pWidth,height);
  }

  public void setPrefs(int num, int foreCol, int backCol, int barType){
    prefs[num-1][0] = foreCol;
    prefs[num-1][1] = backCol;
    prefs[num-1][2] = barType;
  }
}
