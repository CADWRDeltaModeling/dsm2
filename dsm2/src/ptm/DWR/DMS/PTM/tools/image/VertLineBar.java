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
/**
 * @author Aaron Miller
 * @version $Id: VertLineBar.java,v 1.2 2000/08/07 17:15:22 miller Exp $
 * 
 */

public class VertLineBar extends Bar{

  int xStep = 2;

  public void drawBar(Graphics g, int x, int y, int w, int h){
    //    g.fillRect(x,y,w,h);
    g.setColor(getBackGroundColor());
    g.fillRect(x,y,w,h);
    g.setColor(getForeGroundColor());
    for (int i = 0; i <= w; i=i+xStep){
      g.drawLine(x+i,y,x+i,y+h);
    }
    g.setColor(Color.black);
    g.drawRect(x,y,w,h);
  }

}
