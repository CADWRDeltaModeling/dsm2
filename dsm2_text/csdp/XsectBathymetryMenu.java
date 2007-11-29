/*
    Copyright (C) 1998 State of California, Department of Water
    Resources.

    This program is licensed to you under the terms of the GNU General
    Public License, version 2, as published by the Free Software
    Foundation.

    You should have received a copy of the GNU General Public License
    along with this program; if not, contact Dr. Francis Chung, below,
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

    For more information, contact:

    Dr. Francis Chung
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA  95814
    916-653-5601
    chung@water.ca.gov

    or see our home page: http://wwwdelmod.water.ca.gov/
*/
package DWR.CSDP;
import DWR.CSDP.dialog.*;
import java.awt.event.*;
import java.awt.*;
import java.io.*;
public class XsectBathymetryMenu{

  public XsectBathymetryMenu(XsectGraph xsectGraph){
    _xsectGraph = xsectGraph;
  }

    /**
     * color bathymetry points in xsect view by distance from cross-section line
     */
    public class XColorByDistance implements ItemListener{
      public void itemStateChanged(ItemEvent e){
        _xsectGraph.updateGraphCanvas();
        _xsectGraph._gC.redoNextPaint();
        _xsectGraph.validate();
  	//removed for conversion to swing
        //      _xsectGraph._gC.repaint();
      }//itemStateChanged
    }//XColorByDistance

    /**
     * color bathymetry points in xsect view by source
     */
    public class XColorBySource implements ItemListener{
      public void itemStateChanged(ItemEvent e){
        _xsectGraph.updateGraphCanvas();
        _xsectGraph._gC.redoNextPaint();
        _xsectGraph.validate();
  	//removed for conversion to swing
        //      _xsectGraph._gC.repaint();
      }//itemStateChanged
    }//XColorBySource

    /**
     * color bathymetry points in xsect view by year
     */
    public class XColorByYear implements ItemListener{
      public void itemStateChanged(ItemEvent e){
        _xsectGraph.updateGraphCanvas();
        _xsectGraph._gC.redoNextPaint();
        _xsectGraph.validate();
  	//removed for conversion to swing
        //      _xsectGraph._gC.repaint();
      }//itemStateChanged
    }//XColorByYear

    /**
     * Change size of points in cross-section window
     */
    public class BChangePointSize implements ActionListener{
	public void actionPerformed(ActionEvent e){
	    TextDialog d = new TextDialog(_xsectGraph, "Enter new point dimension", true,
					  _xsectGraph.getPointSize());
	    d.show();
//  	    String s = d.tf.getText();
//  	    Float f = new Float(s);
	    float f = Float.parseFloat(d.tf.getText());
	    int nin = (int)(f);
	    if(nin != _xsectGraph.getPointSize()){
		System.out.println("Changing value");
		_xsectGraph.setPointSize(nin);
		_xsectGraph.updateGraphCanvas();
		_xsectGraph._gC.redoNextPaint();
		_xsectGraph.validate();
	//removed for conversion to swing
		//		_xsectGraph._gC.repaint();
	    }//if
	}//actionPerformed
    }//BChangePointSize

  XsectGraph _xsectGraph;
  protected static final boolean DEBUG = true;
}//XsectEditMenu
