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
import java.io.*;
import java.util.*;

/**
 * Write DSM2 input file "irregular_xsects.inp"
 *
 * @author
 * @version
 */
public abstract class IrregularXsectsInpOutput {
  
  public static final boolean DEBUG = false;
  protected static final String FILENAME = "irregular_xsects.inp";
  protected static final String FIRST_LINE="IRREG_GEOM";
  protected static final String SECOND_LINE="CHAN     DIST       FILENAME";
  protected static final String LAST_LINE="END";
    protected static String _directory;

  /**
   * Make instance of subclass of IrregularXsectsInpOutput
   */
  public static IrregularXsectsInpOutput getInstance(String dir, Network net) {
      _directory = dir;
      if((_directory.substring(_directory.length()-1,_directory.length())).
	 equals(File.separator) == false){
	  _directory += File.separator;
      }
      IrregularXsectsInpOutput output = null;
      output = new IrregularXsectsInpAsciiOutput(net);
      return output;
  } //getInstance
  
  /**
   * Calls appropriate write method to write irregularXsectsInp data
   */
  public void writeData(){
    open();
    write();
    close();
  }
  
  /**
   * Open file
   */
  protected abstract void open();
  /**
   * write file
   */
  protected abstract void write();
  /**
   * Close file
   */
  protected abstract void close();
  
}//class IrregularXsectsInpOutput
