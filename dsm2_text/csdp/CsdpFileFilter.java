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
    Chung@water.ca.gov

    or see our home page: http://wwwdelmod.water.ca.gov/
*/
package DWR.CSDP;
import java.awt.*;
import java.io.*;
import javax.swing.*;
import javax.swing.filechooser.*;

public class CsdpFileFilter extends javax.swing.filechooser.FileFilter{

    String[] _extensions;
    int _numExtensions;
    public CsdpFileFilter(String[] extensions, int numExtensions){
	_extensions = extensions;
	_numExtensions = numExtensions;
    }

    // Accept all directories and all file types
    public boolean accept(File f) {
	boolean ok=false;
	if(f.isDirectory()) {
	    return true;
	}
	String s = f.getName();
	int i = s.lastIndexOf(".");
	if(i > 0 &&  i < s.length() - 1) {
	    String extension = 
		s.substring(i+1).toLowerCase();
	    
	    for(int index =0; index<=_numExtensions-1; index++){
		if(_extensions[index].equals(extension)) ok=true;
	    }
	}
	return ok;
    }
    
    // The description of this filter
    public String getDescription() {
	String line = null;
	line = "Filetypes: ";
	for(int index=0; index<=_numExtensions-1; index++){
	    if(index < _numExtensions-1){
		line += "*."+_extensions[index]+", ";
	    }else{
		line += "*."+_extensions[index];
	    }//else
	}//for
	return line;
    }
//JFileChooser filechooser = new JFileChooser();
//filechooser.addChoosableFileFilter(new MyFilter());


}//CsdpFileFilter
