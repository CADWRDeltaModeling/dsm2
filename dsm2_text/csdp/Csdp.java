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
import java.awt.*;
import DWR.CSDP.semmscon.UseSemmscon;
/**
 * Cross-section development Program
 *
 * @author
 * @version $Id: Csdp.java,v 1.2 2002/10/21 19:58:53 btom Exp $
 */
public class Csdp {

  /**
   * main method for Csdp
   */
    public static void main(String args[]) {
	try{
	    App app = new App();
//  	    if(args.length > 0){
//  		CsdpFunctions._csdpHome=args[0];
//  	    }else{
//  		CsdpFunctions._csdpHome="";
//  	    }
	    
	    CsdpFrame gui = new CsdpFrame(app);
	    System.out.println("Cross-Section Development Program Version "+
			       CsdpFunctions.getVersion());
	}catch(Exception e){
	    System.out.println("Error in Csdp.main:");
	    e.printStackTrace();
	}

	UseSemmscon us = new UseSemmscon();
	try{
	    us.init_convert();
	    
	}catch(java.lang.UnsatisfiedLinkError e){
	    System.out.println("java.lang.UnsatisfiedLinkError caught. e="+e);
	}
	
	final short utm83_units = 3;
	final short utm83_zone = 10;
	final short utm27_zone = 10;
	final short utm27_units = 3;
	
	final double utm83x = 577184.5;
	final double utm83y = 4227812.5;
	double[] utm83 = us.utm83ToUtm27(utm83x, utm83y, 
					 utm83_zone, utm83_units,
					 utm27_zone, utm27_units);
	System.out.println("--------------------------------------------");
	System.out.println("Input: utm83x, utm83y="+utm83x+","+utm83y);
	System.out.println("result of utm83ToUtm27:\n");
	System.out.println("utm83x, utm83y="+utm83[0]+","+utm83[1]);
	System.out.println("--------------------------------------------");
      
    }//main
    
}//class Csdp
