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
package DWR.CSDP.semmscon;

/**
 * Uses geographic conversion functions in semmscon.dll, which 
 * contains all the functions in the Corpscon program.
 *
 * To compile this and the c++ program that accesses the functions
 * in the dll, you need the jdk version 1.31_02, the free
 * Borland command line tools, which includes a c++ compiler,
 * and semmscon.bat, available at http://www.semms.org/
 * Put semmscon.dll in your working directory.  Add the bin directory
 * or your Borland command line tools installation to your path.
 * execute the following commands to create a .lib file from your
 * semmscon.dll file:
 *   impdef -a semmscon.def semmscon.dll -- creates a .def file
 *   implib semmscon.lib semmscon.def    -- creates a .lib file
 * To compile this class and create a header file:
 *   c:\progra~1\jdk1.31_02\bin\javac *.java
 *   c:\progra~1\jdk1.31_02\bin\javah -classpath . -jni UseSemmscon
 * To compile the c++ program:
 *   bcc32 -WD -RT- -x- -v -Id:\java\csdp\semmscon utmconvert semmscon.lib
 *
 * @author
 * @version $Id:
 */
public class UseSemmscon{
  
  /*
   * using geoid9396
   */
  public native void init_convert();
  public native double[] utm83ToUtm27(double utm83x, double utm83y, 
				      short utm83_zone, short utm83_units, 
				      short utm27_zone, short utm27_units);
  public native double[] utm27ToUtm83(double utm27x, double utm27y, 
				      short utm27_zone, short utm27_units, 
				      short utm83_zone, short utm83_units);
  
  public native double ngvd29_to_navd88_utm83(double utm83x, double utm83y,
					      short utm83_zone, short utm83_units,
					      double elev29, short elevUnitsIn,
					      short unitsOut);
  public native double navd88_to_ngvd29_utm83(double utm83x, double utm83y,
					      short utm83_zone, short utm83_units,
					      double elev88, short elevUnitsIn,
					      short unitsOut);
  
  public native double ngvd29_to_navd88_utm27(double utm27x, double utm27y,
					      short utm27_zone, short utm27_units,
					      double elev29, short elevUnitsIn,
					      short unitsOut);
  public native double navd88_to_ngvd29_utm27(double utm27x, double utm27y,
					      short utm27_zone, short utm27_units,
					      double elev88, short elevUnitsIn,
					      short unitsOut);
  /*
   * load utmconvert.dll
   */
  static{
    System.loadLibrary("dwr_csdp_semmscon_utmconvert");
  }
  
//        public static void main(String args[]){
//          UseSemmscon us = new UseSemmscon();
//          us.init_convert();
//          //units:  1=Survey feet, 2=international feet, 3=meters
//          //test utm83ToUtm27    
//          final short utm83_units = 3;
//          final short utm83_zone = 10;
//          final short utm27_zone = 10;
//          final short utm27_units = 3;

//          final double utm83x = 577184.5;
//          final double utm83y = 4227812.5;
//          double[] utm83 = us.utm83ToUtm27(utm83x, utm83y, 
//      				     utm83_zone, utm83_units,
//      				     utm27_zone, utm27_units);
//          System.out.println("--------------------------------------------");
//          System.out.println("Input: utm83x, utm83y="+utm83x+","+utm83y);
//          System.out.println("result of utm83ToUtm27:\n");
//          System.out.println("utm83x, utm83y="+utm83[0]+","+utm83[1]);
//          System.out.println("--------------------------------------------");
//          //test utm27ToUtm83
//          final double utm27x = 629355.0;
//          final double utm27y = 4199384.0;
//          double[] utm27 = us.utm27ToUtm83(utm27x, utm27y, 
//      				     utm27_zone, utm27_units,
//      				     utm83_zone, utm83_units);
//          System.out.println("input:  utm27x, utm27y="+utm27x+","+utm27y);
//          System.out.println("result of utm27ToUtm83:\n");
//          System.out.println("utm27x, utm27y="+utm27[0]+","+utm27[1]);
//          System.out.println("--------------------------------------------");

//          //test ngvd29_to_navd88_utm83
//          final double elev29 = 1.55;
//          final short elevUnitsIn = 1;
//          final short unitsOut = 1;
//          double navd88 = us.ngvd29_to_navd88_utm83(utm83x, utm83y, utm83_zone, utm83_units, 
//      					      elev29, elevUnitsIn, unitsOut);
//          System.out.println("Input: utm83x, utm83y, ngvd29="+utm83x+","+utm83y+","+elev29);
//          System.out.println("result of ngvd29_to_navd88_utm83:\n");
//          System.out.println("navd88 = "+navd88);
//          System.out.println("--------------------------------------------");

//          //test navd88_to_ngvd29_utm83
//          final double elev88 = 1.55;
//          double ngvd29 = us.navd88_to_ngvd29_utm83(utm83x, utm83y, utm83_zone, utm83_units, 
//      					      elev88, elevUnitsIn, unitsOut);
//          System.out.println("Input: utm83x, utm83y, navd88="+utm83x+","+utm83y+","+elev88);
//          System.out.println("result of navd88_to_ngvd29_utm83:\n");
//          System.out.println("ngvd29 = "+ngvd29);
//          System.out.println("--------------------------------------------");

//          //test ngvd29_to_navd88_utm27
//          navd88 = us.ngvd29_to_navd88_utm27(utm27x, utm27y, utm27_zone, utm27_units, 
//        				       elev29, elevUnitsIn, unitsOut);
//          System.out.println("Input: utm27x, utm27y, ngvd29="+utm27x+","+utm27y+","+elev29);
//          System.out.println("result of ngvd29_to_navd88_utm27:\n");
//          System.out.println("navd88 = "+navd88);
//          System.out.println("--------------------------------------------");

//          //test navd88_to_ngvd29_utm27
//          ngvd29 = us.navd88_to_ngvd29_utm27(utm27x, utm27y, utm27_zone, utm27_units, 
//        				       elev88, elevUnitsIn, unitsOut);
//          System.out.println("Input: utm27x, utm27y, navd88="+utm27x+","+utm27y+","+elev88);
//          System.out.println("result of navd88_to_ngvd29_utm27:\n");
//          System.out.println("ngvd29 = "+ngvd29);
//          System.out.println("--------------------------------------------");

//        }
}//class UseSemmscon
