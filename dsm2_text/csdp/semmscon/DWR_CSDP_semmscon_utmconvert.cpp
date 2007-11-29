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


//Geodetic Conversion in C++
#include <iostream.h>
#include <stdio.h>
#include <windows.h>
#include <conio.h>
#include "DWR_CSDP_semmscon_UseSemmscon.h"

extern "C" _declspec( dllimport) int init_geoid9396( );
extern "C" _declspec( dllimport) int init_nadcon( );
extern "C" _declspec( dllimport) int init_vertcon( );

extern "C" _declspec( dllimport) void utm83_to_utm27(double, double, short int, short int,
						     double *, double *, short int, short int);
extern "C" _declspec( dllimport) void utm27_to_utm83(double, double, short int, short int,
						     double *, double *, short int, short int);
extern "C" _declspec( dllimport) void utm83_to_geo83(double, double, short int, short int, double *, double *);
extern "C" _declspec( dllimport) void utm27_to_geo83(double, double, short int, short int, double *, double *);
//
extern "C" _declspec( dllimport) void ngvd29_to_navd88(double, double, double, short int, double *, short int);
extern "C" _declspec( dllimport) void navd88_to_ngvd29(double, double, double, short int, double *, short int);

JNIEXPORT void JNICALL
Java_DWR_CSDP_semmscon_UseSemmscon_init_1convert(JNIEnv *env, jobject obj) {
  // Read nadcon data files into memory
  int i = init_nadcon();
  // Read vertcon data files into memory
  int j = init_vertcon();
  // Initialize Geoid in memory
  int k = init_geoid9396();
}

/*
 * Call function in semmscon.dll
 */
JNIEXPORT jdoubleArray JNICALL
Java_DWR_CSDP_semmscon_UseSemmscon_utm83ToUtm27(JNIEnv *env, jobject obj, jdouble utm83x, 
							    jdouble utm83y, jshort utm83_zone, 
							    jshort utm83_units, jshort utm27_zone, 
							    jshort utm27_units){
  double utm27x;
  double utm27y;

  jdoubleArray rtnValues = env->NewDoubleArray((jint)2);
  //create pointer to rtnValues
  double *OutData = env->GetDoubleArrayElements(rtnValues,JNI_FALSE);

  double result[2];
  utm83_to_utm27(utm83x, utm83y, utm83_zone, utm83_units,
		 &utm27x, &utm27y, utm27_zone, utm27_units);
  OutData[0] = utm27x;
  OutData[1] = utm27y;
  
//    char buf3[300] = " ";
//    char s[10] = " ";
//    int j=0;
//    j = sprintf( buf3, " Input Data: %s\n", s);
//    j += sprintf( buf3 +j, "\tInEastUTM: %s",s );
//    j += sprintf( buf3 +j, "%#9.2f", utm83x);
//    j += sprintf( buf3 +j, " InNorthUTM: %s",s);
//    j += sprintf( buf3 +j, "%#9.2f\n", utm83y);
//    j += sprintf( buf3 +j, " Insp_zone: %s",s);
//    j += sprintf( buf3 +j, "%d", utm83_zone);
//    j += sprintf( buf3 +j, " InUnits: %s", s);
//    j += sprintf( buf3 +j, "%d\n", utm83_units);
//    j += sprintf( buf3 +j, "Output Results: %s", s);
//    j += sprintf( buf3 +j, "UTM27x: %s", s);
//    j += sprintf( buf3 +j, "%#9.2f", utm27x);
//    j += sprintf( buf3 +j, " UTM27y: %s", s);
//    j += sprintf( buf3 +j, "%#9.2f", utm27y);
//    printf("***Results***: \n%s", buf3);

  env->ReleaseDoubleArrayElements(rtnValues, OutData, 0);
  
  return rtnValues;
}

/*
 * Call function in semmscon.dll
 */
JNIEXPORT jdoubleArray JNICALL
Java_DWR_CSDP_semmscon_UseSemmscon_utm27ToUtm83(JNIEnv *env, jobject obj, jdouble utm27x, 
						jdouble utm27y, jshort utm27_zone, 
						jshort utm27_units, jshort utm83_zone, 
						jshort utm83_units){
  double utm83x;
  double utm83y;

  jdoubleArray rtnValues = env->NewDoubleArray((jint)2);
  //create pointer to rtnValues
  double *OutData = env->GetDoubleArrayElements(rtnValues,JNI_FALSE);

  double result[2];
  utm27_to_utm83(utm27x, utm27y, utm27_zone, utm27_units,
		 &utm83x, &utm83y, utm83_zone, utm83_units);
  OutData[0] = utm83x;
  OutData[1] = utm83y;
  
//    printf("outdata[0]= %11.4f\n",OutData[0]);

//    char buf3[300] = " ";
//    char s[10] = " ";
//    int j=0;
//    j = sprintf( buf3, " Input Data: %s\n", s);
//    j += sprintf( buf3 +j, "\tInEastUTM: %s",s );
//    j += sprintf( buf3 +j, "%#9.2f", utm27x);
//    j += sprintf( buf3 +j, " InNorthUTM: %s",s);
//    j += sprintf( buf3 +j, "%#9.2f\n", utm27y);
//    j += sprintf( buf3 +j, " Insp_zone: %s",s);
//    j += sprintf( buf3 +j, "%d", utm27_zone);
//    j += sprintf( buf3 +j, " InUnits: %s", s);
//    j += sprintf( buf3 +j, "%d\n", utm27_units);
//    j += sprintf( buf3 +j, "Output Results: %s", s);
//    j += sprintf( buf3 +j, "UTM27x: %s", s);
//    j += sprintf( buf3 +j, "%#9.2f", utm83x);
//    j += sprintf( buf3 +j, " UTM27y: %s", s);
//    j += sprintf( buf3 +j, "%#9.2f", utm83y);
//    printf("***Results***: \n%s", buf3);

  env->ReleaseDoubleArrayElements(rtnValues, OutData, 0);
  
  return rtnValues;
}

/*
 * Call function in semmscon.dll
 */
JNIEXPORT jdouble JNICALL
Java_DWR_CSDP_semmscon_UseSemmscon_ngvd29_1to_1navd88_1utm83(JNIEnv *env, jobject obj,
					   jdouble utm83x, jdouble utm83y,
					   jshort utm83_zone, jshort utm83_units,
					   jdouble elev29, jshort elevUnitsIn,
					   jshort elevUnitsOut){
  double navd88, lat83, lon83;

  //convert utm to lat/lon
  utm83_to_geo83(utm83x, utm83y, utm83_zone, utm83_units, &lat83, &lon83);
  ////  ngvd29_to_navd88(lat83, lon83, &navd88, elevUnitsOut, elev29, elevUnitsIn);
  //reversed order of input/output elevations and units.  assuming documentation is wrong.
  ngvd29_to_navd88(lat83, lon83, elev29, elevUnitsIn, &navd88, elevUnitsOut);

  return navd88;
}

/*
 * Call function in semmscon.dll
 */
JNIEXPORT jdouble JNICALL
Java_DWR_CSDP_semmscon_UseSemmscon_navd88_1to_1ngvd29_1utm83(JNIEnv *env, jobject obj, 
					   jdouble utm83x, jdouble utm83y,
					   jshort utm83_zone, jshort utm83_units,
					   jdouble elev88, jshort elevUnitsIn,
					   jshort elevUnitsOut){
  double ngvd29, lat83, lon83;
  utm83_to_geo83(utm83x, utm83y, utm83_zone, utm83_units, &lat83, &lon83);
  navd88_to_ngvd29(lat83, lon83, elev88, elevUnitsIn, &ngvd29, elevUnitsOut);
  return ngvd29;
}

/*
 * Call function in semmscon.dll
 */
JNIEXPORT jdouble JNICALL
Java_DWR_CSDP_semmscon_UseSemmscon_ngvd29_1to_1navd88_1utm27(JNIEnv *env, jobject obj,
					   jdouble utm27x, jdouble utm27y,
					   jshort utm27_zone, jshort utm27_units,
					   jdouble elev29, jshort elevUnitsIn,
					   jshort elevUnitsOut){
  double navd88, lat83, lon83;
  utm27_to_geo83(utm27x, utm27y, utm27_zone, utm27_units, &lat83, &lon83);
  //reversed order of input/output elevations and units.  assuming documentation is wrong.
  ngvd29_to_navd88(lat83, lon83, elev29, elevUnitsIn, &navd88, elevUnitsOut);
  return navd88;
}

/*
 * Call function in semmscon.dll
 */
JNIEXPORT jdouble JNICALL
Java_DWR_CSDP_semmscon_UseSemmscon_navd88_1to_1ngvd29_1utm27(JNIEnv *env, jobject obj, 
					   jdouble utm27x, jdouble utm27y,
					   jshort utm27_zone, jshort utm27_units,
					   jdouble elev88, jshort elevUnitsIn,
					   jshort elevUnitsOut){
  double ngvd29, lat83, lon83;
  utm27_to_geo83(utm27x, utm27y, utm27_zone, utm27_units, &lat83, &lon83);
  navd88_to_ngvd29(lat83, lon83, elev88, elevUnitsIn, &ngvd29, elevUnitsOut);
  return ngvd29;
}

