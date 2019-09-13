/*<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public !<license as published by
C!    the Free Software Foundation, either version 3 of the !<license, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public !<license for more details.

C!    You should have received a copy of the GNU General Public !<license
C!    along with DSM2.  If not, see <http://www.gnu.org/!<licenses/>.
</license>*/
package DWR.DMS.PTM;
import java.util.TimeZone;
/**
 * Provides access to variables that are global to all
 * call classes of this package.
 *
 * @author Nicky Sandhu
 * @version $Id: Globals.java,v 1.3 2000/08/07 17:00:27 miller Exp $
 */
public class Globals{
  /**
   * initializes by loading library
   */
  public static void initialize(){
    System.loadLibrary("PTM");
  }
  /**
   * current model time
   */
  public static int currentModelTime;
  /**
   * current model time in hours
   */
  public static int currentMilitaryTime;
  /**
   * the environment containing all the information in this model
   */
  public static PTMEnv Environment;
  /**
   * ascii
   */
  public static final int ASCII = 1;
  /**
   * binary constant
   */
  public static final int BINARY = 2;
  /**
   * returns current model data as a string
   */
  public static native String getModelDate(int currentModelTime);
  /**
   * returns current model time as string
   */
  public static native String getModelTime(int currentModelTime);
  /**
   * model time zone
   */
  public static TimeZone TIME_ZONE;
  /**
   * returns time in julian minutes since base date using strings
   * of the model date and time in the format ddMMMyyyy, HHmm respectively
   */
  public static native int getTimeInJulianMins(String modelDate, String modelTime);
  public static boolean DisplaySimulationTimestep = true;
}
