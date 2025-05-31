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
  public static final int NETCDF = 3;
  
  /**
   * returns current model data as a string
   */
  
  public static String getModelDate(int currentModelTime) {
	  int julianDate;
	  String dateStr;
	  	  
	  // The current convention is for midnight to be assigned to the previous day
	  // => subtract one minute to shift midnight to the previous day
	  julianDate = Math.floorDiv(currentModelTime-1, 24*60);
	  
	  dateStr = PTMUtil.juldat(julianDate);
	  
	  return dateStr;
  }
  
  /**
   * returns current model time as string
   */
  
  public static String getModelTime(int modelTime) {
	  int timeTotMinutes, hour, minute;
	  String timeStr;
	  
      timeTotMinutes = modelTime%(24*60);
      hour = Math.floorDiv(timeTotMinutes, 60);
      minute = timeTotMinutes%60;
      
      timeStr = String.format("%02d%02d", hour, minute);
      if(timeStr.equals("0000")) {timeStr = "2400";}
      
      return timeStr; 
  }
  /**
   * model time zone
   */
  public static TimeZone TIME_ZONE;
  
  public static int getTimeInJulianMins(String modelDate, String modelTime) {
	  int julian;
	  int minutes;
	  
	  julian = PTMUtil.datjul(modelDate);
	  
	  minutes = Integer.parseInt(modelTime.substring(0, 2))*60 + Integer.parseInt(modelTime.substring(2, 4));
	  
	  return julian*24*60 + minutes;
  }
  public static boolean DisplaySimulationTimestep = true;
  public static boolean CalculateWritePTMFlux = true;
  
}
