package DWR.DMS.PTM;
/**
 * Defines a function to check if two floats are approximately equal.
 *
 * @author Nicky Sandhu
 * @version $Id: Macro.java,v 1.1.1.1 1999/09/28 23:43:13 miller Exp $
 */
class Macro {
  /**
   * true if x is approximately equal to y, tolerance = +/- 0.01
   */
public static boolean APPROX_EQ( float x , float y ) {
  return ((y-APPROX_EQ_TOL) <= (x)) && ((x) <= (y+APPROX_EQ_TOL));
}
  /**
   *
   */
private static float APPROX_EQ_TOL = 0.01f;
}
