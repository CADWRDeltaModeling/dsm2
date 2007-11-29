package DWR.DMS.PTM.behave;

public class Units {
  
  public static int TIME = 1;
  
  public static int VEL = 2;

  public static int MORT = 3;
  
  public static String time [] = {"sec","min","hour","day","week","year"};
  public static float timeConvert [] = {1.f, 60.f, 3600.f, 86400.f, 604800.f, 31556926.f};

  public static String velocity [] = {"ft/s","m/s","mm/s"};
  public static float velConvert [] = {1.f, 3.2808f, 0.0032808f};

  public static String mortality [] = {"dth/sec","dth/min","dth/hour","dth/day","dth/month","dth/year"};
  public static float mortalConvert [] = {1.f, 0.01667f, 2.7777e-4f, 1.157e-5f, 1.653e-6f, 3.169e-8f};
  
}
