/**
 *
 */
package DWR.DMS.PTM;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Calendar;
import java.util.Set;
import java.util.regex.*;
import java.nio.IntBuffer;
import java.lang.Math;
import java.util.Random;
import java.util.TimeZone;
import edu.cornell.RngPack.*;

/**
 * @author xwang
 *
 */
public class PTMUtil {
	static float EPSILON = 0.000000001f;
	static Random rand = new Random();
	//TODO temporary still use this because java.util.random doesn't work
	// this value will be reset in setRandomNumber()
	private static RandomElement randomNumberGenerator= new Ranecu(32001);
	//TODO changed the way the random numbers are called
	private static boolean _useNewRandomSeed = false;

	/**
	 *
	 */
	public PTMUtil() {
		// TODO Auto-generated constructor stub
	}
	public static BufferedReader getInputBuffer(String fileName){
        BufferedReader buffer = null;
        try{
            buffer = new BufferedReader(new InputStreamReader(new FileInputStream(fileName)));
        }
        catch(FileNotFoundException fe){
             fe.printStackTrace();
        }
        return buffer;
    }
	public static BufferedWriter getOutputBuffer(String fileName){
		BufferedWriter buffer = null;
        try{
            buffer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(fileName)));
        }
        catch(FileNotFoundException fe){
             fe.printStackTrace();
        }
        return buffer;
	}
	public static void closeBuffer(BufferedReader bf){
        try{
            bf.close();
        }
        catch(IOException e){
             e.printStackTrace();
        }
    }
	public static void closeBuffer(BufferedWriter bf){
        try{
            bf.close();
        }
        catch(IOException e){
             e.printStackTrace();
        }
    }
	public static ArrayList<String> getInputBlock(BufferedReader inputBuffer, String start, String end){
        ArrayList<String> blockList = new ArrayList<String>();
        try{
            String line;
            do{
                line = inputBuffer.readLine();
            } while(line != null && !line.trim().toUpperCase().startsWith(start));

            while((line=inputBuffer.readLine()) != null && !(line.trim().toUpperCase()).startsWith(end)){//(line = line.trim().toUpperCase()).startsWith(end)){
            	if (!line.startsWith("#"))
            		blockList.add(line.trim());
            }
        }
        catch(IOException e){
             e.printStackTrace();
        }
        if (blockList.size() == 0)
        		return null;
        return blockList;
    }
	public static ArrayList<String> getInputs(BufferedReader inputBuffer){
        ArrayList<String> blockList = new ArrayList<String>();
        try{
            String line;
            while((line=inputBuffer.readLine()) != null){
            	if (!line.startsWith("#"))
            		blockList.add(line.trim());
            }
        }
        catch(IOException e){
             e.printStackTrace();
        }
        if (blockList.size() == 0)
        		return null;
        return blockList;
    }
	public static ArrayList<String> getInputBlock(ArrayList<String> inputBlocks, String start, String end){
        ArrayList<String> block = null;
        Iterator<String> it;
        start = start.toUpperCase();
        end = end.toUpperCase();
        try{
            if (inputBlocks == null || (it = inputBlocks.iterator())==null || !it.hasNext())
            	return null;
            String line = null;
            do{
                line = it.next();
            } while(it.hasNext() && line != null && !line.trim().toUpperCase().startsWith(start));

            block = new ArrayList<String>();
            while(it.hasNext() && ((line= it.next()) != null) && !(line.trim()).toUpperCase().startsWith(end)){
                block.add(line);
            }
            if(block.size()!=0 && !(line.trim()).toUpperCase().startsWith(end))
            	PTMUtil.systemExit(end + " in the behavior input file is spelled wrong, please check, system exit.");
        }
        catch(Exception e){
             e.printStackTrace();
        }
        if (block.size()==0)
        	return null;
        return block;
    }
	public static void systemExit(String message){
		(new Exception(message)).printStackTrace();
		System.exit(-1);
	}
	public static Calendar getHecTime(TimeZone timeZone){
		Calendar hecTime0 = Calendar.getInstance(timeZone);
		hecTime0.clear();
		hecTime0.set(1899,11,30,23,0);;
		return hecTime0;

	}
	// convert model time (in minutes!!!) to calendar time
	public static Calendar modelTimeToCalendar(long currentTime, TimeZone timeZone){//convertHecTime(long currentTime){
		Calendar cur = Calendar.getInstance(timeZone);
		cur.clear();
		// current time is in minutes
		cur.setTimeInMillis(currentTime*60000+ getHecTime(timeZone).getTimeInMillis());
		return cur;
	}
	// convert calendar time to model time in minutes!!!
	public static long calendarToModelTime(Calendar time, TimeZone timeZone){ //convertCalendar(Calendar time){
		// PTM time is in minute
		return (time.getTimeInMillis() - getHecTime(timeZone).getTimeInMillis())/60000;
	}
	//careful to use this.  Set may not necessary preserve the order
	public static Set<Integer> readSet(ArrayList<String> inText){
		  if (inText == null)
			  return null;
		  Set<Integer> list = new HashSet<Integer>();
		  for (String line: inText){
			  String[] items = line.trim().split("[,\\s\\t]+");
			  for (String item: items){
				  try{
					  list.add(PTMHydroInput.getIntFromExtChan(Integer.parseInt(item)));
				  }catch(NumberFormatException e){
					  PTMUtil.systemExit("Channel numbers in Survival inputs has wrong format: "+item);
				  }
			  }
		  }
		  return list;
	  }
	public static int getIntFromString(String intStr){
		int number = -999999;
		try{
			number = Integer.parseInt(intStr);
		}catch (NumberFormatException e){
			e.printStackTrace();
			PTMUtil.systemExit("number format is wrong in the behavior input file! Should be an integer.");
		}
		return number;
	}

	// only work with format name: number
	public static int getInt(String numberLine){
		int number = -999999;
		try{
			String[] items = numberLine.split("[,:\\s\\t]+");
			number = Integer.parseInt(items[1]);
		}catch (NumberFormatException e){
			e.printStackTrace();
			PTMUtil.systemExit("number format is wrong in the behavior input file! Should be an integer.");
		}
		return number;
	}
	// get a Int from a line with format name: double
	public static int getIntFromLine(String line, String lineName) throws NumberFormatException{
		String[] items = line.split("[,:\\s\\t]+");
		if (items.length < 2 || (!items[0].equalsIgnoreCase(lineName)))
			PTMUtil.systemExit("the input line (" + line +") is not correct! system exit");
		return Integer.parseInt(items[1]);
	}
	// get a double from a line with format name: double
	public static double getDoubleFromLine(String line, String lineName) throws NumberFormatException{
		String[] items = line.split("[,:\\s\\t]+");
		if (items.length < 2 || (!items[0].equalsIgnoreCase(lineName)))
			PTMUtil.systemExit("the input line (" + line +") is not correct! system exit");
		return Double.parseDouble(items[1]);
	}
	// get a double from a line with format name: double
	public static float getFloatFromLine(String line, String lineName) throws NumberFormatException{
		String[] items = line.split("[,:\\s\\t]+");
		if (items.length < 2 || (!items[0].equalsIgnoreCase(lineName)))
			PTMUtil.systemExit("the input line (" + line +") is not correct! system exit");
		return Float.parseFloat(items[1]);
	}
	// get a String from a line with format name: String
	public static String getStringFromLine(String line, String lineName){
		String[] items = line.split("[,:\\s\\t]+");
		if (items.length < 2 || (!items[0].equalsIgnoreCase(lineName)))
			PTMUtil.systemExit("the input line (" + line +") is not correct! system exit");
		return items[1];
	}
	// get a path name from a line with format name: String
	public static String getPathFromLine(String line, char divider){
		if(!line.toUpperCase().contains("PATH"))
			systemExit("Wrong path name: " + line);
		return (line.substring(line.indexOf(divider)+1)).trim();
		//fixed the problem that the path cannot have ":"
		/*
		String[] items = line.split("[,\\s\\t]+");
		if (items.length < 2 || (!items[0].equalsIgnoreCase(lineName)))
			PTMUtil.systemExit("the input line (" + line +") is not correct! system exit");
		return items[1];
		*/
	}
	public static String getNameFromLine(String line, char divider){
		return (line.substring(line.indexOf(divider)+1)).trim();
	}
	public static Pair<Integer, Integer> getPairFromLine(String line, String lineName) throws NumberFormatException{
		String[] items = line.split("[,:\\s\\t]+");
		if (items.length < 3 || (!items[0].equalsIgnoreCase(lineName)))
			PTMUtil.systemExit("the input line (" + line +") is not correct! system exit");
		return new Pair<Integer, Integer> (Integer.parseInt(items[1]), Integer.parseInt(items[2]));
	}

	public static ArrayList<String[]> getStringPairsFromLine(String line, String lineName){
		String[] items = line.split(":");
		if (items.length != 2 || (!items[0].equalsIgnoreCase(lineName))||items[1].contains("."))
			PTMUtil.systemExit("the input line (" + line +") is not correct! system exit");
		Pattern p = Pattern.compile("\\((\\s*\\t*\\w+\\d*\\s*\\t*),(\\s*\\t*-*\\d+\\s*\\t*)\\)");
		Matcher m = p.matcher(items[1]);
		ArrayList<String[]> pairs = new ArrayList<String[]>();
		while(m.find()){
			String[] strPair = {m.group(1).trim(), m.group(2).trim()};
			pairs.add(strPair);
		}
		if (pairs.size()<1)
			PTMUtil.systemExit("the look up line is empty, exit.");
		return pairs;
	}
	public static Map<Integer,String> getIntStrPairsFromLine(String line, String lineName) throws NumberFormatException{
		ArrayList<String[]> lookUpStrs = getStringPairsFromLine(line, lineName);
		Map<Integer, String> lookUpMap = new HashMap<Integer, String>();
		try{
			for (String[] pair: lookUpStrs){
				if (pair.length<2)
					throw new NumberFormatException("pair length < 2:"+pair);
				int in = Integer.parseInt(pair[0].trim());
				lookUpMap.put(in, pair[1]);
			}
		}catch (NumberFormatException e){
			e.printStackTrace();
			PTMUtil.systemExit("number format is wrong in the input file! Should be integers.  The line input:" + line);
		}
		return lookUpMap;
	}
	public static Map<String, Integer> getStrIntPairsFromLine(String line, String lineName) throws NumberFormatException{
		ArrayList<String[]> lookUpStrs = getStringPairsFromLine(line, lineName);
		Map<String, Integer> lookUpMap = new HashMap<String, Integer>();
		try{
			for (String[] pair: lookUpStrs){
				if (pair.length<2)
					throw new NumberFormatException("pair length < 2:"+pair);
				int value = Integer.parseInt(pair[1].trim());
				lookUpMap.put(pair[0], value);
			}
		}catch (NumberFormatException e){
			e.printStackTrace();
			PTMUtil.systemExit("number format is wrong in the input file! Should be integers.  The line input:" + line);
		}
		return lookUpMap;
	}
	public static ArrayList<int[]> getIntPairsFromLine(String line, String lineName) throws NumberFormatException{
		ArrayList<String[]> strs = getStringPairsFromLine(line, lineName);
		ArrayList<int[]> pairs = new ArrayList<int[]>();
		try{
			for (String[] pair: strs){
				if (pair.length<2)
					throw new NumberFormatException("pair length < 2:"+pair);
				int[] intPair = {Integer.parseInt(pair[0].trim()), Integer.parseInt(pair[1].trim())};
				pairs.add(intPair);
			}

		}catch (NumberFormatException e){
			e.printStackTrace();
			PTMUtil.systemExit("number format is wrong in the input file! Should be integers. The line input:" + line);
		}
		return pairs;
	}
	public static ArrayList<IntBuffer> getIntBuffersFromLine(String line, String lineName) throws NumberFormatException{
		String[] items = line.split(":");
		if (items.length != 2 || (!items[0].equalsIgnoreCase(lineName))||items[1].contains("."))
			PTMUtil.systemExit("the input line (" + line +") is not correct! system exit");
		Pattern p = Pattern.compile("\\((\\s*\\t*\\d+\\s*\\t*),(\\s*\\t*\\d+\\s*\\t*)\\)");
		Matcher m = p.matcher(items[1]);
		ArrayList<IntBuffer> pairs = new ArrayList<IntBuffer>();
		try{
			while(m.find()){
				int[] intPair = {Integer.parseInt(m.group(1).trim()), Integer.parseInt(m.group(2).trim())};
				pairs.add(IntBuffer.wrap(intPair));
			}
			if (pairs.size()<1)
				throw new NumberFormatException("no integer pairs found, check the input file!");
		}catch (NumberFormatException e){
			e.printStackTrace();
			PTMUtil.systemExit("number format is wrong in the input file! Should be integers.");
		}
		return pairs;
	}
	public static Pair<ArrayList<Integer>, ArrayList<Integer>> getGroupPair(String numberline){
		if (numberline == null)
			PTMUtil.systemExit("the input line (" + numberline +") is not correct! system exit");
		Pattern p_name = Pattern.compile("([a-zA-Z_]+\\s*\\t*[a-zA-Z_]*)");
		Matcher m_name = p_name.matcher(numberline);
		String name = null;
		if (m_name.find())
			name = m_name.group(1).trim();
		else
			PTMUtil.systemExit("the input line (" + numberline +") is not correct! system exit");
		Pattern p_number = Pattern.compile("\\((.*)\\)\\s*\\t*\\((.*)\\)");
		Matcher m_number = p_number.matcher(numberline);
		String g1Str=null, g2Str=null;
		if(m_number.find()){
			g1Str = m_number.group(1).trim();
			g2Str = m_number.group(2).trim();
		}
		else
			PTMUtil.systemExit("the input line (" + numberline +") is not correct! system exit");
		String[] g1 = g1Str.split(","), g2 = g2Str.split(",");
		ArrayList<Integer> g1Ints = new ArrayList<Integer>(), g2Ints= new ArrayList<Integer>();
		try{
			for(String s: g1)
				g1Ints.add(PTMHydroInput.getIntFromExtChan(Integer.parseInt(s.trim())));
			for(String s: g2)
				g2Ints.add(PTMHydroInput.getIntFromExtChan(Integer.parseInt(s.trim())));
		}catch (NumberFormatException e){
			e.printStackTrace();
			PTMUtil.systemExit("number format is wrong in the input file! Should be integers.");
		}
		return new Pair<ArrayList<Integer>, ArrayList<Integer>>(name, g1Ints, g2Ints);
	}
	// get a boolean from a line with format name: double
	public static boolean getBooleanFromLine(String line, String lineName){
		String[] items = line.split("[,:\\s\\t]+");
		if (items.length < 2 || (!items[0].equalsIgnoreCase(lineName))
				|| (!items[1].equalsIgnoreCase("TRUE") && !items[1].equalsIgnoreCase("FALSE")))
			PTMUtil.systemExit("the input line (" + line +") is not correct! system exit");
		if (items[1].equalsIgnoreCase("FALSE"))
			return false;
		return true;
	}

	public static ArrayList<Integer> getInts(String numberLine){
		ArrayList<Integer> ints = new ArrayList<Integer>();
		try{
			String[] items = numberLine.split("[,:\\s\\t]+");
			for (String item: items)
				ints.add(Integer.parseInt(item));
		}catch (NumberFormatException e){
			e.printStackTrace();
			PTMUtil.systemExit("expect integers but get:"+numberLine);
		}
		return ints;
	}
	public static ArrayList<Double> getDoubles(String numberLine){
		ArrayList<Double> dbls = new ArrayList<Double>();
		try{
			String[] items = numberLine.split("[,:\\s\\t]+");
			for (String item: items)
				dbls.add(Double.parseDouble(item));
		}catch (NumberFormatException e){
			e.printStackTrace();
			PTMUtil.systemExit("expect Doubles but get:"+numberLine);
		}
		return dbls;
	}
	public static ArrayList<String> getStrings(String stringLine) {
		ArrayList<String> strs = new ArrayList<String>();
		String[] items = stringLine.split("[,:\\s\\t]+");
		for(String item: items) {
			strs.add(item);
		}
		return strs;		
	}
	public static boolean check(String[] listToCheck, String[] standards){
		int length = listToCheck.length;
		if (length != standards.length)
			return false;
		for (int i = 0; i < length; i++){
			if (!listToCheck[i].equalsIgnoreCase(standards[i]))
				return false;
		}
		return true;
	}
	public static Calendar getDateTime(String date, String time, TimeZone timeZone) throws NumberFormatException{
		Calendar dateTime = null;
		String[] dateStr = date.trim().split("[-/]+"), timeStr = time.trim().split("[:]+");
		int year = -99, month = -99, day = -99, hour = -99, minute = -99;
		if (dateStr.length<3 || timeStr.length<2)
			throw new NumberFormatException();
		year = Integer.parseInt(dateStr[2]);
		// java month start from 0
		month = Integer.parseInt(dateStr[0])-1;
		day = Integer.parseInt(dateStr[1]);
		hour = Integer.parseInt(timeStr[0]);
		minute = Integer.parseInt(timeStr[1]);
		//if(DEBUG) System.out.println("year:"+year+" month:"+month+" day:"+day+" hour:"+hour+" minute:"+minute);
		dateTime = Calendar.getInstance(timeZone);
		dateTime.clear();
		dateTime.set(year, month, day, hour, minute);
		return dateTime;
	}
	public static void checkTitle(String inTitle, String[] titleShouldBe){
		String [] title = inTitle.trim().split("[,\\s\\t]+");
		if (!PTMUtil.check(title, titleShouldBe))
			PTMUtil.systemExit("SYSTEM EXIT while reading Input info: Title line is wrong:"+inTitle);
	}
	public static boolean floatNearlyEqual(float f1, float f2){
		return f1 == f2 ? true: Math.abs(f1-f2) < EPSILON*Math.min(Math.abs(f1),Math.abs(f2));
	}
	public static boolean doubleNearlyEqual(double d1, double d2){
		return d1 == d2 ? true: Math.abs(d1-d2) < EPSILON*Math.min(Math.abs(d1),Math.abs(d2));
	}
	public static double getRandomNumber(){
		return randomNumberGenerator.uniform(0, 1);
	}
	public static double getNextGaussian(){
		return randomNumberGenerator.gaussian();
	}
	//TODO this version of Java has a bug that the random number generator crashes VM
	// will change to Java random number generator when upgrade to new jre.
	/*
	public static double getRandomNumber(){
		return Math.random();
	}
	public static double getNextGaussian(){
		return rand.nextGaussian();
	}
	*/
	public static void setRandomNumber(){
		randomNumberGenerator = new Ranecu(System.currentTimeMillis());
		//TODO changed the way the random numbers are called
		_useNewRandomSeed = true;
	}
	public static boolean getUseNewRandomSeed(){return _useNewRandomSeed;}
	public static int[] getEnvNodeChanIds(String[] items){
		if (items.length<2)
			PTMUtil.systemExit("SYSTEM EXIT: Barrier or fish screen ID input line should have more than 3 items, and less items noticed. ");
		Integer wbId=null;
		Integer nodeId = getEnvNodeId(items[0]);
		// find external channel id or reservoir/obj2obj name
		try{
			wbId = Integer.parseInt(items[1]);
			wbId = PTMHydroInput.getIntFromExtChan(wbId);
		}catch(NumberFormatException e){
			if ((wbId=PTMEnv.getReservoirObj2ObjEnvId(items[1])) == null){
				e.printStackTrace();
				PTMUtil.systemExit("SYSTEM EXIT:wrong channel/reservior/obj2obj id:" + items[1]);
			}
		}
		if (nodeId == null || wbId == null)
			PTMUtil.systemExit("SYSTEM EXIT: wrong node/channel/reservior/obj2obj ids, node id: " + items[0]+" waterbody Id: "+items[1]);
		return new int[] {nodeId, wbId};
	}
	public static Integer getEnvNodeId(String idStr){
		Integer nodeId = null;
		if (idStr == null)
			PTMUtil.systemExit("SYSTEM EXIT: try to get internal Id for Node but the string is empty. ");
		try{
			nodeId = Integer.parseInt(idStr);
		}catch(NumberFormatException e){
				e.printStackTrace();
				PTMUtil.systemExit("SYSTEM EXIT: node id has a wrong format:" + idStr);
		}
		nodeId = PTMHydroInput.getIntFromExtNode(nodeId);
		return nodeId;
	}
}
