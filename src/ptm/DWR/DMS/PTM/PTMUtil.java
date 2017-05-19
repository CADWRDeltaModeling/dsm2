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
import java.util.Iterator;
import java.util.Calendar;
import java.util.Set;
import java.util.regex.*;
import java.nio.IntBuffer;
import java.lang.Math;
import java.util.Random;
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

	/**
	 * 
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
	public static Calendar getHecTime(){
		Calendar hecTime0 = Calendar.getInstance();
		hecTime0.clear();
		hecTime0.set(1899,11,30,23,0);;
		return hecTime0;
		
	}
	// convert model time (in minutes!!!) to calendar time
	public static Calendar modelTimeToCalendar(long currentTime){//convertHecTime(long currentTime){
		Calendar cur = Calendar.getInstance();
		cur.clear();
		// current time is in minutes
		cur.setTimeInMillis(currentTime*60000+ getHecTime().getTimeInMillis());
		return cur;
	}
	// convert calendar time to model time in minutes!!!
	public static long calendarToModelTime(Calendar time){ //convertCalendar(Calendar time){
		// PTM time is in minute
		return (time.getTimeInMillis() - getHecTime().getTimeInMillis())/60000;
	}
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
	public static String getPathFromLine(String line, String lineName){
		String[] items = line.split("[,\\s\\t]+");
		if (items.length < 2 || (!items[0].equalsIgnoreCase(lineName)))
			PTMUtil.systemExit("the input line (" + line +") is not correct! system exit");
		return items[1];
	}
	public static Pair<Integer, Integer> getPairFromLine(String line, String lineName) throws NumberFormatException{
		String[] items = line.split("[,:\\s\\t]+");
		if (items.length < 3 || (!items[0].equalsIgnoreCase(lineName)))
			PTMUtil.systemExit("the input line (" + line +") is not correct! system exit");
		return new Pair<Integer, Integer> (Integer.parseInt(items[1]), Integer.parseInt(items[2]));
	}
	public static ArrayList<int[]> getIntPairsFromLine(String line, String lineName) throws NumberFormatException{
		String[] items = line.split(":");
		if (items.length != 2 || (!items[0].equalsIgnoreCase(lineName))||items[1].contains("."))
			PTMUtil.systemExit("the input line (" + line +") is not correct! system exit");
		Pattern p = Pattern.compile("\\((\\s*\\t*\\d+\\s*\\t*),(\\s*\\t*\\d+\\s*\\t*)\\)");
		Matcher m = p.matcher(items[1]);
		ArrayList<int[]> pairs = new ArrayList<int[]>();
		try{
			while(m.find()){
				int[] intPair = {Integer.parseInt(m.group(1).trim()), Integer.parseInt(m.group(2).trim())};
				pairs.add(intPair);
			}
			if (pairs.size()<1)
				throw new NumberFormatException("no integer pairs found, check the input file!");
		}catch (NumberFormatException e){
			e.printStackTrace();
			PTMUtil.systemExit("number format is wrong in the input file! Should be integers.");	
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
	public static Calendar getDateTime(String date, String time) throws NumberFormatException{
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
				  
		dateTime = Calendar.getInstance();
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
	} 
}
