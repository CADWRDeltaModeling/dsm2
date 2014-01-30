/**
 * 
 */
package DWR.DMS.PTM;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Calendar;
import java.util.Set;

/**
 * @author xwang
 *
 */
public class PTMUtil {

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
	public static void closeBuffer(BufferedReader bf){
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
	public static ArrayList<String> getInputBlock(ArrayList<String> inputBlocks, String start, String end){
        ArrayList<String> block = null;
        Iterator<String> it;
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
        }
        catch(Exception e){
             e.printStackTrace();
        }
        return block;
    }
	public static void systemExit(String message){
		System.err.println(message);
		System.exit(-1);
	}
	public static Calendar convertHecTime(int currentTime){
		Calendar cur = Calendar.getInstance();
		cur.clear();
		Calendar hecTime0 = Calendar.getInstance();
		hecTime0.clear();
		hecTime0.set(1900,0,0,0,0);
		// current time is in minutes
		cur.setTimeInMillis((long)currentTime*60000+hecTime0.getTimeInMillis());
		return cur;
	}
	public static String concatNodeWbIds(int nodeId, int wbId){	
		return (Integer.toString(nodeId)+"_"+Integer.toString(wbId));
	}
	public static int[] getIntsFromString(String text){
		int[] ints = null;
		try{
			String [] intsStr = text.split("_");
			int idNum = intsStr.length;
			ints = new int[idNum];
			for (int i = 0; i < idNum; i++)
				ints[i] = Integer.parseInt(intsStr[i]);
		}catch (NumberFormatException e){
			e.printStackTrace();
		}
		return ints;
	}
	public static Set<Integer> readSet(ArrayList<String> inText){
		  if (inText == null)
			  return null;
		  Set<Integer> list = new HashSet<Integer>();
		  for (int i = 0; i<inText.size();i++){
			  String[] items = inText.get(i).trim().split("[,\\s\\t]+");
			  for (int j= 0; j<items.length;j++){
				  try{
					  list.add(PTMHydroInput.getIntFromExtChan(Integer.parseInt(items[j])));
				  }catch(NumberFormatException e){
					  PTMUtil.systemExit("Channel numbers in Survival inputs has wrong format: "+items[j]);
				  }
			  }
		  }
		  return list;
	  }
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
	public static double getDouble(String numberLine){
		double number = -999999.0;
		try{
			String[] items = numberLine.split("[,:\\s\\t]+");
			number = Double.parseDouble(items[1]);
		}catch (NumberFormatException e){
			e.printStackTrace();
			PTMUtil.systemExit("number format is wrong in the behavior input file! Should be a double");	
		}
		return number;
	}
	//TODO cleanup
	/*
	public static ArrayList<String> cleanUp(ArrayList<String> inList){
		Iterator<String> it = null;
		ArrayList<String> cleanList = new ArrayList<String>();
		String line = null;
		if (inList == null || (it = inList.iterator()) == null)
			return null;
		else{
			while (it.hasNext()){
				if (!(line = it.next().trim().toUpperCase()).startsWith("#"))
					cleanList.add(line);
			}
		}
		return cleanList;
	}
	*/
}
