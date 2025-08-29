package DWR.DMS.PTM;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;

/**
 * Read transition probabilities for Head of Old River and Turner Cut from a CSV file.
 *
 * @author Doug Jackson, QEDA Consulting, LLC
 */
public class TransProbs {

	private static String inputFilePath;
	private static BufferedReader bReader;
	private static FileReader fReader;

	// Numbers of transition types and stations
	private static int numTrans;
	private static int numStations;

	private static int chunkIndex;
	private static ZonedDateTime firstDatetime;

	// Number of time steps in minutes
	private static int timeStep_min;

	// Flag to indicate whether there's currently a file open
	private static boolean fileOpen;

	private static Map<String, Double> cache;

	static {
		numTrans = 6;
		numStations = 2;

		timeStep_min = 15;

		chunkIndex = 0;

		fileOpen = false;
	}

	/**
	 * Open the CSV file, read the header line, and read the datetime from the first chunk
	 * @param inputFilePath		the full path to the directory containing the CSV file
	 * @param filename		the name of the CSV file
	 * @throws IOException
	 */
	public static void openFile(String inputFilePath) {
		File file;
		String path;
		String line;

		TransProbs.inputFilePath = inputFilePath;

		file = new File(inputFilePath);
		path = file.getPath();
		bReader = null;
		fReader = null;
		try {
			fReader = new FileReader(path);
			bReader = new BufferedReader(fReader);
			// Read the header line
			line = bReader.readLine();

			readFirstDatetime();

			fileOpen = true;

		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Read a single line from the CSV file
	 * @return				a String containing the line
	 */
	public static String readLine() {
		String line = null;
		try {
			line = bReader.readLine();
			if(line==null) {
				PTMUtil.systemExit("Could not find current datetime in South Delta transition probabilities file. Exiting.");
			}

		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return line;
	}

	/**
	 * Read the datetime from the first chunk of transition probabilities
	 */
	public static void readFirstDatetime() {

		Map<String, Double> firstTimeStep;
		Map.Entry<String, Double> entry;
		String firstIndex, firstDatetimeStr;
		String[] fields;

		firstTimeStep = readTimeStep();

		entry = firstTimeStep.entrySet().iterator().next();
		firstIndex = entry.getKey();

		fields = firstIndex.split("_");
		firstDatetimeStr = fields[1];

		firstDatetime = ZonedDateTime.parse(firstDatetimeStr + "(UTC-08:00)",
				DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss(VV)"));
	}

	/**
	 * Read transition probabitilies for a specified datetime (must be later than the current chunk)
	 * @param datetime		a ZonedDateTime containing the datetime to read
	 * @return				a HashMap containing probabilities for all junctions and transitions in the specified datetime
	 */
	public static HashMap<String, Double> readTransProbs(ZonedDateTime datetime) {
		int timeSteps, numChunks;

		// Calculate the number of time steps between datetime and firstDatetime
		// Round down to the nearest time step
		timeSteps = (int) Math.floor(ChronoUnit.MINUTES.between(firstDatetime, datetime)/timeStep_min);

		numChunks = timeSteps - chunkIndex;

		// Return the cache if we've already read the data for this datetime
		if (numChunks==-1) {
			return (HashMap<String, Double>) cache;
		}

		// Burn intermediate time steps
		for (int i=0; i<numChunks; i++) {
			burnTimeStep();
		}

		return readTimeStep();

	}

	/**
	 * Read transition probabilities for the next available time step
	 * @return				a HashMap containing probabilities for all junctions and transitions in the current chunk
	 */
	public static HashMap<String, Double> readTimeStep() {
		int numTransProbs;
		String line;
		String[] fields;

		String junction;
		String datetime;
		String transition;
		String transProbKey;
		double transProb;

		HashMap<String, Double> transProbs = new HashMap<String, Double>();

		numTransProbs = numTrans*numStations;

		for (int i=0; i<numTransProbs; i++) {
			line = readLine();
			fields = line.split(",");

			junction = fields[0];
			datetime = fields[1];
			transition = fields[2];
			transProbKey = junction + "_" + datetime + "_" + transition;
			transProb = Double.parseDouble(fields[3]);
			transProbs.put(transProbKey, transProb);
		}

		chunkIndex++;

		// Save in cache in case we read this datetime again
		cache = transProbs;

		return transProbs;
	}

	/**
	 * Read the current chunk without saving (to quickly step through a file)
	 */
	public static void burnTimeStep() {
		int numTransProbs;
		String scratch;

		numTransProbs = numTrans*numStations;

		for (int i=0; i<numTransProbs; i++) {
			scratch = readLine();
		}

		chunkIndex++;
	}

	/**
	 * Obtain flag indicating whether a file is currently open
	 * @return				fileOpen flag
	 */
	public static boolean getFileOpen() {
		return fileOpen;
	}

	/**
	 * Close the file at the end of a model run
	 */
	public static void closeFile() {
		try {
			fReader.close();
			bReader.close();
		} catch(IOException e) {
			e.printStackTrace();
		}
		fileOpen = false;
	}

}
