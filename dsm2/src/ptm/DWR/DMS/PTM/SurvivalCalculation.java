/**
 *
 */
package DWR.DMS.PTM;

import java.util.AbstractMap.SimpleEntry;
import java.io.BufferedWriter;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.Value;

import ucar.ma2.ArrayChar;
import ucar.ma2.ArrayFloat;
import ucar.ma2.ArrayInt;
import ucar.ma2.DataType;
import ucar.ma2.Index;
import ucar.ma2.InvalidRangeException;
import ucar.nc2.Attribute;
import ucar.nc2.Dimension;
import ucar.nc2.Variable;
import ucar.nc2.write.NetcdfFormatWriter;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author Doug Jackson, QEDA Consulting, LLC
 * Calculate route-specific survival based on equations defined in behavior configuration file.
 */
public class SurvivalCalculation {

	static boolean writeRouteSurvival, writeFates, writeSurvDetail, showRouteSurvivalDetail;
	public static final String WILDCARD = "99999";
	static final int MISSING=-999;

	private Particle [] particleArray;
	private Map<String, Float> reachSurvMap;
	private List<String> survGroups;
	private Map<String, Float> surv;

	private Map<Integer, SimpleEntry<String, Long>> lastStationDatetimes;
	private Map<Integer, String> fates;
	
	// HashMap to store the survival calculation details
	public Map<String, Map<String, String>> survDetails;
	
	static {
		writeRouteSurvival = false;
		writeFates = false;
		writeSurvDetail = false;
		
		// Enable/disable printing of route survival calculation details for testing
		showRouteSurvivalDetail = false;
	}

	public SurvivalCalculation(Particle [] particleArray) {
		this.particleArray = particleArray;

		reachSurvMap = Globals.Environment.getBehaviorInputs().getSurvivalInputs().getReachSurvMap();
		
		survDetails = new HashMap<>();
		survDetails.put("survival", new HashMap<>());
		survDetails.put("routingFraction", new HashMap<>());
	}

	/**
	 * Evaluate the survival equations using GraalJS engine
	 * @param  
	 */
	public void run() {
		String thisSurvEq;
		Object thisSurv;
		Map<String, String> survEqs, dependentSurvEqs;
		Pattern p;
		Matcher m;
		Value thisSurvCalc;

		survEqs = Globals.Environment.getBehaviorInputs().getSurvivalInputs().getSurvEqs();

		if(survEqs==null) {return;}

		survGroups = new ArrayList<>();
		surv = new HashMap<>();

		// Build GraalJS engine
		Engine engine1 = Engine.newBuilder()
				.option("engine.WarnInterpreterOnly", "false")
				.build();      

		Context ctx = Context.newBuilder("js").engine(engine1).allowAllAccess(true).build();
		ctx.getBindings("js").putMember("survCalc", new SurvivalCalculation(particleArray));
		ctx.eval("js", "var StringClass = Java.type('java.lang.String[]');");

		// Gather names of survival groups
		for(String key: survEqs.keySet()) {
			survGroups.add(key);
		}

		// Identify equations that are a function of another survival group
		dependentSurvEqs = new HashMap<>();
		for(String key: survEqs.keySet()) {
			thisSurvEq = survEqs.get(key);

			for(String survGroup : survGroups) {
				p = Pattern.compile("S\\(" + survGroup + "\\)");
				m = p.matcher(thisSurvEq);
				while(m.find()) {
					dependentSurvEqs.put(key, thisSurvEq);
				}
			}
		}

		// Remove dependent equations from survEqs
		for(String key : dependentSurvEqs.keySet()) {
			survEqs.remove(key);
		}

		if(PTMFixedData.getConfig().show_route_survival_detail) {
			// Create and display translated equations
			for (String key: survEqs.keySet()) {
				System.out.println("===================================================================");
				System.out.println("Survival equations for " + key);        	
				thisSurvEq = survEqs.get(key);
				// Replace # wildcards in equation with WILDCARD, which is an allowable component of a variable name
				thisSurvEq = thisSurvEq.replaceAll("#", WILDCARD);
				thisSurvEq = translateEquation(thisSurvEq);
				System.out.println("translated equation: " + thisSurvEq);
			}
		}

		// Calculate survivals
		System.out.println("===================================================================");
		for (String key: survEqs.keySet()) {
			thisSurvEq = survEqs.get(key);
			// Replace # wildcards in equation with WILDCARD, which is an allowable component of a variable name
			thisSurvEq = thisSurvEq.replaceAll("#", WILDCARD);
			thisSurvEq = translateEquation(thisSurvEq);
			thisSurv = ctx.eval("js", thisSurvEq);
			
			System.out.println("Survival output " + key + ": " + thisSurv);
			System.out.println("===================================================================");

			surv.put(key, Float.parseFloat(thisSurv.toString()));
			
			// Store survival output details
			thisSurvCalc = ctx.getBindings("js").getMember("survCalc");
			storeSurvDetails(thisSurvCalc);
		}

		// Calculate dependent survivals after inserting the appropriate survival values calculated above
		System.out.println("===================================================================");
		for(String key: dependentSurvEqs.keySet()) {
			thisSurvEq = dependentSurvEqs.get(key);

			for(String survGroup : survEqs.keySet()) {
				thisSurvEq = thisSurvEq.replaceAll("S\\(" + survGroup + "\\)", surv.get(survGroup).toString());        		
			}
			dependentSurvEqs.put(key, thisSurvEq);

			// Replace # wildcards in equation with WILDCARD, which is an allowable component of a variable name
			thisSurvEq = thisSurvEq.replaceAll("#", WILDCARD);
			thisSurvEq = translateEquation(thisSurvEq);
			thisSurv = ctx.eval("js", thisSurvEq);
			System.out.println("Survival output " + key + ": " + thisSurv);
			System.out.println("===================================================================");

			surv.put(key, Float.parseFloat(thisSurv.toString()));
			
			// Store survival output details
			thisSurvCalc = ctx.getBindings("js").getMember("survCalc");
			storeSurvDetails(thisSurvCalc);
		}

		// Categorize final fates (passed Chipps, died, lost or entrained in pumps, etc.)
		categorizeFates();
		System.out.println("===================================================================");
	}
	
	/**
	 * Store survival details in survDetails HashMap
	 * @param survCalc				GraalVM Polyglot Value containing SurvivalCalculation object
	 */
	public void storeSurvDetails(Value survCalc) {
		Value thisSurvDetails, thisSurvival, thisRoutingFraction, hashKeysIterator, thisVal;
		
		thisSurvDetails = survCalc.getMember("survDetails");
		thisSurvival = thisSurvDetails.getHashValue("survival");
		thisRoutingFraction = thisSurvDetails.getHashValue("routingFraction");
		
		hashKeysIterator = thisSurvival.getHashKeysIterator();
		while(hashKeysIterator.hasIteratorNextElement()) {
			thisVal = hashKeysIterator.getIteratorNextElement();
			survDetails.get("survival").put(thisVal.toString(), thisSurvival.getHashValue(thisVal).asString());
		}
		hashKeysIterator = thisRoutingFraction.getHashKeysIterator();
		while(hashKeysIterator.hasIteratorNextElement()) {
			thisVal = hashKeysIterator.getIteratorNextElement();
			survDetails.get("routingFraction").put(thisVal.toString(), thisRoutingFraction.getHashValue(thisVal).asString());
		}
	}

	/**
	 * Calculate survival fraction from fromStation to toStation
	 * @param fromStation			String containing "from" station name, e.g., MOS
	 * @param toStations			String containing "to" station name, e.g., SJL
	 * @return						survival fraction
	 */
	public double calcSurv(String fromStation, String[] toStations) {

		double surv=0;
		List<SimpleEntry<String, Long>> thisArrivalDatetimes;
		SimpleEntry<Integer, Long> thisDeathDatetime;
		int passedStartCount, survivalCount, arrivalCount;
		Particle p;
		boolean particleDied;
		SimpleEntry<String, Long> thisArrivalDatetime;
		String reachIndex, survDetailsKey;

		// Check to see if there's a predefined survival probability for this combination of fromStation and toStations
		reachIndex = fromStation + "_" + toStations[0];
		for(int i=1; i<toStations.length; i++) {
			reachIndex+="." + toStations[i];
		}
		if(reachSurvMap.containsKey(reachIndex)) {
			surv = reachSurvMap.get(reachIndex);

			if(PTMFixedData.getConfig().show_route_survival_detail) {
				System.out.println("Survival for " + reachIndex + ": surv = " + surv);
			}

			return surv;
		}
		
		survDetailsKey = "S(" +  reachIndex + ")";

		passedStartCount = 0;
		survivalCount = 0;
		arrivalCount = 0;

		// Loop over vFish
		for(int i=0; i<particleArray.length; i++) {

			p = particleArray[i];
			thisArrivalDatetimes = p.getArrivalDatetimes();
			thisDeathDatetime = p.getDeathDatetime();

			if(thisDeathDatetime!=null) {particleDied = true;}
			else {particleDied = false;}

			// Loop over the vFish's arrival history
			for(int j=0; j<thisArrivalDatetimes.size()-1; j++) {		
				thisArrivalDatetime = thisArrivalDatetimes.get(j);
			
				// If the vFish passed the start station, check to see if it survived to each of the end stations
				if(checkEqual(fromStation, thisArrivalDatetime.getKey())) {
					passedStartCount++;

					for(int k=0; k<toStations.length; k++) {

						if(checkEqual(toStations[k], thisArrivalDatetimes.get(j+1).getKey())) {

							// Note that this logic relies on short-circuited evaluation to prevent comparison to a null thisDeathDatetime
							if(!particleDied || 
									(particleDied && thisDeathDatetime.getValue()>thisArrivalDatetimes.get(j+1).getValue())) {
								survivalCount++;
							}
							arrivalCount++;
							
							// No need to check additional toStations if we've found a match
							break;
						}
					}

				}
			}
		}

		surv = (double) survivalCount/ (double) arrivalCount;
		survDetails.get("survival").put(survDetailsKey + "-calculatedSurv", Double.toString(surv));

		if(Double.isNaN(surv)) {surv = 0;}
		survDetails.get("survival").put(survDetailsKey + "-finalSurv", Double.toString(surv));

		if(PTMFixedData.getConfig().show_route_survival_detail) { 
			System.out.print("Survival from " + fromStation + " to " + toStations[0]);
			for(int i=1; i<toStations.length; i++) {
				System.out.print(", " + toStations[i]);
			}
			System.out.println(": survivalCount=" + survivalCount + ", arrivalCount=" + arrivalCount + ", surv=" + surv);
		}
		survDetails.get("survival").put(survDetailsKey + "-survivalCount", Integer.toString(survivalCount));
		survDetails.get("survival").put(survDetailsKey + "-arrivalCount", Integer.toString(arrivalCount));
		survDetails.get("survival").put(survDetailsKey + "-passedStartCount", Integer.toString(passedStartCount));
		
		return surv;
	}

	/**
	 * Calculate routing fraction from fromStation to toStation, of possibleToStations
	 * @param fromStation			String with the "from" station name, e.g., "MOS"
	 * @param toStation				String with the "to" station name, e.g., SJL
	 * @param possibleToStations	String array with the full list of possible "to" stations
	 * @return						routing fraction
	 */
	public double calcFraction(String fromStation, String toStation, String[] possibleToStations) {
		double fraction;
		List<SimpleEntry<String, Long>> thisArrivalDatetimes;
		int passedStartCount, arrivalCount, possibleArrivalCount;
		Particle p;
		SimpleEntry<String, Long> thisArrivalDatetime;
		String numerator, denominator, survDetailsKey;

		// Check to see if there's a predefined survival probability for this combination of fromStation and toStations
		numerator = fromStation + "_" + toStation;
		denominator = fromStation + "_" + possibleToStations[0];
		for(int i=1; i<possibleToStations.length; i++) {
			denominator+="." + possibleToStations[i];
		}
		
		survDetailsKey = "frac(" +  numerator + "/" + denominator + ")";

		fraction = 0;
		passedStartCount = 0;
		arrivalCount = 0;
		possibleArrivalCount = 0;

		// Loop over vFish
		for(int i=0; i<particleArray.length; i++) {

			p = particleArray[i];
			thisArrivalDatetimes = p.getArrivalDatetimes();

			// Loop over the vFish's arrival history
			for(int j=0; j<thisArrivalDatetimes.size()-1; j++) {		
				thisArrivalDatetime = thisArrivalDatetimes.get(j);

				// Check to see if the vFish arrived at fromStation
				if(checkEqual(fromStation, thisArrivalDatetime.getKey())) {
					passedStartCount++;

					// Check to see if the vFish arrived at any possibleToStations
					for(int k=0; k<possibleToStations.length; k++) {

						if(checkEqual(possibleToStations[k], thisArrivalDatetimes.get(j+1).getKey())) {
							possibleArrivalCount++;

							// Check to see if this is toStation
							if(checkEqual(toStation, thisArrivalDatetimes.get(j+1).getKey())) {
								arrivalCount++;
							}

							// No need to check other possibleToStations if we've found a match
							break;
						}
					}
				}
			}	
		}

		// This is the fraction calculation based on Adam's equations
		fraction = (double) arrivalCount/(double) possibleArrivalCount;
		survDetails.get("routingFraction").put(survDetailsKey + "-calculatedFraction", Double.toString(fraction));

		if(PTMFixedData.getConfig().show_route_survival_detail) { 
			System.out.print("Fraction from " + fromStation + " to " + toStation + " of possible stations " + possibleToStations[0]);
			for(int i=1; i<possibleToStations.length; i++) {
				System.out.print(", " + possibleToStations[i]);
			}
			System.out.println(": arrivalCount=" + arrivalCount + ", possibleArrivalCount=" + possibleArrivalCount + ", fraction=" + fraction);
		}

		if(Double.isNaN(fraction)) {fraction = 0;}
		survDetails.get("routingFraction").put(survDetailsKey + "-finalFraction", Double.toString(fraction));
		
		survDetails.get("routingFraction").put(survDetailsKey + "-arrivalCount", Integer.toString(arrivalCount));
		survDetails.get("routingFraction").put(survDetailsKey + "-possibleArrivalCount", Integer.toString(possibleArrivalCount));
		survDetails.get("routingFraction").put(survDetailsKey + "-passedStartCount", Integer.toString(passedStartCount));

		return fraction;
	}

	/**
	 * Convert survival equation from shorthand notation into runnable GraalJS code
	 * @param eqStr					String containing survival equation in shorthand notation
	 * @return						String containing runnable GraalJS code
	 */
	public String translateEquation(String eqStr) {
		String pattern, replacementStr, defStr;
		Pattern p;
		Matcher m;
		StringBuffer sb;
		int survStrNum, fracStrNum;

		for(String c: new String[] {"[", "{", "<"}) {
			eqStr = eqStr.replace(c, "(");
		}
		for(String c: new String[] {"]", "}", ">"}) {
			eqStr = eqStr.replace(c, ")");
		}

		// Replace survivals
		survStrNum = 0;
		defStr = "";
		// Find up to 10 to stations
		for(int i=0; i<10; i++) {

			pattern = "S\\((\\w*)_";
			for(int j=0; j<i; j++) {
				pattern+="(\\w*?)\\.";
			}
			pattern+="(\\w*?)\\)";

			p = Pattern.compile(pattern);
			m = p.matcher(eqStr);
			sb = new StringBuffer();
			while(m.find()) {
				defStr+="var s" + survStrNum + " = new (StringClass)(" + (i+1) + ");\n";
				for(int j=0; j<(i+1); j++) {
					defStr+=" s" + survStrNum + "[" + j + "] ='" + m.group(j+2) + "';";
				}
				defStr+="\n";
				replacementStr = "survCalc.calcSurv('" + m.group(1) + "', s" + survStrNum + ")";
				m.appendReplacement(sb, replacementStr);

				survStrNum++;
			}
			m.appendTail(sb);
			eqStr = sb.toString();
		}        

		// Replace fractions
		fracStrNum = 0;
		// Find up to 10 to stations
		for(int i=0; i<10; i++) {
			pattern = "frac\\((\\w*)_(\\w*?)/\\w*_";
			for(int j=0; j<i; j++) {
				pattern+="(\\w*?)\\.";
			}
			pattern+="(\\w*?)\\)";

			p = Pattern.compile(pattern);
			m = p.matcher(eqStr);
			sb = new StringBuffer();
			while(m.find()) {
				defStr+="var f" + fracStrNum + " = new (StringClass)(" + (i+1) + ");\n";
				for(int j=0; j<(i+1); j++) {
					defStr+=" f" + fracStrNum + "[" + j + "] = '" + m.group(j+3) + "';";
				}
				defStr+="\n";
				replacementStr = "survCalc.calcFraction('" + m.group(1) + "', '" + m.group(2) + "', f" + fracStrNum + ")";
				m.appendReplacement(sb, replacementStr);

				fracStrNum++;
			}
			m.appendTail(sb);
			eqStr = sb.toString();
		}

		eqStr = defStr + eqStr;
		return eqStr;
	}

	/**
	 * Check whether two station names are equal, using a wildcard in targetString to match numbers in testString
	 * @param targetString			target station name to match, possibly including WILDCARD
	 * @param testString			station name to test
	 * @return						boolean indicating match (true) or no match (false)
	 */
	private boolean checkEqual(String targetString, String testString) {
		boolean matches;

		// Replace # wildcard with \d (digit) regular expression character class
		targetString =  targetString.replaceAll(WILDCARD, "\\\\d");

		matches = testString.matches(targetString);

		return matches;
	}

	/**
	 * Categorize final fates (passed Chipps, died, lost, etc.)
	 */
	private void categorizeFates() {
		List<SimpleEntry<String, Long>> thisArrivalDatetimes;
		SimpleEntry<Integer, Long> thisDeathDatetime, thisStuckDatetime, thisTransportDatetime;
		Particle p;
		boolean particleDied, particleStuck, particleTransported;
		int numDied, numStuck, numTransported, numExited, numLost;
		Map<String, Integer> transportPrevStations;
		String transportSeq;

		lastStationDatetimes = new HashMap<>();
		fates = new HashMap<>();

		numDied = 0;
		numStuck = 0;
		numTransported = 0;
		numExited = 0;
		numLost = 0;

		transportPrevStations = new HashMap<>();
		
		// Loop over vFish
		for(int i=0; i<particleArray.length; i++) {

			p = particleArray[i];
			thisArrivalDatetimes = p.getArrivalDatetimes();
			if(thisArrivalDatetimes!=null && thisArrivalDatetimes.size()>0) {
				lastStationDatetimes.put(p.getId(), thisArrivalDatetimes.getLast());
			}
			else {
				lastStationDatetimes.put(p.getId(), new SimpleEntry<String, Long>("", (long) MISSING));
			}

			thisDeathDatetime = p.getDeathDatetime();
			particleDied = thisDeathDatetime!=null;
			
			thisStuckDatetime = p.getStuckDatetime();
			particleStuck = thisStuckDatetime!=null;

			thisTransportDatetime = p.getTransportDatetime();
			particleTransported = thisTransportDatetime!=null;

			if(particleDied) {
				fates.put(p.getId(), "died");
				numDied++;
			}
			else if(particleStuck) {
				fates.put(p.getId(), "stuck");
				numStuck++;
			}
			else if(particleTransported) {
				fates.put(p.getId(), "transported");
				numTransported++;
							
				transportSeq = thisArrivalDatetimes.get(thisArrivalDatetimes.size()-2).getKey() + "_" + thisArrivalDatetimes.getLast().getKey();
				if(transportPrevStations.containsKey(transportSeq)) {
					transportPrevStations.put(transportSeq, transportPrevStations.get(transportSeq)+1);
				}
				else {
					transportPrevStations.put(transportSeq, 1);
				}
			}
			else if(thisArrivalDatetimes!=null && !particleDied) {
				for(String exitStation : PTMFixedData.getConfig().exit_stations) {
					if(thisArrivalDatetimes.size()>0 && thisArrivalDatetimes.getLast().getKey().equals(exitStation)) {
						fates.put(p.getId(), "exited");
						numExited++;
					}
				}
			}
			
			// If a fate hasn't been assigned, mark the vFish as lost
			if (!fates.containsKey(p.getId())) {
				fates.put(p.getId(), "lost");
				numLost++;
			}
		}

		System.out.println("===================================================================");
		System.out.println("Total number of vFish with recorded fates: " + (numDied + numStuck + numTransported + numExited + numLost));
		System.out.println("Number of vFish that died: " + numDied);
		System.out.println("Number of vFish that got stuck: " + numStuck);
		System.out.println("Number of vFish that were transported: " + numTransported);
		System.out.println("Number of vFish that exited: " + numExited);
		System.out.println("Number of vFish that were lost: " + numLost);
		if(PTMFixedData.getConfig().show_route_survival_detail) {
			System.out.println("Counts of unique transport station sequences: " + transportPrevStations.toString());
		}
	}

	/**
	 * Build survival components of netCDF output file
	 * @param builder				NetcdfFormatWriter.Builder
	 */
	public void buildOutput(NetcdfFormatWriter.Builder builder) {
		Dimension survGroupDim, survGroupLen, particleDim, stationCharLen, fateCharLen,
			survDetailsKeyDim, survDetailsCharLen;
		Variable.Builder<?> survBuilder, fatesBuilder;
		List<Dimension> survDims, fatesDims;

		survGroupDim = builder.addDimension("survGroup", surv.size());

		// Create survGroup dimension
		survGroupLen = builder.addDimension("survGroupLen", 20);
		builder.addVariable("survGroup", DataType.CHAR, "survGroup survGroupLen");

		survDims = new ArrayList<Dimension>();
		survDims.add(survGroupDim);

		survBuilder = builder.addVariable("surv", DataType.DOUBLE, survDims);
		survBuilder.addAttribute(new Attribute("units", "fraction"));

		particleDim = builder.addDimension("particle", particleArray.length);
		builder.addVariable("particle", DataType.INT, "particle");

		stationCharLen = builder.addDimension("stationCharLen", 80);
		builder.addVariable("lastStation", DataType.CHAR, "particle stationCharLen");

		fateCharLen = builder.addDimension("fateCharLen", 80);
		builder.addVariable("fate", DataType.CHAR, "particle fateCharLen");
		
		survDetailsCharLen = builder.addDimension("survDetailsCharLen", 80);
		survDetailsKeyDim = builder.addDimension("survDetailsKey", survDetails.get("survival").size() + survDetails.get("routingFraction").size());
		
		builder.addVariable("survDetailsKey", DataType.CHAR, "survDetailsKey survDetailsCharLen");
		builder.addVariable("survDetails", DataType.CHAR, "survDetailsKey survDetailsCharLen");		
	}

	/**
	 * Write survival components of netCDF output file
	 * @param writer				NetcdfFormatWriter
	 * @throws IOException			
	 * @throws InvalidRangeException
	 */
	public void writeOutput(NetcdfFormatWriter writer) throws IOException, InvalidRangeException {
		Variable v;
		int[] shape;
		Index ima;
		ArrayFloat floatArray;
		ArrayChar charArray;
		ArrayInt intArray;
		int survDetailsIndex;

		// Write survival fractions
		v = writer.findVariable("surv");
		shape = v.getShape();
		floatArray = new ArrayFloat.D1(shape[0]);
		ima = floatArray.getIndex();
		for (int i=0; i<shape[0]; i++) {
			floatArray.setFloat(ima.set(i), surv.get(survGroups.get(i)));
		}
		writer.write(v, floatArray);		

		// Set survGroup dimension
		v = writer.findVariable("survGroup");
		shape = v.getShape();
		charArray = new ArrayChar.D2(shape[0], shape[1]);
		ima = charArray.getIndex();
		for (int i=0; i<shape[0]; i++) {
			charArray.setString(ima.set(i), survGroups.get(i));
		}
		writer.write(v, charArray);

		// Write last stations
		v = writer.findVariable("lastStation");
		shape = v.getShape();
		charArray = new ArrayChar.D2(shape[0], shape[1]);
		ima = charArray.getIndex();
		for (int i=0; i<shape[0]; i++) {
			charArray.setString(ima.set(i), lastStationDatetimes.get(particleArray[i].getId()).getKey());
		}
		writer.write(v, charArray);

		// Write fates
		v = writer.findVariable("fate");
		shape = v.getShape();
		charArray = new ArrayChar.D2(shape[0], shape[1]);
		ima = charArray.getIndex();
		for (int i=0; i<shape[0]; i++) {
			charArray.setString(ima.set(i), fates.get(particleArray[i].getId()));
		}
		writer.write(v, charArray);

		// Set particle dimension
		v = writer.findVariable("particle");
		shape = v.getShape();
		intArray = new ArrayInt.D1(shape[0], false);
		ima = intArray.getIndex();
		for (int i=0; i<shape[0]; i++) {
			intArray.set(ima.set(i), particleArray[i].getId());
		}
		writer.write(v, intArray);
		
		// Set survDetailsKey dimension
		v = writer.findVariable("survDetailsKey");
		shape = v.getShape();
		charArray = new ArrayChar.D2(shape[0], shape[1]);
		ima = charArray.getIndex();
		survDetailsIndex = 0;
		for (String key : survDetails.get("survival").keySet()) {
			charArray.setString(ima.set(survDetailsIndex), key);
			survDetailsIndex++;
		}
		for (String key : survDetails.get("routingFraction").keySet()) {
			charArray.setString(ima.set(survDetailsIndex), key);
			survDetailsIndex++;
		}
		writer.write(v, charArray);
		
		// survDetails values
		v = writer.findVariable("survDetails");
		shape = v.getShape();
		charArray = new ArrayChar.D2(shape[0], shape[1]);
		ima = charArray.getIndex();
		survDetailsIndex = 0;
		for (String key : survDetails.get("survival").keySet()) {
			charArray.setString(ima.set(survDetailsIndex), survDetails.get("survival").get(key));
			survDetailsIndex++;
		}
		for (String key : survDetails.get("routingFraction").keySet()) {
			charArray.setString(ima.set(survDetailsIndex), survDetails.get("routingFraction").get(key));
			survDetailsIndex++;
		}
		writer.write(v, charArray);

		System.out.println("Done writing survival.");
	}

	/**
	 * Set writeRouteSurvival
	 * @param writeRouteSurvival	value to set writeRouteSurvival to		
	 */
	public static void setWriteRouteSurvival(boolean writeRouteSurvival) {
		SurvivalCalculation.writeRouteSurvival = writeRouteSurvival;
	}
	
	/**
	 * Set writeFates
	 * @param writeFates			value to set writeFates to
	 */
	public static void setWriteFates(boolean writeFates) {
		SurvivalCalculation.writeFates = writeFates;
	}
	
	/**
	 * set writeSurvDetail
	 * @param writeSurvDetail		value to set writeSurvDetail to
	 */
	public static void setWriteSurvDetail(boolean writeSurvDetail) {
		SurvivalCalculation.writeSurvDetail = writeSurvDetail;
	}
	
	/** Write route-specific survival to a CSV file
	 */
	public void writeOutputCSV() {
		String survOutputPathCSV, fatesOutputPathCSV, survDetailOutputPathCSV, line;
		Particle p;

		// Write survival outputs to route survival outputs CSV file
		survOutputPathCSV =  Globals.Environment.getPTMFixedInput().getFileName("routeSurvival");
		if(writeRouteSurvival && survOutputPathCSV!=null && (!survOutputPathCSV.equals(""))) {

			survOutputPathCSV = Paths.get(survOutputPathCSV).toAbsolutePath().normalize().toString();

			try(BufferedWriter buffer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(survOutputPathCSV)))) {

				line = "route";
				for(String g : survGroups) {
					line = line + "," + g;
				}
				buffer.write(line);
				buffer.newLine();

				line = "survivalFraction";
				for(String g : survGroups) {
					line = line + "," + surv.get(g).toString();
				}
				buffer.write(line);
				buffer.newLine();	

			} catch (IOException e) {
				PTMUtil.systemExit("Failed to write to route-specific survival output file, " + survOutputPathCSV + ": " + e);
			}
			System.out.println("Saved route-specific survival outputs to " + survOutputPathCSV);
		}

		// Write fates to fates outputs CSV files
		fatesOutputPathCSV = Globals.Environment.getPTMFixedInput().getFileName("fates");
		if(writeFates && fatesOutputPathCSV!=null && (!fatesOutputPathCSV.equals(""))) {

			fatesOutputPathCSV = Paths.get(fatesOutputPathCSV).toAbsolutePath().normalize().toString();

			try(BufferedWriter buffer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(fatesOutputPathCSV)))) {

				line = "particleID,lastStation,fate";
				buffer.write(line);
				buffer.newLine();

				for(int i=0; i<particleArray.length; i++) {

					p = particleArray[i];

					line = p.getId() + "," + lastStationDatetimes.get(p.getId()).getKey() + "," + fates.get(p.getId());
					buffer.write(line);
					buffer.newLine();
				}
			} catch (IOException e) {
				PTMUtil.systemExit("Failed to write to fates output file, " + fatesOutputPathCSV + ": " + e);
			}
			System.out.println("Saved vFish fates to " + fatesOutputPathCSV);
		}
		
		// Write survival details to CSV file
		survDetailOutputPathCSV = Globals.Environment.getPTMFixedInput().getFileName("survDetail");
		if(writeSurvDetail && survDetailOutputPathCSV!=null && (!survDetailOutputPathCSV.equals(""))) {
			
			survDetailOutputPathCSV = Paths.get(survDetailOutputPathCSV).toAbsolutePath().normalize().toString();
			
			try(BufferedWriter buffer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(survDetailOutputPathCSV)))) {
				
				line = "key, value";
				buffer.write(line);
				buffer.newLine();
				
				for(String key : survDetails.get("survival").keySet()) {
					line = key + "," + survDetails.get("survival").get(key);
					buffer.write(line);
					buffer.newLine();
				}
				for(String key : survDetails.get("routingFraction").keySet()) {
					line = key + "," + survDetails.get("routingFraction").get(key);
					buffer.write(line);
					buffer.newLine();
				}
				
			} catch (IOException e) {
				PTMUtil.systemExit("Failed to write to survival detail output file, " + survDetailOutputPathCSV + ": " + e);
			}
			System.out.println("Saved survival calculation details to " + survDetailOutputPathCSV);
			
		}
		
	}
}
