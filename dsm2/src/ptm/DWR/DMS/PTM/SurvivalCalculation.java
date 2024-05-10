/**
 *
 */
package DWR.DMS.PTM;

import java.util.AbstractMap.SimpleEntry;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;

import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author Doug Jackson, QEDA Consulting, LLC
 * Calculate route-specific survival based on equations defined in behavior configuration file.
 */
public class SurvivalCalculation {

	static final String WILDCARD = "99999";
	
	private Particle [] particleArray;
	private Map<String, Float> reachSurvMap;

	public SurvivalCalculation(Particle [] particleArray) {
		this.particleArray = particleArray;
		
		reachSurvMap = Globals.Environment.getBehaviorInputs().getSurvivalInputs().getReachSurvMap();
	}

	/**
	 * Evaluate the survival equations using GraalJS engine
	 */
	public void run() {
		String thisSurvEq;
		Object thisSurv;
		Map<String, String> survEqs;
		
        survEqs = Globals.Environment.getBehaviorInputs().getSurvivalInputs().getSurvEqs();
               
		// Build GraalJS engine
        Engine engine1 = Engine.newBuilder()
                .option("engine.WarnInterpreterOnly", "false")
                .build();      
        
        Context ctx = Context.newBuilder("js").engine(engine1).allowAllAccess(true).build();
        ctx.getBindings("js").putMember("survCalc", new SurvivalCalculation(particleArray));
        ctx.eval("js", "var StringClass = Java.type('java.lang.String[]');");      
        
        // Create and display translated equations
        for (String key: survEqs.keySet()) {
        	System.out.println("===================================================================");
        	System.out.println("Survival equations for " + key);        	
        	thisSurvEq = survEqs.get(key);
        	// Remove # wildcards in equation with WILDCARD, which is an allowable component of a variable name
        	thisSurvEq = thisSurvEq.replaceAll("#", WILDCARD);
        	thisSurvEq = translateEquation(thisSurvEq);
        	System.out.println("translated equation: " + thisSurvEq);
        	System.out.println("===================================================================");
        }
        
        // Calculate survivals
        for (String key: survEqs.keySet()) {
        	thisSurvEq = survEqs.get(key);
        	// Remove # wildcards in equation with WILDCARD, which is an allowable component of a variable name
        	thisSurvEq = thisSurvEq.replaceAll("#", WILDCARD);
        	thisSurvEq = translateEquation(thisSurvEq);
        	thisSurv = ctx.eval("js", thisSurvEq);
        	System.out.println("Survival for route " + key + ": " + thisSurv);
        	System.out.println("===================================================================");
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
		int survivalCount, arrivalCount;
		Particle p;
		boolean particleDied;
		SimpleEntry<String, Long> thisArrivalDatetime;
		String reachIndex;
		
		// Check to see if there's a predefined survival probability for this combination of fromStation and toStations
		reachIndex = fromStation + "_" + toStations[0];
		for(int i=1; i<toStations.length; i++) {
			reachIndex+="." + toStations[i];
		}
		if(reachSurvMap.containsKey(reachIndex)) {
			surv = reachSurvMap.get(reachIndex);
			System.out.println("Survival for " + reachIndex + ": surv = " + surv);
			return surv;
		}

		survivalCount = 0;
		arrivalCount = 0;

		// Loop over eFish
		for(int i=0; i<particleArray.length; i++) {

			p = particleArray[i];
			thisArrivalDatetimes = p.getArrivalDatetimes();
			thisDeathDatetime = p.getDeathDatetime();;

			if(thisDeathDatetime!=null) {particleDied = true;}
			else {particleDied = false;}

			// Loop over the eFish's arrival history
			for(int j=0; j<thisArrivalDatetimes.size()-1; j++) {		
				thisArrivalDatetime = thisArrivalDatetimes.get(j);

				// If the eFish passed the start station, check to see if it survived to each of the end stations
				if(checkEqual(fromStation, thisArrivalDatetime.getKey())) {
					
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
				
		if(Double.isNaN(surv)) {surv = 0;}
		
        System.out.print("Survival from " + fromStation + " to ");
        for(int i=0; i<toStations.length; i++) {
            System.out.print(", " + toStations[i]);
        }
        System.out.println(": survivalCount=" + survivalCount + ", arrivalCount=" + arrivalCount + ", surv=" + surv);
        
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
		SimpleEntry<Integer, Long> thisDeathDatetime;
		int arrivalCount, possibleArrivalCount;
		Particle p;
		boolean particleDied;
		SimpleEntry<String, Long> thisArrivalDatetime;
		
		fraction = 0;
		arrivalCount = 0;
		possibleArrivalCount = 0;
		
		// Loop over eFish
		for(int i=0; i<particleArray.length; i++) {

			p = particleArray[i];
			thisArrivalDatetimes = p.getArrivalDatetimes();
			thisDeathDatetime = p.getDeathDatetime();

			if(thisDeathDatetime!=null) {particleDied = true;}
			else {particleDied = false;}

			// Loop over the eFish's arrival history
			for(int j=0; j<thisArrivalDatetimes.size()-1; j++) {		
				thisArrivalDatetime = thisArrivalDatetimes.get(j);
				
				// Check to see if the eFish arrived at fromStation
				if(checkEqual(fromStation, thisArrivalDatetime.getKey())) {
																				
					// Check to see if the particle survived to any possibleToStations
					for(int k=0; k<possibleToStations.length; k++) {
						
						if(checkEqual(possibleToStations[k], thisArrivalDatetimes.get(j+1).getKey())) {
							if(!particleDied || 
									(particleDied && thisDeathDatetime.getValue()>thisArrivalDatetimes.get(j+1).getValue())) {
								possibleArrivalCount++;
								
								// Check to see if this is toStation
								if(checkEqual(toStation, thisArrivalDatetimes.get(j+1).getKey())) {
									arrivalCount++;
								}
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
		
        System.out.print("Fraction from " + fromStation + " to " + toStation + " of possible stations ");
        for(int i=0; i<possibleToStations.length; i++) {
            System.out.print(possibleToStations[i] + ", ");
        }
        System.out.println(": arrivalCount=" + arrivalCount + ", possibleArrivalCount=" + possibleArrivalCount + ", fraction=" + fraction);
		
		if(Double.isNaN(fraction)) {fraction = 0;}
		
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
}
