package DWR.DMS.PTM;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Class to look up and contain the waterbody groups for flux output.
 * 
 * @author Doug Jackson (QEDA Consulting, LLC)
 *
 */
public class FluxGroup {
	private static final int GROUP_ANY_TYPE = -9999;
	private static final int GROUP_ANY_INDEX = -9998;
	
	private String name;
	
	private List<Integer> intIds;
	private List<Integer> types;
	
	public FluxGroup(String name) {
		this.name = name.toUpperCase();
		
		intIds = new ArrayList<>();
		types = new ArrayList<>();
	}

	/**
	 * Add waterbodies specified by the type and pattern
	 * @param type					type of waterbodies
	 * @param pattern				a list of channels, exact name, or pattern specifying a waterbody or waterbodies
	 */
	public void addObjects(String type, String pattern) {
		int fluxType, patternType, startRange, endRange, thisExtChanNum;
		String[] fields;
		List<Integer> extChanNums;
		List<String> reservoirNames, stageBoundaryNames, qextNames;
		Pattern p;
		Matcher m;
		
		final int EXACT = 0, REGEX=1, RANGE=2;
		
		fluxType = PTMFixedData.getFluxTypeCode(type);
		
		// Convert pattern to uppercase to facilitate comparisons
		pattern = pattern.toUpperCase();
		
		// Remove all parentheses and spaces
		pattern = pattern.replace("(", "");
		pattern = pattern.replace(")", "");
		pattern = pattern.replaceAll("\\s+", "");
		
		// Compile the pattern just in case we need it
		p = Pattern.compile(pattern);
		
		// Determine the pattern type
		if(pattern.contains("*")) {
			patternType = REGEX;
		}
		else if(pattern.contains("RANGE:")) {
			patternType = RANGE;
		}
		else {
			patternType = EXACT;
		}
		
		if(type.equalsIgnoreCase("CHAN")) {
			
			// Pattern could be a list of channels, a regular expression, or a range
			switch(patternType) {
			
			case EXACT:
				// Obtain the channel numbers			
				fields = pattern.split("\\|");
				for(String f: fields) {
					intIds.add(PTMFixedData.getIntChanNum(Integer.parseInt(f)));
					types.add(fluxType);
				}
				break;
				
			case REGEX:
				// Search through the external channel numbers and add any that match the pattern
				extChanNums = PTMFixedData.getExtChanNums();
				
				for(int i=0; i<extChanNums.size(); i++) {
					m = p.matcher(Integer.toString(extChanNums.get(i)));
					
					if(m.find()) {
						intIds.add(PTMFixedData.getIntChanNum(extChanNums.get(i)));
						types.add(fluxType);
					}	
				}
				break;
				
			case RANGE:
				pattern = pattern.replace("RANGE:", "");
				fields = pattern.split("-");
				
				startRange = Integer.parseInt(fields[0]);
				endRange = Integer.parseInt(fields[1]);
				
				// Search through the external channel numbers and add any that are within the range
				extChanNums = PTMFixedData.getExtChanNums();
				
				for(int i=0; i<extChanNums.size(); i++) {
					thisExtChanNum = extChanNums.get(i);
					
					if(thisExtChanNum>=startRange && thisExtChanNum<=endRange) {
						intIds.add(PTMFixedData.getIntChanNum(thisExtChanNum));
						types.add(fluxType);
					}
				}
				
				break;
			}
		}
		else if(type.equalsIgnoreCase("RES")) {
			// Search through reservoirNames and add any that match the pattern
			reservoirNames = PTMFixedData.getReservoirNames();
			
			for(int i=0; i<reservoirNames.size(); i++) {
				
				// Pattern could be an exact reservoir name or a regular expression
				switch(patternType) {
				
				case EXACT:
					// Search for an exact match; split on | in case multiple reservoirs are listed			
					fields = pattern.split("\\|");
					for(String f: fields) {
						if(reservoirNames.get(i).equalsIgnoreCase(f)) {
							intIds.add(PTMFixedData.getIntResNum(reservoirNames.get(i)));
							types.add(fluxType);
						};
					}
					break;
				
				case REGEX:
					m = p.matcher(reservoirNames.get(i).toUpperCase());
					
					if(m.find()) {
						intIds.add(PTMFixedData.getIntResNum(reservoirNames.get(i)));
						types.add(fluxType);
					}
					break;			
				}
			}
		}
		else if(type.equalsIgnoreCase("STAGE")) {
			// Search through stageBoundaryNames and add any that match the pattern
			stageBoundaryNames = PTMFixedData.getStageBoundaryNames();
			
			for(int i=0; i<stageBoundaryNames.size(); i++) {
				
				// Pattern could be an exact stage boundary name or a regular expression
				switch(patternType) {
				
				case EXACT:
					// Search for an exact match; split on | in case multiple stage boundaries are listed	
					fields = pattern.split("\\|");
					for(String f: fields) {
						if(stageBoundaryNames.get(i).equalsIgnoreCase(f)) {
							intIds.add(PTMFixedData.getIntStageBoundaryNum(stageBoundaryNames.get(i)));
							types.add(fluxType);
						}
					}
					break;
					
				case REGEX:
					m = p.matcher(stageBoundaryNames.get(i).toUpperCase());
					
					if(m.find()) {
						intIds.add(PTMFixedData.getIntStageBoundaryNum(stageBoundaryNames.get(i)));
						types.add(fluxType);
					}
					break;
				}
			}
		}
		else if(type.equalsIgnoreCase("QEXT")) {			
			// Search through qextNames and add any that match the pattern
			qextNames = PTMFixedData.getQextNames();
			
			for(int i=0; i<qextNames.size(); i++) {
				
				// Pattern could be an exact qext name or a regular expression
				switch(patternType) {
				
				case EXACT:
					// Search for an exact match; split on | in case multiple qext are listed	
					fields = pattern.split("\\|");
					for(String f: fields) {
						if(qextNames.get(i).equalsIgnoreCase(f)) {
							intIds.add(PTMFixedData.getIntQextNum(qextNames.get(i)));
							types.add(fluxType);
						}
					}
					break;
					
				case REGEX:
					m = p.matcher(qextNames.get(i).toUpperCase());
					
					if(m.find()) {
						intIds.add(PTMFixedData.getIntQextNum(qextNames.get(i)));
						types.add(fluxType);
					}
					break;
				}
			}
		}
		else if(type.equalsIgnoreCase("ALL")) {
			intIds.add(GROUP_ANY_INDEX);
			types.add(GROUP_ANY_TYPE);
		}
	}
	
	/**
	 * Obtain the internal IDs for all flux waterbodies
	 * @return						internal IDs
	 */
	public int[] getIntIds() {
		return intIds.stream().mapToInt(Integer::intValue).toArray();
	}
	
	/**
	 * Obtain the types for all flux waterbodies
	 * @return						waterbody types
	 */
	public int[] getTypes() {
		return types.stream().mapToInt(Integer::intValue).toArray();
	}
}
