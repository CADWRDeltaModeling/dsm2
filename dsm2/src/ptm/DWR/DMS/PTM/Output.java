package DWR.DMS.PTM;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import ucar.ma2.ArrayChar;
import ucar.ma2.DataType;
import ucar.ma2.Index;
import ucar.ma2.InvalidRangeException;
import ucar.nc2.Dimension;
import ucar.nc2.Variable;
import ucar.nc2.write.NetcdfFormatWriter;

/**
 * Class that handles creation of and writing to the netCDF output file(s) 
 * @author Doug Jackson (QEDA Consulting, LLC)
 *
 */
public class Output {

	  public Output(Particle [] particleArray, PTMAnimationOutput animationOutput) {
		  PTMEnv Environment;
		  PTMFluxOutput fluxOutput;
		  SurvivalCalculation survOutput;
		  Set<String> netCDFoutputFiles;
		  List<String> fileTypes = Arrays.asList(new String[]{"flux", "survival", "echoConfigNetCDF", "anim"});
		  Map<String, String> fileTypeToPath;
		  NetcdfFormatWriter.Builder builder;
		  Variable v;	  
		  FluxInfo fluxFixedInfo;
		  GroupInfo groupFixedInfo;
		  FluxMonitor fluxMonitor;
		  String thisPath, thisAbsPath, tracefile, timestamp;
		  Dimension timestampLen;
		  int[] shape;
		  ArrayChar charArray;
		  Index ima;
		  Path outputDir;
		  Config config;
		  
		  Environment = Globals.Environment;
		  
		  // Create the output folder, which is where outputs are typically, but not necessarily, saved.
		  // This directory may have already been created by PTMFixedData
		  if(!Files.exists(Paths.get("./output"))) {
			  try {
				  outputDir = Files.createDirectories(Paths.get("./output"));
				  System.out.println("Created output directory at: " + outputDir.toAbsolutePath());
			  } catch (IOException e) {
				  // Just continue if the directory wasn't created
			  }
		  }
		  
		  // Create a Set of netCDF output file paths (ensures no duplicates)
		  netCDFoutputFiles = new HashSet<>();
		  fileTypeToPath = new HashMap<>();
		  for(String f : fileTypes) {
			  thisPath = Environment.getPTMFixedInput().getFileName(f);
			  if(!(thisPath.equalsIgnoreCase("")) && thisPath.toUpperCase().endsWith("NCD")) {
				  thisAbsPath = Paths.get(thisPath).toAbsolutePath().normalize().toString();
				  netCDFoutputFiles.add(thisAbsPath);
				  fileTypeToPath.put(f, thisAbsPath);
			  }
		  }  
		  
		  // Create output files
		  fluxOutput = null;
		  survOutput = null;
		  config = null;
		  for(String path : netCDFoutputFiles) {
			  System.out.println("Creating netCDF output file: " + path);
			  builder = NetcdfFormatWriter.createNewNetcdf3(path);
			  
			  timestamp = LocalDate.now().toString();
			  
			  timestampLen = builder.addDimension("timestampLen", 10);
			  builder.addVariable("timestamp", DataType.CHAR, "timestampLen");
			  
			  // All of the building has to be done prior to writing
			  // Build output file
			  if(Globals.CalculateWritePTMFlux) {
				  if(path.equals(fileTypeToPath.get("flux"))) {
				  
					  // MainPTM already verified that the trace file was specified
					  tracefile = Environment.getTraceFileName();
					  fluxFixedInfo = Environment.getFluxFixedInfo();
					  groupFixedInfo = Environment.getGroupFixedInfo();
					  
					  fluxMonitor = new FluxMonitor(tracefile, Environment.getFileType(tracefile), fluxFixedInfo, groupFixedInfo);
					  
					  fluxMonitor.calculateFlux();
					  
					  fluxOutput = fluxMonitor.prepOutput();
					  fluxOutput.buildOutput(builder);
				  }
			  }
			  
			  if(path.equals(fileTypeToPath.get("survival"))) {
		            survOutput = new SurvivalCalculation(particleArray);
		            survOutput.run();
		            survOutput.buildOutput(builder);
			  }
			  
			  if(path.equals(fileTypeToPath.get("echoConfigNetCDF"))) {
				  config = PTMFixedData.getConfig();
				  config.buildOutput(builder);
			  }
			  
			  System.out.println("fileTypeToPath.get('anim'): " + fileTypeToPath.get("anim"));
			  if(path.equals(fileTypeToPath.get("anim")) && animationOutput!=null) {
				  animationOutput.buildOutput(builder);
			  }
			  
			  // Write output
			  try (NetcdfFormatWriter writer = builder.build()) {
				  v = writer.findVariable("timestamp");
				  shape = v.getShape();
				  charArray = new ArrayChar.D1(shape[0]);
				  ima = charArray.getIndex();
				  charArray.setString(ima.set(0), timestamp);
				  writer.write(v, charArray);				  
				  
				  if(Globals.CalculateWritePTMFlux) {
					  if(path.equals(fileTypeToPath.get("flux"))) {
						  fluxOutput.writeOutput(writer);
					  }
				  }
				  
				  if(path.equals(fileTypeToPath.get("survival"))) {
					  survOutput.writeOutput(writer);
				  }
				  
				  if(path.equals(fileTypeToPath.get("echoConfigNetCDF"))) {
					  config.writeOutput(writer);
				  }
				  
				  if(path.equals(fileTypeToPath.get("anim")) && animationOutput!=null) {
					  animationOutput.writeOutput(writer);
				  }
				  
			  } catch (Exception e) {
				  PTMUtil.systemExit("Failed to write to netCDF output file. " + e);
			  }
		  }
		  
		  // Append survival outputs to survival outputs CSV file
		  if(survOutput!=null) {survOutput.writeOutputCSV();}
	  }
}

