//    Copyright (C) 1996, 2009 State of California, Department of Water
//    Resources.
//
//    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
//    numerical model.  No protection claimed in original FOURPT and
//    Branched Lagrangian Transport Model (BLTM) code written by the
//    United States Geological Survey.  Protection claimed in the
//    routines and files listed in the accompanying file "Protect.txt".
//    If you did not receive a copy of this file contact
//    Tara Smith, below.
//
//    This program is licensed to you under the terms of the GNU General
//    Public License, version 2, as published by the Free Software
//    Foundation.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, contact Tara Smith, below,
//    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
//    02139, USA.
//
//    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
//    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
//    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
//    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
//    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
//    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
//    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
//    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
//    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
//    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
//    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
//    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
//    DAMAGE.
//
//    For more information about DSM2, contact:
//
//    Tara Smith
//    California Dept. of Water Resources
//    Division of Planning, Delta Modeling Section
//    1416 Ninth Street
//    Sacramento, CA  95814
//    916-653-9885
//    tara@water.ca.gov
//
//    or see our home page: http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/
package DWR.DMS.PTM;
import java.io.*;
import java.util.ArrayList;
import java.util.List;

import ucar.ma2.ArrayChar;
import ucar.ma2.ArrayFloat;
import ucar.ma2.ArrayInt;
import ucar.ma2.ArrayShort;
import ucar.ma2.ArrayString;
import ucar.ma2.DataType;
import ucar.ma2.Index;
import ucar.ma2.InvalidRangeException;
import ucar.nc2.Attribute;
import ucar.nc2.Dimension;
import ucar.nc2.Variable;
import ucar.nc2.write.NetcdfFormatWriter;
/**
 * This class outputs information for animation. At any
 * given instant of time the Particle Id
 * and the normalized x,y and z location are output.
 * of the Particle.<br>
 *
 * @author Nicky Sandhu
 * @version $Id: PTMAnimationOutput.java,v 1.7 2000/08/07 17:00:27 miller Exp $
 */
public class PTMAnimationOutput extends PTMOutput{

	private final int MAX_NUMBER_OF_PARTICLES=20000;

	// number of outputs for instantaneous
	private int numberOfParticles;

	// Particle pointer array
	private Particle [] particlePtrArray;

	// keeps track of last output time
	private int previousOutputTime;

	// output interval
	private int outputInterval;

	 // End of File tag
	private String endOfFile = "EOF";
	
	int outputType;
	
	// Variables to hold animation output so they can be written to a netCDF file
	private ArrayList<String> modelDates, modelTimes;
	private ArrayList<Integer> particleNumbers;
	private ArrayList<Short> channelNumbers, xDists;
	
	/**
	 *
	 * @param filename Name of animation output file
	 * @param type format of output: ascii/binary
	 * @param interval The number of minutes between animation information
	 * @param numberOfParticles The number of particles for animation output
	 * @param particleArray The array of particles
	 */
	public PTMAnimationOutput(String filename,
			int type,
			int interval,
			int numberOfParticles,
			int requestedNumberOfParticles,
			Particle [] particleArray) throws IOException{
		super(filename, type);
		outputInterval = interval;
		setOutputParameters(numberOfParticles, requestedNumberOfParticles, particleArray);
		
		outputType = type;
		if (outputType==Globals.NETCDF) {
			modelDates = new ArrayList<>();
			modelTimes = new ArrayList<>();
			particleNumbers = new ArrayList<>();
			channelNumbers = new ArrayList<>();
			xDists = new ArrayList<>();
		}
	}

	/**
	 *  Particle array and size
	 */
	private final void setOutputParameters(int numParts,
			int requestedNumParts,
			Particle [] pArray){
		if (requestedNumParts <= MAX_NUMBER_OF_PARTICLES && requestedNumParts <= numParts)
			numberOfParticles = requestedNumParts;
		else if (numParts <= MAX_NUMBER_OF_PARTICLES)
			numberOfParticles = numParts;
		else
			numberOfParticles = MAX_NUMBER_OF_PARTICLES;

		particlePtrArray = new Particle[numberOfParticles];
		System.arraycopy(pArray,0,particlePtrArray,0,particlePtrArray.length);
		previousOutputTime = Globals.currentModelTime - outputInterval;
	}

	/**
	 *  output function
	 */
	public void output() throws IOException{
		InstantaneousOutput [] outputData = new InstantaneousOutput[MAX_NUMBER_OF_PARTICLES];
		if(Globals.currentModelTime >= previousOutputTime+outputInterval){
			previousOutputTime = Globals.currentModelTime;
			updateOutputStructure(outputData);

			if (outputType == Globals.ASCII) {
				// write out into ascii file
				writeOutputAscii(outputData);
			}
			else if (outputType == Globals.BINARY) {
				// write out into binary file
				writeBinary(outputData);
			}
			else if (outputType==Globals.NETCDF) {
				recordNetCDF(outputData);
			}
		}
	}

	public void writeBinary(InstantaneousOutput [] outputData) throws IOException{
		int julianMin = Globals.currentModelTime;
		String modelDate, modelTime;
		modelDate = Globals.getModelDate(julianMin);
		modelTime = Globals.getModelTime(julianMin);
		String line = modelDate;
		outputStream.writeUTF(line);
		outputStream.writeShort(new Short(modelTime).shortValue());
		outputStream.writeShort(numberOfParticles);
		for (int i=0; i< numberOfParticles; i++){
			outputStream.writeShort(outputData[i].particleNumber);
			outputStream.writeShort(outputData[i].channelNumber);
			outputStream.writeShort(outputData[i].normXDistance);
			outputStream.writeShort(outputData[i].normYDistance);
			outputStream.writeShort(outputData[i].normZDistance);
			outputStream.writeShort(outputData[i].value);
		}
	}

	/**
	 *  output ascii
	 */
	private final void writeOutputAscii( InstantaneousOutput [] outputData ) throws IOException{
		int julianMin = Globals.currentModelTime;
		String modelDate, modelTime;
		modelDate = Globals.getModelDate(julianMin);
		modelTime = Globals.getModelTime(julianMin);

		String line = modelDate + " " + modelTime;
		outputWriter.write(line, 0, line.length());
		outputWriter.newLine();
		line = numberOfParticles + " ";
		outputWriter.write(line, 0, line.length());
		outputWriter.newLine();

		for (int i=0; i< numberOfParticles; i++){
			line = "  " + outputData[i].particleNumber
					+ "  " + outputData[i].channelNumber
					+ "  " + outputData[i].normXDistance
					+ "  " + outputData[i].normYDistance
					+ "  " + outputData[i].normZDistance
					+ "  " + outputData[i].value;
			outputWriter.write(line, 0, line.length());
			outputWriter.newLine();
		}
	}
	
	public void recordNetCDF(InstantaneousOutput[] outputData) {
		int julianMin;
		String modelDate, modelTime;

		julianMin = Globals.currentModelTime;
		modelDate = Globals.getModelDate(julianMin);
		modelTime = Globals.getModelTime(julianMin);
		modelDates.add(modelDate);
		modelTimes.add(modelTime);
		
		for (int i=0; i<numberOfParticles; i++) {
			particleNumbers.add(outputData[i].particleNumber);
			if(outputData[i].channelNumber!=-1) {
				channelNumbers.add((short) PTMFixedData.getExtChanNum(outputData[i].channelNumber));
			} 
			else {
				channelNumbers.add(outputData[i].channelNumber);
			}
			xDists.add(outputData[i].normXDistance);
		}
	}

	/**
	 *  fill up output data structure with current information
	 */
	private final void updateOutputStructure(InstantaneousOutput [] outputData){
		Waterbody wb=null;
		float []  x = new float [1],y = new float [1],z = new float [1];

		for(int i=0; i<numberOfParticles; i++) {
			outputData[i] = new InstantaneousOutput();
			outputData[i].particleNumber = (short) particlePtrArray[i].getId();

			wb = particlePtrArray[i].getLocation(x, y, z);
			if ( wb != null ) {
				if( wb.getPTMType() ==  Waterbody.CHANNEL) {
					outputData[i].channelNumber = (short) wb.getEnvIndex();
					outputData[i].normXDistance =
							(short) ((1.0f-x[0]/((Channel )wb).getLength())*100);
					outputData[i].normYDistance =
							(short) (y[0]/((Channel )wb).getWidth(x[0])*100);
					outputData[i].normZDistance =
							(short) (z[0]/((Channel )wb).getDepth(x[0])*100);
					outputData[i].value = (short) (1);
				}
				else {
					outputData[i].channelNumber = (short) -1;
					outputData[i].normXDistance = (short)-1;
					outputData[i].normYDistance = (short)-1;
					outputData[i].normZDistance = (short)-1;
					outputData[i].value = (short)0;
				}// end if-else (wb.getPTMType)
			}
			else {
				outputData[i].channelNumber = (short) -1;
				outputData[i].normXDistance = (short)-1;
				outputData[i].normYDistance = (short)-1;
				outputData[i].normZDistance = (short)-1;
				outputData[i].value = (short)0;
			}// end if-else (wb)
		}
	}

	public void FlushAndClose() {
		try {
			if (outputType == Globals.ASCII) {
				outputWriter.write(endOfFile, 0, endOfFile.length());
				outputWriter.newLine();
				outputWriter.flush();
				outputWriter.close();
			}
			else if (outputType == Globals.BINARY) {
				outputStream.writeUTF(endOfFile);
				outputStream.flush();
				outputStream.close();
			}
		} catch(IOException e){System.out.println(e); }
	}
	
	public void buildOutput(NetcdfFormatWriter.Builder builder) {
		Dimension particleNumDim, animColsDim, timestepDim, modelDateCharLen, modelTimeCharLen;

		particleNumDim = builder.addDimension("particleNum", particleNumbers.size());
		builder.addVariable("particleNum", DataType.INT, "particleNum");

		animColsDim = builder.addDimension("animCols", 2);
		builder.addVariable("anim", DataType.SHORT, "particleNum animCols");
		
		timestepDim = builder.addDimension("timestep", modelDates.size());
		builder.addVariable("timestep", DataType.INT, "timestep");
		
		modelDateCharLen = builder.addDimension("modelDateChar", 9);
		builder.addVariable("modelDate", DataType.CHAR, "timestep modelDateChar");
		
		modelTimeCharLen = builder.addDimension("modelTimeChar", 4);
		builder.addVariable("modelTime", DataType.CHAR, "timestep modelTimeChar");
	}
	
	public void writeOutput(NetcdfFormatWriter writer) throws IOException, InvalidRangeException {
		Variable v;
		int[] shape;
		Index ima;
		ArrayInt intArray;
		ArrayShort shortArray;
		ArrayChar charArray;
		
		// Set particle dimension
		v = writer.findVariable("particleNum");
		shape = v.getShape();
		intArray = new ArrayInt.D1(shape[0], false);
		ima = intArray.getIndex();
		for (int i=0; i<shape[0]; i++) {
			intArray.set(ima.set(i), particleNumbers.get(i));
		}
		writer.write(v, intArray);
		
		// Set anim
		v = writer.findVariable("anim");
		shape = v.getShape();
		shortArray = new ArrayShort.D2(shape[0], shape[1], false);
		ima = shortArray.getIndex();
		for (int i=0; i<shape[0]; i++) {
			shortArray.set(ima.set(i, 0), channelNumbers.get(i));
			shortArray.set(ima.set(i, 1), xDists.get(i));
		}
		writer.write(v, shortArray);
		
		// Set timestep dimension
		v = writer.findVariable("timestep");
		shape = v.getShape();
		intArray = new ArrayInt.D1(shape[0], false);
		ima = intArray.getIndex();
		for (int i=0; i<shape[0]; i++) {
			intArray.set(ima.set(i), i);
		}
		writer.write(v,  intArray);
		
		// Set modelDate
		v = writer.findVariable("modelDate");
		shape = v.getShape();
		charArray = new ArrayChar.D2(shape[0], shape[1]);
		ima = charArray.getIndex();
		for (int i=0; i<shape[0]; i++) {
			charArray.setString(ima.set(i), modelDates.get(i));
		}
		writer.write(v, charArray);
		
		v = writer.findVariable("modelTime");
		shape = v.getShape();
		charArray = new ArrayChar.D2(shape[0], shape[1]);
		ima = charArray.getIndex();
		for (int i=0; i<shape[0]; i++) {
			charArray.setString(ima.set(i), modelTimes.get(i));
		}
		writer.write(v, charArray);
	}
}
