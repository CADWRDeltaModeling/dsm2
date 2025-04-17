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

import java.io.IOException;
import java.nio.file.Paths;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;

import ucar.ma2.ArrayChar;
import ucar.ma2.ArrayFloat;
import ucar.ma2.ArrayString;
import ucar.ma2.DataType;
import ucar.ma2.Index;
import ucar.ma2.InvalidRangeException;
import ucar.nc2.Attribute;
import ucar.nc2.Dimension;
import ucar.nc2.Variable;
import ucar.nc2.write.NetcdfFormatWriter;

/**
 *
 */
public class PTMFluxOutput extends PTMOutput{

	private String[] datetimes;
	private float[][] nodeFluxes, groupFluxes;

	/**
	 *  constructor
	 */
	public PTMFluxOutput(int startTime){
		super();
	}

	/**
	 *
	 */
	public void GroupOutput(Flux [] groupAt, boolean percent){
		groupPercent = percent;

		if (groupAt.length < MAX_GROUP_OUTPUT){
			nGroup = groupAt.length;
		}else {
			System.out.println("Too many group values required");
			nGroup = MAX_GROUP_OUTPUT;
		}

		groupAtNode = groupAt;
	}

	/**
	 *
	 */
	public void FluxOutput(Flux [] fluxAt, boolean percent){
		fluxPercent = percent;

		if (fluxAt.length < MAX_FLUX_OUTPUT)
			nFlux = fluxAt.length;
		else {
			System.out.println("Too many flux values required");
			nFlux = MAX_FLUX_OUTPUT;
		}

		fluxAtNode = fluxAt;
	}

	/**
	 *
	 */
	public void calcOutput() {

		DateFormat converter;
		String dateStr;
		int numTimesteps, timestepCount, timestepIndex, fluxInterval, downsampleFactor;
		float fluxOut;

		if (fluxAtNode != null)
			genericFlux = fluxAtNode;
		else if (groupAtNode != null)
			genericFlux = groupAtNode;
		else
			return;
		if (genericFlux[0] == null)
			return;
		
		fluxInterval = Globals.Environment.getPTMFixedInput().getIntervalInMin("flux");	
		
		// Calculate the base number of time steps
		numTimesteps = (genericFlux[0].getEndTime()-genericFlux[0].getStartTime())/genericFlux[0].getPTMTimeStep();
		
		// Adjust numTimesteps according to the flux recording interval
		downsampleFactor = Math.max(1, Math.floorDiv(fluxInterval, genericFlux[0].getPTMTimeStep()));
		numTimesteps = Math.floorDiv(numTimesteps, downsampleFactor);
		
		datetimes = new String[numTimesteps];
		nodeFluxes = new float[numTimesteps][nFlux];
		groupFluxes = new float[numTimesteps][nGroup];

		converter = new SimpleDateFormat("MM/dd/yyyy  HH:mm:ss");
		converter.setTimeZone(Globals.TIME_ZONE);

		timestepCount = 0;
		timestepIndex = 0;
		for (int cTime = genericFlux[0].getStartTime(); cTime < genericFlux[0].getEndTime(); 
				cTime += genericFlux[0].getPTMTimeStep()) {
			
			if((timestepCount % downsampleFactor)==0) {
				dateStr = converter.format(PTMUtil.modelTimeToCalendar(cTime, Globals.TIME_ZONE).getTime());
				datetimes[timestepIndex] = dateStr;

				//Node
				for (int i = 0; i < nFlux; i++) {
					if (fluxPercent) {
						fluxOut = (fluxAtNode[i].getFlux(cTime) * 100.0f)
								/ fluxAtNode[i].getNumberOfParticles();
					} else {
						fluxOut = (fluxAtNode[i].getFlux(cTime));
					}

					nodeFluxes[timestepIndex][i] = fluxOut;
				}

				//Group
				for (int i = 0; i < nGroup; i++) {
					if (groupPercent) {
						fluxOut = (groupAtNode[i].getFlux(cTime) * 100.0f)
								/ groupAtNode[i].getNumberOfParticles();
					} else {
						fluxOut = (groupAtNode[i].getFlux(cTime));
					}

					groupFluxes[timestepIndex][i] = fluxOut;
				}
				timestepIndex++;
			}			
			timestepCount++;
			
		}// end for(cTime)
	}

	/**
	 * Build flux components of netCDF output file
	 * @param builder				NetcdfFormatWriter.Builder
	 */
	public void buildOutput(NetcdfFormatWriter.Builder builder) {
		Dimension datetimeDim, datetimeLen, nodeNameDim, nodeNameLen, groupNameDim, groupNameLen;
		List<Dimension> nodeDims, groupDims;
		Variable.Builder<?> nodeBuilder, groupBuilder;
		
		datetimeDim = builder.addDimension("datetime", datetimes.length);
		nodeNameDim = builder.addDimension("nodeName", nFlux);
		groupNameDim = builder.addDimension("groupName", nGroup);

		nodeDims = new ArrayList<Dimension>();
		nodeDims.add(datetimeDim);
		nodeDims.add(nodeNameDim);
		
		groupDims = new ArrayList<Dimension>();
		groupDims.add(datetimeDim);
		groupDims.add(groupNameDim);

		nodeBuilder = builder.addVariable("nodeFlux", DataType.DOUBLE, nodeDims);
		nodeBuilder.addAttribute(new Attribute("units", "percent"));
		
		groupBuilder = builder.addVariable("groupFlux", DataType.DOUBLE, groupDims);
		groupBuilder.addAttribute(new Attribute("units", "percent"));
		
		// Create datetime dimension
		datetimeLen = builder.addDimension("datetimeLen", 20);
		builder.addVariable("datetime", DataType.CHAR, "datetime datetimeLen");
		
		// Create nodeName dimension
		nodeNameLen = builder.addDimension("nodeNameLen", 80);
		builder.addVariable("nodeName", DataType.CHAR, "nodeName nodeNameLen");
		
		// Create groupName dimension
		groupNameLen = builder.addDimension("groupNameLen", 80);
		builder.addVariable("groupName", DataType.CHAR, "groupName groupNameLen");
	}
	
	/**
	 * Write flux components of netCDF output file
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

		v = writer.findVariable("nodeFlux");
		shape = v.getShape();
		floatArray = new ArrayFloat.D2(shape[0], shape[1]);
		ima = floatArray.getIndex();
		for (int i=0; i<shape[0]; i++) {
			for (int j = 0; j < shape[1]; j++) {
				floatArray.setFloat(ima.set(i, j), nodeFluxes[i][j]);
			}
		}

		// Since we dont pass in an origin parameter, it is assumed to be all zeroes.
		writer.write(v, floatArray);
		
		v = writer.findVariable("groupFlux");
		shape = v.getShape();
		floatArray = new ArrayFloat.D2(shape[0], shape[1]);
		ima = floatArray.getIndex();
		for(int i=0; i<shape[0]; i++) {
			for(int j=0; j<shape[1]; j++) {
				floatArray.setFloat(ima.set(i, j), groupFluxes[i][j]);
			}
		}
		writer.write(v, floatArray);			
		
		// Set datetime dimension
		v = writer.findVariable("datetime");
		shape = v.getShape();
		charArray = new ArrayChar.D2(shape[0], shape[1]);
		ima = charArray.getIndex();
		for (int i=0; i<shape[0]; i++) {
			charArray.setString(ima.set(i), datetimes[i]);
		}
		writer.write(v, charArray);
		
		// Set nodeName dimension
		v = writer.findVariable("nodeName");
		shape = v.getShape();
		charArray = new ArrayChar.D2(shape[0], shape[1]);
		ima = charArray.getIndex();
		for (int i=0; i<shape[0]; i++) {
			charArray.setString(ima.set(i), fluxAtNode[i].getInfo().getName());
		}
		writer.write(v, charArray);
		
		// Set groupName dimension
		v = writer.findVariable("groupName");
		shape = v.getShape();
		charArray = new ArrayChar.D2(shape[0], shape[1]);
		ima = charArray.getIndex();
		for(int i=0; i<shape[0]; i++) {
			charArray.setString(ima.set(i), groupAtNode[i].getInfo().getName());
		}
		writer.write(v, charArray);
		
		System.out.println("Done writing flux.");
	}

	//  protected native void initializeFluxOutput(int startTime);
	//  protected native void writeFluxOutput();
	//  protected native void closeFluxOutput();
	//  protected native void setFlux(int fluxId, float fluxValue);
	//  protected native void setGroup(int groupId, float groupValue);

	protected final int MAX_FLUX_OUTPUT=50;
	protected final int MAX_GROUP_OUTPUT=50;
	protected boolean groupPercent;
	protected boolean fluxPercent;
	protected int nFlux;
	protected int nGroup;
	protected Flux [] fluxAtNode;
	protected Flux [] groupAtNode;
	protected Flux [] genericFlux;
}

