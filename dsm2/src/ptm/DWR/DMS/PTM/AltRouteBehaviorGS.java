package DWR.DMS.PTM;
import java.util.HashMap;

/**
 * BioPTMrouteBehavior subclass configured for the Georgiana Slough junction.
 *
 * @author Doug Jackson, QEDA Consulting, LLC
 */
public class AltRouteBehaviorGS extends AltRouteBehavior {
	
	public AltRouteBehaviorGS(RouteInputs rIn, Integer nodeId) {
		super(rIn, nodeId);
		
		className = "BioPTMrouteBehaviorGS";
		junction = "GS";
        
        // Verify that the behavior inputs are appropriate for bioPTM
        checkInputs();        
		
		channelNames = new String[] {"SACUPDCC", "SACUPGS", "DCC", "GS", "SACDOWNGS"};
		channelCrossSectionMap = new HashMap<String, String>();
		channelCrossSectionMap.put("SACUPGS", "GSinsertion1");
		channelCrossSectionMap.put("SACDOWNGS", "SacGS1");
		channelCrossSectionMap.put("GS", "GS1");
		upDown = "DOWN";
		
		upChannelIndex = 1;
		downChannelIndex = 4;
		branchChannelIndex = 3;
		
        // Create the junction interface
        junctionInterface = new AltRouteInterface(junction);
                
        System.out.println("Created " + className);
	}
}

