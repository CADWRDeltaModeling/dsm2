#include "input_storage.h"
#include "ApplicationTextReader.h"
#include "InputState.h"
#include "ItemInputState.h"
#include "IncludeFileState.h"
#include <string>


//////////////////////
ApplicationTextReader::InputStateMap input_state_map()
{ 
  ApplicationTextReader::InputStateMap inputMap;

  //Add all available item readers to map
   InputStatePtr envvarPtr(new ItemInputState<envvar>());
    inputMap["ENVVAR"] = envvarPtr;

   InputStatePtr scalarPtr(new ItemInputState<scalar>());
    inputMap["SCALAR"] = scalarPtr;

   InputStatePtr channelPtr(new ItemInputState<channel>());
    inputMap["CHANNEL"] = channelPtr;

   InputStatePtr xsectPtr(new ItemInputState<xsect>());
    inputMap["XSECT"] = xsectPtr;

   InputStatePtr xsect_layerPtr(new ItemInputState<xsect_layer>());
    inputMap["XSECT_LAYER"] = xsect_layerPtr;

   InputStatePtr reservoirPtr(new ItemInputState<reservoir>());
    inputMap["RESERVOIR"] = reservoirPtr;

   InputStatePtr reservoir_connectionPtr(new ItemInputState<reservoir_connection>());
    inputMap["RESERVOIR_CONNECTION"] = reservoir_connectionPtr;

   InputStatePtr gatePtr(new ItemInputState<gate>());
    inputMap["GATE"] = gatePtr;

   InputStatePtr gate_pipe_devicePtr(new ItemInputState<gate_pipe_device>());
    inputMap["GATE_PIPE_DEVICE"] = gate_pipe_devicePtr;

   InputStatePtr gate_weir_devicePtr(new ItemInputState<gate_weir_device>());
    inputMap["GATE_WEIR_DEVICE"] = gate_weir_devicePtr;

   InputStatePtr transferPtr(new ItemInputState<transfer>());
    inputMap["TRANSFER"] = transferPtr;

   InputStatePtr io_filePtr(new ItemInputState<io_file>());
    inputMap["IO_FILE"] = io_filePtr;

   InputStatePtr tidefilePtr(new ItemInputState<tidefile>());
    inputMap["TIDEFILE"] = tidefilePtr;

   InputStatePtr groupPtr(new ItemInputState<group>());
    inputMap["GROUP"] = groupPtr;

   InputStatePtr group_memberPtr(new ItemInputState<group_member>());
    inputMap["GROUP_MEMBER"] = group_memberPtr;

   InputStatePtr channel_icPtr(new ItemInputState<channel_ic>());
    inputMap["CHANNEL_IC"] = channel_icPtr;

   InputStatePtr reservoir_icPtr(new ItemInputState<reservoir_ic>());
    inputMap["RESERVOIR_IC"] = reservoir_icPtr;

   InputStatePtr operating_rulePtr(new ItemInputState<operating_rule>());
    inputMap["OPERATING_RULE"] = operating_rulePtr;

   InputStatePtr oprule_expressionPtr(new ItemInputState<oprule_expression>());
    inputMap["OPRULE_EXPRESSION"] = oprule_expressionPtr;

   InputStatePtr oprule_time_seriesPtr(new ItemInputState<oprule_time_series>());
    inputMap["OPRULE_TIME_SERIES"] = oprule_time_seriesPtr;

   InputStatePtr rate_coefficientPtr(new ItemInputState<rate_coefficient>());
    inputMap["RATE_COEFFICIENT"] = rate_coefficientPtr;

   InputStatePtr particle_insertionPtr(new ItemInputState<particle_insertion>());
    inputMap["PARTICLE_INSERTION"] = particle_insertionPtr;

   InputStatePtr particle_filterPtr(new ItemInputState<particle_filter>());
    inputMap["PARTICLE_FILTER"] = particle_filterPtr;

   InputStatePtr particle_res_filterPtr(new ItemInputState<particle_res_filter>());
    inputMap["PARTICLE_RES_FILTER"] = particle_res_filterPtr;

   InputStatePtr particle_flux_outputPtr(new ItemInputState<particle_flux_output>());
    inputMap["PARTICLE_FLUX_OUTPUT"] = particle_flux_outputPtr;

   InputStatePtr particle_group_outputPtr(new ItemInputState<particle_group_output>());
    inputMap["PARTICLE_GROUP_OUTPUT"] = particle_group_outputPtr;

   InputStatePtr input_climatePtr(new ItemInputState<input_climate>());
    inputMap["INPUT_CLIMATE"] = input_climatePtr;

   InputStatePtr input_transfer_flowPtr(new ItemInputState<input_transfer_flow>());
    inputMap["INPUT_TRANSFER_FLOW"] = input_transfer_flowPtr;

   InputStatePtr input_gatePtr(new ItemInputState<input_gate>());
    inputMap["INPUT_GATE"] = input_gatePtr;

   InputStatePtr boundary_stagePtr(new ItemInputState<boundary_stage>());
    inputMap["BOUNDARY_STAGE"] = boundary_stagePtr;

   InputStatePtr boundary_flowPtr(new ItemInputState<boundary_flow>());
    inputMap["BOUNDARY_FLOW"] = boundary_flowPtr;

   InputStatePtr source_flowPtr(new ItemInputState<source_flow>());
    inputMap["SOURCE_FLOW"] = source_flowPtr;

   InputStatePtr source_flow_reservoirPtr(new ItemInputState<source_flow_reservoir>());
    inputMap["SOURCE_FLOW_RESERVOIR"] = source_flow_reservoirPtr;

   InputStatePtr node_concentrationPtr(new ItemInputState<node_concentration>());
    inputMap["NODE_CONCENTRATION"] = node_concentrationPtr;

   InputStatePtr reservoir_concentrationPtr(new ItemInputState<reservoir_concentration>());
    inputMap["RESERVOIR_CONCENTRATION"] = reservoir_concentrationPtr;

   InputStatePtr output_channelPtr(new ItemInputState<output_channel>());
    inputMap["OUTPUT_CHANNEL"] = output_channelPtr;

   InputStatePtr output_reservoirPtr(new ItemInputState<output_reservoir>());
    inputMap["OUTPUT_RESERVOIR"] = output_reservoirPtr;

   InputStatePtr output_channel_source_trackPtr(new ItemInputState<output_channel_source_track>());
    inputMap["OUTPUT_CHANNEL_SOURCE_TRACK"] = output_channel_source_trackPtr;

   InputStatePtr output_reservoir_source_trackPtr(new ItemInputState<output_reservoir_source_track>());
    inputMap["OUTPUT_RESERVOIR_SOURCE_TRACK"] = output_reservoir_source_trackPtr;

   InputStatePtr output_gatePtr(new ItemInputState<output_gate>());
    inputMap["OUTPUT_GATE"] = output_gatePtr;

  

  // Include file definitions
    vector<string> configurationContextItems;
    configurationContextItems.push_back("ENVVAR");
    InputStatePtr configurationPtr(new IncludeFileState(configurationContextItems));
    inputMap["CONFIGURATION"] = configurationPtr;
    vector<string> parameterContextItems;
    parameterContextItems.push_back("SCALAR");
    InputStatePtr parameterPtr(new IncludeFileState(parameterContextItems));
    inputMap["PARAMETER"] = parameterPtr;
    vector<string> gridContextItems;
    gridContextItems.push_back("CHANNEL");
    gridContextItems.push_back("XSECT");
    gridContextItems.push_back("XSECT_LAYER");
    gridContextItems.push_back("RESERVOIR");
    gridContextItems.push_back("RESERVOIR_CONNECTION");
    gridContextItems.push_back("GATE");
    gridContextItems.push_back("GATE_WEIR_DEVICE");
    gridContextItems.push_back("GATE_PIPE_DEVICE");
    gridContextItems.push_back("TRANSFER");
    gridContextItems.push_back("SOURCE_FLOW_RESERVOIR");
    gridContextItems.push_back("INPUT_GATE");
    gridContextItems.push_back("INPUT_TRANSFER_FLOW");
    InputStatePtr gridPtr(new IncludeFileState(gridContextItems));
    inputMap["GRID"] = gridPtr;
    vector<string> initial_conditionContextItems;
    initial_conditionContextItems.push_back("CHANNEL_IC");
    initial_conditionContextItems.push_back("RESERVOIR_IC");
    InputStatePtr initial_conditionPtr(new IncludeFileState(initial_conditionContextItems));
    inputMap["INITIAL_CONDITION"] = initial_conditionPtr;
    vector<string> hydro_time_seriesContextItems;
    hydro_time_seriesContextItems.push_back("BOUNDARY_STAGE");
    hydro_time_seriesContextItems.push_back("BOUNDARY_FLOW");
    hydro_time_seriesContextItems.push_back("SOURCE_FLOW");
    hydro_time_seriesContextItems.push_back("SOURCE_FLOW_RESERVOIR");
    hydro_time_seriesContextItems.push_back("INPUT_GATE");
    hydro_time_seriesContextItems.push_back("INPUT_TRANSFER_FLOW");
    hydro_time_seriesContextItems.push_back("RESERVOIR");
    hydro_time_seriesContextItems.push_back("RESERVOIR_CONNECTION");
    hydro_time_seriesContextItems.push_back("GATE");
    hydro_time_seriesContextItems.push_back("GATE_WEIR_DEVICE");
    hydro_time_seriesContextItems.push_back("GATE_PIPE_DEVICE");
    hydro_time_seriesContextItems.push_back("TRANSFER");
    InputStatePtr hydro_time_seriesPtr(new IncludeFileState(hydro_time_seriesContextItems));
    inputMap["HYDRO_TIME_SERIES"] = hydro_time_seriesPtr;
    vector<string> operationContextItems;
    operationContextItems.push_back("OPERATING_RULE");
    operationContextItems.push_back("OPRULE_EXPRESSION");
    operationContextItems.push_back("OPRULE_TIME_SERIES");
    InputStatePtr operationPtr(new IncludeFileState(operationContextItems));
    inputMap["OPERATION"] = operationPtr;
    vector<string> groupsContextItems;
    groupsContextItems.push_back("GROUP");
    groupsContextItems.push_back("GROUP_MEMBER");
    InputStatePtr groupsPtr(new IncludeFileState(groupsContextItems));
    inputMap["GROUPS"] = groupsPtr;
    vector<string> qual_time_seriesContextItems;
    qual_time_seriesContextItems.push_back("NODE_CONCENTRATION");
    qual_time_seriesContextItems.push_back("RESERVOIR_CONCENTRATION");
    qual_time_seriesContextItems.push_back("INPUT_CLIMATE");
    InputStatePtr qual_time_seriesPtr(new IncludeFileState(qual_time_seriesContextItems));
    inputMap["QUAL_TIME_SERIES"] = qual_time_seriesPtr;
    vector<string> qual_spatialContextItems;
    qual_spatialContextItems.push_back("RATE_COEFFICIENT");
    InputStatePtr qual_spatialPtr(new IncludeFileState(qual_spatialContextItems));
    inputMap["QUAL_SPATIAL"] = qual_spatialPtr;
    vector<string> gtm_time_seriesContextItems;
    gtm_time_seriesContextItems.push_back("NODE_CONCENTRATION");
    gtm_time_seriesContextItems.push_back("RESERVOIR_CONCENTRATION");
    gtm_time_seriesContextItems.push_back("INPUT_CLIMATE");
    InputStatePtr gtm_time_seriesPtr(new IncludeFileState(gtm_time_seriesContextItems));
    inputMap["GTM_TIME_SERIES"] = gtm_time_seriesPtr;
    vector<string> gtm_spatialContextItems;
    gtm_spatialContextItems.push_back("RATE_COEFFICIENT");
    InputStatePtr gtm_spatialPtr(new IncludeFileState(gtm_spatialContextItems));
    inputMap["GTM_SPATIAL"] = gtm_spatialPtr;
    vector<string> output_time_seriesContextItems;
    output_time_seriesContextItems.push_back("OUTPUT_CHANNEL");
    output_time_seriesContextItems.push_back("OUTPUT_RESERVOIR");
    output_time_seriesContextItems.push_back("OUTPUT_GATE");
    output_time_seriesContextItems.push_back("OUTPUT_CHANNEL_SOURCE_TRACK");
    output_time_seriesContextItems.push_back("OUTPUT_RESERVOIR_SOURCE_TRACK");
    InputStatePtr output_time_seriesPtr(new IncludeFileState(output_time_seriesContextItems));
    inputMap["OUTPUT_TIME_SERIES"] = output_time_seriesPtr;
    vector<string> particleContextItems;
    particleContextItems.push_back("PARTICLE_INSERTION");
    particleContextItems.push_back("PARTICLE_FILTER");
    particleContextItems.push_back("PARTICLE_RES_FILTER");
    particleContextItems.push_back("PARTICLE_GROUP_OUTPUT");
    particleContextItems.push_back("PARTICLE_FLUX_OUTPUT");
    InputStatePtr particlePtr(new IncludeFileState(particleContextItems));
    inputMap["PARTICLE"] = particlePtr;


  return inputMap;
}    


const std::vector<std::string> profile(const std::string& name)
{  
     std::vector<std::string> out;

    if(name =="PTM")
    {
        out.push_back("ENVVAR");
        out.push_back("SCALAR");
        out.push_back("IO_FILE");
        out.push_back("TIDEFILE");
        out.push_back("GROUP");
        out.push_back("GROUP_MEMBER");
        out.push_back("PARTICLE_INSERTION");
        out.push_back("PARTICLE_FILTER");
        out.push_back("PARTICLE_RES_FILTER");
        out.push_back("PARTICLE_GROUP_OUTPUT");
        out.push_back("PARTICLE_FLUX_OUTPUT");
        out.push_back("CONFIGURATION");
        out.push_back("PARAMETER");
        out.push_back("GROUPS");
        out.push_back("PARTICLE");
    }
    
    if(name =="GTM")
    {
        out.push_back("ENVVAR");
        out.push_back("SCALAR");
        out.push_back("IO_FILE");
        out.push_back("TIDEFILE");
        out.push_back("NODE_CONCENTRATION");
        out.push_back("RESERVOIR_CONCENTRATION");
        out.push_back("INPUT_CLIMATE");
        out.push_back("GROUP");
        out.push_back("GROUP_MEMBER");
        out.push_back("RATE_COEFFICIENT");
        out.push_back("OUTPUT_CHANNEL");
        out.push_back("OUTPUT_RESERVOIR");
        out.push_back("OUTPUT_CHANNEL_SOURCE_TRACK");
        out.push_back("OUTPUT_RESERVOIR_SOURCE_TRACK");
        out.push_back("CONFIGURATION");
        out.push_back("PARAMETER");
        out.push_back("GTM_TIME_SERIES");
        out.push_back("GROUPS");
        out.push_back("GTM_SPATIAL");
        out.push_back("OUTPUT_TIME_SERIES");
    }
    
    if(name =="Qual")
    {
        out.push_back("ENVVAR");
        out.push_back("SCALAR");
        out.push_back("IO_FILE");
        out.push_back("TIDEFILE");
        out.push_back("NODE_CONCENTRATION");
        out.push_back("RESERVOIR_CONCENTRATION");
        out.push_back("INPUT_CLIMATE");
        out.push_back("GROUP");
        out.push_back("GROUP_MEMBER");
        out.push_back("RATE_COEFFICIENT");
        out.push_back("OUTPUT_CHANNEL");
        out.push_back("OUTPUT_RESERVOIR");
        out.push_back("OUTPUT_CHANNEL_SOURCE_TRACK");
        out.push_back("OUTPUT_RESERVOIR_SOURCE_TRACK");
        out.push_back("CONFIGURATION");
        out.push_back("PARAMETER");
        out.push_back("QUAL_TIME_SERIES");
        out.push_back("GROUPS");
        out.push_back("QUAL_SPATIAL");
        out.push_back("OUTPUT_TIME_SERIES");
    }
    
    if(name =="Grid")
    {
        out.push_back("CHANNEL");
        out.push_back("XSECT");
        out.push_back("XSECT_LAYER");
        out.push_back("RESERVOIR");
        out.push_back("RESERVOIR_CONNECTION");
        out.push_back("GATE");
        out.push_back("GATE_WEIR_DEVICE");
        out.push_back("GATE_PIPE_DEVICE");
        out.push_back("TRANSFER");
        out.push_back("SOURCE_FLOW_RESERVOIR");
        out.push_back("INPUT_GATE");
        out.push_back("INPUT_TRANSFER_FLOW");
        out.push_back("GRID");
    }
    
    if(name =="envvar")
    {
        out.push_back("ENVVAR");
        out.push_back("CONFIGURATION");
    }
    
    if(name =="Hydro")
    {
        out.push_back("ENVVAR");
        out.push_back("SCALAR");
        out.push_back("IO_FILE");
        out.push_back("CHANNEL");
        out.push_back("XSECT");
        out.push_back("XSECT_LAYER");
        out.push_back("RESERVOIR");
        out.push_back("RESERVOIR_CONNECTION");
        out.push_back("GATE");
        out.push_back("GATE_WEIR_DEVICE");
        out.push_back("GATE_PIPE_DEVICE");
        out.push_back("TRANSFER");
        out.push_back("CHANNEL_IC");
        out.push_back("RESERVOIR_IC");
        out.push_back("BOUNDARY_STAGE");
        out.push_back("BOUNDARY_FLOW");
        out.push_back("SOURCE_FLOW");
        out.push_back("SOURCE_FLOW_RESERVOIR");
        out.push_back("INPUT_GATE");
        out.push_back("INPUT_TRANSFER_FLOW");
        out.push_back("OPERATING_RULE");
        out.push_back("OPRULE_EXPRESSION");
        out.push_back("OPRULE_TIME_SERIES");
        out.push_back("OUTPUT_CHANNEL");
        out.push_back("OUTPUT_RESERVOIR");
        out.push_back("OUTPUT_GATE");
        out.push_back("CONFIGURATION");
        out.push_back("PARAMETER");
        out.push_back("GRID");
        out.push_back("INITIAL_CONDITION");
        out.push_back("OPERATION");
        out.push_back("HYDRO_TIME_SERIES");
        out.push_back("OUTPUT_TIME_SERIES");
    }
    
    
    if (out.empty()){
       std::string message("Input profile name not found or is empty (case sensitivity?):\n");
       message += name;
       
       throw logic_error(message);
    }  
    return out;
}




