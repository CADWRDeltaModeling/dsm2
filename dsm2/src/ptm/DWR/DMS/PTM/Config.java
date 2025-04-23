package DWR.DMS.PTM;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import ucar.ma2.ArrayBoolean;
import ucar.ma2.ArrayChar;
import ucar.ma2.ArrayDouble;
import ucar.ma2.ArrayInt;
import ucar.ma2.DataType;
import ucar.ma2.Index;
import ucar.ma2.InvalidRangeException;
import ucar.nc2.Dimension;
import ucar.nc2.Variable;
import ucar.nc2.Group;
import ucar.nc2.write.NetcdfFormatWriter;

/**
 * Top-level class defining the structure of the YAML file for the Jackson project parser
 * 
 * @author Doug Jackson (QEDA Consulting, LLC)
 */
public class Config {

	public static final int PARTICLE_FLUX_NAME_INDEX = 0;
	public static final int PARTICLE_FLUX_FROM_WB_INDEX = 1;
	public static final int PARTICLE_FLUX_FROM_WB_TYPE_INDEX = 2;
	public static final int PARTICLE_FLUX_TO_WB_INDEX = 3;
	public static final int PARTICLE_FLUX_TO_WB_TYPE_INDEX = 4;

	public static final int GROUPS_NAME_INDEX = 0;
	public static final int GROUPS_TYPE_INDEX = 1;
	public static final int GROUPS_PATTERN_INDEX = 2;

	public static final int PARTICLE_GROUP_OUTPUT_NAME_INDEX = 0;
	public static final int PARTICLE_GROUP_OUTPUT_GROUPNAME_INDEX = 1;

	private List<String> valuesSet;

	public String tidefile;

	// ECO-PTM behavior configuration parameters
	public String particle_type;
	public String time_zone;
	public boolean use_new_random_seed;

	public String travel_time_output_path;
	public String[] travel_time_header;
	public List<List<Object>> travel_time;

	public List<ReleaseGroup> release_groups;

	public String[] particle_insertion_header;
	public List<List<Object>> particle_insertion;

	public String sunrise;
	public String sunset;
	public float stst_threshold;
	public int tidal_cycles_to_calculate_channel_direction;
	public float confusion_probability_constant;
	public float max_confusion_probability;
	public float confusion_probability_slope;
	public boolean random_assess;
	public float assess_probability;
	public int stuck_threshold;

	public String[] swimming_vel_header;
	public List<List<Object>> swimming_vel;
	public List<ChannelGroup> channel_groups;

	public String output_path_entrainment;
	public String trans_probs_path;
	public String output_path_flux;

	public String[] channel_name_lookup_header;
	public List<List<Object>> channel_name_lookup;

	public String[] special_behavior_header;
	public List<List<Object>> special_behavior;

	public List<Barrier> barriers; 

	public float dicu_filter_efficiency;

	public String[] fish_screens_header;
	public List<List<Integer>> fish_screens;

	public String survival_output_path;
	public List<SurvivalGroup> survival_groups;

	public String simulation_start_date;
	public String simulation_scenario;

	public String[] particle_flux_header;
	public List<List<Object>> particle_flux;

	public String[] individual_route_survival_header;
	public List<List<Object>> individual_route_survival;

	public String route_survival_output_path;
	public String[] route_survival_equations_header;
	public List<List<Object>> route_survival_equations;
	public boolean show_route_survival_detail;

	public String[] individual_reach_survival_header;
	public List<List<Object>> individual_reach_survival;

	public String fates_output_path;
	public String[] exit_stations;

	public boolean display_simulation_timestep_write_all;
	public boolean flux_write_all;
	public boolean entrainment_write_all;
	public boolean survival_write_all;
	public boolean route_survival_write_all;
	public boolean fates_write_all;
	public boolean survival_detail_write_all;

	// PTM configuration parameters
	public String ptm_start_date;
	public String ptm_start_time;
	public String ptm_end_date;
	public String ptm_end_time;
	public String ptm_time_step;
	public String display_intvl;

	public float theta;

	public boolean ptm_ivert;
	public boolean ptm_itrans;
	public boolean ptm_iey;
	public boolean ptm_iez;
	public boolean ptm_iprof;

	public boolean ptm_igroup;

	public boolean ptm_flux_percent;
	public boolean ptm_group_percent;
	public boolean ptm_flux_cumulative;

	public int ptm_random_seed;
	public float ptm_trans_constant;
	public float ptm_vert_constant;
	public float ptm_trans_a_coef;
	public float ptm_trans_b_coef;
	public float ptm_trans_c_coef;

	public int ptm_num_animated;

	public String[] particle_group_output_header;
	public List<List<Object>> particle_group_output;

	public String[] particle_flux_output_header;
	public List<List<Object>> particle_flux_output;

	public String[] groups_header;
	public List<List<Object>> groups;

	public List<IOfileLine> io_file;

	public String smelt_input_filename;
	
	public String cross_stream_frac_method;
	public float cross_stream_frac_beta_a;
	public float cross_stream_frac_beta_b;

	public Config() {
		valuesSet = new ArrayList<>();
	}

	public void setSunrise(String sunrise) {
		this.sunrise = sunrise;
		valuesSet.add("sunrise");
	}

	public void setSunset(String sunset) {
		this.sunset = sunset;
		valuesSet.add("sunset");
	}

	public void setStst_threshold(float stst_threshold) {
		this.stst_threshold = stst_threshold;
		valuesSet.add("stst_threshold");
	}
	public void setTidal_cycles_to_calculate_channel_direction(int tidal_cycles_to_calculate_channel_direction) {
		this.tidal_cycles_to_calculate_channel_direction = tidal_cycles_to_calculate_channel_direction;
		valuesSet.add("tidal_cycles_to_calculate_channel_direction");
	}

	public void setConfusion_probability_constant(float confusion_probability_constant) {
		this.confusion_probability_constant = confusion_probability_constant;
		valuesSet.add("confusion_probability_constant");
	}

	public void setMax_confusion_probability(float max_confusion_probability) {
		this.max_confusion_probability = max_confusion_probability;
		valuesSet.add("max_confusion_probability");
	}

	public void setConfusion_probability_slope(float confusion_probability_slope) {
		this.confusion_probability_slope = confusion_probability_slope;
		valuesSet.add("confusion_probability_slope");
	}

	public void setRandom_assess(boolean random_assess) {
		this.random_assess = random_assess;
		valuesSet.add("random_assess");
	}

	public void setAssess_probability(float assess_probability) {
		this.assess_probability = assess_probability;
		valuesSet.add("assess_probability");
	}

	public void setStuck_threshold(int stuck_threshold) {
		this.stuck_threshold = stuck_threshold;
		valuesSet.add("stuck_threshold");
	}

	public void setDicu_filter_efficiency(int dicu_filter_efficiency) {
		this.dicu_filter_efficiency = dicu_filter_efficiency;
		valuesSet.add("dicu_filter_efficiency");
	}

	public void setDisplay_simulation_timestep_write_all(boolean display_simulation_timestep_write_all) {
		this.display_simulation_timestep_write_all = display_simulation_timestep_write_all;
		valuesSet.add("display_simulation_timestep_write_all");
	}

	public void setFlux_write_all(boolean flux_write_all) {
		this.flux_write_all = flux_write_all;
		valuesSet.add("flux_write_all");
	}

	public void setEntrainment_write_all(boolean entrainment_write_all) {
		this.entrainment_write_all = entrainment_write_all;
		valuesSet.add("entrainment_write_all");
	}

	public void setSurvival_write_all(boolean survival_write_all) {
		this.survival_write_all = survival_write_all;
		valuesSet.add("survival_write_all");
	}
	
	public void setRoute_survival_write_all(boolean route_survival_write_all) {
		this.route_survival_write_all = route_survival_write_all;
		valuesSet.add("route_survival_write_all");
	}
	
	public void setFates_write_all(boolean fates_write_all) {
		this.fates_write_all = fates_write_all;
		valuesSet.add("fates_write_all");
	}
	
	public void setSurvival_detail_write_all(boolean survival_detail_write_all) {
		this.survival_detail_write_all = survival_detail_write_all;
		valuesSet.add("survival_detail_write_all");
	}
	
	public void setCross_stream_frac_method(String cross_stream_frac_method) {
		this.cross_stream_frac_method = cross_stream_frac_method;
		valuesSet.add("cross_stream_frac_method");
	}
	
	public void setCross_stream_frac_beta_a(float cross_stream_frac_beta_a) {
		this.cross_stream_frac_beta_a = cross_stream_frac_beta_a;
		valuesSet.add("cross_stream_frac_beta_a");
	}
	
	public void setCross_stream_frac_beta_b(float cross_stream_frac_beta_b) {
		this.cross_stream_frac_beta_b = cross_stream_frac_beta_b;
		valuesSet.add("cross_stream_frac_beta_b");
	}

	/**
	 * Check if parameter has been explicitly specified in the config file
	 * @param parameter				parameter name
	 * @return						boolean indicating whether the parameter was set explicitly
	 */
	public boolean isSet(String parameter) {
		return valuesSet.contains(parameter);
	}

	/**
	 * Inner class defining the structure of the release groups
	 */
	public static class ReleaseGroup {
		public String name;
		public String[] release_loc_header;
		public List<Object> release_loc;
		public String[] releases_header;
		public List<List<Object>> releases;
	}

	/**
	 * Inner class defining the structure of the channel groups
	 */
	public static class ChannelGroup {
		public String name;
		public int[] channels;
	}

	/**
	 * Obtain the list of channelIDs associated with the specified channel group
	 * @param name					name of the channel group
	 * @return						list of channelIDs
	 */
	public int[] getChannels(String name) {
		for(ChannelGroup c : channel_groups) {
			if(c.name.equalsIgnoreCase(name)) {
				return c.channels;
			}
		}
		PTMUtil.systemExit("Could not find " + name + " in channel_groups. System exit.");
		return null;
	}

	/**
	 * Inner class defining the structure of the barrier information
	 */
	public static class Barrier {
		public String name;
		public int nodeID;
		public int waterbodyID;
		public String[] schedule_header;
		public List<List<Object>> schedule;
	}

	/**
	 * Inner class defining the structure of the survival groups
	 */
	public static class SurvivalGroup {
		public int number;
		public String name;
		public List<List<Object>> start_stations;
		public List<List<Object>> end_stations;
		public List<List<Object>> exchangeable_start_stations;
		public String[] survival_params_header;
		public List<List<Object>> survival_params;
		public String[] barriers_header;
		public List<String> barriers;
	}

	/**
	 * Inner class defining the structure of the I/O file lines
	 */
	public static class IOfileLine {
		public String type;
		public String interval;
		public String file;
	}
	
	/**
	 * Build config components of netCDF output file
	 * @param builder				NetcdfFormatWriter.Builder
	 */
	public void buildOutput(NetcdfFormatWriter.Builder builder) {
		Dimension strLen10, strLen50, strLen100, strLen300, strLen10000, travelTimeColDim, travelTimeRowDim,
			releaseLocColDim, releaseLocRowDim, releasesColDim, releasesRowDim, swimmingVelColDim, swimmingVelRowDim,
			channelGroupsColDim, channelGroupsRowDim, channelNameLookupColDim, channelNameLookupRowDim,
			specialBehaviorColDim, specialBehaviorRowDim, barriersColDim, barriersRowDim, fishScreensColDim, fishScreensRowDim, 
			startStationsColDim, startStationsRowDim, endStationsColDim, endStationsRowDim, exchStationsColDim, exchStationsRowDim,
			survParamsColDim, survParamsRowDim, particleFluxColDim, particleFluxRowDim, indRouteSurvColDim, indRouteSurvRowDim,
			routeSurvEqColDim, routeSurvEqRowDim, indReachSurvColDim, indReachSurvRowDim, exitStationsDim,
			particleGroupOutputColDim, particleGroupOutputRowDim, particleFluxOutputColDim, particleFluxOutputRowDim,
			groupsColDim, groupsRowDim, ioFileColDim, ioFileRowDim;
		int totalReleases, totalChannelGroups, totalBarrierSchedules, totalSurvivalGroups, totalEndStations, totalExchStations;
		
		// Dimensions for storing strings
		strLen10 = builder.addDimension("strLen10", 10);
		strLen50 = builder.addDimension("strLen50", 50);
		strLen100 = builder.addDimension("strLen100", 100);
		strLen300 = builder.addDimension("strLen300", 300);
		strLen10000 = builder.addDimension("strLen10000", 10000);
		
		travelTimeColDim = addDimension(builder, "travelTimeCol", this.travel_time_header);
		travelTimeRowDim = addDimension(builder, "travelTimeRow", this.travel_time);
		
		// Dimensions for release_groups:release_loc and release_groups:releases
		// Include one column for the group name
		totalReleases = 0;
		if(this.release_groups!=null) {
			releaseLocColDim = addDimension(builder, "releaseLocCol", this.release_groups.get(0).release_loc_header, 1);
			releasesColDim = addDimension(builder, "releasesCol", this.release_groups.get(0).releases_header, 1);
			for(ReleaseGroup rG : this.release_groups) {
				totalReleases+=rG.releases.size();
			}
		}
		else {
			releaseLocColDim = builder.addDimension("releaseLocCol", 0);
			releasesColDim = builder.addDimension("releasesCol", 0);
		}
		releaseLocRowDim = addDimension(builder, "releaseLocRow", this.release_groups);
		releasesRowDim = builder.addDimension("releasesRow", totalReleases);
		
		swimmingVelColDim = addDimension(builder, "swimmingVelCol", this.swimming_vel_header);
		swimmingVelRowDim = addDimension(builder, "swimmingVelRow", this.swimming_vel);
		
		channelNameLookupColDim = addDimension(builder, "channelNameLookupCol", this.channel_name_lookup_header);
		channelNameLookupRowDim = addDimension(builder, "channelNameLookupRow", this.channel_name_lookup);
		
		specialBehaviorColDim = addDimension(builder, "specialBehaviorCol", this.special_behavior_header);
		specialBehaviorRowDim = addDimension(builder, "specialBehaviorRow", this.special_behavior);
		
		// Dimensions for channel_groups
		// Include one column for the group name
		channelGroupsColDim = builder.addDimension("channelGroupsCol", 2);
		totalChannelGroups = 0;
		if(this.channel_groups!=null) {
			for(ChannelGroup cG : this.channel_groups) {
				totalChannelGroups+=cG.channels.length;
			}
		}
		channelGroupsRowDim = builder.addDimension("channelGroupsRow", totalChannelGroups);
		
		// Dimensions for barriers
		// Include columns for name, nodeID, and waterbodyID
		totalBarrierSchedules = 0;
		if(this.barriers!=null) {
			barriersColDim = addDimension(builder, "barriersCol", this.barriers.get(0).schedule_header, 3);
			for(Barrier b : this.barriers) {
				totalBarrierSchedules+=b.schedule.size();
			}
		}
		else {
			barriersColDim = builder.addDimension("barriersCol", 0);
		}
		barriersRowDim = builder.addDimension("barriersRow", totalBarrierSchedules);
		
		fishScreensColDim = addDimension(builder, "fishScreensCol", this.fish_screens_header);
		fishScreensRowDim = addDimension(builder, "fishScreensRow", this.fish_screens);
		
		// Dimensions for survival_groups
		// Include columns for group number and name
		totalSurvivalGroups = this.survival_groups!=null ? this.survival_groups.size() : 0;
		startStationsRowDim = builder.addDimension("startStationsRow", totalSurvivalGroups);
		// Use a 4 here (name, number, channel, distance) because we can't know which group will actually have exchangeable_start_stations
		exchStationsColDim = builder.addDimension("exchStationsCol", 4);
		totalEndStations = 0;
		totalExchStations = 0;
		if(this.survival_groups!=null) {
			startStationsColDim = addDimension(builder, "startStationsCol", this.survival_groups.get(0).start_stations.get(0), 2);
			endStationsColDim = addDimension(builder, "endStationsCol", this.survival_groups.get(0).end_stations.get(0), 2);
			survParamsColDim = addDimension(builder, "survParamsCol", this.survival_groups.get(0).survival_params_header, 2);
			for(SurvivalGroup sG : this.survival_groups) {
				totalEndStations+=sG.end_stations.size();
				if(sG.exchangeable_start_stations!=null) {
					totalExchStations+=sG.exchangeable_start_stations.size();
				}
			}
		}
		else {
			startStationsColDim = builder.addDimension("startStationsCol", 0);
			endStationsColDim = builder.addDimension("endStationsCol", 0);
			survParamsColDim = builder.addDimension("survParamsCol", 0);
		}
		endStationsRowDim = builder.addDimension("endStationsRow", totalEndStations);
		exchStationsRowDim = builder.addDimension("exchStationsRow", totalExchStations);
		survParamsRowDim = builder.addDimension("survParamsRow", totalEndStations);
		
		particleFluxColDim = addDimension(builder, "particleFluxCol", this.particle_flux_header);
		particleFluxRowDim = addDimension(builder, "particleFluxRow", this.particle_flux);
		
		indRouteSurvColDim = addDimension(builder, "indRouteSurvCol", this.individual_route_survival_header);
		indRouteSurvRowDim = addDimension(builder, "indRouteSurvRow", this.individual_route_survival);
		
		routeSurvEqColDim = addDimension(builder, "routeSurvEqCol", this.route_survival_equations_header);
		routeSurvEqRowDim = addDimension(builder, "routeSurvEqRow", this.route_survival_equations);
		
		indReachSurvColDim = addDimension(builder, "indReachSurvCol", this.individual_reach_survival_header);
		indReachSurvRowDim = addDimension(builder, "indReachSurvRow", this.individual_reach_survival);
		
		exitStationsDim = addDimension(builder, "exitStationsDim", this.exit_stations);
		
		particleGroupOutputColDim = addDimension(builder, "particleGroupOutputCol", this.particle_group_output_header);
		particleGroupOutputRowDim = addDimension(builder, "particleGroupOutputRow", this.particle_group_output);
		
		particleFluxOutputColDim = addDimension(builder, "particleFluxOutputCol", this.particle_flux_output_header);
		particleFluxOutputRowDim = addDimension(builder, "particleFluxOutputRow", this.particle_flux_output);
		
		groupsColDim = addDimension(builder, "groupsCol", this.groups_header);
		groupsRowDim = addDimension(builder, "groupsRow", this.groups);
		
		// columns: type, interval, file
		ioFileColDim = builder.addDimension("ioFileCol", 3);
		ioFileRowDim = this.io_file!=null ? builder.addDimension("ioFileRow", this.io_file.size()) : builder.addDimension("ioFileRow", 0);
		
		builder.addVariable("simulation_start_date", DataType.CHAR, "strLen50");
		builder.addVariable("simulation_scenario", DataType.CHAR, "strLen50");
		builder.addVariable("particle_type", DataType.CHAR, "strLen100");
		builder.addVariable("travel_time", DataType.CHAR, "travelTimeRow travelTimeCol strLen10");
		builder.addVariable("release_groups:release_loc", DataType.CHAR, "releaseLocRow releaseLocCol strLen50");
		builder.addVariable("release_groups:releases", DataType.CHAR, "releasesRow releasesCol strLen10");
		builder.addVariable("swimming_vel", DataType.CHAR, "swimmingVelRow swimmingVelCol strLen50");
		builder.addVariable("channel_groups", DataType.CHAR, "channelGroupsRow channelGroupsCol strLen50");
		builder.addVariable("channel_name_lookup", DataType.CHAR, "channelNameLookupRow channelNameLookupCol strLen50");
		builder.addVariable("special_behavior", DataType.CHAR, "specialBehaviorRow specialBehaviorCol strLen50");
		builder.addVariable("barriers", DataType.CHAR, "barriersRow barriersCol strLen50");
		builder.addVariable("fish_screens", DataType.INT, "fishScreensRow fishScreensCol");
		builder.addVariable("survival_groups:start_stations", DataType.CHAR, "startStationsRow startStationsCol strLen10");
		builder.addVariable("survival_groups:end_stations", DataType.CHAR, "endStationsRow endStationsCol strLen10");
		builder.addVariable("survival_groups:exchangeable_start_stations", DataType.CHAR, "exchStationsRow exchStationsCol strLen10");
		builder.addVariable("survival_groups:survival_params", DataType.CHAR, "survParamsRow survParamsCol strLen50");
		builder.addVariable("particle_flux", DataType.CHAR, "particleFluxRow particleFluxCol strLen10");
		builder.addVariable("individual_route_survival", DataType.CHAR, "indRouteSurvRow indRouteSurvCol strLen50");
		builder.addVariable("route_survival_equations", DataType.CHAR, "routeSurvEqRow routeSurvEqCol strLen10000");
		builder.addVariable("individual_reach_survival", DataType.CHAR, "indReachSurvRow indReachSurvCol strLen50");
		builder.addVariable("exit_stations", DataType.CHAR, "exitStationsDim strLen10");
		builder.addVariable("particle_group_output", DataType.CHAR, "particleGroupOutputRow particleGroupOutputCol strLen50");
		builder.addVariable("particle_flux_output", DataType.CHAR, "particleFluxOutputRow particleFluxOutputCol strLen50");
		builder.addVariable("groups", DataType.CHAR, "groupsRow groupsCol strLen50");
		builder.addVariable("io_file", DataType.CHAR, "ioFileRow ioFileCol strLen300");
		
		// strLen10
		for(String s: new String[] {"time_zone", "use_new_random_seed", "sunrise", "sunset", "random_assess", "ptm_end_date", 
				"ptm_end_time", "ptm_time_step", "display_intvl", "show_route_survival_detail", "ptm_start_date", "ptm_start_time"}) {
			builder.addVariable(s, DataType.CHAR, "strLen10");
		}
		
		// strLen300
		for(String s: new String[] {"tidefile", "travel_time_output_path", "output_path_entrainment", "trans_probs_path",
				"output_path_flux", "survival_output_path", "route_survival_output_path", "fates_output_path"}) {
			builder.addVariable(s, DataType.CHAR, "strLen300");
		}
		
		// Boolean variables
		for(String s : new String[] {"display_simulation_timestep_write_all", "flux_write_all", "entrainment_write_all", "survival_write_all",
				"ptm_ivert", "ptm_itrans", "ptm_iey", "ptm_iez", "ptm_iprof", "ptm_igroup", "ptm_flux_percent", "ptm_group_percent", "ptm_flux_cumulative"}) {
			builder.addVariable(s, DataType.CHAR, "strLen10");
		}
		
		// Scalars
		for(String varName : new String[] {"stst_threshold", "tidal_cycles_to_calculate_channel_direction", "confusion_probability_constant",
				"max_confusion_probability", "confusion_probability_slope", "assess_probability", "stuck_threshold",
				"dicu_filter_efficiency", "theta", "ptm_random_seed", "ptm_trans_constant", "ptm_vert_constant", "ptm_trans_a_coef",
				"ptm_trans_b_coef", "ptm_trans_c_coef", "ptm_num_animated"}) {
			builder.addVariable(varName, DataType.DOUBLE, new ArrayList<Dimension>());
		}	
		
		// Create column header dimensions
		for(String s : new String[] {"channelNameLookupCol", "startStationsCol", "endStationsCol", "exchStationsCol",
				"particleGroupOutputCol", "particleFluxOutputCol", "ioFileCol"}) {
			builder.addVariable(s, DataType.CHAR, s + " strLen10");
		}
		for(String s : new String[] {"travelTimeCol", "releaseLocCol", "releasesCol", "swimmingVelCol",
				"channelGroupsCol", "specialBehaviorCol", "barriersCol", "fishScreensCol", "survParamsCol",
				"particleFluxCol", "indRouteSurvCol", "routeSurvEqCol", "indReachSurvCol", "groupsCol"}) {
			builder.addVariable(s, DataType.CHAR, s + " strLen50");
		}
	}
	
	/**
	 * Write survival components of netCDF output file
	 * @param writer				NetcdfFormatWriter
	 * @throws IOException			
	 * @throws InvalidRangeException
	 */
	public void writeOutput(NetcdfFormatWriter writer) {		
		Variable v;
		int[] shape;
		int len, rowNum;
		ArrayChar ac;
		ArrayChar.D3 charArray;
		Index ima;
		 		
		writeStr(writer, "tidefile", this.tidefile);
		writeStr(writer, "particle_type", this.particle_type);
		writeStr(writer, "time_zone", this.time_zone);
		writeStr(writer, "travel_time_output_path", this.travel_time_output_path);
		writeStr(writer, "sunrise", this.sunrise);
		writeStr(writer, "sunset", this.sunset);
		writeStr(writer, "output_path_entrainment", this.output_path_entrainment);
		writeStr(writer, "trans_probs_path", this.trans_probs_path);
		writeStr(writer, "output_path_flux", this.output_path_flux);
		writeStr(writer, "survival_output_path", this.survival_output_path);
		writeStr(writer, "simulation_start_date", this.simulation_start_date);
		writeStr(writer, "simulation_scenario", this.simulation_scenario);
		writeStr(writer, "route_survival_output_path", this.route_survival_output_path);
		writeStr(writer, "fates_output_path", this.fates_output_path);
		writeStr(writer, "ptm_start_date", this.ptm_start_date);
		writeStr(writer, "ptm_start_time", this.ptm_start_time);
		writeStr(writer, "ptm_end_date", this.ptm_end_date);
		writeStr(writer, "ptm_end_time", this.ptm_end_time);
		writeStr(writer, "ptm_time_step", this.ptm_time_step);
		writeStr(writer, "display_intvl", this.display_intvl);
		
		writeStr(writer, "use_new_random_seed", Boolean.toString(this.use_new_random_seed));
		writeStr(writer, "random_assess", Boolean.toString(this.random_assess));
		writeStr(writer, "show_route_survival_detail", Boolean.toString(this.show_route_survival_detail));
		writeStr(writer, "display_simulation_timestep_write_all", Boolean.toString(this.display_simulation_timestep_write_all));
		writeStr(writer, "flux_write_all", Boolean.toString(this.flux_write_all));
		writeStr(writer, "entrainment_write_all", Boolean.toString(this.entrainment_write_all));
		writeStr(writer, "survival_write_all", Boolean.toString(this.survival_write_all));
		writeStr(writer, "ptm_igroup", Boolean.toString(this.ptm_igroup));
		writeStr(writer, "ptm_ivert", Boolean.toString(this.ptm_ivert));
		writeStr(writer, "ptm_itrans", Boolean.toString(this.ptm_itrans));
		writeStr(writer, "ptm_iey", Boolean.toString(this.ptm_iey));
		writeStr(writer, "ptm_iez", Boolean.toString(this.ptm_iez));
		writeStr(writer, "ptm_iprof", Boolean.toString(this.ptm_iprof));
		writeStr(writer, "ptm_flux_percent", Boolean.toString(this.ptm_flux_percent));
		writeStr(writer, "ptm_group_percent", Boolean.toString(this.ptm_group_percent));
		writeStr(writer, "ptm_flux_cumulative", Boolean.toString(this.ptm_flux_cumulative));
		
		writeScalar(writer, "stst_threshold", (double) this.stst_threshold);
		writeScalar(writer, "tidal_cycles_to_calculate_channel_direction", (double) this.tidal_cycles_to_calculate_channel_direction);
		writeScalar(writer, "confusion_probability_constant", (double) this.confusion_probability_constant);
		writeScalar(writer, "max_confusion_probability", (double) this.max_confusion_probability);
		writeScalar(writer, "confusion_probability_slope", (double) this.confusion_probability_slope);
		writeScalar(writer, "assess_probability", (double) this.assess_probability);
		writeScalar(writer, "stuck_threshold", (double) this.stuck_threshold);
		writeScalar(writer, "dicu_filter_efficiency", (double) this.dicu_filter_efficiency);
		writeScalar(writer, "theta", (double) this.theta);
		writeScalar(writer, "ptm_random_seed", (double) this.ptm_random_seed);
		writeScalar(writer, "ptm_trans_constant", (double) this.ptm_trans_constant);
		writeScalar(writer, "ptm_vert_constant", (double) this.ptm_vert_constant);
		writeScalar(writer, "ptm_trans_a_coef", (double) this.ptm_trans_a_coef);
		writeScalar(writer, "ptm_trans_b_coef", (double) this.ptm_trans_b_coef);
		writeScalar(writer, "ptm_trans_c_coef", (double) this.ptm_trans_c_coef);
		writeScalar(writer, "ptm_num_animated", (double) this.ptm_num_animated);
		
		writeStrArray(writer, "travel_time", this.travel_time);		
		writeStrArray(writer, "swimming_vel", this.swimming_vel);
		writeStrArray(writer, "channel_name_lookup", this.channel_name_lookup);
		writeStrArray(writer, "special_behavior", this.special_behavior);
		writeStrArray(writer, "particle_flux", this.particle_flux);
		writeStrArray(writer, "individual_route_survival", this.individual_route_survival);
		writeStrArray(writer, "route_survival_equations", this.route_survival_equations);
		writeStrArray(writer, "individual_reach_survival", this.individual_reach_survival);
		writeStrArray(writer, "particle_group_output", this.particle_group_output);
		writeStrArray(writer, "particle_flux_output", this.particle_flux_output);
		writeStrArray(writer, "groups", this.groups);
				
		writeIntArray(writer, "fish_screens", this.fish_screens);
				
		// Set header dimensions
		setColDim(writer, "travelTimeCol", this.travel_time_header);
		setColDim(writer, "swimmingVelCol", this.swimming_vel_header);
		setColDim(writer, "channelNameLookupCol", this.channel_name_lookup_header);
		setColDim(writer, "specialBehaviorCol", this.special_behavior_header);
		setColDim(writer, "fishScreensCol", this.fish_screens_header);
		setColDim(writer, "particleFluxCol", this.particle_flux_header);
		setColDim(writer, "indRouteSurvCol", this.individual_route_survival_header);
		setColDim(writer, "routeSurvEqCol", this.route_survival_equations_header);
		setColDim(writer, "indReachSurvCol", this.individual_reach_survival_header);
		setColDim(writer, "particleGroupOutputCol", this.particle_group_output_header);
		setColDim(writer, "particleFluxOutputCol", this.particle_flux_output_header);
		setColDim(writer, "groupsCol", this.groups_header);

		// release_loc
		try {
			v = writer.findVariable("releaseLocCol");
			shape = v.getShape();
			ac = new ArrayChar.D2(shape[0], shape[1]);
			ima = ac.getIndex();
			ac.setString(ima.set(0), "name");
			for(int i=1; i<shape[0]; i++) {
				ac.setString(ima.set(i), this.release_groups.get(0).release_loc_header[i-1]);
			}
			writer.write(v, ac);
			
			v = writer.findVariable("release_groups:release_loc");
			shape = v.getShape();
			charArray = new ArrayChar.D3(shape[0], shape[1], shape[2]);
			ima = charArray.getIndex();
			rowNum = 0;
			for(int relGroup=0; relGroup<this.release_groups.size(); relGroup++) {
				for(int j=0; j<shape[1]; j++) {
					if(j==0) {
						charArray.setString(ima.set(rowNum, j), this.release_groups.get(relGroup).name);
					}
					else {
						charArray.setString(ima.set(rowNum, j), this.release_groups.get(relGroup).release_loc.get(j-1).toString());
					}
				}
				rowNum++;
			}
			writer.write(v, charArray);
		}
		catch (Exception e) {
			System.out.println("Could not write release_loc to netCDF output file. Skipping.");
		}

		// releases
		try {
			v = writer.findVariable("releasesCol");
			shape = v.getShape();
			ac = new ArrayChar.D2(shape[0], shape[1]);
			ima = ac.getIndex();
			ac.setString(ima.set(0), "name");
			for (int i=1; i<shape[0]; i++) {
				ac.setString(ima.set(i), this.release_groups.get(0).releases_header[i-1]);
			}
			writer.write(v, ac);
			
			v = writer.findVariable("release_groups:releases");
			shape = v.getShape();
			charArray = new ArrayChar.D3(shape[0], shape[1], shape[2]);
			ima = charArray.getIndex();
			rowNum = 0;
			for(int relGroup=0; relGroup<this.release_groups.size(); relGroup++) {
				
				for(int rel=0; rel<this.release_groups.get(relGroup).releases.size(); rel++) {
					
					for(int j=0; j<shape[1]; j++) {
						
						if(j==0) {
							charArray.setString(ima.set(rowNum, j), this.release_groups.get(relGroup).name);
						}
						else {
							charArray.setString(ima.set(rowNum, j), this.release_groups.get(relGroup).releases.get(rel).get(j-1).toString());
						}
					}
					rowNum++;
				}
			}
			writer.write(v, charArray);
		}
		catch (Exception e) {
			System.out.println("Could not write releases to netCDF output file. Skipping.");
		}

		// channel_groups
		try {
			v = writer.findVariable("channelGroupsCol");
			shape = v.getShape();
			ac = new ArrayChar.D2(shape[0], shape[1]);
			ima = ac.getIndex();
			ac.setString(ima.set(0), "name");
			ac.setString(ima.set(1), "channel");
			
			v = writer.findVariable("channel_groups");
			shape = v.getShape();
			charArray = new ArrayChar.D3(shape[0], shape[1], shape[2]);
			ima = charArray.getIndex();
			rowNum = 0;
			for(int chanGroup=0; chanGroup<this.channel_groups.size(); chanGroup++) {
				for(int chanNum=0; chanNum<this.channel_groups.get(chanGroup).channels.length; chanNum++) {
					charArray.setString(ima.set(rowNum, 0), this.channel_groups.get(chanGroup).name);
					charArray.setString(ima.set(rowNum, 1), Integer.toString(this.channel_groups.get(chanGroup).channels[chanNum]));
					rowNum++;
				}
			}
			writer.write(v, charArray);
		}
		catch (Exception e) {
			System.out.println("Could not write channel_groups to netCDF output file. Skipping.");
		}
		
		// barriers
		try {
			v = writer.findVariable("barriersCol");
			shape = v.getShape();
			ac = new ArrayChar.D2(shape[0], shape[1]);
			ima = ac.getIndex();
			ac.setString(ima.set(0), "name");
			ac.setString(ima.set(1), "nodeID");
			ac.setString(ima.set(2), "waterbodyID");
			for(int i=3; i<shape[0]; i++) {
				ac.setString(ima.set(i), this.barriers.get(0).schedule_header[i-3]);
			}
			writer.write(v, ac);
			
			v = writer.findVariable("barriers");
			shape = v.getShape();
			charArray = new ArrayChar.D3(shape[0], shape[1], shape[2]);
			ima = charArray.getIndex();
			rowNum = 0;
			for(int b=0; b<this.barriers.size(); b++) {
				for(int sched=0; sched<this.barriers.get(b).schedule.size(); sched++) {
					for(int j=0; j<shape[1]; j++) {
						if(j==0) {
							charArray.setString(ima.set(rowNum, j), this.barriers.get(b).name);
						}
						else if(j==1) {
							charArray.setString(ima.set(rowNum, j), Integer.toString(this.barriers.get(b).nodeID));
						}
						else if(j==2) {
							charArray.setString(ima.set(rowNum, j), Integer.toString(this.barriers.get(b).waterbodyID));
						}
						else {
							charArray.setString(ima.set(rowNum, j), this.barriers.get(b).schedule.get(sched).get(j-3).toString());
						}
					}
					rowNum++;
				}
			}
			writer.write(v,  charArray);
		}
		catch (Exception e) {
			System.out.println("Could not write barriers to netCDF output file. Skipping.");
		}
		
		// start_stations
		try {
			v = writer.findVariable("startStationsCol");
			shape = v.getShape();
			ac = new ArrayChar.D2(shape[0], shape[1]);
			ima = ac.getIndex();
			ac.setString(ima.set(0), "name");
			ac.setString(ima.set(1), "number");
			ac.setString(ima.set(2), "channel");
			ac.setString(ima.set(3), "distance");
			writer.write(v, ac);
			
			v = writer.findVariable("survival_groups:start_stations");
			shape = v.getShape();
			charArray = new ArrayChar.D3(shape[0], shape[1], shape[2]);
			ima = charArray.getIndex();
			for(int i=0; i<shape[0]; i++) {
				for(int j=0; j<shape[1]; j++) {
					if(j==0) {
						charArray.setString(ima.set(i, j), this.survival_groups.get(i).name);
					}
					else if(j==1) {
						charArray.setString(ima.set(i, j), Integer.toString(this.survival_groups.get(i).number));
					}
					else {
						charArray.setString(ima.set(i, j), this.survival_groups.get(i).start_stations.get(0).get(j-2).toString());
					}
				}
			}
			writer.write(v, charArray);
		}
		catch (Exception e) {
			System.out.println("Could not write start_stations to netCDF output file. Skipping.");
		}
		
		// end_stations
		try {
			v = writer.findVariable("endStationsCol");
			shape = v.getShape();
			ac = new ArrayChar.D2(shape[0], shape[1]);
			ima = ac.getIndex();
			ac.setString(ima.set(0), "name");
			ac.setString(ima.set(1), "number");
			ac.setString(ima.set(2), "channel");
			ac.setString(ima.set(3), "distance");
			writer.write(v, ac);
			
			v = writer.findVariable("survival_groups:end_stations");
			shape = v.getShape();
			charArray = new ArrayChar.D3(shape[0], shape[1], shape[2]);
			ima = charArray.getIndex();
			rowNum = 0;
			for(SurvivalGroup sG : this.survival_groups) {
				for(int eS=0; eS<sG.end_stations.size(); eS++) {
					for(int j=0; j<shape[1]; j++) {
						if(j==0) {
							charArray.setString(ima.set(rowNum, j), sG.name);
						}
						else if(j==1) {
							charArray.setString(ima.set(rowNum, j), Integer.toString(sG.number));
						}
						else {
							charArray.setString(ima.set(rowNum, j), sG.end_stations.get(eS).get(j-2).toString());
						}
					}
					rowNum++;
				}
			}
			writer.write(v, charArray);
		}
		catch (Exception e) {
			System.out.println("Could not write end_stations to netCDF output file. Skipping.");
		}
		
		// exchangeable_start_stations
		try {
			v = writer.findVariable("exchStationsCol");
			shape = v.getShape();
			ac = new ArrayChar.D2(shape[0], shape[1]);
			ima = ac.getIndex();
			ac.setString(ima.set(0), "name");
			ac.setString(ima.set(1), "number");
			ac.setString(ima.set(2), "channel");
			ac.setString(ima.set(3), "distance");
			writer.write(v, ac);
			
			v = writer.findVariable("survival_groups:exchangeable_start_stations");
			shape = v.getShape();
			charArray = new ArrayChar.D3(shape[0], shape[1], shape[2]);
			ima = charArray.getIndex();
			rowNum = 0;
			for(SurvivalGroup sG : this.survival_groups) {
				if(sG.exchangeable_start_stations!=null) {
					for(int eS=0; eS<sG.exchangeable_start_stations.size(); eS++) {
						for(int j=0; j<shape[1]; j++) {
							if(j==0) {
								charArray.setString(ima.set(rowNum, j), sG.name);
							}
							else if(j==1) {
								charArray.setString(ima.set(rowNum, j), Integer.toString(sG.number));
							}
							else {
								charArray.setString(ima.set(rowNum, j), sG.exchangeable_start_stations.get(eS).get(j-2).toString());
							}
						}
						rowNum++;
					}
				}
			}
			writer.write(v, charArray);
		}
		catch (Exception e) {
			System.out.println("Could not write exchangeable_start_stations to netCDF output file. Skipping.");
		}
		
		// survival_params
		try {
			v = writer.findVariable("survParamsCol");
			shape = v.getShape();
			ac = new ArrayChar.D2(shape[0], shape[1]);
			ima = ac.getIndex();
			ac.setString(ima.set(0), "name");
			ac.setString(ima.set(1), "number");
			for(int i=2; i<shape[0]; i++) {
				ac.setString(ima.set(i), this.survival_groups.get(0).survival_params_header[i-2]);
			}
			writer.write(v, ac);
			
			v = writer.findVariable("survival_groups:survival_params");
			shape = v.getShape();
			charArray = new ArrayChar.D3(shape[0], shape[1], shape[2]);
			ima = charArray.getIndex();
			rowNum = 0;
			for(SurvivalGroup sG : this.survival_groups) {
				for(int sP=0; sP<sG.survival_params.size(); sP++) {
					for(int j=0; j<shape[1]; j++) {
						if(j==0) {
							charArray.setString(ima.set(rowNum, j), sG.name);
						}
						else if(j==1) {
							charArray.setString(ima.set(rowNum, j), Integer.toString(sG.number));
						}
						else {
							charArray.setString(ima.set(rowNum, j), sG.survival_params.get(sP).get(j-2).toString());
						}
					}
					rowNum++;
				}
			}
			writer.write(v, charArray);
		}
		catch (Exception e) {
			System.out.println("Could not write survival_params to netCDF output file. Skipping.");
		}
		
		// exit_stations
		try {
			v = writer.findVariable("exit_stations");
			shape = v.getShape();
			ac = new ArrayChar.D2(shape[0], shape[1]);
			ima = ac.getIndex();
			for(int i=0; i<shape[0]; i++) {
				System.out.println("exit_stations: " + this.exit_stations[i]);
				ac.setString(ima.set(i), this.exit_stations[i]);
			}
			writer.write(v, ac);
		}
		catch (Exception e) {
			System.out.println("Could not write exit_stations to netCDF output file. Skipping.");
		}
		
		// io_file
		try {
			v = writer.findVariable("ioFileCol");
			shape = v.getShape();
			ac = new ArrayChar.D2(shape[0], shape[1]);
			ima = ac.getIndex();
			ac.setString(ima.set(0), "type");
			ac.setString(ima.set(1), "interval");
			ac.setString(ima.set(2), "file");
			writer.write(v, ac);
			
			v = writer.findVariable("io_file");
			shape = v.getShape();
			charArray = new ArrayChar.D3(shape[0], shape[1], shape[2]);
			ima = charArray.getIndex();
			rowNum = 0;
			for(int i=0; i<shape[0]; i++) {
				for(int j=0; j<shape[1]; j++) {
					if(j==0) {
						charArray.setString(ima.set(i, j), this.io_file.get(i).type);
					}
					else if(j==1) {
						charArray.setString(ima.set(i, j), this.io_file.get(i).interval);
					}
					else {
						charArray.setString(ima.set(i, j), this.io_file.get(i).file);
					}
				}
			}
			writer.write(v, charArray);
		}
		catch (Exception e) {
			System.out.println("Could not write io_file to netCDF output file. Skipping.");
		}

	}
	
	/**
	 * Write column names to the netCDF output file
	 * @param writer				NetcdfFormatWriter			
	 * @param varName				name of the variable (dimension)
	 * @param header				String[] containing headers
	 * @throws InvalidRangeException 
	 * @throws IOException 
	 */
	public void setColDim(NetcdfFormatWriter writer, String varName, String[] header) {
		Variable v;
		int[] shape;
		ArrayChar ac;
		Index ima;
		
		try {
			v = writer.findVariable(varName);
			shape = v.getShape();
			ac = new ArrayChar.D2(shape[0], shape[1]);
			ima = ac.getIndex();
			for (int i=0; i<shape[0]; i++) {
				ac.setString(ima.set(i), header[i]);
			}
			writer.write(v, ac);
		}
		catch (Exception e) {
			System.out.println("Could not write " + varName + " to netCDF output file. Skipping.");
		}

	}
	
	/**
	 * Write a string to the netCDF output file
	 * @param writer				NetcdfFormatWriter
	 * @param varName				name of the variable
	 * @param val					value of the variable
	 * @throws InvalidRangeException 
	 * @throws IOException 
	 */
	public void writeStr(NetcdfFormatWriter writer, String varName, String val) {
		Variable v;
		int[] shape;
		int len;
		ArrayChar ac;
		
		if(val==null) {return;}
		
		try {
			v = writer.findVariable(varName);
			shape = v.getShape();
			len = shape[0];
			ac = new ArrayChar.D1(len);
			ac.setString(val);
			writer.write(v, ac);
		}
		catch (Exception e) {
			System.out.println("Could not write " + varName + " to netCDF output file. Skipping.");
		}
		

	}
	
	/**
	 * Write a string array to the netCDF output file
	 * @param writer				NetcdfFormatWriter
	 * @param varName				name of variable
	 * @param valArray				array containing values
	 * @throws IOException
	 * @throws InvalidRangeException
	 */
	public void writeStrArray(NetcdfFormatWriter writer, String varName, List<List<Object>> valArray) {
		Variable v;
		int[] shape;
		ArrayChar.D3 charArray;
		Index ima;
		
		if(valArray==null) {return;}

		try {
			v = writer.findVariable(varName);
			shape = v.getShape();
			charArray = new ArrayChar.D3(shape[0], shape[1], shape[2]);
			ima = charArray.getIndex();
			for (int i=0; i<shape[0]; i++) {
				for(int j=0; j<shape[1]; j++) {
					charArray.setString(ima.set(i, j), valArray.get(i).get(j).toString());
				}
			}
			writer.write(v, charArray);
		}
		catch (Exception e) {
			System.out.println("Could not write " + varName + " to netCDF output file. Skipping.");
		}

	}
	
	/**
	 * Write an integer array to the netCDF output file
	 * @param writer				NetcdfFormatWriter
	 * @param varName				name of variable
	 * @param valArray				array containing values
	 * @throws IOException
	 * @throws InvalidRangeException
	 */
	public void writeIntArray(NetcdfFormatWriter writer, String varName, List<List<Integer>> valArray) {
		Variable v;
		int[] shape;
		ArrayInt.D2 intArray;
		Index ima;
		
		if(valArray==null) {return;}
		
		try {
			v = writer.findVariable(varName);
			shape = v.getShape();
			intArray = new ArrayInt.D2(shape[0], shape[1], false);
			ima = intArray.getIndex();
			for(int i=0; i<shape[0]; i++) {
				for(int j=0; j<shape[1]; j++) {
					intArray.set(ima.set(i, j), valArray.get(i).get(j));
				}
			}
			writer.write(v, intArray);
		}
		catch (Exception e) {
			System.out.println("Could not write " + varName + " to netCDF output file. Skipping.");
		}
	}
	
	/**
	 * Write a scalar value to the netCDF output file
	 * @param writer				NetcdfFormatWriter
	 * @param varName				name of variable
	 * @param val					value
	 * @throws IOException
	 * @throws InvalidRangeException
	 */
	public void writeScalar(NetcdfFormatWriter writer, String varName, Double val) {
		Variable v;
		ArrayDouble.D0 datas;
		
		if(val==null) {return;}
		
		try {
			v = writer.findVariable(varName);
			datas = new ArrayDouble.D0();
			datas.set(val);
			writer.write(v, datas);
		}
		catch (Exception e) {
			System.out.println("Could not write " + varName + " to netCDF output file. Skipping.");
		}

	}
	
	/**
	 * Add a dimension to the builder, protecting against null variables
	 * @param builder				NetcdfFormatWriter.Builder
	 * @param varName				name of variable
	 * @param obj					Object whose length or size we want to use
	 * @return 						Dimension
	 */
	public Dimension addDimension(NetcdfFormatWriter.Builder builder, String varName, Object obj) {
		Dimension dim;
		
		if(obj==null) {
			dim = builder.addDimension(varName, 0);
		}
		else if(obj.getClass().isArray()) {
			dim = builder.addDimension(varName, java.lang.reflect.Array.getLength(obj));
		}
		else {
			dim = builder.addDimension(varName, ((java.util.Collection<?>) obj).size());
		}
		return dim;
	}
	
	/**
	 * Add a dimension to the builder, protecting against null variables
	 * @param builder				NetcdfFormatWriter.Builder
	 * @param varName				name of variable
	 * @param obj					Object whose length or size we want to use
	 * @param addLength				additional length to add to dimension
	 * @return 						Dimension
	 */
	public Dimension addDimension(NetcdfFormatWriter.Builder builder, String varName, Object obj, int addLength) {
		Dimension dim;
		
		if(obj==null) {
			dim = builder.addDimension(varName, 0);
		}
		else if(obj.getClass().isArray()) {
			dim = builder.addDimension(varName, java.lang.reflect.Array.getLength(obj) + addLength);
		}
		else {
			dim = builder.addDimension(varName, ((java.util.Collection<?>) obj).size() + addLength);
		}
		return dim;
	}
}
