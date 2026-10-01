// Implementation of the mock Fortran layer. See Dsm2FortranMock.h for the purpose and conventions.
// Each extern "C" function states the Fortran routine it mirrors. The declarations come from the
// real binding headers so a signature mismatch is a compile error.
#include "Dsm2FortranMock.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <sstream>

#include "dsm2_expressions.h"
#include "dsm2_interface_fortran.h"
#include "dsm2_time_interface_fortran.h"
#include "dsm2_time_series_node.h"

namespace dsm2mock {

namespace {

std::string lower(std::string s) {
   for (size_t i = 0; i < s.size(); ++i) s[i] = (char)std::tolower((unsigned char)s[i]);
   return s;
}

std::string trim(const std::string& s) {
   size_t b = s.find_first_not_of(' ');
   if (b == std::string::npos) return "";
   size_t e = s.find_last_not_of(' ');
   return s.substr(b, e - b + 1);
}

void check_index(int i, size_t n, const char* what) {
   if (i < 1 || i > (int)n) {
      std::ostringstream m;
      m << "Fortran would index " << what << "(" << i << ") with extent 1.." << n << " (undefined behaviour)";
      throw FortranContractViolation(m.str());
   }
}

long days_from_civil(int y, unsigned m, unsigned d) {
   y -= m <= 2;
   const int era = (y >= 0 ? y : y - 399) / 400;
   const unsigned yoe = (unsigned)(y - era * 400);
   const unsigned doy = (153 * (m + (m > 2 ? -3 : 9)) + 2) / 5 + d - 1;
   const unsigned doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
   return era * 146097L + (long)doe - 719468L;
}

void civil_from_days(long z, int& y, int& m, int& d) {
   z += 719468;
   const long era = (z >= 0 ? z : z - 146096) / 146097;
   const unsigned doe = (unsigned)(z - era * 146097);
   const unsigned yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
   y = (int)(yoe + era * 400);
   const unsigned doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
   const unsigned mp = (5 * doy + 2) / 153;
   d = (int)(doy - (153 * mp + 2) / 5 + 1);
   m = (int)(mp < 10 ? mp + 3 : mp - 9);
   y += (m <= 2);
}

const int MIN_PER_DAY = 1440;

}  // namespace

int julday(int year, int month, int day) { return (int)(days_from_civil(year, month, day) + 25568L); }

void civil_from_julday(int jday, int& year, int& month, int& day) { civil_from_days(jday - 25568L, year, month, day); }

// ---------------------------------------------------------------------------------- Model

void Model::reset() {
   channels.clear();
   nodes.clear();
   reservoirs.clear();
   gates.clear();
   qexts.clear();
   transfers.clear();
   paths.clear();
   comp_point_requests.clear();
   stage_points.assign(1, 0.);
   flow_points.assign(1, 0.);
   next_point = 1;
   dt_seconds = 900;
   set_time(2001, 1, 1, 0, 0);
}

void Model::add_channel(int ext, double length, int npts) {
   Channel c;
   c.ext = ext;
   c.length = length;
   c.npts = npts;
   c.first_point = next_point;
   next_point += npts;
   stage_points.resize(next_point, 0.);
   flow_points.resize(next_point, 0.);
   channels.push_back(c);
   // ASSUMPTION: ext2int does a binary search over a sorted list of external numbers, so internal
   // numbers follow the sorted external order. Keep the vector sorted to mirror that.
   std::sort(channels.begin(), channels.end(), [](const Channel& a, const Channel& b) { return a.ext < b.ext; });
}

static Channel& channel_by_ext(Model& m, int ext) {
   for (size_t i = 0; i < m.channels.size(); ++i)
      if (m.channels[i].ext == ext) return m.channels[i];
   throw std::logic_error("test error: unknown channel in mock");
}

static void fill_points(Model& m, Channel& c, const std::function<double(double)>& fn, std::vector<double>& pts) {
   double dx = c.length / (c.npts - 1);
   for (int k = 0; k < c.npts; ++k) pts[c.first_point + k] = fn(k * dx);
}

void Model::set_stage(int ext, std::function<double(double)> fn) {
   Channel& c = channel_by_ext(*this, ext);
   c.stage_fn = fn;
   fill_points(*this, c, fn, stage_points);
}
void Model::set_flow(int ext, std::function<double(double)> fn) {
   Channel& c = channel_by_ext(*this, ext);
   c.flow_fn = fn;
   fill_points(*this, c, fn, flow_points);
}
void Model::set_velocity(int ext, std::function<double(double)> fn) { channel_by_ext(*this, ext).velocity_fn = fn; }

void Model::add_node(int ext_node) { nodes.push_back(ext_node); }

void Model::add_reservoir(const std::string& name, const std::vector<int>& ext_nodes) {
   Reservoir r;
   r.name = lower(name);
   for (size_t i = 0; i < ext_nodes.size(); ++i) {
      int internal = 0;
      for (size_t k = 0; k < nodes.size(); ++k)
         if (nodes[k] == ext_nodes[i]) internal = (int)k + 1;
      if (internal == 0) throw std::logic_error("test error: add_node before add_reservoir");
      r.internal_nodes.push_back(internal);
   }
   r.qres.assign(ext_nodes.size(), 0.);
   reservoirs.push_back(r);
}

void Model::add_external_flow(const std::string& name, double initial_flow) {
   ExternalFlow q;
   q.name = lower(name);
   q.flow = initial_flow;
   q.datasource.value = initial_flow;
   qexts.push_back(q);
}

void Model::add_transfer(const std::string& name, double initial_flow) {
   Transfer t;
   t.name = lower(name);
   t.flow = initial_flow;
   t.datasource.value = initial_flow;
   transfers.push_back(t);
}

int Model::add_gate(const std::string& name) {
   Gate g;
   g.name = lower(name);
   g.install.value = 1.;
   gates.push_back(g);
   return (int)gates.size();
}

void Model::add_device(const std::string& gate_name, const std::string& device_name, const DeviceInit& in) {
   Gate& g = gate(gate_name);
   Device d;
   d.name = lower(device_name);
   d.opCoefToNode = in.opCoefToNode;
   d.opCoefFromNode = in.opCoefFromNode;
   d.baseElev = in.baseElev;
   d.height = in.height;
   d.maxWidth = in.maxWidth;
   d.nDuplicate = in.nDuplicate;
   d.flowCoefToNode = in.flowCoefToNode;
   d.flowCoefFromNode = in.flowCoefFromNode;
   d.flow = 0.;
   d.op_to_node.value = in.opCoefToNode;
   d.op_from_node.value = in.opCoefFromNode;
   d.elev.value = in.baseElev;
   d.height_ds.value = in.height;
   d.width.value = in.maxWidth;
   d.nduplicate.value = in.nDuplicate;
   g.devices.push_back(d);
}

void Model::add_path_input(const std::string& name, double value, bool is_oprule) {
   PathInput p;
   p.name = lower(name);
   p.is_oprule = is_oprule;
   p.value = value;
   paths.push_back(p);
}

int Model::gate_no(const std::string& name) const {
   for (size_t i = 0; i < gates.size(); ++i)
      if (gates[i].name == lower(name)) return (int)i + 1;
   throw std::logic_error("test error: unknown gate " + name);
}
Gate& Model::gate(const std::string& name) { return gates[gate_no(name) - 1]; }
Device& Model::device(const std::string& g, const std::string& d) {
   Gate& gt = gate(g);
   for (size_t i = 0; i < gt.devices.size(); ++i)
      if (gt.devices[i].name == lower(d)) return gt.devices[i];
   throw std::logic_error("test error: unknown device " + d);
}
ExternalFlow& Model::qext(const std::string& name) {
   for (size_t i = 0; i < qexts.size(); ++i)
      if (qexts[i].name == lower(name)) return qexts[i];
   throw std::logic_error("test error: unknown external flow " + name);
}
Transfer& Model::transfer(const std::string& name) {
   for (size_t i = 0; i < transfers.size(); ++i)
      if (transfers[i].name == lower(name)) return transfers[i];
   throw std::logic_error("test error: unknown transfer " + name);
}
Reservoir& Model::reservoir(const std::string& name) {
   for (size_t i = 0; i < reservoirs.size(); ++i)
      if (reservoirs[i].name == lower(name)) return reservoirs[i];
   throw std::logic_error("test error: unknown reservoir " + name);
}
PathInput& Model::path(const std::string& name) {
   for (size_t i = 0; i < paths.size(); ++i)
      if (paths[i].name == lower(name)) return paths[i];
   throw std::logic_error("test error: unknown path input " + name);
}

void Model::set_time(int year, int month, int day, int hour, int minute) {
   julmin = julday(year, month, day) * MIN_PER_DAY + hour * 60 + minute;
}

// model_interface.f90: fetch_data(). Fetch time varying data from a data source.
double Model::fetch_data(const DataSource& s) {
   if (s.source_type == CONST_DATA) return s.value;
   if (s.source_type == DSS_DATA) {
      check_index(s.indx_ptr, paths.size(), "pathinput");
      return paths[s.indx_ptr - 1].value;
   }
   if (s.source_type == EXPRESSION_DATA) {
      int idx = s.indx_ptr;
      return get_expression_data(&idx);  // real C++: evaluates the node registered by the oprule
   }
   return MISS_R;  // FORTRAN QUIRK: an unset source_type yields miss_val_r every step
}

// netbnd.f90: store_values(). Called every step from SetBoundaryValuesFromData.
void Model::store_values() {
   for (size_t i = 0; i < transfers.size(); ++i) transfers[i].flow = fetch_data(transfers[i].datasource);
   for (size_t i = 0; i < qexts.size(); ++i) qexts[i].flow = fetch_data(qexts[i].datasource);
   for (size_t g = 0; g < gates.size(); ++g) {
      for (size_t j = 0; j < gates[g].devices.size(); ++j) {
         Device& d = gates[g].devices[j];
         d.opCoefToNode = fetch_data(d.op_to_node);
         d.opCoefFromNode = fetch_data(d.op_from_node);
         d.baseElev = fetch_data(d.elev);
         d.height = fetch_data(d.height_ds);
         d.maxWidth = fetch_data(d.width);
         d.nDuplicate = fetch_data(d.nduplicate);
      }
      // FORTRAN QUIRK: the install data source is fetched but the result is not applied
      // (the call to setFree is commented out in store_values), so it has no effect.
      (void)fetch_data(gates[g].install);
   }
}

void Model::set_boundary_values_from_data(const std::function<double(const std::string&, int)>& ts_source) {
   if (ts_source)
      for (size_t i = 0; i < paths.size(); ++i) paths[i].value = ts_source(paths[i].name, julmin);
   store_values();
}

Model& model() {
   static Model m;
   return m;
}

}  // namespace dsm2mock

using namespace dsm2mock;

// ============================================================================ extern "C"
// Index/name lookups (model_interface.f90)

// gateNdx: lower-cases the query, compares to GateArray(i)%name, returns miss_val_i if absent.
extern "C" int gate_index(const char* name, unsigned int len) {
   std::string q = lower(std::string(name, len));
   Model& m = model();
   for (size_t i = 0; i < m.gates.size(); ++i)
      if (trim(m.gates[i].name) == trim(q)) return (int)i + 1;
   return MISS_I;
}

// deviceNdx -> gates.f90 deviceIndex: lower-cases the query, returns miss_val_i if the gate has no such
// device. The gate index is NOT validated: an out-of-range gate (including miss_val_i) is undefined.
extern "C" int device_index(const int& gateno, const char* name, unsigned int len) {
   Model& m = model();
   check_index(gateno, m.gates.size(), "GateArray");
   std::string q = lower(std::string(name, len));
   const Gate& g = m.gates[gateno - 1];
   for (size_t i = 0; i < g.devices.size(); ++i)
      if (trim(g.devices[i].name) == trim(q)) return (int)i + 1;
   return MISS_I;
}

// grid_data.f90 ext2int: binary search of the external channel numbers.
// FORTRAN QUIRK: returns 0 (not miss_val_i) for an unknown channel.
extern "C" int ext2int(const int& extchan) {
   Model& m = model();
   for (size_t i = 0; i < m.channels.size(); ++i)
      if (m.channels[i].ext == extchan) return (int)i + 1;
   return 0;
}

// grid_data.f90 ext2intnode: binary search of external node numbers; 0 if not found.
extern "C" int ext2intnode(const int& extnode) {
   Model& m = model();
   for (size_t i = 0; i < m.nodes.size(); ++i)
      if (m.nodes[i] == extnode) return (int)i + 1;
   return 0;
}

// resNdx: lower-cases the query; miss_val_i if absent.
extern "C" int reservoir_index(const char* name, unsigned int len) {
   std::string q = lower(std::string(name, len));
   Model& m = model();
   for (size_t i = 0; i < m.reservoirs.size(); ++i)
      if (m.reservoirs[i].name == q) return (int)i + 1;
   return MISS_I;
}

// resConnectNdx: index (1..nnodes) of the reservoir connection whose internal node is given, else miss_val_i.
extern "C" int reservoir_connect_index(const int& resndx, const int& internal_node) {
   Model& m = model();
   check_index(resndx, m.reservoirs.size(), "res_geom");
   const Reservoir& r = m.reservoirs[resndx - 1];
   for (size_t i = 0; i < r.internal_nodes.size(); ++i)
      if (r.internal_nodes[i] == internal_node) return (int)i + 1;
   return MISS_I;
}

// Comparison is exact, no lower-casing (the stored names are assumed lower case).
extern "C" int qext_index(const char* name, unsigned int len) {
   std::string q(name, len);
   Model& m = model();
   for (size_t i = 0; i < m.qexts.size(); ++i)
      if (m.qexts[i].name == q) return (int)i + 1;
   return MISS_I;
}

extern "C" int transfer_index(const char* name, unsigned int len) {
   std::string q(name, len);
   Model& m = model();
   for (size_t i = 0; i < m.transfers.size(); ++i)
      if (m.transfers[i].name == q) return (int)i + 1;
   return MISS_I;
}

// ts_index: exact match against pathinput(i)%name over ALL input paths (not just oprule ones);
// first match wins. FORTRAN QUIRK: returns -1 (not miss_val_i) when absent.
extern "C" int ts_index(const char* name, unsigned int len) {
   std::string q = trim(std::string(name, len));
   Model& m = model();
   for (size_t i = 0; i < m.paths.size(); ++i)
      if (trim(m.paths[i].name) == q) return (int)i + 1;
   return -1;
}

// pathinput(i)%value, refreshed each step by get_inp_data.
extern "C" double value_from_inputpath(const int* i) {
   Model& m = model();
   check_index(*i, m.paths.size(), "pathinput");
   return m.paths[*i - 1].value;
}

extern "C" int direct_to_node() { return FLOW_COEF_TO_NODE; }
extern "C" int direct_from_node() { return FLOW_COEF_FROM_NODE; }
extern "C" int direct_to_from_node() { return FLOW_COEF_TO_FROM_NODE; }

// channel_length: chan_geom(intno)%length. FORTRAN QUIRK: intno is not validated. Combined with
// ext2int returning 0 for an unknown channel, an unknown channel number in a rule reads chan_geom(0).
extern "C" double channel_length(const int& intno) {
   Model& m = model();
   check_index(intno, m.channels.size(), "chan_geom");
   return m.channels[intno - 1].length;
}

// chan_comp_point -> CompPointAtDist: the two computational points bracketing `distance` and the
// linear interpolation weights (up, down) for the value at that distance.
// ASSUMPTION: evenly spaced points; weights sum to 1.
extern "C" void chan_comp_point(const int& intchan, const double& distance, int points[], double weights[]) {
   Model& m = model();
   check_index(intchan, m.channels.size(), "chan_geom");
   m.comp_point_requests.push_back(std::make_pair(intchan, distance));
   const Channel& c = m.channels[intchan - 1];
   double dx = c.length / (c.npts - 1);
   int k = (int)std::floor(distance / dx);
   if (k > c.npts - 2) k = c.npts - 2;
   if (k < 0) k = 0;
   double w_down = (distance - k * dx) / dx;
   points[0] = c.first_point + k;
   points[1] = c.first_point + k + 1;
   weights[0] = 1. - w_down;
   weights[1] = w_down;
}

// channel_status.f90 GlobalStreamSurfaceElevation / GlobalStreamFlow: value at a global comp point.
extern "C" double get_surf_elev(const int& comp_pt) {
   check_index(comp_pt, model().stage_points.size() - 1, "WS");
   return model().stage_points[comp_pt];
}
extern "C" double get_flow(const int& comp_pt) {
   check_index(comp_pt, model().flow_points.size() - 1, "Q");
   return model().flow_points[comp_pt];
}

// tidefile.f90 ChannelVelocity(ChannNum, XX): velocity at distance XX of internal channel ChannNum.
extern "C" double get_chan_velocity(const int& chan, const double& dist) {
   Model& m = model();
   check_index(chan, m.channels.size(), "chan_geom");
   const Channel& c = m.channels[chan - 1];
   return c.velocity_fn ? c.velocity_fn(dist) : 0.;
}

// reservoirs.f90 get_res_flow / get_res_surf_elev: QRes(res, conn) and YRes(res).
extern "C" double get_res_flow(const int& resndx, const int& conn) {
   Model& m = model();
   check_index(resndx, m.reservoirs.size(), "QRes");
   check_index(conn, m.reservoirs[resndx - 1].qres.size(), "QRes(.,conn)");
   return m.reservoirs[resndx - 1].qres[conn - 1];
}
extern "C" double get_res_surf_elev(const int& resndx) {
   Model& m = model();
   check_index(resndx, m.reservoirs.size(), "YRes");
   return m.reservoirs[resndx - 1].stage;
}

// ------------------------------------------------------------------------------ data sources

// model_interface.f90 set_datasource: source.indx_ptr = expr; source.value = val;
// source_type = expression_data if timedep else const_data.
static void set_datasource(DataSource& s, int expr, double val, bool timedep) {
   s.indx_ptr = expr;
   s.value = val;
   s.source_type = timedep ? EXPRESSION_DATA : CONST_DATA;
}

// ------------------------------------------------------------------------ external flow, transfers

extern "C" double get_external_flow(const int& ndx) {
   check_index(ndx, model().qexts.size(), "qext");
   return model().qexts[ndx - 1].flow;
}
extern "C" void set_external_flow(const int& ndx, const double& val) {
   check_index(ndx, model().qexts.size(), "qext");
   model().qexts[ndx - 1].flow = val;
}
extern "C" void set_external_flow_datasource(const int& ndx, const int& expr, const double& val, const bool& timedep) {
   check_index(ndx, model().qexts.size(), "qext");
   set_datasource(model().qexts[ndx - 1].datasource, expr, val, timedep);
}

extern "C" double get_transfer_flow(const int& ndx) {
   check_index(ndx, model().transfers.size(), "obj2obj");
   return model().transfers[ndx - 1].flow;
}
extern "C" void set_transfer_flow(const int& ndx, const double& val) {
   check_index(ndx, model().transfers.size(), "obj2obj");
   model().transfers[ndx - 1].flow = val;
}
extern "C" void set_transfer_flow_datasource(const int& ndx, const int& expr, const double& val, const bool& timedep) {
   check_index(ndx, model().transfers.size(), "obj2obj");
   set_datasource(model().transfers[ndx - 1].datasource, expr, val, timedep);
}

// ----------------------------------------------------------------------------------- gates

static Gate& gate_at(int ndx) {
   check_index(ndx, model().gates.size(), "GateArray");
   return model().gates[ndx - 1];
}
static Device& device_at(int g, int d) {
   Gate& gt = gate_at(g);
   check_index(d, gt.devices.size(), "GateArray%Devices");
   return gt.devices[d - 1];
}

// set_gate_install: install == 0.0 exactly -> setFree(gate, .true.); anything else -> setFree(.false.).
// setFree zeroes every device flow when a gate becomes free (non-redundantly), then stores the flag.
extern "C" void set_gate_install(const int& ndx, const double& install) {
   Gate& g = gate_at(ndx);
   bool want_free = (install == 0.);
   if (want_free && !g.free)
      for (size_t i = 0; i < g.devices.size(); ++i) g.devices[i].flow = 0.;
   g.free = want_free;
}
// is_gate_install: 0.0 if the gate is free, else 1.0.
extern "C" double is_gate_install(const int& ndx) { return gate_at(ndx).free ? 0. : 1.; }
extern "C" void set_gate_install_datasource(const int& ndx, const int& expr, const int& val, const bool& timedep) {
   set_datasource(gate_at(ndx).install, expr, val, timedep);
}

// get_device_op_coef: to_node -> opCoefToNode; from_node -> opCoefFromNode; to_from -> "average".
// FORTRAN QUIRK: the to_from average is (opCoefFromNode + opCoefFromNode)/2, i.e. the from-node value.
// An unrecognised direction returns -901.0.
extern "C" double get_device_op_coef(const int& ndx, const int& devndx, const int& direction) {
   Device& d = device_at(ndx, devndx);
   if (direction == FLOW_COEF_TO_NODE) return d.opCoefToNode;
   if (direction == FLOW_COEF_FROM_NODE) return d.opCoefFromNode;
   if (direction == FLOW_COEF_TO_FROM_NODE) return (d.opCoefFromNode + d.opCoefFromNode) / 2.;
   return -901.0;
}
// set_device_op_coef: to_from sets both directions; an unrecognised direction silently does nothing.
extern "C" void set_device_op_coef(const int& ndx, const int& devndx, const int& direction, const double& val) {
   Device& d = device_at(ndx, devndx);
   if (direction == FLOW_COEF_TO_NODE) d.opCoefToNode = val;
   else if (direction == FLOW_COEF_FROM_NODE) d.opCoefFromNode = val;
   else if (direction == FLOW_COEF_TO_FROM_NODE) d.opCoefToNode = d.opCoefFromNode = val;
}
extern "C" void set_device_op_datasource(const int& ndx, const int& devndx, const int& direction, const int& expr,
                                         const double& val, const bool& timedep) {
   Device& d = device_at(ndx, devndx);
   if (direction == FLOW_COEF_TO_NODE) set_datasource(d.op_to_node, expr, val, timedep);
   else if (direction == FLOW_COEF_FROM_NODE) set_datasource(d.op_from_node, expr, val, timedep);
   else if (direction == FLOW_COEF_TO_FROM_NODE) {
      set_datasource(d.op_from_node, expr, val, timedep);
      set_datasource(d.op_to_node, expr, val, timedep);
   }
}

extern "C" double get_device_height(const int& n, const int& d) { return device_at(n, d).height; }
extern "C" void set_device_height(const int& n, const int& d, const double& v) { device_at(n, d).height = v; }
extern "C" void set_device_height_datasource(const int& n, const int& d, const int& e, const double& v, const bool& t) {
   set_datasource(device_at(n, d).height_ds, e, v, t);
}

// width is stored in Device%maxWidth
extern "C" double get_device_width(const int& n, const int& d) { return device_at(n, d).maxWidth; }
extern "C" void set_device_width(const int& n, const int& d, const double& v) { device_at(n, d).maxWidth = v; }
extern "C" void set_device_width_datasource(const int& n, const int& d, const int& e, const double& v, const bool& t) {
   set_datasource(device_at(n, d).width, e, v, t);
}

// elevation is stored in Device%baseElev
extern "C" double get_device_elev(const int& n, const int& d) { return device_at(n, d).baseElev; }
extern "C" void set_device_elev(const int& n, const int& d, const double& v) { device_at(n, d).baseElev = v; }
extern "C" void set_device_elev_datasource(const int& n, const int& d, const int& e, const double& v, const bool& t) {
   set_datasource(device_at(n, d).elev, e, v, t);
}

// set_device_nduplicate stores nint(val): the value is rounded to the nearest integer.
extern "C" double get_device_nduplicate(const int& n, const int& d) { return device_at(n, d).nDuplicate; }
extern "C" void set_device_nduplicate(const int& n, const int& d, const double& v) {
   device_at(n, d).nDuplicate = std::floor(std::fabs(v) + 0.5) * (v < 0 ? -1. : 1.);
}
extern "C" void set_device_nduplicate_datasource(const int& n, const int& d, const int& e, const double& v,
                                                 const bool& t) {
   // ABI NOTE: the Fortran routine declares timedep as `logical(c_bool), value` (by value) while the C++
   // header passes it by reference; the other *_datasource routines declare a 4-byte `logical` by
   // reference. The mock cannot reproduce that mismatch; it takes the intended meaning.
   set_datasource(device_at(n, d).nduplicate, e, v, t);
}

// get/set_device_flow_coef: only to_node / from_node are handled.
// FORTRAN QUIRK: any other direction (including to_from_node) writes "Flow direction not recognized" and
// calls exit(3).
extern "C" double get_device_flow_coef(const int& n, const int& d, const int& direct) {
   Device& dev = device_at(n, d);
   if (direct == FLOW_COEF_TO_NODE) return dev.flowCoefToNode;
   if (direct == FLOW_COEF_FROM_NODE) return dev.flowCoefFromNode;
   throw FortranExit(3);
}
extern "C" void set_device_flow_coef(const int& n, const int& d, const int& direct, const double& val) {
   Device& dev = device_at(n, d);
   if (direct == FLOW_COEF_TO_NODE) dev.flowCoefToNode = val;
   else if (direct == FLOW_COEF_FROM_NODE) dev.flowCoefFromNode = val;
   else throw FortranExit(3);
}

// ------------------------------------------------------------------------------------- time
// model_interface.f90 getModel*; julmin (runtime_data) is minutes with 01JAN1900 00:00 = 1440
// (ASSUMPTION; only relative consistency matters to the oprule code).

static int model_julday() { return model().julmin / MIN_PER_DAY; }
static int model_minute_of_day() { return model().julmin % MIN_PER_DAY; }

extern "C" int get_model_year() { int y, m, d; civil_from_julday(model_julday(), y, m, d); return y; }
extern "C" int get_model_month() { int y, m, d; civil_from_julday(model_julday(), y, m, d); return m; }
extern "C" int get_model_day() { int y, m, d; civil_from_julday(model_julday(), y, m, d); return d; }
// 0-based: jday - julday(year, 1, 1)
extern "C" int get_model_day_of_year() {
   int y, m, d;
   civil_from_julday(model_julday(), y, m, d);
   return model_julday() - julday(y, 1, 1);
}
extern "C" int get_model_ticks() { return model().julmin; }
extern "C" int get_model_minute_of_day() { return model_minute_of_day(); }
extern "C" int get_model_minute_of_year() { return MIN_PER_DAY * get_model_day_of_year() + model_minute_of_day(); }
extern "C" int get_model_hour() { return model_minute_of_day() / 60; }
extern "C" int get_model_minute() { return model_minute_of_day() % 60; }
// getReferenceMinuteOfYear: minutes into the CURRENT MODEL YEAR of mon/day/hour/min.
extern "C" int get_reference_minute_of_year(int& mon, int& day, int& hour, int& min) {
   int yr = get_model_year();
   int dayyr = julday(yr, mon, day) - julday(yr, 1, 1);
   return MIN_PER_DAY * dayyr + 60 * hour + min;
}
// time_step_seconds: netcntrl DT (seconds)
extern "C" int time_step_seconds() { return model().dt_seconds; }

// utilities.f90 cdt2jmin_c: "ddMONyyyy hhmm" -> julian minute. The format is the one built by
// DSM2HydroTimeNodeFactory::getDateTimeNode(date, time), e.g. "28SEP1992 0000".
// ASSUMPTION: 2400 is accepted and means midnight at the end of the day.
extern "C" int cdate_to_jul_min(const char* name, unsigned int len) {
   std::string s(name, len);
   static const char* mons[] = {"JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"};
   if (s.size() < 14 || s[9] != ' ')
      throw FortranContractViolation("cdate_to_jul_min: expected 'ddMONyyyy hhmm', got '" + s + "'");
   int day = std::atoi(s.substr(0, 2).c_str());
   std::string mon = s.substr(2, 3);
   for (size_t i = 0; i < mon.size(); ++i) mon[i] = (char)std::toupper((unsigned char)mon[i]);
   int month = 0;
   for (int i = 0; i < 12; ++i)
      if (mon == mons[i]) month = i + 1;
   if (month == 0) throw FortranContractViolation("cdate_to_jul_min: bad month in '" + s + "'");
   int year = std::atoi(s.substr(5, 4).c_str());
   int hh = std::atoi(s.substr(10, 2).c_str());
   int mm = std::atoi(s.substr(12, 2).c_str());
   return julday(year, month, day) * MIN_PER_DAY + hh * 60 + mm;
}
