// A C++ stand-in for the Fortran side of the DSM2 hydro model, as seen by the oprule interface.
//
// WHY THIS EXISTS
//   dsm2/src/oprule_interface/*.cpp (the real C++ binding: name lookup, factories, model
//   interfaces, time nodes, resolver) calls extern "C" functions that are implemented in Fortran
//   (dsm2/src/common/model_interface.f90 and friends). The tests link the REAL binding code
//   against this mock, so what is under test is the C++ binding; the mock only has to behave like
//   the Fortran. Every function in Dsm2FortranMock.cpp says which Fortran routine it mirrors and
//   what that routine does, so the file doubles as an executable description of the contract.
//
// WHAT IS MIRRORED EXACTLY, WHAT IS ASSUMED
//   Mirrored from source (quirks included, flagged "FORTRAN QUIRK"): lookups by name, gate and
//   device state setters/getters, data-source semantics (set_datasource / fetch_data /
//   store_values), setFree, time functions, comp-point interpolation interface.
//   Assumed (flagged "ASSUMPTION"): the layout of model arrays, that gate/device/ext-flow/
//   transfer/time-series names are stored lower case by the input readers, and the Julian-minute
//   epoch. These are not verified against a running model.
//
// FORTRAN UNDEFINED BEHAVIOUR
//   Where the Fortran would index an array out of bounds the mock throws
//   FortranContractViolation, so a test fails loudly instead of reading garbage.
//   Where the Fortran calls exit(n) the mock throws FortranExit{n}.
#ifndef OPRULE_TEST_DSM2_FORTRAN_MOCK_H
#define OPRULE_TEST_DSM2_FORTRAN_MOCK_H

#include <functional>
#include <map>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace dsm2mock {

// dsm2/src/dsm2_defs/constants.f90
enum SourceType { CONST_DATA = 128, DSS_DATA = 256, EXPRESSION_DATA = 512 };
const int MISS_I = -901;
const double MISS_R = -901.;
// dsm2/src/common/gates_data.f90
const int FLOW_COEF_TO_NODE = 1;
const int FLOW_COEF_FROM_NODE = -1;
const int FLOW_COEF_TO_FROM_NODE = 0;

struct FortranExit {
   explicit FortranExit(int c) : code(c) {}
   int code;
};

struct FortranContractViolation : public std::logic_error {
   explicit FortranContractViolation(const std::string& m) : std::logic_error(m) {}
};

// dsm2/src/dsm2_defs/type_defs.f90: type datasource_t {value, source_type, indx_ptr}
struct DataSource {
   DataSource() : value(0.), source_type(CONST_DATA), indx_ptr(0) {}
   double value;
   int source_type;
   int indx_ptr;
};

// Initial state of a gate device. ASSUMPTION: the input processors (process_gate.f90,
// process_input_gate.f90) leave every property with a CONST_DATA source holding its input value
// unless a DSS time series is attached, so store_values() reproduces the initial value each step.
struct DeviceInit {
   DeviceInit()
      : opCoefToNode(1.), opCoefFromNode(1.), baseElev(0.), height(0.), maxWidth(0.), nDuplicate(1.),
        flowCoefToNode(0.), flowCoefFromNode(0.) {}
   double opCoefToNode, opCoefFromNode, baseElev, height, maxWidth, nDuplicate, flowCoefToNode, flowCoefFromNode;
};

struct Device {
   Device() : structureType(1) {}
   std::string name;  // stored lower case (gates.f90 deviceIndex lower-cases it before comparing)
   double opCoefToNode, opCoefFromNode, baseElev, height, maxWidth, nDuplicate, flowCoefToNode, flowCoefFromNode, flow;
   DataSource op_to_node, op_from_node, elev, height_ds, width, nduplicate;
   int structureType;  // 1 weir, 2 pipe
};

struct Gate {
   Gate() : free(false), objType(1), objId(1), compPoint(0), nodeCompPoint(0), nodeId(0), flow(0.) {}
   std::string name;  // ASSUMPTION: stored lower case (gateNdx lower-cases the query, then compares)
   bool free;         // gates_data: Gate%free; true = physically removed ("GATE_FREE")
   std::vector<Device> devices;
   DataSource install;
   // what the gate is connected to (gates_data: objConnectedType 1 channel, 3 reservoir)
   int objType, objId, compPoint, nodeCompPoint, nodeId;
   std::string objName;  // label returned by get_gate_object_name
   double flow;          // Gate%flow
};

struct ExternalFlow {  // grid_data: qext(i)
   ExternalFlow() : flow(0.) {}
   std::string name;  // ASSUMPTION: stored lower case; qext_index does NOT lower-case the query
   float flow;        // real*4 in type_defs (qext_t): values read back are rounded to single precision (checked in the Fortran test)
   DataSource datasource;
};

struct Transfer {  // grid_data: obj2obj(i)
   Transfer() : flow(0.) {}
   std::string name;
   float flow;        // real*4 in type_defs (obj2obj_t)
   DataSource datasource;
};

struct Reservoir {
   Reservoir() : stage(0.) {}
   std::string name;               // lower case (resNdx lower-cases the query)
   std::vector<int> internal_nodes;  // res_geom(i)%node_no(1:nnodes)
   std::vector<double> qres;       // QRes(i, 1:nnodes), flow per connection
   double stage;                   // YRes(i)
};

struct Channel {
   int ext;               // external channel number
   double length;
   int first_point;       // global computational-point index of the first point (1-based)
   int npts;              // evenly spaced computational points; ASSUMPTION about the real grid
   std::function<double(double)> stage_fn, flow_fn, velocity_fn;  // value as a function of distance
};

struct PathInput {  // iopath_data: pathinput(i); one array shared by ALL input time series
   std::string name;   // lower case (process_input_oprule.f90 calls locase(name))
   bool is_oprule;
   double value;       // refreshed every step by get_inp_data
};

class Model {
public:
   Model() { reset(); }
   void reset();

   // ---- grid
   void add_channel(int ext, double length, int npts = 11);
   void set_stage(int ext, std::function<double(double)> fn);
   void set_flow(int ext, std::function<double(double)> fn);
   void set_velocity(int ext, std::function<double(double)> fn);
   void add_node(int ext_node);  // external node numbers; internal index = order added (1-based)
   void add_reservoir(const std::string& name, const std::vector<int>& ext_nodes);
   void add_external_flow(const std::string& name, double initial_flow);
   void add_transfer(const std::string& name, double initial_flow);
   int add_gate(const std::string& name);
   void add_device(const std::string& gate, const std::string& device, const DeviceInit& init = DeviceInit());
   void add_path_input(const std::string& name, double value, bool is_oprule = true);

   // ---- accessors for tests (1-based internal indices as in Fortran)
   int gate_no(const std::string& name) const;
   Gate& gate(const std::string& name);
   Device& device(const std::string& gate, const std::string& device);
   ExternalFlow& qext(const std::string& name);
   Transfer& transfer(const std::string& name);
   Reservoir& reservoir(const std::string& name);
   PathInput& path(const std::string& name);

   // ---- time (runtime_data: julmin = minutes since 31DEC1899 2400; 01JAN1900 00:00 = 1440, day of year 0-based:
   //      verified against the real Fortran by test_time in dsm2/tests/model_interface)
   void set_time(int year, int month, int day, int hour, int minute);
   int julmin;
   int dt_seconds;  // netcntrl: DT

   // ---- logging
   // model_interface.f90 get_oprule_log_level(): the oprule_log_level scalar if set, else derived from
   // print_level (4, 5, 6 give 1, 2, 3; lower gives 0). The mock holds the already-resolved value.
   int oprule_log_level;

   // model_interface.f90 option getters (SCALAR table): the mock holds the already-resolved values
   std::string oprule_log_file;
   int oprule_log_text;         // also write the text log
   int oprule_log_devices, oprule_log_context, oprule_log_trace_interval, tidefile_gate_state;
   double oprule_log_tol_op, oprule_log_tol_dim, oprule_log_flush_hours;
   std::string tidefile_name;   // io_files(hydro, io_hdf5, io_write)%filename; empty: no tide file

   // ---- data sources
   // model_interface.f90 fetch_data()
   double fetch_data(const DataSource& source);
   // netbnd.f90 store_values(): copy every data source into the current model values
   void store_values();
   // netbnd.f90 SetBoundaryValuesFromData(): refresh time-series values for julmin, then store_values()
   void set_boundary_values_from_data(const std::function<double(const std::string&, int)>& ts_source);

   // ---- bookkeeping for tests
   std::vector<std::pair<int, double> > comp_point_requests;  // (internal channel, distance) per chan_comp_point call

   std::vector<Channel> channels;
   std::vector<int> nodes;
   std::vector<Reservoir> reservoirs;
   std::vector<Gate> gates;
   std::vector<ExternalFlow> qexts;
   std::vector<Transfer> transfers;
   std::vector<PathInput> paths;
   std::vector<double> stage_points, flow_points;  // by global comp point (index 0 unused)
   int next_point;
};

Model& model();

// Calendar helpers (jliymd / iymdjl equivalents).
int julday(int year, int month, int day);  // days since 31DEC1899; 01JAN1900 = 1
void civil_from_julday(int jday, int& year, int& month, int& day);

}  // namespace dsm2mock

#endif
