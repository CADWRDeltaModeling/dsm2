---
description: "Guidelines and best practices for developing, modernizing, refactoring, profiling, and documenting the DSM2 Fortran codebase using AI assistance."
---

# DSM2 Project AI Assistant Instructions

## Overview & Scope
DSM2 (Delta Simulation Model 2) is a 1-D mathematical modeling suite (hydrodynamics via HYDRO, water quality transport via QUAL/GTM, and particle tracking via PTM) developed by the California Department of Water Resources.
The codebase is historically written across Fortran 77, Fortran 90, and modern Fortran standards, with auxiliary components in C/C++, and Python. It supports Linux and Windows platforms.

These instructions guide AI assistants in modernizing the Fortran codebase, eliminating bugs, simplifying legacy architectures, optimizing numerical performance, and generating accurate code and user documentation.

---

## 1. Project Objectives
1. **Modernize Fortran Code**: Transition legacy F77 fixed-form and transitional F90/F95 patterns into clean, standard-compliant Modern Fortran (Fortran 2008/2018 free-form).
2. **Bug Hunting and Defect Resolution**: Detect subtle numerical inaccuracies, uninitialized variables, array bounds violations, race conditions, memory leaks, and I/O format mismatches.
3. **Simplify and Clean Code**: Eliminate dead code, obsolete common blocks, cyclic module dependencies, spaghetti `goto` constructs, and excessive nesting.
4. **Comprehensive Testing Suite**: Add and expand unit tests, system tests, regression tests, and fast smoke tests across the entire modeling suite.
5. **Profile and Optimize**: Identify computational hotspots in time-stepping loops, vectorization bottlenecks, and unnecessary memory allocations.
6. **Improve Documentation**: Generate comprehensive FORD/Doxygen module docblocks, inline algorithm explanations, and synchronize user/developer guides.
7. **Use Modern Data Tools**: Migrate to NetCDF format following the CF convention from the DSS file while maintaining the support of the DSS format by providing pre- and post-processing tools.
8. **Wrap in Python**: Update the code to import in C-compatible libraries using ISO binding and wrap in Python so that Python can drive DSM2.

---

## 2. Fortran Modernization Standards

### 2.1 Code Style & Formatting
- **Source Form**: Free-form Fortran (`.f90` extension). Avoid fixed-form column dependencies.
- **Indentation & Whitespace**:
  - Indent with 4 spaces. Never use tab characters (`\t`).
  - Strip trailing whitespace on all lines.
  - End files with Unix line endings (`LF`).
  - Keep maximum line length between 80 and 132 characters; use `&` for line continuation.
- **Keywords and Case**:
  - Consistent lower-case for Fortran keywords, standard types, and intrinsic functions (`subroutine`, `function`, `module`, `implicit none`, `integer`, `real`, `do`, `if`, etc.).
  - Descriptive `snake_case` or readable CamelCase for module, procedure, and variable names. Avoid cryptic shortened acronyms.
- **Explicit Typing**:
  - Always require `implicit none` at the module level and in every standalone procedure.
  - Avoid implicit typing or `dimension` statements without types.
- **Precision and Kinds**:
  - Use `iso_fortran_env` kinds (`real64`, `real32`, `int32`, `int64`) or centralized precision definitions (e.g., `DP`). Never use non-standard `real*8` or `real*4`.
  - Use explicit kind suffixes on literals (e.g., `0.0_dp`).

### 2.2 Procedures & Interfaces
- **Argument Intents**: Explicitly declare `intent(in)`, `intent(out)`, or `intent(inout)` for all subroutine and function dummy arguments.
- **Pure and Elemental Procedures**: Mark side-effect-free helper procedures as `pure` or `elemental` to aid compiler vectorization and static analysis.
- **Modules over Common Blocks**:
  - Eliminate F77 `COMMON` blocks and `EQUIVALENCE` statements. Encapsulate shared state inside dedicated modules with explicit `public` and `private` accessibility attributes.
  - When importing symbols with `use`, prefer `only:` clauses (e.g., `use mod_foo, only: bar, baz`) to clarify symbol provenance and prevent namespace pollution.
- **Elimination of Obsolete Constructs**:
  - Refactor `goto` and arithmetic `if` into structured control flow (`do`, `do while`, `cycle`, `exit`, `select case`, `if-then-else`).
  - Replace statement functions with internal or module functions.
  - Replace fixed-length arrays with `allocatable` dynamic arrays where size varies at runtime. Always verify allocation status with `allocated()` and check `stat` parameters where appropriate.

---

## 3. Structural Simplification & Refactoring

### 3.1 Resolving Cyclic Dependencies
- When two modules depend on each other, refactor by:
  1. Extracting shared types and constants into a lower-level leaf module (e.g., `*_types` or `*_data`).
  2. Utilizing submodule architecture (`submodule (parent_mod) submod_impl`) to separate interface definitions from routine implementations.
  3. Passing dependencies via procedure arguments rather than cross-referencing module globals.

### 3.2 Dead Code & Redundancy Removal
- Identify unused subroutines, legacy diagnostic prints, unreachable blocks, and duplicated helper functions (e.g., duplicate string parsing or time conversion utilities).
- Verify removal against the entire project using symbol references and build targets before deleting legacy code.

---

## 4. Static Analysis, Quality & Profiling Tooling

When analyzing, cleaning, or optimizing code, use and recommend the following toolchain:

| Tool | Purpose | Usage in DSM2 Workflow |
|------|---------|-------------------------|
| **Fortitude** | Modern Fortran linter & rule checker | Lint for code smells, obsolescent language features, missing intents, implicit typing |
| **jscpd** | Copy/paste code duplication detection | Scan Fortran and C/C++ files across `dsm2/src/` to identify repeated logic across modules |
| **Lizard** | Cyclomatic complexity & code metrics analyzer | Identify overly complex subroutines (CCN > 15), deeply nested logic, and bloated procedures |
| **fpretty** | Fortran auto-formatter | Enforce consistent indentation, operator spacing, and keyword casing |
| **GProf / Intel VTune / Tachyon** | CPU profiling | Pinpoint computational bottlenecks in time-step loops (`gtm_loop`, `hydro_loop`, matrix solvers) |
| **Valgrind / AddressSanitizer (ASan)** | Memory profiling | Check for memory leaks, invalid deallocations, and array out-of-bounds errors |
| **FORD ** | Automated code documentation generator | Parse module comments, interfaces, types, and procedures into static HTML docs |

---

## 5. Testing & Verification Guardrails

### 5.1 Test Pyramid & Strategy
- **Unit Tests**:
  - Test individual procedures, mathematical kernels (e.g., flux limiters, interpolation routines, Thomas algorithm solver), and parsing functions in isolation.
  - Structure test programs/modules cleanly using modern Fortran testing frameworks (e.g., `test-drive`, or Boost.Test).
- **Smoke Tests**:
  - Fast, lightweight runs (< 30 seconds) that execute basic sanity checks on core executables (`hydro`, `qual`, `gtm`, `ptm`) to immediately verify build integrity and catch crashing regressions.
- **System & Integration Tests**:
  - End-to-end multi-module pipeline execution (e.g., running Hydro to generate an HDF5 tidefile, followed by GTM/QUAL simulating constituent transport over a short network).
- **Regression Tests**:
  - Full-scale test suite located in `/scratch/dms/knam/repos/dsm2_regression_tests`.
  - Validate output files (HDF5 and DSS datasets) against historical benchmark baselines within defined numerical tolerances using pytest (`test_dsm2_regression_*.py`).

### 5.2 Verification Guardrails
- **Preserve Numerical Behavior**:
  - Hydrodynamic and transport calculations must match baseline simulation outputs within strict numerical tolerance (e.g., `rtol=1e-4`, `atol=1e-6`).
  - Avoid reordering floating-point operations where strict reproducibility is required, unless mathematically equivalent and validated against test suites.
- **Compile Verification**:
  - Ensure code compiles cleanly with Intel Fortran (`ifx`/`ifort`) and GNU Fortran (`gfortran`) without new compiler warnings.
  - Keep build scripts such as `./build_all_hpc4_mine.sh` and CMake target definitions in sync when adding or renaming source files.

---

## 6. Documentation Standards

### 6.1 Code-Level Documentation
- Use FORD-compatible (or Doxygen-compatible) docblocks (`!>` and `!!`) for modules, types, and procedures:
  ```fortran
  !> Calculates face-centered advection fluxes with TVD limiters
  function compute_face_flux(ncell, flow, conc) result(status)
  ```
- Document the physical units of all state variables and model parameters (e.g., `[ft]`, `[cfs]`, `[mg/L]`, `[hours]`).

### 6.2 Manuals & Architectural Guides
- Document simulation workflow pipelines (e.g., DSM2 Hydro -> HDF5 Tidefile -> GTM/QUAL -> DSS).
- Keep Markdown guides under `docs/` or `docsrc/` up to date with algorithmic changes, input deck specifications, and build prerequisites.

---

## 7. Git Commit Message Standards

Follow the standard prefix convention for subject lines combined with the 7 rules of good commit messages.

### 7.1 Commit Message Structure
```text
<type>(<scope>): <Subject line summary>

<Body explaining what and why vs. how>

<Optional Footer / Issue references>
```

### 7.2 Common Prefixes (Types)
- `feat`: A new feature or capability (e.g., NetCDF reader, new constituent transport scheme).
- `fix`: A bug fix or defect correction.
- `refactor`: Code change that neither fixes a bug nor adds a feature (e.g., F77 to F90/F2008 conversion, removing dead code, resolving circular dependencies).
- `perf`: A code change that improves performance or optimizes computational hotspots.
- `test`: Adding missing tests, updating regression tests, or adding unit/smoke test infrastructure.
- `docs`: Documentation-only changes (e.g., FORD docblocks, manuals, markdown guides).
- `style`: Changes that do not affect code logic (e.g., white-space, formatting with `fpretty`, line continuation).
- `build`: Changes affecting the build system, CMake scripts, compilers, or external dependencies.
- `ci`: Changes to CI/CD workflows, regression test runners, or automation scripts.
- `chore`: Routine maintenance, updating `.gitignore`, license headers, or repo housekeeping.

*(Scope is optional but encouraged, e.g., `feat(gtm):`, `fix(hydro):`, `refactor(qual):`, `test(regression):`.)*

### 7.3 Seven Rules for the Commit Subject and Body
1. **Separate subject from body with a blank line.**
2. **Limit the subject line to 50 characters**
3. **Do not end the subject line with a period.**
4. **Use the imperative mood in the subject line** (e.g., "add", "fix", "refactor", "change", not "added", "fixes", "changing"). A good heuristic: *"If applied, this commit will `<your subject line>`"*.
5. **Wrap the body at 72 characters.**
6. **Use the body to explain *what* and *why* vs. *how*.** Focus on context, the problem being solved, and why this solution was chosen; the diff already shows how it was implemented.
