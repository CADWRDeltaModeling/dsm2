/* This file is meant only for documentation for doxygen.*/
/**
 * \namespace oprule::parser
 * Package to parse operating rules from text. The package includes
 * several factory classes that model developers must provide in order
 * to use the operating rule package with text parsing.
 */
namespace oprule{ namespace parser {}}

/**
 *\namespace oprule::expression
 * Package defining tree based expressions
 */
namespace oprule{ namespace expression {}}


/**
 *\namespace oprule::rule
 * Package defining operating rule classes
 */
namespace oprule{ namespace rule {}}

/**
\mainpage The oprule package
Runtime steering for models using expressions parsed from text.
\section Requirements
Use of the oprule package requires:
  - Writing some interface routines
  - Inclusion of the library
\subsection interface Interface routines
In order to hook the oprule package into a model, you need to write:
  - A ModelTimeNodeFactory to create nodes that evaluate model time
  - A NamedValueLookup to create
    - nodes having to do with named values that your model understands (such as time series)
    - nodes that observe model states (ExpressionNode) and
    - nodes that can observe and control inputs (ModelInterfaceNode)
  - An action resolver to discover and deal with actions that conflict

\subsection inclusion Inclusion of the library
The oprule library compiles separately from the model application. The various interface routines should compile with the model, referencing headers from this project as desired.

In addition to including the library, you will need to call the library both at parse time, at the beginning of a time loop and at the end of the time loop.

\section modelinterface ModelInterface: controlling and observing inputs.
The oprule::rule::ModelInterface class is the one by which the operating rule controls the model. Since the types of controls are entirely application specific, the model developer has to write these. In order to write a model interface, you need to:
  - create a mechanism for reading (getting) and writing (setting) the model component
  - decide whether it is time-varying or static
  - if it time-varying, the model must have someway of permanently changing its time-varying data source.

*/
