# Operating Rule

## Overview:

Operating rules are user-written rules that manipulate model inputs such
as gate operations, boundary flows based on observations of the current
state of the running model. Operating rules are documented in detail in
the DSM2 Op Rule Guide. The Operating Rules table lists the time series,
expressions and rule definitions.

  

## Tables:

-   [OPERATING RULE](#OperatingRule-operating_rule)
-   [OPRULE_TIME_SERIES](#OperatingRule-oprule_time_series)
-   [OPRULE_EXPRESSION](#OperatingRule-oprule_expression)

  

### OPERATING_RULE

Defines the name, action and trigger of the operating rule.

#### Field Descriptions

##### NAME

Name of the operating rule. This is the identifier of the rule.

##### ACTION

Definition of the action to be taken when the trigger transitions from
FALSE to TRUE.

##### TRIGGER

Trigger that activates the rule when it transitions from FALSE to TRUE.
If the trigger is NULL it will become the trivial TRUE trigger, which is
assumed to make a transition from FALSE to TRUE at startup (it is not
"always" active).Use

#### Table Info

##### Identifier:

NAME

##### Parent Table:

Table is parent

##### Include Block:

OPERATIONS

------------------------------------------------------------------------

  

### OPRULE_TIME_SERIES

This table lists time series that are used in forming action and trigger
definitions. The table is not a child table -- it is a top-level layered
table.

#### Field Descriptions

##### NAME

Name assigned to the time series. This is the identifier of the series.
It is also the name used to refer to the series in expressions.

##### FILLIN

Method used to interpolate when the model time step is finer than the
time series time step. Use "last" to use the last time stamp in the
period (a HEC-DSS convention) and "linear" to interpolate linearly

##### FILE

Input  file (HEC-DSS or text file in HEC-DSS format) storing the time
series or the word constant if the series is assigned a fixed value.

##### PATH

HEC-DSS path of the data within the Input File or the value (e.g. 2.0)
if the series is assigned a fixed value.

#### Table Info

##### Identifier:

NAME

##### Parent Table:

Table is parent

##### Include Block:

OPERATIONS

------------------------------------------------------------------------

  

### OPRULE_EXPRESSION

This table allows the user to list expressions that can be reused later
in operating rule actions and triggers. Expressions cannot depend on
other expressions. Expressions are not a child table -- the table is a
top-level layered table.

#### Field Descriptions

##### NAME

Name of the expression. This is the identifier of the expression. It is
also the name used to refer to the expression in expressions. 

##### DEFINITION

Definition of the expression -- this will be a forumula involving model
variables, seasons and time series. The time series can be from the
above time series table or elsewhere in the Input Time Series section.
Please see the Operating Rules Guide for more details.

#### Table Info

##### Identifier:

NAME

##### Parent Table:

Table is parent

##### Include Block:

OPERATIONS

------------------------------------------------------------------------

  

-   Numerous usage comments in the Operating Rules Guide.
-   Time series referenced in the operating rules may be defined in an
    OPRULE_TIME_SERIES table or they may be time series defined
    elsewhere, such as the name of a boundary flow.
-   Neither the OPRULE_EXPRESSION or OPRULE_TIME_SERIES table is a child
    table of OPERATING_RULE. However, it is common to put related items
    in the same file.
