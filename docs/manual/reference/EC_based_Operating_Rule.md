# EC-based Operating Rule

## Overview

EC-based Operating rules are user-defined rules that control gate operations based on EC results from GTM module. <span style="background-color: yellow;font-weight: bold">User need to use coupled Hydro/GTM binary("hydro_gtm.exe") for EC-based operating rule</span>.

To run coupled HYDRO/GTM, provide the hydro input file and gtm input file as arguments. For instance, the command to run coupled Hydro/GTM with hydro input <span style="background-color: lightgray;">hydro.inp</span> and gtm input <span style="background-color: lightgray;">gtm.inp</span> is:

    hydro_gtm.exe hydro.inp gtm.inp


The input file used for EC-based operating rule is the same as the ones used for general operating rules in DSM2. The use of general operating rules in DSM2 is documented in detail in [Operating Rule Guide](Operating_Rule_Guide.md) and [Operating Rule](Operating_Rule.md).

This document specifically describes the use of EC-based Operating Rule.

## Tables of Contents:

-   [EC-BASED OPERATING RULE](#ec-based_operating_rule)
-   [EC-BASED OPRULE EXPRESSION](#ec-based_oprule_expression)
-   [EC-BASED OPRULE EXAMPLE](#ec-based_oprule_example)


### EC-based operating rule

Defines the name, action and trigger of the EC-based operating rule in OPERATING_RULE table in the input file.
#### OPERATING_RULE Table Field Descriptions

##### NAME

Same as the general operating rule. This is the identifier of the EC-based operatinf rule.

##### ACTION

Definition of the action to be taken when the EC-based trigger transitions from FALSE to TRUE.

##### TRIGGER

An EC-based trigger that activates the ACTION when it transitions from FALSE to TRUE.
It can either be the name of the trigger variable or direct definition of the trigger.

------------------------------------------------------------------------

### EC-based oprule expression

Defines the trigger name and expression in OPRULE_EXPRESSION Table in the input file.

#### OPRULE_EXPRESSION Table Field Descriptions

##### NAME

Name for the trigger. It should be consistent with the name used in [TRIGGER](#trigger).

##### DEFINITION

Definition of the expression -- this will be a formula involving model variables. In the general operating rule, time series can be used; however, **EC-based operating rule can only use model varaibles**.

The trigger keyword for using EC-based operating rule is <span style="background-color: yellow;font-weight: bold">chan_ec</span>.

Inside of the trigger keyword, users need to specify the trigger location (i.e., the channel number and the distance of the location on the channel.)

------------------------------------------------------------------------

### EC-based oprule example

The following example uses the EC-based operating rule to close the gate named <span style="background-color: lightgray;">montezuma_salinity_control</span> when EC at location <span style="background-color: lightgrey;">channel number 517</span> and <span style="background-color: lightgrey;">distance 7876 ft</span> exceeds 8500 µmho/cm.



    OPERATING_RULE
    NAME                 ACTION                                                                                                    TRIGGER
    mscs_close           "SET gate_op(gate=montezuma_salinity_control,device=radial_gates,direction=from_node) TO 0.0"             "ec_too_high"


    OPRULE_EXPRESSION
    NAME                 DEFINITION
    ec_too_high          "chan_ec(channel=517, dist=7876) > 8500"


The operating rule has an identifier <span style="background-color: lightgray;">mscs_close</span>. The trigger name is <span style="background-color: lightgray;">ec_too_high</span>, which is the same in both OPERATING_RULE table and OPRULE_EXPRESSION table. When the trigger condition becomes True, the specified Action in OPERATING_RULE table will change the gate operation.
