# Tutorial 6: Operating Rules

**Task**

-   Operate a gate based on stage criteria
-   Regulate a source/sink inflow

  
**Skills Gained** Get an introduction to operating rules

  
The purpose of this tutorial is to practice using Operating Rule
Language (ORL) statements to set gate operations and flows. With
operating rules, expressions can be crafted to steer the model
on-the-fly; e.g., a gate can be directed to automatically close when
stage conditions reach a certain threshold. In this tutorial we will
create operating rules to operate a gate and to regulate a source/sink
inflow.  
<img src="../../images/icon_warning.png" width="29" height="29" />
Extensive documentation on the DSM2 operating rules can be found at:  
START menu *Programs*  *DSM2_v8*  *DSM2_documentation* Operating
Rules

<img src="../../images/fig_res_conn_w_trans.png" width="623" height="347" />
**Figure 1:** **Simple channel with a reservoir, gate, flow transfer and
dummy reservoir.**

1.  **Adding a Second Gate Where Op Rule Will Be Applied**

In this step of the tutorial we will prepare a new layer and add the
gate that will be manipulated by the op rule.

1.  1.  In Windows Explorer, navigate to the directory,
        *\\{DSM2_home}\tutorial\simple\t6_oprule*.
    2.  Create a file *grid_tutorial_opitems.inp*.
    3.  Open *grid_tutorial_base.inp*. We are going to copy items from
        this file into the new file with minor changes:
        1.  Copy the GATE table with *gate_1,* paste it into
            *grid_tutorial_opitems.inp* and change the following fields:
            1.  NAME: *gate_2*

            2.  FROM_OBJ: channel

            3.  FROM_IFENTIFIERS: 5

            4.  TO_NODE: 5
    4.  In the *Gate_Weir Devices table*:
        1.  Copy the data from gate_1 to *grid_tutorial_opitems.inp*,
            change the gate name to gate_2 and change the following
            fields:

        2.  1.  GATE_NAME: Gate_1
            2.  Elev: *-2*
    5.  Save the current settings.
    6.  Add *grid_tutorial_opitems.inp* to the list of included files in
        *hydro.inp.*

  

1.  **Adding Output for the Second Gate:**
    1.  Create a file called *output_oprule_tutorial.inp*.
    2.  Create the OUTPUT_GATE table:

OUTPUT_GATE  
NAME GATE_NAME DEVICE VARIABLE INTERVAL PERIOD_OP FILE  
END

1.  1.  In the output table enter the following values into the
        appropriate fields:
        1.  1.  Output Name: *gate_2_weirop*
            2.  Gate name: *gate_2*
            3.  Device: *weir*
            4.  Variable: *op-from-node*
            5.  Time Interval: *15min*
            6.  Period Op: *inst*
            7.  File: *${HYDROOUTDSSFILE}*
    2.  Add the following channel outputs in a new OUTPUT_CHANNEL table:

OUTPUT_CHANNEL  
NAME CHAN_NO DISTANCE VARIABLE INTERVAL PERIOD_OP FILE  
**trigger_loc** **4 7500 stage 15min inst ${HYDROOUTDSSFILE}**  
**ds_gate2 5 0 flow 15min inst** **${HYDROOUTDSSFILE}**  
END

1.  1.  Add the output layer to the list of include files in *hydro.inp*
        and save your work.

  

1.  **Create an Operating Rule to Close the Weir when Stage is Low:**

Now we are ready to write the first operating rule. This rule closes the
new gate we created during times where stage at a monitoring point is
low. First we will define the rule in terms of an expression called
*stage_critical* (the condition where stage violates a minimum) and
*op_applies* (a seasonal condition that is True when we are controlling
the gate for stage. In a later step we will define these variables.

1.  1.  Create a file called *oprule_tutorial.inp*.
    2.  Create the *Operating Rules* table:

OPERATING_RULE  
NAME ACTION TRIGGER  
END

1.  Enter the following values into the appropriate fields:
        1.  Name: *weir_close*
        2.  Action Definition: "*SET gate_op(gate=gate_2,
            device=weir, direction=from_node) TO CLOSE RAMP 30MIN"*

<img src="../../attachments/87228781/87228782.png" width="29" height="29" /> You
must use quotes for inputs with spaces.

1.  Trigger Definition: "*stage_critical AND op_applies"*

    2.  Create an OPERATION include block in hydro.inp and add the new
        file so that it will be used by DSM2-HYDRO.

OPERATION  
oprule_tutorial.inp  
END

1.  Save the current settings.

  
Note that the expressions stage_critical and op_applies will be created
in a later step.

1.  **Create an Operating Rule to Open the Weir when Stage is High:**

As before, we will enter the rule to open the weir first in terms of the
expressions *stage_relax* (a condition where stage is safely above a
threshold where we can open the gate) and *op_applies*. In the next step
we will define these expressions.

1.  1.  In the *Operating Rules* *table* enter the following values into
        the appropriate fields:
        1.  Name: *weir_open*
        2.  Action Definition: "*SET gate_op(gate=gate_2,
                device=weir, direction=from_node) TO OPEN RAMP 30MIN"*
        3.  Trigger Definition: "( stage_relax AND op_applies) OR
                NOT(op_applies)"
    2.  Save the current settings.
    3.  In the *hydro.inp* file, add the following environmental
        variables and values into the ENVVAR section:

STAGE_CRITICAL 1.4  
STAGE_RELAX 1.6

1.  **Define Expressions used in the rule**
    1.  In the file oprule_tutorial.inp, create the OPRULE_EXPRESSION
        table:

OPRULE_EXPRESSION  
NAME DEFINITION  
END

1.  1.  1.  Enter the following values into the appropriate fields:
            1.  Name: *op_applies*
            2.  Definition: "*SEASON \< 01FEB"*
        2.  Enter the following values into the appropriate fields.
            Don't forget quotes!!
            1.  Name: *stage_critical*
            2.  Definition: "*chan_stage(channel=4, dist=7500) \<
                ${STAGE_CRITICAL}"*
        3.  Enter the following values into the appropriate fields:
            1.  Name: *stage_relax*
            2.  Definition: "*chan_stage(channel=4, dist=7500) \>
                ${STAGE_RELAX}"*

    2.  Save the current settings.

    3.  Now run HYDRO and QUAL:
        1.  Open a command window for the *t6_oprule* directory.
        2.  In the command window, type: *hydro hydro.inp*.
        3.  In the command window, type: *qual qual.inp*.
        4.  Open the *output.dss* file in the *t6_oprule* directory, and
            examine the results.

  

1.  **Add a Reduced Flow Operating Rule:**

In our next operating rule, we will control the inflow to a node by
having it toggle back and forth between a larger "full flow" and a
"reduced flow". First we will enter the rule and then we will define the
full and reduced flows.

1.  1.  In the *Operating Rules table* enter the following values into
        the appropriate fields:
        1.  1.  Name: *flow_reduce*
            2.  Action Definiton: *SET ext_flow(name=source1) TO
                ifelse(stage_critical,reduced_flow,full_flow)*
            3.  Trigger Definition: *TRUE*
    2.  Now create the expressions that define *full_flow* and
        *reduced_flow*. In the *Oprule* *Expressions* table:
        1.  Enter the following values into the appropriate fields that
            define *full_flow.* This will involve the time series
            *source_flow* which we will enter later:
            1.  Input Name: *full_flow*

            2.    

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

                Definition: *ts(name=source_flow) \[note: this is a
                reference to a time series we haven't defined yet\].*

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  
        2.  Do the same for *reduced_flow.* Note: we are defining
            *reducedflow in terms of the time series. There is no
            guarantee of what order expressions will be evaluated, so
            you cannot safely define \_reduced_flow* in terms of another
            expression such as *full_flow*. Enter the following values
            into the appropriate fields:
            1.  Input Name: *reduced_flow*
            2.  Definition: *0.5\*ts(name=source_flow).*
    3.  Save the current settings.
        1.  Now we will define the *source_flow* time series upon which
            the *full_flow* and *reduced_flow* expressions are based.
    4.  Create the *Operation Time Series* table:

OPRULE_TIME_SERIES  
NAME FILLIN FILE PATH

1.  1.  1.  Enter the following values into the appropriate fields:
            1.  Input Name: *source_flow*

            2.  Input File: *${TUTORIALINPUT}*

            3.    

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

                Path: */TUTORIAL/SOURCE/FLOW//15MIN/CONSTANT/ \[ Note:
                there are two forward slashes between FLOW* and
                *15MIN\]*

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

            4.  Fillin: *none*

    2.  Save the current settings.

<!-- -->

1.  **Override the Expression op_applies:**

Recall that *op_applies* is used to determine when the weir is operated.
Previously the definition of this expression was seasonal: the
expression was SEASON \< 01FEB. The goal now is to make the same
expression depend on a time series. Rather than change the expression,
we will override it in a new layer.

1.  1.  Add a new Operating Rules Layer:
        1.  Create a file called *oprule*\_ *tutorial* revision.inp\_
    2.  Redefine the expressions that define *op_applies*. In the
        *Expressions* table:
        1.  Create the OPRULE_EXPRESSION table.
        2.  Enter the following values into the appropriate fields:
            1.  Input Name: *op_applies*

            2.    

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

                Definition: "ts(name=op_used)\>0.0" \[note: this is a
                reference to a time series we will define in the next
                step\].\_

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  

                  
    3.  Define the time series *op_used* on which the *op_applies*
        expression depends. In the *Operation Time Series* table:
        1.  Right-click and select *Insert row*.
        2.  Enter the following values into the appropriate fields:
            1.  Input Name: *op_used*
            2.  Input File: *${TUTORIALINPUT}*
            3.  Path: */TUTORIAL/GATE/FLAP_OP//IR-YEAR/TIMEVAR/*
            4.  Fillin: *none*
    4.  Add *oprule_tutorial_revision.inp* after *oprule_tutorial.inp*
        in the OPERATIONS block of hydro.inp so that it will be used by
        HYDRO.
    5.  Run HYDRO and QUAL and examine the results.

  
  
  
  
  
  
  
  
  
  
  

## Attachments:

<img src="../images/icons/bullet_blue.gif" width="8" height="8" />
[worddavd56eb5f63f4c9181cb2a8632c8c6c562.png](../../attachments/87228781/87228780.png)
(image/png)  
<img src="../images/icons/bullet_blue.gif" width="8" height="8" />
[worddave8e1df4e853bb46c4ee6f68afece040d.png](../../attachments/87228781/87228782.png)
(image/png)  
