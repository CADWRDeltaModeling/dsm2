# Operating Rule Guide

# Operating Rule Guide

## Introduction

DSM2 uses a text language for operating rules, and the rules are stored
in the database. Operating rules combine trigger and action directives,
each of which is an expression based on observed model states, seasonal
information and exogenous time series input as well as other
expressions.

Actions are things the operating rule does. In DSM2-DB, the actions
affect either gate devices or source/sink flow boundaries. For gate
devices the operating flow coefficient can be changed. For sources and
sinks, flow may be set to a new constant value or a new time series.
Expressions for actions tend to be of the form:

    SET model_object TO numerical_expression

The action becomes applicable when a corresponding trigger goes from
false to true. Triggers are written with expressions that evaluate true
or false:

    chan_stage(channel=132, dist=1000) < -0.1

Some rules are immediate responses to model conditions (close the gate
when stage dips below 0.5). Other rules use triggers to describe seasons
or situations where the action is applicable (reduce a boundary flow
when the month is between May and September). Still others rules apply
from the beginning of the run and the trigger column is just a nuisance
–.

## Expressions

An expression is just a named quantity that is derived from model data,
outside time series data, math and time functions. An example of a
simple numerical expression based on current DSM2-DB flow looks like
this:

    ebb := chan_flow(channel=132, dist=1000) > 0.01

This example samples the current time step model flow 1,000 ft
downstream of the upstream node in channel 132 and checks whether it is
greater than 0.01 cfs. The expression assigns the answer the name ebb,
so it can be reused in later expressions. Note that ebb is a logical
expression which evaluates to true or false depending on the model time
step. Numerical expressions will be introduced shortly.

Assignments of named expressions always start with a name the assignment
operator “:=”. Spaces around the assignment and greater-than operators
are optional. The assignment operator isn’t actually used in the GUI,
because there is a separate column for the name and definition.

The chan_flow part of the expression represents the value of a model
variable. Model variables typically require identifiers, which are
included in parenthesis and are a comma-separated list with elements
that depend on the context (see the section below on DSM2 model variable
identifiers). These identifiers can be numerical or text strings:

    chan_flow(channel=132, dist=1000) ...numerical

    gate_op(gate=middle_river_barrier, device=weir) ...strings

The examples thus far have been logical expressions. Logical expressions
usually appear in triggers rather than actions. Besides logical
expressions, expressions that evaluate to numerical values can be
defined:

    ebbmagnitude := log(chan_flow(channel=132, dist=1000))

and expressions can also involve simple math operators. For instance:

    ebbmagnitude := log(chan_flow(channel=132, dist=1000))

is an expression that evaluates flow, applies the log function to it and
then assigns it to the variable name ebbmagnitude. For details, see the
section below on Math Operators)

Model time can also be used in expressions. The following expression
describes the VAMP season for San Joaquin river management:

    vamp := (MONTH == APR) or (MONTH == MAY)

The definition could also include the date, day of the month, or time of
day.

month, or time of day.  
Finally, the following example combines a model state (stage/water
surface) observation, an external time series (called tide_level) and
some simple arithmetic. The expression might be used with a slowly
fluctuating tide or sea level datum to provide an idea of critical stage
in the South Delta compared to ambient tide conditions.

    critical_stage := chan_stage(channel=132,dist=1000)<(tide_level-1.0)

## Operating Rules

It is now straightforward to use expressions in operating rules. The
following example is based on expressions that were developed above.
**Bold face** words correspond to tables or columns of the GUI.

### Name

    middle_vamp_ebb

### Expressions

    ebb := chan_flow(channel=132, dist=1000) > 0.01
    vamp := (month == Apr) OR (month == May)

### Trigger

    vamp AND ebb

### Action

    SET gate_op(gate=middle_river_barrier, device=weir) TO ts(new_time_series)

  

The *middle_vamp_ebb* operating rule lies dormant until the first time
step when vamp and ebb (a compound expression based on the expressions
**vamp** and **ebb**) becomes true. At that point the action will be
taken and the weir operating coefficient will start to operate according
to the values in the DSS time series new_time_series. Note that except
for the expression definitions, the parts of this operating rule can be
united using the name assignment (:=) and WHERE directives:

    middle_vamp_ebb := SET gate_op(gate= middle_river_barrier,device = weir) TO ts(new_time_series) WHERE (vamp AND ebb)

This is the form of the operating rule that would be used, say, when
parsing a text file rather than using the GUI.

## Prediction

Anticipation using linear or quadratic extrapolation can be added to
numerical expressions in expressions using the PREDICT function. What is
nice about PREDICT is that it allows trigger expressions to more
accurately express the intent of a rule, because you don't need
"buffers" which are confusing and inaccurate.

For instance lets say you want to take some action like close a gate to
protect stage in channel 206 in the South Delta from going below zero.
If you use a buffer, you write the following:

    SET [some action] WHEN chan_stage(chan=206, dist=0) <1);

This is confusing because the value "1" is used as the trigger criterion
when the intent has to do with stage of 0 and not 1. It is inaccurate
because it will go off no matter what the trend is. With anticipation,
the same rule would look like this:

    SET [some action] WHEN PREDICT(chan_stage(chan=206, dist=0),LINEAR, 30MIN) < 0;

This states the trigger clearly in terms of the value 0. It is also much
less likely to go off by accident, because the time trend is used (stage
going below 1 is not significant if it is dropping very slowly and not
likely to make it to 0). In addition to LINEAR extrapolation quadratic
predictions are available using QUAD as the second argument to PREDICT.
Over time periods of less than an hour (and not right next to a gate or
reservoir), quadratic interpolation is markedly more accurate than
linear.

## RAMP (transition)

For actions, there is also a way to smooth time. The keyword RAMP after
an action (together with a number of minutes) will transition in the
action gradually, if such a transition makes physical sense.

For instance, a ramping version of middle_vamp_ebb might use the
definition for ebb:

    SET gate_op( gate=middle_r_barrier, device=radial) TO ts(new_time_series) RAMP 60min

## Complementary Triggers and IFELSE

Often, an operating rule is paired with a complimentary rule that will
reverse its action. For instance, to complement the above rule for ebb
flow the following operating rule for flood flow might be added:

### Name

middle_vamp_flood

### Expressions

    flood := chan_flow(channel=132, dist=1000) < -0.01
    vamp := (month == Apr) or (month == May)

### Trigger

    vamp and flood

### Action

    SET gate_op( gate=middle_r_barrier, device=barrier,direction=to_node) TO old_time_series

This rule effectively undoes the ebb action. The example underscores a
necessary but somewhat unintuitive point about triggers: they are
one-time and unidirectional. A rule whose trigger is vamp and ebb will
activate when this expression changes from false to true but will not do
anything or even notice if vamp and ebb subsequently becomes false
again. If the complementary behavior is desired, this intent must be
specified in a second rule. Often the complementary rule is subtly
different from the exact negation of the original; for instance, the
trigger vamp and flood is not the same as not(vamp and ebb). In the case
of the Montezuma Salinity Control Structure, the flood and ebb triggers
are not even based on the same variable (the gate is opened based on a
head difference, closed based on velocity).

The middle_vamp_ebb example combines vamp, which is the seasonal
applicability of the rule with ebb, which is a tidal phenomenon. There
are also meaningful operating rules that do not need a trigger at all.
For instance, the user might want to operate SWP and CVP pumping based
on a time series but bound it by some fraction of Sacramento inflow. The
trigger in this case is “TRUE” and it will go off once at startup. This
is the default in the GUI if you leave the trigger blank.

If what you really want is a trigger that continuously monitors a
true-false condition and applies a value accordingly, you may want to
consider using the IFELSE function and no trigger. For instance:

    SET ext_flow(node=17) TO IFELSE( vamp, ts1, ts2)

will set the boundary flow at node 17 (San Joaquin River) to time series
ts1 whenever vamp is true and to ts2 when vamp is not true.

## Misfires and Redundant Triggering

Extra triggering and rule activation may seem harmless when you consider
one rule in isolation. Rerunning an action hurts performance, but the
action is redundant rather than harmful. The real problem with rules
that misfire is that they are active too often and tend to interfere
with (“lock out” or “bump”) other rules that are trying to manipulate
the same model variable.

Here is an example of misfiring trigger based on an expression using
date terms:

    (YEAR >= 1990 AND MONTH>=APR AND DAY>=14)

(note: a much better way to write this expression using the DATE keyword
is given in the reference section)

Because of the ANDs, this expression requires three conditions to be
true at once in order to evaluate to TRUE. It goes off as intended on or
about 14APR1990. But what happens on 01MAY1990? On 14MAY1990? This
trigger is going to evaluate to FALSE and then back to TRUE. When it
makes the FALSE-TRUE transition it will cause the trigger to go off,
which is probably not what was intended.

There is a fix for the above expression (not the recommended on) that
illustrates that the only thing that matter are FALSE-TO-TRUE
transitions. There is one more curious point about this example is that
the correct behavior is obtained using:

    (YEAR == 1990 AND MONTH == APR AND DAY >= 14)

Why? The rule will evaluate FALSE on or about 01MAY1990, but it will
stay false!

These date examples are so common that there is a special way of dealing
with them. See the function reference for DATE and SEASON.

## Default (TRUE) Trigger

If you leave the trigger definition blank in the GUI the trigger
expression will be set to WHEN TRUE.

The TRUE trigger is roughly equivalent to "at startup" and you should be
sure not to confuse it with "always true". Recall it is transitions that
are important, and this trigger makes its only nominal FALSE-TO-TRUE
transition once at the very beginning of the run. Once displaced by an
overlapping action, the rule will never activate again

A rule that evaluates to a trivial FALSE will never do anything.

As an example of a situation where these concepts matter, consider a
rule that toggles use of a gate for the entire simulation. By default, a
gate in the model is installed. Assume we have set up an expression
named use_barriers or remove_barriers indicating whether we want to use
gates. Three possibilities for writing the rule are:

       TRIGGER                ACTION 
    1. TRUE                SET gate_install(gate=...) TO use_gate
    2. use_gate            SET gate_install(gate...) TO INSTALL
    3. remove_gate         SET gate_install(gate=...) TO REMOVE

Option 1 uses the default trigger. It will be activated at startup and
the gate installation will be set to the expression variable use_gate.
Option 2 is interesting because it will never do anything useful. It
will be evaluated once at the start of the run, but it will never
trigger if use_gate is FALSE. It will trigger if use_gate is TRUE, but
this merely carries out the default. Option 3 remedies this by using
remove_gate -- the non-default -- as the trigger. Different users seem
to regard different options (1) and (3) more intuitive.

## Conflicts

When a rule is triggered, it will be activated unless it conflicts with
another, active rule. Rules conflict when they operate on the same model
variable. For instance, two rules that act to change a weir coefficient
in the same gate/weir conflict.

Two specifications govern conflicts:

1\. When a rule conflicts with an active rule it is deferred. Deferred
rules are not activated, but they are tricked into thinking they
evaluated FALSE so that the can possibly make a FALSE-TRUE transition
again the next time step.

2\. When a rule conflicts with another potentially activating rule, the
results are “undefined”. We are unaware of any universal solution in
this situation. The best solution is to write rules that don’t do this –
we are currently working on a better warning system to detect when this
happens.

## DSM2 Variable and Function Reference:

### Variables

The variables from DSM2 that can be used in operating rules include
boundary and grid variables that can be changed and those that are
merely observable (read-only). The observable variables are divided
between variables that can be set to time series (Dynamic Variables)
that will apply ever-after and variables that can only be set to new
static values (Static Variables)

### Dynamic Control Variables

These variables are dynamically controllable and can be set to a time
series. Once the new time series is set, the boundary or structure being
controlled will have no memory of its old controlling time series. Most
dynamic variables are gate and boundary data.

    gate_op(gate=textname,device=textname, direction=[to_node|from_node|to_from_node])

Device operating coefficients (0..1) in corresponding direction. Use
keywords CLOSE (=0) and OPEN (=1) to make rules more readable. The
option "to_from_node" is write-only -- a convenience feature that writes
to two otherwise separate variables.

    gate_position(gate=textname,device=textname)

Physical operation of control structure such as radial gate height
(physical units). The interpretation of "position" is dependent on the
"control_type" of the gate. If it is gated from the bottom, position
indicates elevation and is the same as elev. If the control type is
gated from the top, as in a radial gate, the position is the height.
This variable is deprecated now, in favor of directly using "elev" or
"height".

    gate_height(gate=textname,device=textname)

Height of gate device.

    gate_elev(gate=textname,device=textname)

Crest elevation or invert elevation of gate device.

    gate_width(gate=textname,device=textname)

Width or radius of gate device.

    ext_flow(name=textname)

External flow (boundary flows, source/sink)

    transfer_flow(transfer=textname)

Flows in object-to-object transfers

## Static Control Variables

These are variables that are normally static. You can set them to a
constant. If you set them to a time series, the model will not complain,
but the result may not be what you expect. The model variable will only
be set to the current value of the series at the time the rule was
activated. The variable won't keep changing with the time series.

    gate_install(gate=textname)

Determines or inquires whether the given gate is installed.

    SET gate_install(...) TO [REMOVE|FALSE]

completely removes the gate and restores an equal-stage compatibility
condition to the channel junction.

    SET gate_install(...) TO [INSTALL|TRUE]

installs the gate.

    gate_coef(gate=textname,device=textname,direction=[to_node|from_node])

Gate coefficient of the device in the given direction. This is a
physical quantitity of the structure, representing the roughness or
efficiency of flow. It should not be used for operating controls such as
flap gates. The coefficients will change only rarely when the actual
site is altered and should never leave the range (0,1).

    gate_nduplicate(gate=textname,device=textname)

Number of duplicate devices.

## Observable Variables

These are read-only model variables that cannot be manipulated directly,
but can be observed and used in expressions for triggers and actions.

    chan_flow(channel=number,dist=[number|length])

Flow in channel.dist=length indicates the end of the channel.

    chan_vel(channel=number, dist=[number|length])

Velocity at given channel and distance.

    chan_stage(channel=number,dist=[number|length])

Water Surface at given channel and distance.

    chan_surf

Same as stage (water surface) in channel

    res_stage(res=textname)

Water surface in reservoir

    res_flow(res=textname, node=number)

Flow from reservoir to node

    ts(name=textname)

Any time series named in the Operating Rule View of the GUI may be used
by referencing the name. Time series evaluate to their value at the
current time step.

## Model Time Queries

The following commands retrieve model date or seasonal information:

    YEAR, MONTH, DAY

Retrieves the year, month and day associated with the current model time
step. These are returned as numbers. When testing them, you can (for
clarity) use 3-letter abbreviations for the months. Examples:

    YEAR >= 1991

    MONTH + 1 < MAY

    HOUR, MIN

Retrieve the (24 hour) hour and minute associated with the current model
time step.

    DATE

Returns a time stamp corresponding to the beginning of the day on the
current model date. Example:

    DATE >= 11OCT1992 (not time part)

    DT

Represents the model time step in seconds. This is often useful for use
with ACCUMULATE

    DATETIME

Returns a time stamp corresponding to the current model date and time.
Example:

    DATETIME > 04FEB1990 00:00 (date plus time)

    SEASON

Returns a time stamp relative to the beginning of the year corresponding
to the beginning of the day on the current model date and time.
Comparisons such as SEASON \> 15APR AND SEASON \<01MAY avoid common
logical mistakes from building this from scratch.  
There is one other gotcha with SEASON that comes up at the end of time
periods because the timestamp is always at 00:00. Compare SEASON \>
15APR AND SEASON \<01MAY SEASON \> 15APR AND SEASON ≤30APR and notice
that the latter does not include the entire day 30APR.

Note SEASON and DATE/DATETIME to combined expressions built from atomic
expressions like day and month. They are clearer and avoid some curious
gotchas. For instance DATE \>= 14APR1990 will evaluate true only once
per year, whereas (YEAR \>= 1990 AND MONTH\>=APR AND DAY\>=14) will
evaluate true on Apr 14, false on May 1 and true again on May 14. You
could get the intended behavior with (YEAR == 1990 AND MONTH == APR AND
DAY\>=14), which will go from false to true only once, but the fix
hardly seems worth the trouble.

## Numerical Operations

The following operators and functions are available

    +, -, *, /

Arithmetic operators with standard precedence of operations. You can use
parenthesis to change the evaluation order.

    x^3, x^y

Power of x and x to the power of y

    MIN2(x,y)

Minimum of two arguments.

    MAX2(x,y)

Maximum of two arguments.

    MIN3(x,y,z)

Minimum of three arguments.

    MAX3(x,y,z)

Maximum of three arguments.

    SQRT(x)

Square root of x

    EXP(x)

Exponent function (e to the power of x)

    LN(x)

Natural log of x

    LOG(x)

Base 10 log of x

## Logical Operations

    x==y

Tests equality.

    x<>y

Tests inequality.

    x<y,x>y, x<=y, x>=y

Comparisons.

    TRUE

The value TRUE

    FALSE

The value FALSE

    NOT expression

Negation of expression, as in NOT(x \< y)

    expr1 AND expr2

Logical ‘and’, which evaluates to TRUE only if both the expressions it
joins are true. Expression (expr2) will not be evaluated if expr1
evaluates to FALSE.

    expr1 OR expr2

Logical ‘or’

## Special Functions

    ACCUMULATE(expression, initval [,resetcond])

Cumulative value function. Accumulates additively the value of
expression using initval (another numerical expression) as the initial
condition and resetting the total anytime the resetcond evaluates to
true. If you want to integrate you should multiply the expression by DT
or else your rule won't be robust if someone changes the time step.

    IFELSE(boolexpr, valexp1,valexpr2)

The ternary operator. If boolexpr returns true, returns the value given
by valexpr1. If boolexpr returns false, returns the value given by
valexpr2.

    LOOKUP(expression, lookup_array,value_array)

Lookup values from a small user supplied table. The lookup array is
provided using a bracketed, comma-separated list of values such as
\[1000.,2000.,3000.\]. The value_array return values are similar but
must have a length one smaller than the number of lookup values. The
array values must be hard-wired numbers at the present time --
expressions are not allowed. The LOOKUP compares expression to elements
of lookup_array. The highest element of the lookup table is currently a
limit, not an actual lookup slot. The function returns the component of
value_array corresponding to the highest index in lookup array that is
\<= expression, e.g.:

    LOOKUP(1000.,[1000.,2000.,3000.], [1.,2.]) returns 1.

    LOOKUP(2000.,[1000.,2000.,3000.], [1.,2.]) returns 2.

    LOOKUP(3000.,[1000.,2000.,3000.], [1.,2.]) is an error.

    PID( PID(expression,target,low,high,K, Ti,Td,Tt,b)

Use PID (Proportional, Integral, Derivative) control to try to guide
expression towards target. The parameters are as follows  
low: lower bound on control representing the minimum value the control
value can take (e.g. for gate height this might be zero).

high: upper bound on control.

K: The constant representing the Proportion component of the control.
The constant multiplies (expression-target) to change a control value,
so choose a factor that is reasonable that takes the scaling of the
expression to the scaling of the control.

Ti: Integral time constant of control

Td: Derivative time constant of control.

Tt: Time basis of "anti-windup"

b: Set-point weighting (use 1.0 if you are new to PID).
