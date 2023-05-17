# ENVVARS Section

## Overview:

*ENVVARs* are values used in text substitution elsewhere in the input.
DSM2 attempts to replace any text that is preceded by a "$" and wrapped
in curly braces: ${EXAMPLE}. By convention, these variables are always
used in upper case. The substitution will be made from either system
environmental variables or pseudo-environmental variables defined in
this section. For instance, the SCALAR input section might indicate that
run_start_time be set to ${START_TIME}. DSM2 will then search the system
environmental variables and user-specified environmental variables for
START_TIME and substitute the value (or print a warning if it finds
nothing).

ENVVARs can be specified in text, or set by manipulating the command
environmental variables. In production runs, many of the ENVVARs are set
in a special file called the "configuration" file. Such a file is often
included in the main input file using the CONFIGURATION include block.

  

## Reference

#### Keyword Descriptions

##### NAME

Name of the envvar. This is the alias that will be used elsewhere in the
input system where the substitution is desired. For instance, if the
NAME is START_TIME, ${START_TIME} would be used elsewhere.

##### VALUE

Value assigned during substitution. For instance, for an ENVVAR with
name START_TIME, a likely value would be "0000".

#### Table Info

##### Identifier:

NAME

##### Include Block:

CONFIGURATION

  

## Examples:

Definition and use: The following example defines an ENVVAR section and
then uses the variables later in a SCALAR section.

    ENVVARS        
    NAME    VALUE   
    START_DATE  01JAN1990   # Runtime using envvars
    END_DATE    01JAN2001   
    START_TIME  0000    
    END_TIME    0000    
    END     


    SCALAR      
    model_name  historical_hydro    
    run_start_date  ${START_DATE}   
    run_end_date    ${END_DATE} 
    run_start_time  ${START_TIME}   
    run_end_time    ${END_TIME} 
    END 

  

Identifier:Table Info

NAME

##### Parent Table:

Table is parent

##### Include Block:

CONFIGURATION

------------------------------------------------------------------------

  

-   ENVVARs can also be used on each other -- in text input that occurs
    after the ENVVAR definition.

    ENVVARS        
    NAME    VALUE   
    DSM2MODIFIER             historical_v81               #Study name used for DSM2 output 
    #Output                  
    OUTPUTFILE               ${DSM2MODIFIER}.dss
    #hydro                   
    HYDROOUTDSSFILE          ${DSM2OUTPUTDIR}/${OUTPUTFILE}
    END 
