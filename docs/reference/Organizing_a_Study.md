# Organizing a Study

## Overview

The DSM2 installation directory is as follows:

    /dsm2
      /bin            # where the executable files are
      /common_input   # a repository of common input files
      /studies
         [empty]      # DO YOUR WORK HERE (if inside the distribution)
      /study_templates    # ...and NOT here
         /historical
         /ocap_sdip
            config_ocap_sdip.inp
            hydro.inp
            qual_ec.inp
            /timeseries
      /timeseries     # Archivable time series (historical
                      # and series used as basis for preprocessing)
      /tutorials

### Explaining the directory structure

### bin

"Bin" stands for "binary" and refers to the executables. When you
installed dsm2, your path variable got set to the new distribution.

### common_input

The common input directory is a repository of files that are used in the
standard templates. These files start with the name of the (parent)
object, then the "layer" name then the version date. The templates refer
to this directory often. You do not have to maintain these links, but
please do not edit the files... the Layering system should help with
this.

### study_templates

This directory houses samples for historical and planning runs. They
represent our latest setup as of the distribution. As the templates are
updated they may point to newer files in *common_input*.

### studies

A study is an associated group of simulations, which might involve any
combination of DSM2 modules. Often the study compares several different
alternatives. Many DSM2 modelers prefer to house different alternatives
in different folders, but there are good reasons to house them in one
study folder and just use different configuration files. To get started
you will typically copy one of the study_template sub-directories to the
/study folder. Don't change the ones in study_templates!

### timeseries

The *timeseries* directory contains the timeseries you will need in the
regular course of working with DSM2. Since the data that are most
reusable are historical and DICU, that is most of what you will find
here. We don't recommend putting study-specific files (e.g. CALSIM
output) in this directory.

### tutorials

The *tutorials* directory is a workspace for using the tutorials. It is
a lot like /studies in the sense that you will copy templates here.
