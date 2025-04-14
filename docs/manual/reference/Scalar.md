# Scalar

## Overview

*Scalars* are scalar model variables used to specify model-wide numerical properties and echoed output levels. They are the equivalent of the text input SCALAR section. All of the parameters are interpreted as text and can be replaced by ENVVARS.

## Tables

### Example

```text
SCALAR
NAME VALUE
binary_output false
checkdata false
cont_bad false
cont_missing true
END
```

The SCALAR table comprises name-value pairs for scalars. The scalars that are allowed depend on the specific model.

### Field Descriptions

- **NAME**: Name of the parameter. This is the identifier of the parameter.
- **VALUE**: Value assigned to the parameter. These are interpreted by the model first as text (to allow substitution using ENVVARS) and then converted to the correct data type and validated. For boolean (true/false), one letter is sufficient.

### Table Info

- **Identifier**: NAME
- **Parent Table**: Table is parent
- **Include Block**: PARAMETER

> Generally, you will work with the standard parameters distributed with DSM2. You always have to provide `RUN_START_DATE` and `RUN_END_DATE` as the defaults are deliberately designed to halt the model.
