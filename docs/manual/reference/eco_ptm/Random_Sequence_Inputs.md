# Random Sequence Inputs

---

## Overview

This parameter specifies whether to use the default random number seed or generate a new random number seed for each run.

- The default seed is typically used for debugging or tuning purposes.
- Using the default seed ensures that certain aspects of the ECO-PTM use the same sequence of pseudorandom values every time the model is run.
- This makes the model outputs more repeatable and facilitates run-to-run comparisons.

---

## Example

```plaintext
Random_Sequence_Inputs
Use_New_Random_Seed: No
End_Random_Sequence_Inputs
```

## Field Descriptions

Allowable entries: Use_New_Random_Seed: YES or Use_New_Random_Seed: NO

---

---

> ⚠️ **Warning:**
> This block is required
