YAML Specification (yspec) Generation Guide
================

## Overview

This README provides a detailed guide on generating a `yspec`-compatible
YAML specification file from two primary input sources:

- **Code List File**: A comprehensive listing of variables and their
  associated categorical codes and values.
- **Specification (Spec) File**: A dataset-specific definition of the
  final data structure, including variable names, labels, units, and
  data types.

The end result: a `yspec` YAML file suitable for producing FDA define
documents, generating data inventories, and other standardized outputs.

------------------------------------------------------------------------

## Understanding the Inputs

### Code List

- **Purpose**: Provides a reference of variables and their allowed
  categorical codes/values.
- **Format**: Typically an Excel file with rows defining:
  - **column**
  - **decode**
  - **values**

**Example**: For variable `SEX`:

- `SEX`, `0`, `Female`
- `SEX`, `1`, `Male`

This establishes a controlled vocabulary for categorical variables.

------------------------------------------------------------------------

### Specification (Spec) File

- **Purpose**: Describes the dataset variables, including labels, units,
  and data types.
- **Format**: Typically an Excel sheet with rows defining:
  - **column**
  - **short**
  - **type** (e.g., Numeric or Character)
  - **unit** (if applicable, e.g., kg, hr)

**Example**:

- `column = WT`
- `short = "Weight"`
- `type = "Num"`
- `unit = "kg"`

Combining this with the code list produces a fully qualified `yspec`
YAML specification.

------------------------------------------------------------------------

## Process Overview

1.  **Load Code List**: A global “dictionary” of categorical variables.
2.  **Load Spec File**: Defines the final dataset structure.
3.  **Integrate**: Match each variable in the spec with corresponding
    codes/values from the code list.
4.  **Create YAML**: Use the provided R function `create_yaml_spec()` to
    merge these inputs into a `yspec` YAML file.
5.  **Use yspec**: Load the YAML with `yspec::ys_load()` and use `yspec`
    tools to generate define documents, data inventories, and more.

------------------------------------------------------------------------

## Example Workflow

Below is a conceptual workflow using the updated script and files.

### Libraries and Setup

``` r
library(dplyr)
library(readxl)
library(here)
library(yspec)

source(here::here("scripts/functions/create-yaml-spec.R"))
```

------------------------------------------------------------------------

## Read the Code List File

``` r
code_list <- readxl::read_xlsx(
  path = here::here("data/example-specs/code_list.xlsx"),
  sheet = "Sheet1"
) %>%
  select(
    column = `Code Names`,
    decode = Codes,
    values = Values
  )
```

**Key Point**: The code list provides categorical mappings. If `SEX` has
codes `0` and `1`, this file will map them to `Female` and `Male`.

------------------------------------------------------------------------

## Code List Preview

``` r
code_list %>% slice(1:10) %>% knitr::kable()
```

| column | decode | values                |
|:-------|:-------|:----------------------|
| C      | .      | Not a comment         |
| C      | C      | Comment for exclusion |
| SEQ    | 0      | Dose                  |
| SEQ    | 1      | Observation           |
| CMT    | 1      | Dose                  |
| CMT    | 2      | Observation           |
| EVID   | 0      | Observation event     |
| EVID   | 1      | Dosing event          |
| SEX    | 0      | Male                  |
| SEX    | 1      | Female                |

------------------------------------------------------------------------

## Read the Spec File

``` r
spec_data <- 
  readxl::read_xlsx(
    path = here::here("data/example-specs/pk_spec.xlsx"),
    sheet = "Dataset Specifications",
    skip = 1
  ) %>% 
  clean_column_names() %>% 
  select(
    column = Variable_Name,
    short = SAS_Label,
    type = SAS_Type,
    unit = Unit
  )
```

**Key Point**: The spec file defines each variable’s label, type, and
(if applicable) unit.

------------------------------------------------------------------------

## Spec File Preview

``` r
spec_data %>% slice(1:10) %>% knitr::kable()
```

| column | short                 | type | unit  |
|:-------|:----------------------|:-----|:------|
| C      | Commented rows        | Char | NA    |
| NUM    | Row number            | Num  | NA    |
| ID     | NONMEM ID number      | Num  | NA    |
| TIME   | Time after first dose | Num  | hour  |
| SEQ    | Data type             | Num  | NA    |
| CMT    | Compartment           | Num  | NA    |
| EVID   | Event identifier      | Num  | NA    |
| AMT    | Dose amount           | Num  | mg    |
| DV     | Concentration         | Num  | ng/mL |
| AGE    | Age                   | Num  | years |

------------------------------------------------------------------------

## Create the yspec YAML File

Use the custom function `create_yaml_spec()`:

``` r
create_yaml_spec(
  description = "PK Dataset",
  spec_data = spec_data,
  code_list = code_list,
  output_file = here::here("data/derived/pk.yml")
)
```

    ## YAML specification file created at: /data/spec-builder/data/derived/pk.yml

**Result**: A `pk.yml` file that combines variable definitions and
code/value mappings into a single YAML specification.

------------------------------------------------------------------------

### View the Generated `yspec` file

``` yaml
 SETUP__:
  description: PK Dataset
C:
  short: Commented rows
  type: character
  values:
  - '.'
  - C
  decode:
  - Not a comment
  - Comment for exclusion
NUM:
  short: Row number
  type: numeric
ID:
  short: NONMEM ID number
  type: numeric
TIME:
  short: Time after first dose
  type: numeric
  unit: hour
SEQ:
  short: Data type
  type: numeric
  values:
  - 0
  - 1
  decode:
  - Dose
  - Observation
CMT:
  short: Compartment
  type: numeric
  values:
  - 1
  - 2
  decode:
  - Dose
  - Observation
EVID:
  short: Event identifier
  type: numeric
  values:
  - 0
  - 1
  decode:
  - Observation event
  - Dosing event
AMT:
  short: Dose amount
  type: numeric
  unit: mg
DV:
  short: Concentration
  type: numeric
  unit: ng/mL
AGE:
  short: Age
  type: numeric
  unit: years
WT:
  short: Weight
  type: numeric
  unit: kg
HT:
  short: Height
  type: numeric
  unit: cm
EGFR:
  short: Estimated GFR
  type: numeric
  unit: mL/min/1.73m2
ALB:
  short: Albumin
  type: numeric
  unit: g/dL
BMI:
  short: Body mass index
  type: numeric
  unit: kg/m2
SEX:
  short: Sex
  type: numeric
  values:
  - 0
  - 1
  decode:
  - Male
  - Female
AAG:
  short: Alpha-1-acid glycoprotein
  type: numeric
  unit: mg/dL
SCR:
  short: Serum creatinine
  type: numeric
  unit: mg/dL
AST:
  short: Aspartate aminotransferase
  type: numeric
  unit: U/L
ALT:
  short: Alanine aminotransferase
  type: numeric
  unit: U/L
CP:
  short: Child-Pugh score
  type: numeric
  values:
  - 0
  - 1
  - 2
  - 3
  decode:
  - Normal
  - Score=1
  - Score=2
  - Score=3
TAFD:
  short: Time after first dose
  type: numeric
  unit: hour
TAD:
  short: Time since last dose
  type: numeric
  unit: hour
LDOS:
  short: Last dose amount
  type: numeric
  unit: mg
MDV:
  short: Missing dependent variable
  type: numeric
  values:
  - 0
  - 1
  decode:
  - 'No'
  - 'Yes'
BLQ:
  short: Below limit of quantification
  type: numeric
  values:
  - 1
  - 0
  decode:
  - Above QL
  - Below QL
PHASE:
  short: Study phase indicator
  type: numeric
STUDYN:
  short: Study number
  type: numeric
DOSE:
  short: Dose
  type: numeric
  unit: mg
  values:
  - 5
  - 10
  - 25
  - 50
  - 75
  - 100
  - 150
  - 200
  decode:
  - 5 mg
  - 10 mg
  - 25 mg
  - 50 mg
  - 75 mg
  - 100 mg
  - 150 mg
  - 200 mg
SUBJ:
  short: Subject identifier
  type: character
USUBJID:
  short: Unique subject identifier
  type: character
STUDY:
  short: Study name
  type: character
  values:
  - 101-DEMO-001
  - 101-DEMO-002
  - 201-DEMO-003
  - 201-DEMO-004
  decode:
  - 101-DEMO-001
  - 101-DEMO-002
  - 201-DEMO-003
  - 201-DEMO-004
ACTARM:
  short: Treatment arm
  type: character
  values:
  - DEMO 5 mg
  - DEMO 10 mg
  - DEMO 10 mg qd x7
  - DEMO 25 mg
  - DEMO 25 mg qd x7
  - DEMO 50 mg qd x7
  - DEMO 75 mg qd x7
  - DEMO 100 mg
  - DEMO 100 mg qd x7
  - DEMO 150 mg
  - DEMO 200 mg
  decode:
  - DEMO 5 mg
  - DEMO 10 mg
  - DEMO 10 mg qd x7
  - DEMO 25 mg
  - DEMO 25 mg qd x7
  - DEMO 50 mg qd x7
  - DEMO 75 mg qd x7
  - DEMO 100 mg
  - DEMO 100 mg qd x7
  - DEMO 150 mg
  - DEMO 200 mg
RF:
  short: Renal function
  type: character
  values:
  - norm
  - mild
  - mod
  - sev
  decode:
  - Normal
  - Mild
  - Moderate
  - Severe 
```

------------------------------------------------------------------------

## Load the yspec Specification

``` r
pk_spec <- yspec::ys_load(here::here("data/derived/pk.yml"))
```

**Check**: The `pk_spec` object now holds the integrated data
specification. Use `yspec` functions to inspect and leverage this
specification.

------------------------------------------------------------------------

## Validate Against Your Data

Suppose you have a data file `pk.csv`:

``` r
pk <- 
  data.table::fread(
    file = here::here("data/derived/pk.csv"), 
    na.strings = '.'
  )

yspec::ys_check(
  data = pk,
  spec = pk_spec
)
```

    ## The data set passed all checks.

**Purpose**: Ensures that the data conforms to the specification. This
helps identify any discrepancies before regulatory submission or
analysis.

------------------------------------------------------------------------

## Using yspec for Further Outputs

- **Add Factors**: Convert categorical variables into factors with
  labeled levels:

``` r
pk <- pk %>% yspec::ys_add_factors(pk_spec)
```

------------------------------------------------------------------------

## Summary

- **Code List & Spec File**: Two main inputs that define variables,
  their data types, codes, and decodes.
- **Integration & YAML Generation**: The `create_yaml_spec()` function
  merges these definitions into a `yspec` YAML file.
- **Downstream Use**: With `yspec`, you can validate data, generate
  define documents (e.g., via `render_fda_define()`), produce data
  inventories, and maintain traceability and consistency across
  projects.
