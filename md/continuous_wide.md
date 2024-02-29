Data in wide format, should be in arm based format, with one trial per row and contain the following columns (case and order insensitive).

|                Study                 |                        Components.1                        |       Mean.1       |                 SD.1                 |                  Total.1                  |                         Components.n                         |         Mean.n         | SD.n                                 |                  Total.n                  |
|:------------------------------------:|:----------------------------------------------------------:|:------------------:|:------------------------------------:|:-----------------------------------------:|:------------------------------------------------------------:|:----------------------:|--------------------------------------|:-----------------------------------------:|
| Study name and year e.g., Smith 2020 | Components for the 1st arm, separated by a + e.g., `A + B` | Mean for 1^st^ arm | Standard Deviation for the 1^st^ arm | Total number of patients in the 1^st^ arm | Components for the n^th^ arm, separated by a + e.g., `A + C` | Mean for the n^th^ arm | Standard Deviation for the n^th^ arm | Total number of patients in the n^th^ arm |

Where each arm of a study contains a `Components`, `Events` and `Total` column.

**Notes:** Study names should be unique per study

Components should be separated by a `+` if an arm contains more than one component.
