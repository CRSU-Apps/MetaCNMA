Data in long format, should be in arm based format, with one arm per row and contain the following columns (case and order insensitive).

|                 Study                  |                      Components                       |       Mean       |               SD               |                Total                |
|:--------------------------------------:|:-----------------------------------------------------:|:----------------:|:------------------------------:|:-----------------------------------:|
| Study name and year e.g., `Smith 2020` | Components of the arm, separated by a + e.g., `A + B` | Mean for the arm | Standard Deviation for the arm | Total number of patients in the arm |

**Notes:** Study names should be unique per study

Components should be separated by a `+` if and arm contains more than one component.
