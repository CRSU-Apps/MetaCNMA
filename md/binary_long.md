Data in long format, should be in arm based format, with one arm per row and contain the following columns (case and order insensitive).

|                 Study                  |                       Components                       |           Events            |                Total                |
|:----------------:|:----------------:|:----------------:|:----------------:|
| Study name and year e.g., `Smith 2020` | Components for the arm, separated by a + e.g., `A + B` | Number of events in the arm | Total number of patients in the arm |

**Notes:** Study names should be unique per study

Components should be separated by a `+` if an arm contains more than one component.
