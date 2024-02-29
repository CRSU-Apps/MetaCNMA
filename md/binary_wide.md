Data in wide format, should be in arm based format, with one trial per row and contain the following columns (case and order insensitive).

|                Study                 |                         Components.1                         |           Events.1            |                  Total.1                  |                         Components.n                         |             Events.n              |                  Total.n                  |
|:------------------------------------:|:------------------------------------------------------------:|:-----------------------------:|:-----------------------------------------:|:------------------------------------------------------------:|:---------------------------------:|:-----------------------------------------:|
| Study name and year e.g., Smith 2020 | Components for the 1^st^ arm, separated by a + e.g., `A + B` | Number of events in the 1^st^ | Total number of patients in the 1^st^ arm | Components for the n^th^ arm, separated by a + e.g., `A + C` | Number of events in the n^th^ arm | Total number of patients in the n^th^ arm |

Where each arm of a study contains a `Components`, `Events` and `Total` column.

**Notes:** Study names should be unique per study

Components should be separated by a `+` if an arm contains more than one component.
