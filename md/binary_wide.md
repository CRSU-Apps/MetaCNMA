Data in wide format, should be in arm based format, with one trial per row and contain the following columns (case and order insensitive).

|                Study                 |                         Components.1                         |           Events.1            |                  Total.1                  |                         Components.n                         |             Events.n              |                  Total.n                  |
|:------------------------------------:|:------------------------------------------------------------:|:-----------------------------:|:-----------------------------------------:|:------------------------------------------------------------:|:---------------------------------:|:-----------------------------------------:|
| Study name and year e.g., Smith 2020 | Components for the 1^st^ arm, separated by a + e.g., `A + B` | Number of events in the 1^st^ | Total number of patients in the 1^st^ arm | Components for the n^th^ arm, separated by a + e.g., `A + C` | Number of events in the n^th^ arm | Total number of patients in the n^th^ arm |

Where each arm of a study contains a `Components`, `Events` and `Total` column.

**Notes:** Study names should be unique per study

Components should be separated by a `+` if an arm contains more than one component.

If the data has more than two arms, and one or more trials are missing arms, these arms should be left blank. e.g.

|  Study  | Components.1 | Mean.1 | SD.1 | Total.1 | Components.2 | Mean.2 | SD.2 | Total.2 | Components.3 | Mean.3 | SD.3 | Total.3 |
|:-------:|:------------:|:------:|:----:|:-------:|:------------:|:------:|:----:|:-------:|:------------:|:------:|:----:|:-------:|
| Study 1 |      UC      |   5.3  |  2.1 |    90   |      A+B     |   5.6  |  2.3 |   100   |       C      |   5.1  |   2  |    95   |
| Study 2 |      UC      |   5.5  |  2.3 |   115   |       C      |   5.8  |  2.1 |   115   |              |        |      |         |
| Study 3 |      UC      |   5.1  |  2.5 |    80   |       A      |   5.7  |  2.0 |    85   |       B      |   5.5  |  2.5 |    90   |