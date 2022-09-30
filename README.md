# Intellectual-property-reform-in-the-laboratory-data-and-analysis

Data and analysis for the paper "Intellectual property reform in the laboratory" by Benslimane et al.

## Data

In the `Data` folder you can find the basic data from which the results are generated. These are 
*not* the raw data from the experiment. `oTree` generates somewhat messy data that have to be treated to be suitable for analysis using `R`. The datasets created here are hence the result of a first data cleaning, that is not reported on this repository. 

The `Data` folder contains:

- `word_task_data.csv` contains data from the initial word-creation task used to exogenously assess skills
- `summary_dataset.csv` contains one row per subject, telling us the *final* number of points and summarising all indicators at the subject level
- `round_level_data.csv` contains one row per subject per period, and details the actions of each player at each moment in the game
- `payoff_data.csv` contains one observation per subject, indicating the total payoff received
- `final_survey.csv` contains one row per subject, the replies to the demographic final questionnaire
- `choicesets.csv` contains, for each period and each subject, the list of *potential* words a subject *could* have created, and allows us to determine the performance of each subject *relative to the best she could have done*
- `extendibility.csv` contains the *Extendibility Potential Index* for each 3-letter word in the French Scrabble dictionary


## Dependencies

The analysis depends on several `R` libraries:

- `tidyverse` -- the specific R dialect used in this script
- `kableExtra` -- export beautiful tables (for descriptive statistics)
- `huxtable` -- export beautiful tables (for regressions)
- `broom` -- run efficiently statistical tests and export them to data frames
- `effsize` -- computing effect sizes using coehn's d


## Analysis

To run the analysis, first install all the needed dependencies, and then run the file `Analysis.R`. This file calls a series of ancillary scripts located in the `Scripts` directory. The scripts can be run in any order, once the first part of `Analysis.R` (i.e., up until the *data cleaning* part) has been run.

Results are stored in the `Tables` directory, where for each data point, test, and analysis carried out in the paper a `.pdf` file exists. 

Figures are stored in the `Figures` directory.

## To know more

Contact us to get more details, included the oTree code of the experimental software, the python scripts used to generate the choice sets and the Extendibility Potential Index, or just to have a chat about the paper. 