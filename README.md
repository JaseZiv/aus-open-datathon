# Aus Open Datathon
This repo is for the 2019 Australian Open Datathon hosted by Betfair.

The official Betfair repo can be found here: https://github.com/betfair-datascientists/aus-open-datathon


# My Submission
I created an ensemble model by using the h2o (provided by Betfair) and xgboost machine learning algorithms. The weights given were as follows:

* 70% h2o
* 30% xgboost

# The Process
Betfair provided starter code for feature engineering and model building. Betfair's offered model only used the differences between player 1 and player 2 as features (ie first serve won difference, rank difference, etc). I used both these differences and the actual metrics for each of the players.

## Running the code
To build the ensemble model, first build the h20 model using the file *h2oModel.R*. Once this has run, it will save this model in the *submission* sub-directory. Then clear the workspace and run the *ensemble_h2o_xgboost.R*. This will build an xgboost model and perform the ensembling.

# Notes on the model
There appeared to be an issue with the data provided in that *Steve Darcis* in the men's draw and *Clara Burel* in the women's weren't appearing with training/feature data to their names. To handle this in the short amount of time I worked on it, I simply made the predictions for their games to be 95% chance in their opponents' favour - risky I know!