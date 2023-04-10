# Credit Risk Case Study

## Abstract
The German credit dataset, donated by Professor Hans Hofmann via the European Statlog project and available on the [UCI Machine
Learning Repository](https://archive.ics.uci.edu/ml/datasets/South+German+Credit+%28UPDATE%29), has been widely used in machine learning research. However, upon closer inspection, the dataset was found to
have many inconsistencies, rendering it unsuitable for interpretable machine learning experiments. However, upon closer inspection, the dataset was found to
have many inconsistencies, rendering it unsuitable for interpretable machine learning experiments. Nowadays we meet
lots of problems like this, so we try to create and develop new models to test the good and bad level of the credit.

## Objective
Build a model to predict the status of credit (good or bad)

## Content
* Import Module and Data<br>
* Data Analysis<br>
* Data Classification<br>
* Data Visualization<br>
* Data Preprocessing<br>
* Data Partition<br>
* Building Models
  * Logit Model
  * Probit Model
  * Random forest Model
 
## [Credit Risk Predictor](https://929txs-ken.shinyapps.io/Rshiny-CreditRisk/) - Rshiny
The R Shiny app for credit risk is designed to predict whether a client's credit is good or bad
using various input variables. The app utilizes the random forest algorithm, a
machine-learning technique that builds multiple decision trees and combines their predictions
to generate a final outcome.<br>
In the app, users can input various client data points such as income, debt-to-income ratio,
credit score, and other relevant information. The app then uses this data to predict the client's
creditworthiness. The output is displayed to the user, indicating whether the client's credit is
classified as good or bad.<br>
Overall, the R Shiny app provides a convenient and user-friendly way to predict credit risk
and can be a valuable tool for financial institutions, credit unions, and other organizations that
need to make decisions about lending and credit.
  
  ## Files
  File Creditrisk_final.rmd contains data visalisation, preprocessing steps and everything needed to be done in order to find the best model.<br>
  File app.r is the R shiny app for credit risk predictor.
  

