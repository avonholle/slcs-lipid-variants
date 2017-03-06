library(PredictABEL)

# specify dataset with outcome and predictor variables
data(ExampleData) 
class(ExampleData)
head(ExampleData)

# specify column numbers of genetic predictors
cGenPred <- c(11:16)

# fit a logistic regression model
# all steps needed to construct a logistic regression model are written in a function
# called 'ExampleModels', which is described on page 4-5
riskmodel <- ExampleModels()$riskModel2

# compute unweighted risk scores 
riskScore <- riskScore(weights=riskmodel, data=ExampleData, 
                       cGenPreds=cGenPred, Type="unweighted")