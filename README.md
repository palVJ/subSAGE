# subSAGE
subSAGE is a Shapley value based framework to infer feature importance in high-dimensional data.
It is based on [SAGE (Shapley Additive Global importancE)](https://github.com/iancovert/sage), but adjusted
for high-dimensional data. We also demonstrate how to perform paired bootstrapping in order to estimate confidence intervals.
We investimate in particular subSAGE applied on tree ensemble models.
We emphasize the importance of computing subSAGE on independent test data not used during training of the model.

## Preprint

Preprint is available [here](https://arxiv.org/pdf/2109.00855.pdf).

## Usage

Given an xgboost-model, test data, and a particular feature,
the subSAGE estimate can be computed, in R, as:

```R
source("~/subSAGE/subSAGE.R")
t = xgb.model.dt.tree(model = model)
trees = as.data.table(xgboost.trees(xgb_model = model, data = data, recalculate = FALSE))
estimate = subSage_cpp(data,trees,feature,loss = "RMSE")

```

In the preprint we show how to do paired bootstrapping for tree ensemble models, and use syntetic data to emphasize that
independent test data must be used to estimate subSAGE values:
