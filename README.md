# Project-Beat-The-Binge

## Run in R:

```R
library("openxlsx")
library("splines")
load("model.Rdata")
data = openxlsx::read.xlsx("train_data.xlsx")
y_hat = predict(model_full,data,allow.new.levels=T,type="response")
```

## Run in Python:

```py
from rpy2 import robjects
from rpy2.robjects.packages import importr

utils = importr('utils')
rep = "https://cloud.r-project.org"

utils.install_packages('openxlsx', repos=rep)
utils.install_packages('splines', repos=rep)

y_hat = robjects.r("""
library("openxlsx")
library("splines")
load("model.Rdata")
data = openxlsx::read.xlsx("train_data.xlsx")
y_hat = predict(model_full,data,allow.new.levels=T,type="response")
y_hat
""")
```
