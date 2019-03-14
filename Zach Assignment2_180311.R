





Accuracy <- function(df)
{
  names    = c("class", "scored.class")
  cmatrix  = table(df[, names])
  accuracy = (cmatrix[2,2] + cmatrix[1,1]) / (cmatrix[2,2] + cmatrix[1,2] + cmatrix[1,1] + cmatrix[2,1])
  return(round(accuracy, 2))
}


Classification_error_rate <- function(df)
{
  names    = c("class", "scored.class")
  cmatrix  = table(df[, names])
  classification_error_rate = (cmatrix[1,2] + cmatrix[2,1]) / (cmatrix[2,2] + cmatrix[1,2] + cmatrix[1,1] + cmatrix[2,1])
  return(round(classification_error_rate, 2))
  
}


Precision <- function(df)
{
  names    = c("class", "scored.class")
  cmatrix  = table(df[, names])
  precision = (cmatrix[2,2] / (cmatrix[2,2] + cmatrix[1,2]))
  return(round(precision, 2))
  
}


Sensitivity <- function(df)
{
  names    = c("class", "scored.class")
  cmatrix  = table(df[, names])
  sensitivity = cmatrix[2,2] / (cmatrix[2,2] + cmatrix[2,1])
  return(round(sensitivity, 2))
  
}

Specificity <- function(df)
{
  names    = c("class", "scored.class")
  cmatrix  = table(df[, names])
  specificity =  cmatrix[1,1] / (cmatrix[1,1] + cmatrix[1,2])
  return(round(specificity, 2))
  
}

F1_Score <- function(df)
{
  names    = c("class", "scored.class")
  cmatrix  = table(df[, names])
  precision = Precision(df)
  sensitivity = Sensitivity(df)
  f1_score =  (2 * precision * sensitivity) /(precision + sensitivity)
  
  return(round(f1_score, 2))
}


## Q11 - Using the functions to generate the classification metrics

data <- read.csv('classification-output-data.csv')
##class is the truth or gold standard
##scored.class is the measurement system

Accuracy(data)
Classification_error_rate(data)
Precision(data)
Sensitivity(data)
Specificity(data)
F1_Score(data)


## Q12 - Investigating the caret package

if (!"caret" %in% installed.packages()) install.packages(caret)
require(caret)

ls(pos = "package:caret")

?sensitivity
?confusionMatrix
?precision


## Transposing the table so that the actual referenced value (i.e., truth, "class") is in columns, and the predicted measurement system (i.e. "scored.class")  is in rows

df <- data[c("class","scored.class")]
cmatrix.t <- t(table(df))
cmatrix.t
str(cmatrix.t)

## Comparing the home-made functions and the caret package ones

sens.caret <- round(sensitivity(cmatrix.t, positive = rownames(cmatrix)[2]),2)
sens.caret
identical(Sensitivity(data), sens.caret)

spec.caret <- round(specificity(cmatrix.t, negative = rownames(cmatrix)[1]),2)
spec.caret
identical(Specificity(data), spec.caret)

cMat.caret <- confusionMatrix(cmatrix.t, positive = "1")
cMat.caret 

str(cMat.caret) 

prec.caret <- round(precision(cmatrix.t, relevant = "1"),2)
prec.caret
identical(Precision(data), prec.caret)

acc.caret <- round(cMat.caret$overall[1],2)
acc.caret
identical(Accuracy(data), acc.caret)  ## same value, but fail to match with identical function
