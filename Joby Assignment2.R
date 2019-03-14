

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


data <- read.csv('classification-output-data.csv')
cmatrix <-  table(data$class, data$scored.class) 

