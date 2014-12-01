### http://ww2.coastal.edu/kingw/statistics/R-tutorials/logistic.html
##Logistic Regression: One Numerical Predictor

##In the "MASS" library there is a data set called "menarche" (Milicer, H. and Szczotka, F., 1966, Age at Menarche 
##in Warsaw girls in 1965, Human Biology, 38, 199-203), in which there are three variables: "Age" (average age of 
##age homogeneous groups of girls), "Total" (number of girls in each group), and "Menarche" (number of girls in the 
##group who have reached menarche)..

library("MASS")
data(menarche)
str(menarche)

##
##'data.frame':   25 obs. of  3 variables:
##  $ Age     : num   9.21 10.21 10.58 10.83 11.08 ...
##$ Total   : num  376 200 93 120 90 88 105 111 100 93 ...
##$ Menarche: num  0 0 0 2 2 5 10 17 16 29 ...

summary(menarche)

##
#Age            Total           Menarche      
#Min.   : 9.21   Min.   :  88.0   Min.   :   0.00  
#1st Qu.:11.58   1st Qu.:  98.0   1st Qu.:  10.00  
#Median :13.08   Median : 105.0   Median :  51.00  
#Mean   :13.10   Mean   : 156.7   Mean   :  92.32  
#3rd Qu.:14.58   3rd Qu.: 117.0   3rd Qu.:  92.00  
#Max.   :17.58   Max.   :1049.0   Max.   :1049.00

> plot(Menarche/Total ~ Age, data=menarche)

test_data <- function(conn) {
  taposh_test.hex = h2o.uploadFile(conn, locate("/Users/taposhdr/workspace/decision_science/kaggle/ctr_prediction/predict_1.csv"), key="taposh_test.hex", header=TRUE) 
  print("Summary of  data: ")
  summary(taposh_test.hex)
  head(taposh_test.hex)
  
  
  
  
  taposh_test.hex = h2o.uploadFile("/Users/taposhdr/workspace/decision_science/kaggle/ctr_prediction/predict_1.csv")
}
