#rm(list = ls())
#.rs.restartR()
#setwd("c:/Users/hp/R/OwnProject")
#getwd()
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(gtools)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lattice)) install.packages("lattice", repos = "http://cran.us.r-project.org")
if(!require(Rcpp)) install.packages("Rcpp", repos = "http://cran.us.r-project.org")
if(!require(gdata)) install.packages("gdata", repos = "http://cran.us.r-project.org")
if(!require(rattle)) install.packages("rattle", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(bitops)) install.packages("bitops", repos = "http://cran.us.r-project.org")
library(lattice)
library(ggplot2)
library(caret)
library(tidyverse)
library("readxl")
library(gdata)
library(readxl)
#import of the data
chord_fingers<-read.csv("https://raw.githubusercontent.com/NicHa1986/CHORDS/main/chord-fingers.csv")



# 1 data wrangling and descriptive analysis



# let`s first split the finger position variable in its components 
chord_fingers_n<-str_split_fixed(chord_fingers$FINGER_POSITIONS,",",n=6)
# Now try with apply function to change the "string" value and give it an order
# 0 - open string, 1 - mutated string, 2 - index finger, 3 -middle finger, 4 - ring finger, 5 - pinky
# afterwards convert in it numerical values
m1<-chord_fingers_n[,1:6]
c<-function(n){
  n<-str_replace_all(n,"4","5")
  n<-str_replace_all(n,"3","4")
  n<-str_replace_all(n,"2","3")
  n<-str_replace_all(n,"1","2")
  n<-str_replace_all(n,"x","1")
  n<-as.numeric(n)
}
chord_fingers_n<-as.data.frame(apply(m1,2,c))

# I also create a cross sum or row sum variable that helps to see the level of all strings together
chord_fingers_n<-chord_fingers_n %>% mutate(crosssum=rowSums(chord_fingers_n)) 

# now use a new data set with the variables just created
chord_fingers_new <- chord_fingers_n %>% mutate(string_1=chord_fingers_n$V1,string_2=chord_fingers_n$V2,string_3=chord_fingers_n$V3,string_4=chord_fingers_n$V4,string_5=chord_fingers_n$V5,string_6=chord_fingers_n$V6,cross=chord_fingers_n$crosssum,CHORD_ROOT=chord_fingers$CHORD_ROOT,CHORD_TYPE=chord_fingers$CHORD_TYPE)
# Let`s take a first look in terms of a scatter plot with three dimensions - finger position (ordered),type and root`
chord_fingers_new %>% ggplot(aes(cross, CHORD_TYPE,col=CHORD_ROOT)) + 
  geom_point(size=3, position=position_jitter(h=0.1,w=0.2)) +
  xlab("cross sum of all strings") + ylab("chord type") + scale_color_discrete(name = "chord root") + theme_bw() + ggtitle("Unstructured Data - Chord Root,Type and Difficulty Level")
# weird structure, it seems that almost all roots and types appear over the whole range of the cross sum
# Therefore, reduce the class of the root
# summarize the root in 7 levels: A=1,B=2,C=3,D=4,E=5,F=6,G=7 and combine also type of the chord in five groups: dur,moll,sus,dim,aug
chord_fingers_new<-chord_fingers_new %>% mutate(root=ifelse(chord_fingers_new$CHORD_ROOT=="A" | chord_fingers_new$CHORD_ROOT=="A#" |chord_fingers_new$CHORD_ROOT=="Ab",1,ifelse(chord_fingers_new$CHORD_ROOT=="B" |chord_fingers_new$CHORD_ROOT=="B#" |chord_fingers_new$CHORD_ROOT=="Bb",2,ifelse(chord_fingers_new$CHORD_ROOT=="C" |chord_fingers_new$CHORD_ROOT=="C#" |chord_fingers_new$CHORD_ROOT=="Cb",3,ifelse(chord_fingers_new$CHORD_ROOT=="D" |chord_fingers_new$CHORD_ROOT=="D#" |chord_fingers_new$CHORD_ROOT=="Db",4,ifelse(chord_fingers_new$CHORD_ROOT=="E#" |chord_fingers_new$CHORD_ROOT=="Eb" | chord_fingers_new$CHORD_ROOT=="E",5,ifelse(chord_fingers_new$CHORD_ROOT=="F#" |chord_fingers_new$CHORD_ROOT=="Fb" | chord_fingers_new$CHORD_ROOT=="F",6,7)))))))
chord_fingers_new<-chord_fingers_new %>% mutate(type=ifelse(CHORD_TYPE=="sus4" | CHORD_TYPE=="sus2" | CHORD_TYPE=="7sus4","sus",ifelse(CHORD_TYPE=="maj9" | CHORD_TYPE=="maj7" | CHORD_TYPE=="maj13" | CHORD_TYPE=="maj" | CHORD_TYPE=="add9" | CHORD_TYPE=="9b5" | CHORD_TYPE=="9(#5)" | CHORD_TYPE=="9(#11)" | CHORD_TYPE=="9" | CHORD_TYPE=="7b5" | CHORD_TYPE=="7(b9)" | CHORD_TYPE=="7(b13)" | CHORD_TYPE=="7(#9)" | CHORD_TYPE=="7(#5)" | CHORD_TYPE=="7(#11)" | CHORD_TYPE=="7" | CHORD_TYPE=="6(#11)" | CHORD_TYPE=="6" | CHORD_TYPE=="5" | CHORD_TYPE=="44080" | CHORD_TYPE=="13(b9)" | CHORD_TYPE=="13(#9)" | CHORD_TYPE=="13(#11)" | CHORD_TYPE=="13" | CHORD_TYPE=="11" | CHORD_TYPE=="+(#11)","dur",ifelse(CHORD_TYPE=="m7b5" | CHORD_TYPE=="m9" | CHORD_TYPE=="m7" | CHORD_TYPE=="m6/9" | CHORD_TYPE=="m6" | CHORD_TYPE=="m13" | CHORD_TYPE=="m11" | CHORD_TYPE=="m(maj9)" | CHORD_TYPE=="m(maj7)" | CHORD_TYPE=="m","moll",ifelse(CHORD_TYPE=="aug","aug",ifelse(CHORD_TYPE=="dim7" | CHORD_TYPE=="dim","dim",0))))))
chord_fingers_new$type<-as.factor(chord_fingers_new$type)
chord_fingers_new$root<-as.factor(chord_fingers_new$root)
# take a first look in terms of a scatter plot 
chord_fingers_new %>% ggplot(aes(cross, type,col=root)) + 
  geom_point(size=3, position=position_jitter(h=0.1,w=0.2)) +
  xlab("cross sum of all strings") + ylab("chord type") + scale_color_discrete(name = "chord root") + theme_bw() + ggtitle("Raw data of three dimensions - Chord root,type and difficulty level")
# It seems that we have a bunch of points in the rightmost part of the graph for every type and root.
# Because we are interest in starter/beginner level, we should later remove high levels of the finger positions/cross sum of them
# But first let`s once again split the root in two groups: 1 - A,D,E and 0 - B,C,F,G
# The split is reasonable because A,D,E is a simple cadence (Calling to musicians) so that also the chords should be easier
chord_fingers_new<-chord_fingers_new%>%mutate(easyroot=ifelse(root==1 | root==4 | root==5,1,0))
chord_fingers_new$easyroot<-as.factor(chord_fingers_new$easyroot)
chord_fingers_new<-chord_fingers_new %>% select(-CHORD_ROOT,-CHORD_TYPE)
# Now continue only with cross<11 observations (most easy finger combinations)
chord_fingers_new %>% filter(cross<11) %>% ggplot(aes(cross, type,col=easyroot)) + 
  geom_point(size=3, position=position_jitter(h=0.1,w=0.2)) +
  xlab("cross sum of all strings") + ylab("chord type") + scale_color_discrete(name = "chord root") + theme_bw() + ggtitle("Only two classes - Chord root,type and difficulty level")
# summary statistics for especially the first two strings
chord_fingers_new %>% filter(cross<11) %>% group_by(easyroot,string_1) %>% summarize(n=n()) %>% as.data.frame()
chord_fingers_new %>% filter(cross<11) %>% group_by(easyroot,string_2) %>% summarize(n=n()) %>% as.data.frame()
# now use the split for predictions
chord_fingers_new<-chord_fingers_new %>% filter(cross<11)



# 2  Predictions



# split the data in 80 % training and 20 % test set
library(caret)
library(rpart)
chord_fingers_new$string_1<-as.numeric(chord_fingers_new$string_1)
chord_fingers_new$string_2<-as.numeric(chord_fingers_new$string_2)
chord_fingers_new$easyroot<-as.factor(chord_fingers_new$easyroot)
class(chord_fingers_new$cross)
class(chord_fingers_new$string_1)
set.seed(2, sample.kind = "Rounding")
test_index <- createDataPartition(chord_fingers_new$easyroot, times = 1, p = 0.8, list = FALSE)
test_index
test_set <- chord_fingers_new %>% slice(-test_index)
train_set <- chord_fingers_new %>% slice(test_index)
train_set

# 2A - logistic regression with cut - off 0.5

# as a starting point use logistic regression with two predictors (string_1 and string_2)
# first, train the model
glm_fit <- train_set %>% 
  mutate(y = as.numeric(easyroot==1)) %>%
  glm(y ~ string_1+string_2, data=., family = "binomial")
# now, use the model to make predictions on the test set
p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")
p_hat_logit
# set the cut-off to 0.5
y_hat_logit <- ifelse(p_hat_logit > 0.5, 1, 0) %>% factor
y_hat_logit
# look to the confusionmatrix to determine accuracy
confusionMatrix(y_hat_logit, as.factor(test_set$easyroot))$overall[["Accuracy"]]

# 2B - K nearest neighbor with cross validation (best tune for k=1)

# now use k nearest neighbors - use cross validation to choose k
set.seed(2, sample.kind = "Rounding")
control <- trainControl(method = "cv", number = 10, p = .9)
fit_cross <- train(easyroot~string_1+string_2, method = "knn", data=train_set,tuneGrid = data.frame(k = seq(1, 9, 2)),trControl=control)
y_hat_knn <- predict(fit_cross, test_set,type="raw")
fit_cross$bestTune
fit_cross$results
confusionMatrix(y_hat_knn, as.factor(test_set$easyroot))$overall["Accuracy"]
confusionMatrix(y_hat_knn, as.factor(test_set$easyroot))
# so best k=1, but why do we then have a different result to logistic regression?

# 2C - logistic regression with cut off 0.56

# let`s look what happens when we change the cut-off in logistic regression`
glm_fit <- train_set %>% 
  mutate(y = as.numeric(easyroot==1)) %>%
  glm(y ~ string_1+string_2, data=., family = "binomial")
glm_fit
p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")
p_hat_logit
y_hat_logit <- ifelse(p_hat_logit > 0.56, 1, 0) %>% factor
y_hat_logit
test_set
confusionMatrix(y_hat_logit, as.factor(test_set$easyroot))$overall[["Accuracy"]]
# in using a cut off of 0.56 in logistic regression leads to the same result as k nearest neighbors with k=1
# make also a graph
rm("c")
test_set %>%
  mutate(p_hat = p_hat_logit) %>%
  ggplot() +
  stat_contour(aes(string_1, string_2, z=p_hat), breaks=c(0.56), color="black") +
  geom_point(mapping = aes(string_1, string_2, color=easyroot), data = test_set)

# 2D - K nearest neighbor with more predictors (k=5)

# check if we can not increase accuracy by incorporating a all strings or only the third one (k=5)
fit <- knn3(easyroot ~ string_1+string_2+string_3,data = train_set)
y_hat_knn <- predict(fit, test_set,type="class")
test_set
y_hat_knn
confusionMatrix(y_hat_knn, as.factor(test_set$easyroot))$overall["Accuracy"]
confusionMatrix(y_hat_knn, as.factor(test_set$easyroot))
# fit <- knn3(easyroot ~ string_1+string_2,data = train_set)
fit <- knn3(easyroot ~ string_1+string_2+string_3+string_4+string_5+string_6,data = train_set)
y_hat_knn <- predict(fit, test_set,type="class")
test_set
y_hat_knn
confusionMatrix(y_hat_knn, as.factor(test_set$easyroot))$overall["Accuracy"]
confusionMatrix(y_hat_knn, as.factor(test_set$easyroot))

# 2E - Decision tree

# Not bad but can we increase accuracy? Try also a decision tree
fit <- rpart(easyroot ~ string_1+string_2,data = train_set,method="class")
#load
library(bitops)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
#plot
fancyRpartPlot(fit,main="Classification Tree for root of chords")
plot(fit, margin = 0.1,main="Classification Tree for root of chords")
text(fit, cex = 0.75)
y_hat <- predict(fit, test_set,type="class") %>% factor()
y_hat
test_set
confusionMatrix(y_hat, as.factor(test_set$easyroot))$overall["Accuracy"]


