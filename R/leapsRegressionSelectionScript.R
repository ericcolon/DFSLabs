# All Subsets Regression
library(leaps)

leaps<-regsubsets(nbaMidDecY~.,data=nbaMidDecX,nbest=3)
# view results
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps,scale="r2")
# plot statistic by subset size
library(car)
subsets(leaps, statistic="rsq")
