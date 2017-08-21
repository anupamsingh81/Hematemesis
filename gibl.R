getwd()

hematemesis = read.csv(file="hematemesis.csv")
hematemesis=na.omit(hematemesis)


hematemesis$PNED.SCORE = as.numeric(hematemesis$PNED.SCORE)
hematemesis$DEATH = as.factor(hematemesis$DEATH)
summary(hematemesis)
x[is.na(x)] <- 0
hematemesis1$REBLEED[is.na(hematemesis1$REBLEED)] = "n"
summary(hematemesis1)
summary(hematemesis1$REBLEED)

  hematemesis1$REBLEED = replace(hematemesis1$REBLEED,hematemesis1$REBLEED=="nn","n")
  summary(hematemesis1)
  str(hematemesis1$REBLEED)
  hematemesis1$REBLEED = as.character(hematemesis1$REBLEED)
  summary(hematemesis1)
  
  
  hematemesis1$REBLEED = as.factor(hematemesis1$REBLEED)
hematemesis1= read.csv("hematemesis1.csv")
summary(hematemesis1)



library(OptimalCutpoints)
library(dplyr)
library(plyr)
#rename(hematemesis0PNED SCORE(0-24)="PNED.SCORE"

names(hematemesis)[7]<- "PNED.SCORE"
names(hematemesis)[4]= "ROCKALL.SCORE"
names(hematemesis)[5]= "GBS.SCORE"
names(hematemesis)[6]= "AIMS65"

y=c("REBLEED")

ROCKALL <- optimal.cutpoints(X = "ROCKALL.SCORE", status = y, tag.healthy = "n", 
                                             methods = "Youden", data = hematemesis1, pop.prev = NULL, 
                                             control = control.cutpoints(), ci.fit = FALSE, conf.level = 0.95, trace = FALSE)

summary(ROCKALL)

plot(ROCKALL)


GBS <- optimal.cutpoints(X = "GBS.SCORE", status = y, tag.healthy = "n", 
                             methods = "Youden", data = hematemesis1, pop.prev = NULL, 
                             control = control.cutpoints(), ci.fit = FALSE, conf.level = 0.95, trace = FALSE)

summary(GBS)

plot(GBS)



AIMS65 <- optimal.cutpoints(X = "AIMS65", status = y, tag.healthy = "n", 
                         methods = "Youden", data = hematemesis1, pop.prev = NULL, 
                         control = control.cutpoints(), ci.fit = FALSE, conf.level = 0.95, trace = FALSE)

summary(AIMS65)

plot(AIMS65)


PNED <- optimal.cutpoints(X = "PNED.SCORE",  status = y, tag.healthy = "n" ,
                            methods = "Youden", data = hematemesis1, pop.prev = NULL, 
                            control = control.cutpoints(), ci.fit = FALSE, conf.level = 0.95, trace = FALSE)

summary(PNED)

plot(PNED)






