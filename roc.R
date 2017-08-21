library(pROC)

summary(hematemesis)

hematemesis$`DEATH IN 7 DAYS`=as.factor(hematemesis$`DEATH IN 7 DAYS`)
hematemesis$`DEATH IN 7-28 DAYS`=as.factor(hematemesis$`DEATH IN 7-28 DAYS`)

hematemesis$`REBLEED EPISODE`=as.factor(hematemesis$`REBLEED EPISODE`)

summary(hematemesis)

summary(miscell)

miscell$Lactate[is.na(miscell$Lactate)] <- 1.9

summary(miscell)
miscell$I.C.U = as.factor(miscell$I.C.U)
miscell$`BLOOD TRANSFUSION`=as.factor(miscell$`BLOOD TRANSFUSION`)
miscell$`SURGICAL INTERVENTION`=as.factor(miscell$`SURGICAL INTERVENTION`)
summary(miscell)

hematemesis$Lactate = miscell$Lactate
hematemesis$Blood_Transfusion = miscell$`BLOOD TRANSFUSION`
hematemesis$Surgery = miscell$`SURGICAL INTERVENTION`

summary(hematemesis)

t.test(hematemesis$Lactate~hematemesis$DEATH)


X = glm(DEATH~hematemesis$`PNED SCORE(0-24
                                      
                                      )`, family=binomial(link = 'logit'),data=hematemesis)
summary(X)
Y = glm(DEATH~hematemesis$`PNED SCORE(0-24)`+hematemesis$Lactate, family=binomial(link = 'logit'),data=hematemesis)
summary(Y)

z = glm(DEATH~hematemesis$`PNED SCORE(0-24)`+hematemesis$`ROCKALL SCORE(0-11)`, family=binomial(link = 'logit'),data=hematemesis)
summary(z)
z1 = glm(DEATH~hematemesis$`PNED SCORE(0-24)`+hematemesis$`ROCKALL SCORE(0-11)`+hematemesis$Lactate, family=binomial(link = 'logit'),data=hematemesis)
summary(z1)

library(arm)

display(X)
display(Y)
display(z)
display(z1)

anova(X,Y)
library(loo)

summary(miscell)
glm
hematemesis$DEATH = ifelse(hematemesis$`DEATH IN 7 DAYS`=="y" | hematemesis$`DEATH IN 7-28 DAYS`=="y","y","n")
hematemesis$DEATH = as.factor(hematemesis$DEATH)

summary(hematemesis)
a= roc(hematemesis$`DEATH IN 7-28 DAYS`,hematemesis$`ROCKALL SCORE(0-11)`)

b=roc(hematemesis$`DEATH IN 7-28 DAYS`,hematemesis$`GBS SCORE(0-23)`)

c=roc(hematemesis$`DEATH IN 7-28 DAYS`,hematemesis$`PNED SCORE(0-24)`)


d=roc(hematemesis$`DEATH IN 7-28 DAYS`,hematemesis$`AIMS65 (0-5)`)






k = paste("AUC FOR ROCKALL SCORE,GBS SCORE ,PNED SCORE,AIMS65 are",a$auc,b$auc,c$auc,d$auc,"respectively")

k

        a
        b
        c
        d
        
        
        
        
        
        
        
        
        
        
      a=  roc(hematemesis$`DEATH IN 7 DAYS`,hematemesis$`ROCKALL SCORE(0-11)`)
        
       b= roc(hematemesis$`DEATH IN 7 DAYS`,hematemesis$`GBS SCORE(0-23)`)
        
        c=roc(hematemesis$`DEATH IN 7 DAYS`,hematemesis$`PNED SCORE(0-24)`)
        
        
        d= roc(hematemesis$`DEATH IN 7 DAYS`,hematemesis$`AIMS65 (0-5)`)
        
        
        plot(a,col="red")
        plot(b,add=TRUE,col="blue")
        plot(c,add=TRUE,col="green")
        plot(d,add=TRUE,col="orange")
        legend('bottomright', names(hematemesis)[c(4:7)] , 
               lty=1, col=c('red', 'blue', 'green',' orange'),  cex=.75)
        
        
        title(main =" Comparison Of Area under Curves with Death Within 7 Days as Outcome",line = 3.0)
        
        
        dev.off()
        
        
      a=  roc(hematemesis$`REBLEED EPISODE`,hematemesis$`ROCKALL SCORE(0-11)`)
        
      b=  roc(hematemesis$`REBLEED EPISODE`,hematemesis$`GBS SCORE(0-23)`)
        
      c=  roc(hematemesis$`REBLEED EPISODE`,hematemesis$`PNED SCORE(0-24)`)
        
        
       d= roc(hematemesis$`REBLEED EPISODE`,hematemesis$`AIMS65 (0-5)`)
       
       
       
       k = paste("AUC FOR ROCKALL SCORE,GBS SCORE ,PNED SCORE,AIMS65 are",a$auc,b$auc,c$auc,d$auc,"respectively")
       
       k
       
       a
       coords(a,"best")
       b
       coords(b,"best")
       a1=smooth(a)
       coords(a1, x="best", input="sensitivity", best.method="closest.topleft")
       c
       d
        
        
        plot(a,col="red")
        plot(b,add=TRUE,col="blue")
        plot(c,add=TRUE,col="green")
        plot(d,add=TRUE,col="orange")
        legend('bottomright', names(hematemesis)[c(4:7)] , 
               lty=1, col=c('red', 'blue', 'green',' orange'),  cex=.75)
        
        
        title(main =" Comparison Of Area under Curves with Death With REBLEED as Outcome",line = 3.0)
        
        hematemesis$`ROCKALL SCORE(0-11)`
        
        library(OptimalCutpoints)
        a$thresholds
        
        optimal.cutpoints(hematemesis$`ROCKALL SCORE(0-11)`~hematemesis$`REBLEED EPISODE` ,status=hematemesis$`REBLEED EPISODE`,tag.healthy="n",methods = "Youden",data=hematemesis)
      
        optimal.cutpoint.Youden<-optimal.cutpoints(X = hematemesis$`GBS SCORE(0-23)`, status = hematemesis$`REBLEED EPISODE`, tag.healthy = 0,methods = "Youden",  data = hematemesis)
        
        
        na.omit(hematemesis)
        ifelse(hematemesis$`REBLEED EPISODE`=="y",0,1)
      hematemesis=  complete.cases(hematemesis)
      
      hematemesis$`REBLEED EPISODE`
      
      
      