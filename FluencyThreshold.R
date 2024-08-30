

###### Count data appoach: verbal fluency




############ Fit data sets: first fixed difficulty functions + plots
#                          later Splines + plots

library(splines)
library(tidyr)
library(spatstat) 
library("foreign") 
#library(degreenet)  ## masks gauss.hermite!
library(glmmTMB)


### run before fitting

source("ProgramsFixed.R")
source("ProgramsFixedandBasis.R")
###################

### data sets

### vfnew.dat

############### verbal fluency 
#mydata <- read.table("vfnew.dat", header=TRUE,sep="", row.names="id")
#mydata <- read.table("vfnew.dat", header=TRUE,sep="")
datvf<-read.table("vfnew.dat",header=T)
summary(datvf)
dataw <- spread(datvf, item, value)
dataw
dat<- na.omit(dataw)
summary(dat)
newdata <- dat[c(2:5)]
dat <- t(newdata)
################

I <- dim(dat)[1]
P <- dim(dat)[2]


### fitting with varying slopes 

lin <- "log"
ThrV <- ThreshModFixed(dat,commonslope="var" ,indicator= "D", der="der",lin =lin,start, startind="no")
ThrV


##############################


### fitting with fixed slopes logarithmic difficulty functions

  

Thr <- ThreshModFixed(dat,commonslope="com" ,indicator= "D", der="der",lin =lin,start, startind="no")
Thr

# restart until Thr$convergence=0
start <- Thr$par
Thr <- ThreshModFixed(dat,commonslope="com" ,indicator= "D", der="no",lin =lin,start=start, startind="yes")
Thr
##############################









###############################################
#### Plots for fixed difficulty functions
###############################################

parmatrest <- Thr$parmatrix ### for common
#parmatrest <- ThrV$parmatrix ###  for varying slopes


### for data transformation if lin= inv
width<- max(dat)-min(dat)
norm<- 10  ### 10 means in (0.0833 0.91666)
mintr <- min(dat)- width/norm
maxtr <- max(dat)+ width/norm
datstd<- (dat -mintr)/(maxtr-mintr)
#####################################

###plot   delta fct 

min <- min(dat)
max <- max(dat)
ylims <- c(-6,6)
x<- seq(min,max,(max-min)/40)
pcdum <-c ('1','2','3','4','5','6')
#x<- seq(0,maxresp,1)
y<- 0*x
if(lin == "lin")delta <- parmatrest[1,1]+ parmatrest[1,2]*x
if(lin == "log")delta <- parmatrest[1,1]+ parmatrest[1,2]*log(1+x)
if(lin == "inv"){xt <- (x -mintr)/(maxtr-mintr)
delta <- parmatrest[1,1]+ parmatrest[1,2]*log(xt/(1-xt))}
plot(x,delta,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="Difficulty functions",xlab ="y", type="b",lwd=1.2,
     ylab="",pch =pcdum[1],cex = 1.1, ylim=ylims )

for (i in 2:I){
  if(lin == "lin")  delta <- parmatrest[i,1]+ parmatrest[i,2]*x
  if(lin == "log")  delta <- parmatrest[i,1]+ parmatrest[i,2]*log(1+x)
  if(lin == "inv"){xt <- (x -mintr)/(maxtr-mintr)
  delta <- parmatrest[i,1]+ parmatrest[i,2]*log(xt/(1-xt))}
  lines(x,delta,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lwd=1.2, type="b",pch =pcdum[i],cex = 1.1)
}



####PT function 

pcdum <-c ('1','2','3','4','5','6')

y <- seq(min,max,(max-min)/40)
if(lin == "lin")delta <- parmatrest[1,1]+ parmatrest[1,2]*y
if(lin == "log")delta <- parmatrest[1,1]+ parmatrest[1,2]*log(1+y)
if(lin == "inv"){xt <- (y -mintr)/(maxtr-mintr)
delta <- parmatrest[1,1]+ parmatrest[1,2]*log(xt/(1-xt))}
theta <- 0 
problargy <- pnorm(theta - delta,0,1)
plot(y,problargy,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="P(Y>y)",xlab ="y", type="b",lwd=1.2,ylim=c(0,1),
     ylab="", pch =pcdum[1])

for (i in 2:I){
  if(lin == "lin")  delta <- parmatrest[i,1]+ parmatrest[i,2]*y
  if(lin == "log")  delta <- parmatrest[i,1]+ parmatrest[i,2]*log(1+y)
  if(lin == "inv"){xt <- (y -mintr)/(maxtr-mintr)
  delta <- parmatrest[i,1]+ parmatrest[i,2]*log(xt/(1-xt))}
  problargy <- pnorm(theta - delta,0,1)
  
  lines(y,problargy,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lty=2,lwd=1.2, type="b",pch =pcdum[i],cex = 1)
}

##### IC curves

y=0 ### specified y-value

theta <- seq(-4,4,0.1)
numtheta <- length(theta)

if(lin == "lin")delta <- parmatrest[1,1]+ parmatrest[1,2]*y
if(lin == "log")delta <- parmatrest[1,1]+ parmatrest[1,2]*log(1+y)
if(lin == "inv"){xt <- (y -mintr)/(maxtr-mintr)
delta <- parmatrest[1,1]+ parmatrest[1,2]*log(xt/(1-xt))}
problargy <- pnorm(theta - delta,0,1)
plot(theta,problargy,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="IC function",xlab ="theta", type="b",
     lwd=2.0,ylim=c(0,1), pch =pcdum[1])

for (l in 2:I){
  if(lin == "lin") delta <- parmatrest[l,1]+ parmatrest[l,2]*y
  if(lin == "log") delta <- parmatrest[l,1]+ parmatrest[l,2]*log(1+y)
  if(lin == "inv"){xt <- (y -mintr)/(maxtr-mintr)
  delta <- parmatrest[l,1]+ parmatrest[l,2]*log(xt/(1-xt))}
  problargy <- pnorm(theta - delta,0,1)
  
  lines(theta,problargy,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lty=2,lwd=2.0, type="b",pch =pcdum[l],cex = 1)
}

#### densities for continuous responses - do not use for discrete fits
theta <- 0  # specify theta

pcdum <-c ('1','2','3','4','5','6')
ylims <- c(0,0.5) 
y <- seq(min,max,(max-min)/70)
if(lin == "lin")delta <- parmatrest[1,1]+ parmatrest[1,2]*y
if(lin == "log")delta <- parmatrest[1,1]+ parmatrest[1,2]*log(1+y)
if(lin == "inv"){xt <- (y -mintr)/(maxtr-mintr)
delta <- parmatrest[1,1]+ parmatrest[1,2]*log(xt/(1-xt))}

problargy <- dnorm(theta - delta,0,1)
plot(y,problargy,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="Densities",xlab ="y", type="b",lwd=1.2,ylim=ylims,
     ylab="", pch =pcdum[1])

for (i in 2:I){
  if(lin == "lin")  delta <- parmatrest[i,1]+ parmatrest[i,2]*y
  if(lin == "log")  delta <- parmatrest[i,1]+ parmatrest[i,2]*log(1+y)
  if(lin == "inv"){xt <- (y -mintr)/(maxtr-mintr)
  delta <- parmatrest[i,1]+ parmatrest[i,2]*log(xt/(1-xt))}
  problargy <- dnorm(theta - delta,0,1)
  
  lines(y,problargy,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lty=2,lwd=1.2, type="b",pch =pcdum[i],cex = 1)
}

#### densities  for discrete responses
theta <- 0  # specify theta

pcdum <-c ('1','2','3','4','5','6')
ylims <- c(0,0.5)
limit<-30
y <- seq(0,limit,1)
if(lin == "lin")delta <- parmatrest[1,1]+ parmatrest[1,2]*y
if(lin == "log")delta <- parmatrest[1,1]+ parmatrest[1,2]*log(1+y)
if(lin == "inv"){xt <- (y -mintr)/(maxtr-mintr)
delta <- parmatrest[1,1]+ parmatrest[1,2]*log(xt/(1-xt))}


problargy <- pnorm(theta - delta,0,1)
limit1<-limit+1
prob<- matrix(0,limit1,1)
prob[1,1]<-1-problargy[1]
for (l in 2:limit1) prob[l,1]<-problargy[l-1] -problargy[l]

#plot(y,problargy ,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="Densities",xlab ="y", type="b",lwd=1.2,ylim=c(0,1),
#     ylab="", pch =pcdum[1])
plot(y,prob ,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="Densities",xlab ="y", type="b",lwd=1.2,
     ylab="", pch =pcdum[1],ylim=c(0,0.20), cex=1.2)
sum(prob)

for (i in 2:I){
  if(lin == "lin")  delta <- parmatrest[i,1]+ parmatrest[i,2]*y
  if(lin == "log")  delta <- parmatrest[i,1]+ parmatrest[i,2]*log(1+y)
  if(lin == "inv"){xt <- (y -mintr)/(maxtr-mintr)
  delta <- parmatrest[i,1]+ parmatrest[i,2]*log(xt/(1-xt))}
  problargy <- pnorm(theta - delta,0,1)
  prob[1,1]<-1-problargy[1]
  for (l in 2:limit1) prob[l,1]<-problargy[l-1] -problargy[l]
  print(sum(prob))
  lines(y,prob,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lwd=1.2, type="b",pch =pcdum[i],cex = 1.2)
}


##### posterior person parameters  

parmatrest<-ThrV$parmatrix
stdest <- ThrV$stdmixt
grid <- seq(-3,5,0.1)  ##theta grid to compute estimates

PostFit <- PosteriorEstimates(grid,dat,I,indicator,lin =lin,parmatrest,stdest)

pers<-seq(1,P,1)
plot(pers,PostFit,main ="Person parameters",xlab ="Persons", lwd=2,
     ylab="Estimated parameters",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)  ### plot true against posterior

perssum<- rep(0,P)
for (l in 1:P)perssum[l]<- sum(dat[,l])

plot(perssum,PostFit,main ="Person parameters",xlab ="Sums of scores", lwd=2,
     ylab="Estimated parameters",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)  ### plot true against posterior

cor(perssum,PostFit) ## crrelatio with total score



###############################################
##### comparison with Conway-Maxwell

library(glmmTMB) # version 0.2.1.0
library(Rcpp) # version 0.12.17
sourceCpp("COM_poisson.cpp")
library(psych) # version 1.8.4
library(rlecuyer) # version 0.3-4


###### delete missing values from datvf

tab <-table(datvf$subject_id,datvf$item)
tabdf <-as.data.frame(tab)

### persons with missing values:
indicpers<-1
for (l in 1:dim(tab))if(sum(tab[l,])<4)indicpers<-c(indicpers,dimnames(tab)[[1]][l])
indicpers<-indicpers[2:length(indicpers)]

newdata <- datvf[which(datvf$subject_id !=indicpers[1]),]
for (l in 2:length(indicpers))newdata<-newdata[which(newdata$subject_id !=indicpers[l]),]

dim(newdata)
dim(datvf)

#### Poisson fit

Poisson <-
  glmmTMB(value ~ -1 + item + (1 | subject_id), data =newdata, family = poisson)
summary(Poisson)

AIC(Poisson)  
reff <-ranef(Poisson, condVar = TRUE)
reffm<- as.data.frame(reff)

cor(PostFit,reffm$condval) ## correlation with threshold

### estimate CMPCM with global dispersion parameter

Global <- glmmTMB(value ~ -1 + item + (1 | subject_id), data = newdata, 
                 family = compois, dispformula = ~ 1)
summary(Global)

refg <-ranef(Global, condVar = TRUE)
reffmg<- as.data.frame(refg)

cor(PostFit,reffmg$condval)


### estimate CMPCM with item-specific dispersion parameters
Disp <- glmmTMB(value ~ -1 + item + (1 | subject_id), data = newdata, 
                  family = compois, dispformula = ~ -1 + item)

summary(Disp)

refd <-ranef(Disp, condVar = TRUE)
reffmd<- as.data.frame(refd)

cor(PostFit,reffmd$condval)



####################################################
#################  Fixed+Splines Fits
#########################################



###splines with Fixed+Basis

indicatorsingle  <- "D0"
indicatorvec  <- indicatorsingle
for (l in 2:I )indicatorvec<- c(indicatorvec,indicatorsingle)

numknotsstart<-2 #( 3 produces 5 Bsplines, 6 produces 8 Bsplines)
ord <-4

#splinesplus<-"log"
ThrExt <- ThreshFixedBasis(dat,basis="splines", splinesplus="log",indicatorvec,ord, 
                           numknotsstart,lambdpenin=0,lambdc=0,start, startind="no",der="der")
ThrExt




##############################################
##################plots fixed+splines  only
###################################################

datt <- dat

itemmatrixest <- ThrExt$parmatrix  #### - matrix!!!!! 
numbasis<-ThrExt$numbasis
minresp <- min(datt)
maxresp <- max(datt)
dif<- maxresp-minresp


knots <- seq(minresp-(ord-1)*dif/(numknotsstart-1),maxresp+(ord-1)*dif/(numknotsstart-1),dif/(numknotsstart-1))


###plot splines - delta fct
pcdum <-c ('1','2','3','4','5','6')

x<- seq(minresp,maxresp,(maxresp-minresp)/50)
#x<- seq(1,7,0.15)
y<- 0*x
for (l in 1:length(x)){s <-splineDesign(knots,x[l], ord = ord, derivs=0, outer.ok = TRUE, sparse = FALSE)
y[l]<- itemmatrixest[1,1] + itemmatrixest[1,2]*log(1+x[l])  +s%*%itemmatrixest[1,3:numbasis]
#y[l]<- s%*%par2
}



plot(x,y,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="Difficulty functions",xlab ="y", type="b",lwd=1.2,
     ylim=c(-6,8), ylab="",pch =pcdum[1])

for(it in 2:I){
  for (l in 1:length(x)){s <-splineDesign(knots,x[l], ord = ord, derivs=0, outer.ok = TRUE, sparse = FALSE)
  y[l]<- itemmatrixest[it,1] + itemmatrixest[it,2]*log(1+x[l])  +s%*%itemmatrixest[it,3:numbasis]}
  lines(x,y,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, xlab ="y", type="b",lwd=1.2,
        pch =pcdum[it])}
#######

####PT function  
pcdum <-c ('1','2','3','4','5','6')

y <- seq(minresp,maxresp,(maxresp-minresp)/50)
s <-splineDesign(knots,y, ord = ord, derivs=0, outer.ok = TRUE, sparse = FALSE)

delta <-  itemmatrixest[1,1] + itemmatrixest[1,2]*log(1+y)+s%*%itemmatrixest[1,3:numbasis]
#plot(y,delta)


theta <- 0 
problargy <- pnorm(theta - delta,0,1)
plot(y,problargy,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="P(Y>y)",xlab ="y", type="b",lwd=1.2,ylim=c(0,1),
     ylab="", pch =pcdum[1])

for (i in 2:I){
  delta <-  itemmatrixest[i,1] + itemmatrixest[i,2]*log(1+y)+s%*%itemmatrixest[i,3:numbasis]
  problargy <- pnorm(theta - delta,0,1)
  
  lines(y,problargy,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lty=2,lwd=1.2, type="b",pch =pcdum[i],cex = 1)
}

##### density continuous
theta <- 0 ### specify value

pcdum <-c ('1','2','3','4','5','6')

y <- seq(minresp,maxresp,(maxresp-minresp)/50)
s <-splineDesign(knots,y, ord = ord, derivs=0, outer.ok = TRUE, sparse = FALSE)

delta <-   itemmatrixest[1,1] + itemmatrixest[1,2]*log(1+y)+s%*%itemmatrixest[1,3:numbasis]

problargy <- dnorm(theta - delta,0,1)
plot(y,problargy,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="Densities",xlab ="y", type="b",lwd=1.2,ylim=c(0,0.5),
     ylab="", pch =pcdum[1])

for (i in 2:I){
  delta <-  itemmatrixest[i,1] + itemmatrixest[i,2]*log(1+y)+s%*%itemmatrixest[i,3:numbasis]
  problargy <- dnorm(theta - delta,0,1)
  
  lines(y,problargy,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lty=2,lwd=1.2, type="b",pch =pcdum[i],cex = 1)
}

##### density discrete
theta <- 0 ### specify value

pcdum <-c ('1','2','3','4','5','6')
limit<-20
y <- seq(0,limit,1)
s <-splineDesign(knots,y, ord = ord, derivs=0, outer.ok = TRUE, sparse = FALSE)

delta <-   itemmatrixest[1,1] + itemmatrixest[1,2]*log(1+y)+s%*%itemmatrixest[1,3:numbasis]

problargy <- pnorm(theta - delta,0,1)
limit1<-limit+1
prob<- matrix(0,limit1,1)
prob[1,1]<-1-problargy[1]
for (l in 2:limit1) prob[l,1]<-problargy[l-1] -problargy[l]

#plot(y,problargy ,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="Densities",xlab ="y", type="b",lwd=1.2,ylim=c(0,1),
#     ylab="", pch =pcdum[1])
plot(y,prob ,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="Densities",xlab ="y", type="b",lwd=1.6,
     ylab="", pch =pcdum[1],ylim=c(0,0.40), cex=1.3)
sum(prob)

for (i in 2:I){
  delta <-  itemmatrixest[i,1] + itemmatrixest[i,2]*log(1+y)+s%*%itemmatrixest[i,3:numbasis]
  problargy <- pnorm(theta - delta,0,1)
prob[1,1]<-1-problargy[1]
for (l in 2:limit1) prob[l,1]<-problargy[l-1] -problargy[l]
print(sum(prob))
lines(y,prob,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lwd=1.2, type="b",pch =pcdum[i],cex = 1.3)
}







#########################################
#####   B-Splines only
########################################


lambdpenin=0.0 
lambdc=0
#indicatorsingle <- "C" # sonst D0, D1
#indicator <- "D1"
indicatorsingle  <- "D0"
indicatorvec  <- indicatorsingle
for (l in 2:I )indicatorvec<- c(indicatorvec,indicatorsingle)
###
numknotsstart<-6
ThrB <- ThreshFixedBasis(dat,basis="splines", splinesplus="no",indicatorvec,ord, 
                           numknotsstart,lambdpenin=0,lambdc,start, startind="no",der="der")
ThrB
#ThrB6<-ThrB
#ThrB4<-ThrB

### alternative
TrB1 <-ThreshModSplines(dat,commonslope,indicatorvec,ord, numknotsstart,lambdpenin=0,lambdc=0,start, startind="no")
TrB1
###refit
start <- TrB1$par
TrB1 <-ThreshModSplines(dat,commonslope,indicatorvec,ord, numknotsstart,lambdpenin=0.1,lambdc=0,start, startind="yes")

summary(dat[2,])
order(dat[2,])

datred<- as.matrix(dat[2,])
order(datred[,1])


######################################
##################plots splines


itemmatrixest <- ThrB$parmatrix
minresp <- min(datt)
maxresp <- max(datt)
dif<- maxresp-minresp
knots <- seq(minresp-(ord-1)*dif/(numknotsstart-1),maxresp+(ord-1)*dif/(numknotsstart-1),dif/(numknotsstart-1))


###plot splines - delta fct

x<- seq(minresp,maxresp,(max-min)/50)
#x<- seq(1,7,0.15)
y<- 0*x
for (l in 1:length(x)){s <-splineDesign(knots,x[l], ord = ord, derivs=0, outer.ok = TRUE, sparse = FALSE)
y[l]<- s%*%itemmatrixest[1,]
#y[l]<- s%*%par2
}
plot(x,y,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="Difficulty functions",xlab ="y", type="b",lwd=1.2,
     ylim=c(-6,6), ylab="",pch =pcdum[1])
for(it in 2:I){
  for (l in 1:length(x)){s <-splineDesign(knots,x[l], ord = ord, derivs=0, outer.ok = TRUE, sparse = FALSE)
  y[l]<- s%*%itemmatrixest[it,]}
  lines(x,y,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, xlab ="y", type="b",lwd=1.2,
        pch =pcdum[it])}
#######

####PT function  
pcdum <-c ('1','2','3','4','5','6')

y <- seq(minresp,maxresp,(max-min)/50)
s <-splineDesign(knots,y, ord = ord, derivs=0, outer.ok = TRUE, sparse = FALSE)

delta <-  s%*%itemmatrixest[1,]
#plot(y,delta)


theta <- 0 
problargy <- pnorm(theta - delta,0,1)
plot(y,problargy,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="P(Y>y)",xlab ="y", type="b",lwd=1.2,ylim=c(0,1),
     ylab="", pch =pcdum[1])

for (i in 2:I){
  delta <-  s%*%itemmatrixest[i,]
  problargy <- pnorm(theta - delta,0,1)
  
  lines(y,problargy,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lty=2,lwd=1.2, type="b",pch =pcdum[i],cex = 1)
}

##### density continuous
theta <- 0 ### specify value

pcdum <-c ('1','2','3','4','5','6')

y <- seq(minresp,maxresp,(max-min)/50)
s <-splineDesign(knots,y, ord = ord, derivs=0, outer.ok = TRUE, sparse = FALSE)

delta <-  s%*%itemmatrixest[1,]

problargy <- dnorm(theta - delta,0,1)
plot(y,problargy,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="Densities",xlab ="y", type="b",lwd=1.2,ylim=c(0,0.5),
     ylab="", pch =pcdum[1])

for (i in 2:I){
  delta <-  s%*%itemmatrixest[i,]
  problargy <- dnorm(theta - delta,0,1)
  
  lines(y,problargy,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lty=2,lwd=1.2, type="b",pch =pcdum[i],cex = 1)
}

##### density discrete
theta <- 0 ### specify value

pcdum <-c ('1','2','3','4','5','6')
limit<-30
y <- seq(0,limit,1)

s <-splineDesign(knots,y, ord = ord, derivs=0, outer.ok = TRUE, sparse = FALSE)

delta <-  s%*%itemmatrixest[1,]

problargy <- pnorm(theta - delta,0,1)
limit1<-limit+1
prob<- matrix(0,limit1,1)
prob[1,1]<-1-problargy[1]
for (l in 2:limit1) prob[l,1]<-problargy[l-1] -problargy[l]

#plot(y,problargy ,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="Densities",xlab ="y", type="b",lwd=1.2,ylim=c(0,1),
#     ylab="", pch =pcdum[1])
plot(y,prob ,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="Densities",xlab ="y", type="b",lwd=1.6,
     ylab="", pch =pcdum[1],ylim=c(0,0.20), cex=1.3)
sum(prob)

for (i in 2:I){
  delta <-  s%*%itemmatrixest[i,]
  problargy <- pnorm(theta - delta,0,1)
  prob[1,1]<-1-problargy[1]
  for (l in 2:limit1) prob[l,1]<-problargy[l-1] -problargy[l]
  print(sum(prob))
  lines(y,prob,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lwd=1.2, type="b",pch =pcdum[i],cex = 1.3)
}
###### comparison links


#### inverse yields fit for standardized data
Thrinv <- ThreshModFixed(dat,commonslope="com" ,indicator= "C", der="no",lin ="inv",start, startind="no")
Thrinv$Loglik

####  compute std data 

width<- max(dat)-min(dat)
norm<- 10
mintr <- min(dat)- width/norm
maxtr <- max(dat)+ width/norm
datstd<- (dat -mintr)/(maxtr-mintr)

Thrstdlinear <- ThreshModFixed(datstd,commonslope="com" ,indicator= "C", der="no",lin ="lin",start, startind="no")
Thrstdlinear$Loglik


#####
1-pchisq(3.84,1)
1-pchisq(107.36,32)
