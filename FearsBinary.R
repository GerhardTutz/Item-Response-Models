



######  Example 2.4 
 

################## Fears data binary

dat <- read.table("fear", header=TRUE,sep= " ")
###################


##########

I <- dim(dat)[1]
P <- dim(dat)[2]


dim(dat)
datnew<-dat

for (i in 1:5){
  for (j in 1:200){
    if(dat[i,j] <= "4"){datnew[i,j]<-0}
    if(dat[i,j] >= "5"){datnew[i,j]<-1}
  }}

sum(datnew[1,])/P

dattr<-t(as.matrix(datnew))



### conditional likelihood fit


claust<- PCM(dattr,  se = TRUE, sum0 = TRUE)
claust$loglik

#### Andersen

lraust<-LRtest(claust, splitcr = "mean", se = TRUE)
lraust





par(cex.lab=1,cex.axis=1.0,cex.main=1.0,pch=16,cex=1.0)

plotGOF(lraust, beta.subset = "all", main = "Graphical Model Check", xlab="rawscores < mean", ylab="rawscores > mean",
        tlab = "number",  type = "p", pos = 4, conf = NULL, ctrline = NULL,
        smooline = NULL, asp = 1, x_axis = TRUE, y_axis = TRUE, set_par = TRUE,
        reset_par = TRUE,cex.lab=1.8,cex.axis=1.4)



plotGOF(lraust, beta.subset = "all", main = "Graphical Model Check", xlab="rawscores < mean", ylab="rawscores > mean",
        tlab = "number",  type = "p", pos = 4, conf = list(gamma=0.95,col=1), ctrline = NULL,
        smooline = NULL, asp = 1, x_axis = TRUE, y_axis = TRUE, set_par = TRUE,
        reset_par = TRUE)



##### data with covariates
#######################GESIS
load("GLES17angst.rda")
summary(GLES)

datg <- GLES
names(datg) <- c("R", "C", "T",
                "G", "Tu",   "N", "Age","Gender", "EastWest", "Abitur","Unemployment")

head(datg)
dim(datg)
datfull<- cbind(datg[,1:4],datg[1:2036,6:6])
head(datfull)
names(datfull) <- c("R", "C", "T", "G",    "N")

# select, same as in data set fear, which is without covariates

set.seed(300)
dats <- datg[sample(1:nrow(datg), 200, replace=FALSE),]
head(dats)

###without Turkey
datc <- cbind(dats[,1:4],dats[,6:11])   #P x I with covariates
datw<-datc[,1:5]                 #P x I without covariates  

head(datc)
head(datw)

#### binary
datbin<-0*datw
P<- dim(datw)[1]
I<- dim(datw)[2]
for (p in 1:P){for (i in 1:I) {if (datw[p,i] >=5) datbin[p,i]<-1}}


t(dat)-datw             
################

###############


lraust<-LRtest(claust, splitcr = "mean", se = TRUE)
lraust


lraustgender<-LRtest(claust, splitcr = dats$Gender, se = TRUE)
lraustgender

par(cex.lab=1,cex.axis=1.0,cex.main=1.0,pch=16,cex=1.0)

plotGOF(lraustgender, beta.subset = "all", main = "Graphical Model Check", xlab="Gender = 0", ylab="Gender = 1",
        tlab = "number",  type = "p", pos = 4, conf = NULL, ctrline = NULL,
        smooline = NULL, asp = 1, x_axis = TRUE, y_axis = TRUE, set_par = TRUE,
        reset_par = TRUE,cex.lab=1.8,cex.axis=1.4)


plotGOF(lraustgender, beta.subset = "all", main = "Graphical Model Check", xlab="Gender = 0", ylab="Gender = 1",
        tlab = "number",  type = "p", pos = 4, conf = list(gamma=0.95,col=1), ctrline = NULL,
        smooline = NULL, asp = 1, x_axis = TRUE, y_axis = TRUE, set_par = TRUE,
        reset_par = TRUE)



