#11/11/2015#
##ERGM examples

setwd("C://Users//Dong//Desktop//2015 Fall//Network Analysis//Course-ERGM")
set.seed(11235)
library(ergm)

# DATA
advice <- read.csv("Advice.csv", header=F)

reportsto <- read.csv("ReportsTo.csv", header = F) ##associating the advice and reportsto network

# Read in vertex attribute data
attributes <- read.csv("KrackhardtVLD.csv")

adviceNet <- network(advice)

set.vertex.attribute(adviceNet,names(attributes),attributes)

set.network.attribute(adviceNet,"reportsto",as.matrix(reportsto))


#Let's look at our network
adviceNet

#directed network; size=21;
#this is a binary network, so there is no edge attributes; for weighted/valued ties, we can have edge attributes

windows()
plot(adviceNet, displaylabels=T,edge.col="grey50",xpd=T)


#Simple ERGM

## Just basic covariate model
spec0 <- ergm(adviceNet~edges+edgecov("reportsto")+nodeicov("Tenure")+nodeocov("Tenure")+absdiff("Tenure")+nodeicov("Age")+nodeocov("Age")+absdiff("Age"))
summary(spec0)
#edges are like the intercept in a ERGM. they go in every model
#edgecovariate of "reportsto": whether I report to you --> whetehr I ask you for advice;
#nodeicov: indegree and tenure: tenure, more indegree 
#nodeocov: outdegree and tenure: tenure, less outdegree
#absdiff: how long I work there v.s. how long you work there
#nodeicov(age): indegree and age: older, more indegree 
#nodeocov(age): outdegree and age: older, less outdegree
#absdiff(age): how long I work there v.s. how long you work there


##result##
Monte Carlo MLE Results:
  Estimate Std. Error MCMC %  p-value    
edges             -0.22089    0.83101      0 0.790522    
edgecov.reportsto  3.17309    1.06300      0 0.003004 **  
  nodeicov.Tenure    0.10798    0.02056      0  < 1e-04 ***   ##whether tenture is very singificant: more tenure, more likely
  nodeocov.Tenure   -0.05448    0.01765      0 0.002166 ** 
  absdiff.Tenure    -0.07551    0.02212      0 0.000706 ***
  nodeicov.Age      -0.02184    0.01781      0 0.220847    
nodeocov.Age       0.02489    0.01502      0 0.098338 .  
absdiff.Age       -0.02618    0.01792      0 0.144676    
---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Null Deviance: 582.2  on 420  degrees of freedom
Residual Deviance: 488.1  on 412  degrees of freedom

AIC: 504.1    BIC: 536.5    (Smaller is better.)

##estimate: exponential; log-odds
##calculation of probability --> see later coursenote


# Now adding transitivity
spec2 <- ergm(adviceNet~edges+mutual+ostar(2:3)+transitive+edgecov("reportsto")+nodeicov("Tenure")+nodeocov("Tenure")+absdiff("Tenure")+nodeicov("Age")+nodeocov("Age")+absdiff("Age"))
summary(spec2)

##ostar: the starting value



# Use GWESP Instead
spec3 <- ergm(adviceNet~edges+mutual+ostar(2:3)+gwesp(0,fixed=T)+edgecov("reportsto")+nodeicov("Tenure")+nodeocov("Tenure")+absdiff("Tenure")+nodeicov("Age")+nodeocov("Age")+absdiff("Age"))
summary(spec3)

##gwesp: shared partnership;
##0: the initiate altha
##fixed: if T, then the compuater only use this altha; if false, the computer will try different altha


?ergm.terms
####Ok, back to how this works



##Erico: try my own data with AM's political discussion network and attributes
AM_discuss <- read.csv("AM_discuss.csv", header=F)

AM_attribute <- read.csv("AM_attribute.csv", header = T) ##associating the advice and reportsto network

discussNet <- network(AM_discuss)

set.vertex.attribute(discussNet,names(AM_attribute),AM_attribute)


#Let's look at our network
discussNet

#directed network; size=21;
#this is a binary network, so there is no edge attributes; for weighted/valued ties, we can have edge attributes

windows()
plot(discussNet, displaylabels=T,edge.col="grey50",xpd=T)


discuss01 <- ergm(discussNet~edges+absdiff("age")+absdiff("ideo"))
summary(discuss01)








#ERGM Examples Round 2
#find some cool data
data(package = "ergm")

#let's use samplike ##monks esteem network##
data(sampson, package="ergm")
samplike


cols<-vector()
for(i in 1:18){
  ifelse(samplike$val[[i]]$cloisterville==0, 
         cols[i] <- "red", cols[i]<-"blue")
}

plot(samplike, vertex.col=cols)

#what structure do we want to use to model this network?
?ergm.terms




###Let's estimate some models
model1 <- ergm(samplike ~ edges + nodefactor("cloisterville"))
model1  
summary(model1)
?ergm
#Note MLE v MSPE choice

model2 <- ergm(samplike ~ edges + sender + receiver + mutual, estimate="MLE") #MLE is the default
#See this is going to take a while......
summary(model2)

model2 <- ergm(samplike ~ edges + sender + receiver + mutual, estimate="MPLE")
summary(model2)

model2 <- ergm(samplike ~ edges + ostar(2) + istar(2) + mutual + transitive, estimate="MLE")
summary(model2)

#Using MCMC-MLE
model3 <- ergm(samplike ~ edges + odegree((2:5), by="cloisterville", homophily=T) + idegree((2:5), by="cloisterville", homophily=T), control=control.ergm(MCMC.samplesize=5000,MCMC.burnin=10000,MCMLE.maxit=10))
#MLE still takes a moment, but not so long with this one
##MCMC.burnin:Number of proposals before any MCMC sampling is done. It typically is set to a fairly large number.
##MCMC.samplesize:
##homophily: if you esteen me, I am more likely to esteem you;
##odgree(2:5): which degrees I am going to look at
summary(model3)



###More on how to interpret this and what else we can do with it next week :)

###But first: your turn
allylist <- dget("ally2000.txt")
names(allylist)
# 'net' is the adjacency matrix
# 'labels' is a vector of country names
polity <- read.csv("polity.csv")

# Compute polity distance
poldist <- as.matrix(dist(polity,diag=0, upper=T))


# Initialize a Network
AllyNet <- network(allylist$net, directed=F)

# Add the states' names
network.vertex.names(AllyNet) <- allylist$labels


