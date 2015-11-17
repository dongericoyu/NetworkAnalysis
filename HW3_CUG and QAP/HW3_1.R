###SNA Homework Rscript shell
set.seed(11235)
#Q1 getting the data ready
library(network)
library(sna)
data(emon)
g<-emon$HurrFrederic

#Let's look at our network
g
windows()
plot(g, displaylabels=T,edge.col="grey50",xpd=T)



#Executing

#a. conduct a CUG to test if the amount of TRANSITIVITY of this network is significantly higher (or lower)
#then would be expected at random

#code reference:http://www.inside-r.org/packages/cran/sna/docs/cug.test


#Explaining CUG test:


##size: we control the size (the same # of nodes), and each edge is assumed be randomly seleced based same probability,
#and we replicate for many times, and see whether the observered network is unique or not #very similar to permutaiton
#An easy way to imtepret: the null hypothesis: the observed network is randomly created given conditions

##Butt 2007: "the CUG test is a test of the hypothesis that an observed statistics, s(g),
#was drawn from the distribution of s arising from the CUG distribution specified by t, x.
#(one-sided)

##In this exercise, we want to determine whether the degree of transitivity of a given structure (observered network)
#is greater than would be expected from its size and density alone, we perform an upper tail CUG test
#of the transitivity score against the N, m distribution.

#A high p-value for the associated test suggests that the observed graph is NOT more transitive than would be
#anticipated from its size and density and, hence, that some additional process or contraint may be at work.


q1a1<-cug.test(g,gtrans,cmode="size") 
q1a1
plot.cug.test(q1a1)



#edges: the same density
##Note: this is an additional conditions: edges+size: it is a cumulative condition
q1a2<-cug.test(g,gtrans,cmode="edges")
q1a2
plot.cug.test(q1a2)

#dyad.census
##similarlly, this is a cumulative condition based on edges+sizes
q1a3<-cug.test(g,gtrans,cmode="dyad.census")
q1a3
plot.cug.test(q1a3)


##b. Try different Graphic Network Statistics
##Update: Nov17 Test two Graphic Statistics: Density and Reciprocity

#1. Graph Density: the number of the edges (observed edges dived by total possible edges)
q1b1<-cug.test(g,gden,cmode="size") ##control the # of nodes
q1b1
plot.cug.test(q1b1)

##controling for node number, it is unique; we see more diverse networks; the observed network is more sparse compare to what we expected


q1b2<-cug.test(g,gden,cmode="edges") ##control the # of edges
q1b2
plot.cug.test(q1b2)

##It is still unique; (Probablity=1) but there is not much variation between the observed and the random networks thaat created based on these conditions;
##it does not make sence; here we are overfitting/over-controlling the conditions
##Elizabeth: for example: we are actually setting the odd
##possible reason getting this result: (1)we have too small network, and we are overcontrolling

#controlling for edge number, not unique
## what does a perfect Pr=1 mean??


q1b3<-cug.test(g,gden,cmode="dyad.census")
q1b3
plot.cug.test(q1b3)


#2.Graphic Reciprocity
##Nov 17: included in HW
q1b1<-cug.test(g,grecip,cmode="size") 
q1b1
plot.cug.test(q1b1)


q1b2<-cug.test(g,grecip,cmode="edges")
q1b2
plot.cug.test(q1b2)

##very unique when controling both size and edges

q1b3<-cug.test(g,grecip,cmode="dyad.census")
q1b3
plot.cug.test(q1b3)

##not unique when controling dyad census....





## Note: CUG and QAP are testing hypotheses at two different levels of analysis:
#CUG: graphic/the complete network level
#QAP: node-level; dyad-level

##Thus: 3.Eigenvector centrality: dyad statistics, a different level analysis, not applicable to cug test
#4.degree centrality: the same as 3
q1b1<-cug.test(g,degree,cmode="size") 
q1b1
plot.cug.test(q1b1)


q1b2<-cug.test(g,degree,cmode="edges")
q1b2
plot.cug.test(q1b2)


q1b3<-cug.test(g,degree,cmode="dyad.census")
q1b3
plot.cug.test(q1b3)

##again, not graphic statistics, wield results




#Q2 data
library(NetData) #just for the data
data(studentnets.mrqap173, package="NetData")


# We need the data in matrix format
#Our "IV" networks
m173_sem1_SSL <- as.matrix(m173_sem1_SSL) #student social interaction: undirected, unvalued
m173_sem1_RCE <- as.matrix(m173_sem1_RCE) #homophily of race:undirected, unvalued: coded as 1 if same race; 0 if not
m173_sem1_GND <- as.matrix(m173_sem1_GND) #homophily of gender: undirected, unvalued; code as 1 if same sex; 0 if not


#let's plot the networks
##plot(m173_sem1_SSL, displaylabels=T,edge.col="grey50",xpd=T) ##incorrect plot code


# Our "DV" networks
m173_sem2_SSL <- as.matrix(m173_sem2_SSL)
m173_sem2_TSL <- as.matrix(m173_sem2_TSL)

#Putting the predictor matrices into an array so the QAP regression command will be happy
response_matrices <- array(NA, c(3, length(m173_sem1_SSL[1,]),length(m173_sem1_SSL[1,]))) ##not pretty sure what is going on here
response_matrices[1,,] <- m173_sem1_SSL
response_matrices[2,,] <- m173_sem1_RCE
response_matrices[3,,] <- m173_sem1_GND

#creating Distance matrix for homophily using race
racedis<-as.matrix(dist(m173_sem1_RCE[]))





# Estimate OLS model
library(sna)

##Note (Nov.17): Since all the three varaibles (social, race and gender) are categorical, not as contionus as the senator idealogoy score in
#the course example, thus, it is not necessary to create a distance matrix of homophily of each of the three variables.

# Create the 'graph stack' of covariates
covariates <- list(m173_sem1_SSL= m173_sem1_SSL,m173_sem1_RCE=m173_sem1_RCE,m173_sem1_GND=m173_sem1_GND)

# Run QAP for contunious outcomes (netlogit is for dichotomous ties)
# First OLS (nullhyp="classical")
# Second QAP

#DV here is the adjacency matrix
ols <- netlm(m173_sem2_SSL,covariates,nullhyp="classical")

resultsOLS <- cbind(summary(ols)[[1]],summary(ols)[[9]],summary(ols)[[10]],summary(ols)[[11]])
summary(ols)
plot(m173_sem2_SSL)

set.seed(52246)
system.time(qap <- netlm(m173_sem2_SSL,covariates,nullhyp="qap",reps=1988))

summary(qap)

resultsQAP <- cbind(summary(qap)[[1]],summary(qap)[[10]],summary(qap)[[11]],summary(qap)[[12]])


##Notice R2
##Notice one-sided and two-sided pvalues. Anything strange?
#Note: UCINet only gives one-sided
#Notice: Coefficient distribution summary

resultsALL <- cbind(summary(qap)[[1]],summary(ols)[[11]],summary(qap)[[12]])



#Execution
q2a<-netlm(STUFF GOES HERE BE SURE TO SPECIFY QAP STATISTICAL TESTING)
summary(q2a)

q2c<-netlm(STUFF GOES HERE BE SURE TO SPECIFY CLASSICAL STATISTICAL TESTING)
summary(q2c)


