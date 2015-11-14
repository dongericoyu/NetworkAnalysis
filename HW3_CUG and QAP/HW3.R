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
#1.Eigenvector Centrality
q1b1<-cug.test(g,evcent,cmode="size") 
q1b1
plot.cug.test(q1b1)


q1b2<-cug.test(g,evcent,cmode="edges")
q1b2
plot.cug.test(q1b2)


q1b3<-cug.test(g,evcent,cmode="dyad.census")
q1b3
plot.cug.test(q1b3)

##the plots are strange; ask Elizabeth and Christina


#2. Graph Density
q1b1<-cug.test(g,gden,cmode="size") 
q1b1
plot.cug.test(q1b1)

##controling for node number, it is unique;

q1b2<-cug.test(g,gden,cmode="edges")
q1b2
plot.cug.test(q1b2)

#controlling for edge number, not unique
## what does a perfect Pr=1 mean??


q1b3<-cug.test(g,gden,cmode="dyad.census")
q1b3
plot.cug.test(q1b3)


#3.Graphic Reciprocity
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


#4.degree centrality
q1b1<-cug.test(g,degree,cmode="size") 
q1b1
plot.cug.test(q1b1)


q1b2<-cug.test(g,degree,cmode="edges")
q1b2
plot.cug.test(q1b2)


q1b3<-cug.test(g,degree,cmode="dyad.census")
q1b3
plot.cug.test(q1b3)

##again, not graphic statistics, wield results; check bacl later



##Other random statistics with SNA package
clique.census(g, mode = "digraph", tabulate.by.vertex = TRUE,
              clique.comembership = c("none", "sum", "bysize"), enumerate = TRUE)







#Q2 data
library(NetData) #just for the data
data(studentnets.mrqap173, package="NetData")





# We need the data in matrix format
#Our "IV" networks
m173_sem1_SSL <- as.matrix(m173_sem1_SSL) #student social interaction: undirected, unvalued
m173_sem1_RCE <- as.matrix(m173_sem1_RCE) #homophily of race:undirected, unvalued
m173_sem1_GND <- as.matrix(m173_sem1_GND) #homophily of gender: undirected, unvalued


#let's plot the networks
##plot(m173_sem1_SSL, displaylabels=T,edge.col="grey50",xpd=T) ##incorrect plot code


# Our "DV" networks
m173_sem2_SSL <- as.matrix(m173_sem2_SSL)
m173_sem2_TSL <- as.matrix(m173_sem2_TSL)

#Putting the predictor matrices into an array so the QAP regression command will be happy
response_matrices <- array(NA, c(3, length(m173_sem1_SSL[1,]),length(m173_sem1_SSL[1,]))) 
response_matrices[1,,] <- m173_sem1_SSL
response_matrices[2,,] <- m173_sem1_RCE
response_matrices[3,,] <- m173_sem1_GND


#still missing something before running QAP....

##QAP coursenote:
# Read in the cosponsorship data (108th Senate)
senlist <- dget("sennet.txt") #list of senators and their adjacency matrix
SenNet <- senlist$net #extracting the adjacency matrix
dwnom <- read.csv("dwnom.csv") #dwnominate scores for each Senator

# Create Distance Matrix for homophily
ideoDist <- as.matrix(dist(dwnom[,1])) #focusing on 1st column

# Create Sender covariate
# Building matrix column by column
# element ij is i's value
ideoSend <- matrix(dwnom[,1],nrow(dwnom),nrow(dwnom),byrow=F)

# Create Receiver covariate
# Building matrix row by row
# element ij is j's value
ideoRec <- matrix(dwnom[,1],nrow(dwnom),nrow(dwnom),byrow=T)

# Create Distance Matrix for homophily using the second dimension of DW-Nominate
ideoDist2 <- as.matrix(dist(dwnom[,2]))

# Create Sender covariate
ideoSend2 <- matrix(dwnom[,2],nrow(dwnom),nrow(dwnom),byrow=F)

# Create Receiver covariate
ideoRec2 <- matrix(dwnom[,2],nrow(dwnom),nrow(dwnom),byrow=T)


# Estimate OLS model
library(sna)

# Create the 'graph stack' of covariates
covariates <- list(ideoDist=ideoDist,ideoSend=ideoSend,ideoRec=ideoRec,ideoDist2=ideoDist2,ideoSend2=ideoSend2,ideoRec2=ideoRec2)

# Run QAP for contunious outcomes (netlogit is for dichotomous ties)
# First OLS (nullhyp="classical")
# Second QAP

#DV here is the adjacency matrix
ols <- netlm(SenNet,covariates,nullhyp="classical")

resultsOLS <- cbind(summary(ols)[[1]],summary(ols)[[9]],summary(ols)[[10]],summary(ols)[[11]])

set.seed(345589)
system.time(qap <- netlm(SenNet,covariates,nullhyp="qap",reps=100))

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


