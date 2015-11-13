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


#Plot the EMONs
par(mfrow=c(3,3))
for(i in 1:length(emon))
  plot(emon[[i]],main=names(emon)[i],edge.lwd="Frequency")

plot(emon$MtStHelens %s% which(type=="State"), displaylabels=TRUE)

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


#Q2 data
library(NetData) #just for the data
data(studentnets.mrqap173, package="NetData")

# We need the data in matrix format
#Our "IV" networks
m173_sem1_SSL <- as.matrix(m173_sem1_SSL)
m173_sem1_RCE <- as.matrix(m173_sem1_RCE)
m173_sem1_GND <- as.matrix(m173_sem1_GND)

# Our "DV" networks
m173_sem2_SSL <- as.matrix(m173_sem2_SSL)
m173_sem2_TSL <- as.matrix(m173_sem2_TSL)

#Putting the predictor matrices into an array so the QAP regression command will be happy
response_matrices <- array(NA, c(3, length(m173_sem1_SSL[1,]),length(m173_sem1_SSL[1,]))) 
response_matrices[1,,] <- m173_sem1_SSL
response_matrices[2,,] <- m173_sem1_RCE
response_matrices[3,,] <- m173_sem1_GND

#Execution
q2a<-netlm(STUFF GOES HERE BE SURE TO SPECIFY QAP STATISTICAL TESTING)
summary(q2a)

q2c<-netlm(STUFF GOES HERE BE SURE TO SPECIFY CLASSICAL STATISTICAL TESTING)
summary(q2c)


