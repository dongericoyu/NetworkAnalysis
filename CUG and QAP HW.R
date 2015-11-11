###SNA Homework Rscript shell
set.seed(11235)
#Q1 getting the data ready
library(network)
library(sna)
data(emon)
g<-emon$HurrFrederic
g

#Executing

q1a<-cug.test(g, INCLUDE INFO HERE)
q1a
plot.cug.test(q1a)

q1b<-cug.test(g, INCLUDE INFO HERE)
q1b
plot.cug.test(q1b)

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


