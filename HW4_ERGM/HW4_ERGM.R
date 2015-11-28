##ERGM examples
##remeber: EGRM can only run with dichonmous/binary network data; it is the biggest limitations to EGRM


setwd("C://Users//Dong//Documents//Data//SNA-Course//Intro to Network Statistics//CUG_QAP_ERGM//HW4_ERGM")
set.seed(52246)
library(ergm)

# DATA
capefear <- read.csv("Cape_Fear.csv", header = F)

attributes <- read.csv("Cape_Fear_Attributes.csv", header = T)

# Read in vertex attribute data
estuaryNet <- network(capefear)


TrustDist <- as.matrix(dist(attributes[,1]))

Policy <- as.matrix(dist(attributes[,2]))

Govact <- as.matrix(dist(attributes[,3]))

# Create Sender covariate
# Building matrix column by column
# element ij is i's value
ideoSend <- matrix(dwnom[,1],nrow(dwnom),nrow(dwnom),byrow=F)

# Create Receiver covariate
# Building matrix row by row
# element ij is j's value
ideoRec <- matrix(dwnom[,1],nrow(dwnom),nrow(dwnom),byrow=T)


set.vertex.attribute(estuaryNet,attrname="trust",val=attributes) ###Questions: how to separates the three atributes???????

set.vertex.attribute(estuaryNet,attrname="prodev",val=attributes) ###Questions: how to separates the three atributes???????

set.vertex.attribute(estuaryNet,attrname="govact",val=attributes) ###Questions: how to separates the three atributes???????


set.network.attribute(estuaryNet,"capefear",as.matrix(capefear))


#Let's look at our network
estuaryNet

windows()
plot(estuaryNet, displaylabels=T,edge.col="grey50",xpd=T)


# ergm with simple trust homophily
est1 <- ergm(estuaryNet~edges+absdiff("trust")) ##DV: binary cosponsor network; IV: edges: as intercepts; IV of interests: absdiff - difference of the ideology
summary(est1)

mcmc.diagnostics(est1)


##ergm adding policy orientation homophily
est2 <- ergm(estuaryNet~edges+absdiff("trust")+absdiff("prodev")) ##instar (2): we capturing indegree/instar 2-instars
summary(est2)

mcmc.diagnostics(est2)

##result not significant. Erico: possible collinearity/ endogeneity?
##try to remove trust
est2 <- ergm(estuaryNet~edges+absdiff("prodev")) ##instar (2): we capturing indegree/instar 2-instars
summary(est2)
##policy orientation itself still not significant


##let's try government actor
est2<-ergm(estuaryNet~edges+absdiff("govact,pow=1"))
summary(est2)

##ergm adding popularity
est3 <- ergm(estuaryNet~edges+absdiff("trust")+istar(2)) ##instar (2): we capturing indegree/instar 2-instars
summary(est3)

mcmc.diagnostics(est3)


##adding sender's ideology and receiver's ideology

est4 <- ergm(estuaryNet~edges+absdiff("trust")+istar(2)+istar(2)+edgecov(TrustDist)+nodeocov("trust")+nodeicov("trust"))
summary(est4)

mcmc.diagnostics(est4)