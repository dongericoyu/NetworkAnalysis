##ERGM examples
##remeber: EGRM can only run with dichonmous/binary network data; it is the biggest limitations to EGRM


setwd("C://Users//Dong//Desktop//2015 Fall//Network Analysis//Week 12_Intepretating EGRM")
set.seed(11235)
library(ergm)

# DATA
advice <- read.csv("Advice.csv", header=F)

reportsto <- read.csv("ReportsTo.csv", header = F)

# Read in vertex attribute data
attributes <- read.csv("KrackhardtVLD.csv")

adviceNet <- network(advice)

set.vertex.attribute(adviceNet,names(attributes),attributes)

set.network.attribute(adviceNet,"reportsto",as.matrix(reportsto))


#Let's look at our network
adviceNet

windows()
plot(adviceNet, displaylabels=T,edge.col="grey50",xpd=T)


#Simple ERGM

## Just basic covariate model
spec0 <- ergm(adviceNet~edges+edgecov("reportsto")+nodeicov("Tenure")+nodeocov("Tenure")+absdiff("Tenure")+nodeicov("Age")+nodeocov("Age")+absdiff("Age"))
summary(spec0)
#edges are like the intercept. they go in every model


# Now adding transitivity
spec2 <- ergm(adviceNet~edges+mutual+ostar(2:3)+transitive+edgecov("reportsto")+nodeicov("Tenure")+nodeocov("Tenure")+absdiff("Tenure")+nodeicov("Age")+nodeocov("Age")+absdiff("Age"))
summary(spec2)


# Use GWESP Instead
spec3 <- ergm(adviceNet~edges+mutual+ostar(2:3)+gwesp(0,fixed=T)+edgecov("reportsto")+nodeicov("Tenure")+nodeocov("Tenure")+absdiff("Tenure")+nodeicov("Age")+nodeocov("Age")+absdiff("Age"))
summary(spec3)

?ergm.terms
####Ok, back to how this works





#ERGM Examples Round 2
#find some cool data
data(package = "ergm")

#let's use samplike
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
summary(model3)



###More on how to interpret this and what else we can do with it next week :)

####ERGM Interpretation and Diagnostics
library(ergm)
set.seed(235813)

# Read in the Senate data
amat <- read.csv("amat.csv",row.names=1)
View(amat)

# Read in euclidean distance (units in lat/long)
edist <- read.csv("edist.csv",row.names=1)
edist <- as.matrix(edist)
View(edist)

# Dichotomize ties ## you need to choose a treshold: in this case, if two senators cosponsor a bill, code as 1;
amat <- (amat > 1)*1
View(amat)

# Read in DW-Nominate scores
dwnom <- read.csv("dwnom10.csv",row.names=1)
dwnom <- dwnom$dwnom
View(dwnom)

# Create the network
net <- network(amat)

# set the vertex attribute
# Must define vertex attributes to use node
# covariates with ergm()
set.vertex.attribute(net,attrname="ideol",val=dwnom) ##associate the ideology (attribute) with the nodes

net

# Nice package for creating color spectra
library(colorRamps)

cols <- blue2red(10)[rank(dwnom)] ##specify the color by logic (rank by dwnom)
plot(net, displaylabels=T, vertex.col=cols,edge.col="grey50",xpd=T)

# ergm with simple ideological homophily
est1 <- ergm(net~edges+absdiff("ideol")) ##DV: binary cosponsor network; IV: edges: as intercepts; IV of interests: absdiff - difference of the ideology
summary(est1)

##results:
#Formula:   net ~ edges + absdiff("ideol")

#Iterations:  4 out of 20 

#Monte Carlo MLE Results:
#  Estimate Std. Error MCMC % p-value   
#edges           0.1213     0.3990      0 0.76189   
#absdiff.ideol  -2.7436     1.0043      0 0.00761 **


##MCMC %: 0; because by default, it is using MLE; 
##Remember in last class: MLE better: gives the estimates you want, and standard error is better; but it cannot be done with complicated model
##When MLE is not applicable or it cannot run quickly, then the EGRM will turn to MCMC
#so the warning:
# "Error in mcmc.diagnostics.ergm(est1) : 
# MCMC was not run or MCMC sample was not stored." it is an unimportant warning; we are ok with it

mcmc.diagnostics(est1)


# ergm adding geographic distance 
est2 <- ergm(net~edges+absdiff("ideol")+edgecov(edist))
summary(est2)

#Monte Carlo MLE Results:
#Estimate Std. Error MCMC % p-value   
#edges          0.334488   0.530747      0 0.53020   
#absdiff.ideol -2.804015   1.023168      0 0.00744 **
#  edgecov.edist -0.007329   0.011936      0 0.54078   

##MCMC # is still 0; Elizabeth: not useful indicator

##AIC and BIC: measuring deviation; smaller (closer to 0) is better; but this is not a strong evidence for your deciding which variables to be included or not



##ergm adding popularity
est3 <- ergm(net~edges+absdiff("ideol")+edgecov(edist)+istar(2)) ##instar (2): we capturing indegree/instar 2-instars
summary(est3)

##adding sender's ideology and receiver's ideology
est4 <- ergm(net~edges+absdiff("ideol")+edgecov(edist)+istar(2)+nodeocov("ideol")+nodeicov("ideol"))
summary(est4)


Monte Carlo MLE Results:
  Estimate Std. Error MCMC % p-value   
edges          -0.473199   0.658315      0 0.47426   
absdiff.ideol  -4.490434   1.592571      0 0.00600 **
  edgecov.edist  -0.007657   0.012496      0 0.54170   
istar2          0.408938   0.139280      0 0.00429 **
  nodeocov.ideol  2.111430   1.239286      0 0.09213 . 
nodeicov.ideol -1.805724   1.134773      0 0.11531 

##more conserative senators are more likly to send a request to cosponsor;
##more conservative senators are less likely to receiver

##the coefficients are log-odds here
exp(-0.493025)##change it into odds

exp(nodecov.ideo)##if we add one more most extreme



##Back to slides

library(xergm)

interpret(est4, i="Baucus_Max", j="Biden_Joseph", type="tie") #tie is the default
interpret(est4, j="Biden_Joseph") ## the predicted probability of forming a tie with Biden; not considering sender (the average, a tie from another nodes to Joe Biden)
interpret(est4, i="Baucus_Max", j="Biden_Joseph", type="dyad") #since directed

##results: probability

int<-interpret(est4, i="Biden_Joseph", j=c(1, 2, 3, 4, 5, 6, 7, 9, 10), type="node")
View(int)
?interpret

int2<-interpret(est4, i=c(1, 2, 3, 8), j=c(4, 5, 6, 7, 9, 10), type="node")
View(int2) ##Why does this not work?


####Diagnostics
detach("package:xergm")
library(ergm)

mcmc.diagnostics(est4)

##Result
Sample statistics summary:
  
  Iterations = 16384:4209664 ##starting from 16384 until 4209664
Thinning interval = 1024  ##not storing all the interations ; store in memory at every 1024 interation; also prevening autocorrelation
Number of chains = 1 
Sample size per chain = 4096 ##4096

1. Empirical mean and standard deviation for each variable,
plus standard error of the mean:
  
                  Mean      SD  Naive SE      Time-series SE
edges          -0.07544   5.979  0.09342        0.09342
absdiff.ideol  -0.05411   2.111  0.03298        0.03470
edgecov.edist   1.64233 187.306  2.92666        2.92666
istar2         -0.38867  21.684  0.33881        0.33881
nodeocov.ideol -0.07448   1.199  0.01874        0.01952
nodeicov.ideol  0.06418   1.604  0.02507        0.02507

##we want the NSE and TS SE similar
## If not: the store in your memory have a autocorrelation problem (????)


2. Quantiles for each variable:
  
                2.5%       25%     50%      75%   97.5%
edges           -11.000   -4.0000   0.000   4.0000  12.000
absdiff.ideol    -3.840   -1.5515  -0.195   1.3198   4.417
edgecov.edist  -336.000 -131.0000 -11.000 121.0000 400.250
istar2          -36.625  -16.0000  -3.000  13.0000  47.000
nodeocov.ideol   -2.378   -0.9212  -0.086   0.7433   2.312
nodeicov.ideol   -3.169   -0.9943   0.058   1.1565   3.164


Sample statistics cross-correlations:
                   edges absdiff.ideol edgecov.edist     istar2 nodeocov.ideol nodeicov.ideol
edges           1.0000000     0.9111607    0.86205215  0.9112542     0.30436875     -0.5744820
absdiff.ideol   0.9111607     1.0000000    0.77786624  0.8899043     0.44869522     -0.6250782
edgecov.edist   0.8620522     0.7778662    1.00000000  0.7703969     0.09396585     -0.6631080
istar2          0.9112542     0.8899043    0.77039691  1.0000000     0.23814989     -0.6855595
nodeocov.ideol  0.3043687     0.4486952    0.09396585  0.2381499     1.00000000      0.1236687
nodeicov.ideol -0.5744820    -0.6250782   -0.66310796 -0.6855595     0.12366872      1.0000000



Sample statistics auto-correlation:
  Chain 1 
             edges absdiff.ideol edgecov.edist       istar2 nodeocov.ideol nodeicov.ideol
Lag 0     1.000000000   1.000000000   1.000000000  1.000000000    1.000000000    1.000000000
Lag 1024  0.006095652   0.007859947  -0.009253705  0.013524009    0.040846481   -0.004550607 ## depends on thnning #
Lag 2048  0.017282694   0.018996084   0.024945654  0.022170456   -0.012665935    0.004992436
Lag 3072 -0.024910650  -0.026487165  -0.027887934 -0.016099102    0.001445549   -0.015481504
Lag 4096  0.016545931   0.015592979   0.019765124  0.002567208   -0.012961067    0.013619852
Lag 5120  0.027126994   0.033930572   0.022003239  0.025795941   -0.003420749    0.011949278

## actual autocorrelation overtime: it is 
## does not a strong

## the solution to autocorreltion: increasing the burn-in; increasing the length of the chain to run



Sample statistics burn-in diagnostic (Geweke): ##a null hypotheis test: comparing the first 10% of the iteration and the last 10% iteration
  # whether the two chains come from the same population; Null: the same population
  
  #
  
  #MCMC chain runs long enough, find a stable space
  
Chain 1 

Fraction in 1st window = 0.1 #first 10% 
Fraction in 2nd window = 0.5 #first 50%

##we want high p-value (1) and small coefficient

    edges  absdiff.ideol  edgecov.edist         istar2 nodeocov.ideol nodeicov.ideol 
-1.1883        -1.3164        -1.4869        -1.4424         0.4127         1.2884 

Individual P-values (lower = worse):
  edges  absdiff.ideol  edgecov.edist         istar2 nodeocov.ideol nodeicov.ideol 
0.2347288      0.1880323      0.1370488      0.1491812      0.6798127      0.1975970 
Joint P-value (lower = worse):  0.6102524 .
Loading required namespace: latticeExtra
Failed with error:  ‘there is no package called ‘latticeExtra’’
Package latticeExtra is not installed. Falling back on coda's default MCMC diagnostic plots'






## Diagnostic of degenracy:
gof4<-gof(est4, control=control.gof.ergm(nsim=200))
gof4
plot(gof4)

#result:
Area under the ROC/PR curve:
  t AUC ROC AUC PR
1  0.7940 0.5778

Degeneracy check for network 1:
                     obs    sim       est        se  zval pval
edges               27.00  26.84 -1.60e-01  5.82e+01  0.00 1.00
absdiff.ideol        7.32   7.18 -1.42e-01  2.01e+01 -0.01 0.99
edgecov.edist[[i]] 689.00 679.98 -9.02e+00  1.91e+03  0.00 1.00
istar2              51.00  50.40 -6.00e-01  2.10e+02  0.00 1.00
nodeocov.ideol       1.38   1.38  3.04e-03  1.09e+01  0.00 1.00
nodeicov.ideol      -3.44  -3.48 -3.83e-02  1.59e+01  0.00 1.00


#p-value higher, then it is better




est5 <- ergm(net~edges+absdiff("ideol")+edgecov(edist)+istar(2)+gwesp(0, fixed=T))
summary(est5)
mcmc.diagnostics(est5)
gof5<-gof(est5, control=control.gof.ergm(nsim=200))
gof5




library(sna)

### Simulation from the model (checking mutual ties)
# simulate 400 networks
sims4 <- simulate(est4,nsim=400) ##in EGRM package

# compute mutuality of observed network
obs.mutual <- mutuality(net) ##sna pakage
# compute mutuality of simulated networks
sim.mutual <- mutuality(sims4)

# plot observed against simulated
# boxplot of simulated
boxplot(sim.mutual)
# Add red line at observed value
abline(h=obs.mutual,col="red",lwd=3)

