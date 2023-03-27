##
## Drew A. Linzer
## Emory Univeristy
## Department of Political Science
## dlinzer@emory.edu
##
## Jeffrey B. Lewis
## University of California, Los Angeles
## Department of Political Science
## jblewis@polisci.ucla.edu
##
## April 7, 2011
##
## Commands to produce examples in Journal of Statistical Software article
## "poLCA: An R Package for Polytomous Variable Latent Class Analysis".
##

library(poLCA)

#############################################
## Section 4.4. Predicted cell frequencies ##
#############################################

data(gss82)
f <- cbind(PURPOSE,ACCURACY,UNDERSTA,COOPERAT)~1
gss.lc2 <- poLCA(f,gss82,nclass=2)

gss.lc2$predcell

poLCA.table(formula=COOPERAT~1,
            condition=list(PURPOSE=3,ACCURACY=1,UNDERSTA=2),
            lc=gss.lc2)

poLCA.table(formula=COOPERAT~UNDERSTA,
            condition=list(PURPOSE=3,ACCURACY=1),
            lc=gss.lc2)

poLCA.predcell(lc=gss.lc2,y=c(1,1,1,1))


########################################################
## Section 4.7. Recognizing and avoiding local maxima ##
########################################################

data(gss82)
f <- cbind(PURPOSE,ACCURACY,UNDERSTA,COOPERAT)~1

mlmat <- NULL
for (i in 1:500) { # note, this simulation takes some time to run
    gss.lc <- poLCA(f,gss82,nclass=3,maxiter=3000,verbose=FALSE)
    o <- order(gss.lc$probs$UNDERSTA[,1],decreasing=T) # ideal, skeptic, believer
    mlmat <- rbind(mlmat,c(gss.lc$llik,gss.lc$P[o]))
}

tab <- table(round(mlmat[,1],3)) # Table 1

gss.lc <- poLCA(f,gss82,nclass=3,maxiter=3000,nrep=10)


######################################################################
## Section 5.1. Basic latent class modeling with the carcinoma data ##
######################################################################

data(carcinoma)
f <- cbind(A,B,C,D,E,F,G)~1
lc2 <- poLCA(f,carcinoma,nclass=2)
lc3 <- poLCA(f,carcinoma,nclass=3,graphs=T)
lc4 <- poLCA(f,carcinoma,nclass=4,maxiter=5000)


##########################################################################
## Section 5.2. Latent class regression modeling with the election data ##
##########################################################################

data(election)

## one covariate

f.party <- cbind(MORALG,CARESG,KNOWG,LEADG,DISHONG,INTELG,
                 MORALB,CARESB,KNOWB,LEADB,DISHONB,INTELB)~PARTY
nes.party <- poLCA(f.party,election,nclass=3)       # log-likelihood: -16222.32

probs.start <- poLCA.reorder(nes.party$probs.start,
                             order(nes.party$P, decreasing = TRUE))
nes.party <- poLCA(f.party, election, nclass = 3, probs.start = probs.start)

pidmat <- cbind(1,c(1:7)) # matrix of hypothetical party ID values
exb <- exp(pidmat %*% nes.party$coeff)
matplot(c(1:7),(cbind(1,exb)/(1+rowSums(exb))),
                main="Party ID as a predictor of candidate affinity class",
                xlab="Party ID: strong Democratic (1) to strong Republican (7)",
                ylab="Probability of latent class membership",
                ylim=c(0,1),type="l",lwd=3,col=1)
text(5.9,0.40,"Other")
text(5.4,0.75,"Bush affinity")
text(1.8,0.65,"Gore affinity")

## multiple covariates

f.3cov <- cbind(MORALG,CARESG,KNOWG,LEADG,DISHONG,INTELG,
                MORALB,CARESB,KNOWB,LEADB,DISHONB,INTELB)~PARTY*AGE
nes.3cov <- poLCA(f.3cov,election,nclass=3)          # log-likelihood: -16135.39 

probs.start <- poLCA.reorder(nes.3cov$probs.start,
                             order(nes.3cov$P,decreasing=TRUE))
nes.3cov <- poLCA(f.3cov,election,nclass=3,probs.start=probs.start)


strdems <- cbind(1,1,c(18:80),(c(18:80)*1))
exb.strdems <- exp(strdems %*% nes.3cov$coeff)
matplot(c(18:80),(cbind(1,exb.strdems)/(1+rowSums(exb.strdems))),
                  main="Age and candidate affinity for strong Democrats",
                  xlab="Age",ylab="Probability of latent class membership",
                  ylim=c(0,1),type="l",col=1,lwd=3)
text(50,0.3,"Other")
text(50,0.05,"Bush affinity")
text(50,0.7,"Gore affinity")

strreps <- cbind(1,7,c(18:80),(c(18:80)*7))
exb.strreps <- exp(strreps %*% nes.3cov$coeff)
matplot(c(18:80),(cbind(1,exb.strreps)/(1+rowSums(exb.strreps))),
                  main="Age and candidate affinity for strong Republicans",
                  xlab="Age",ylab="Probability of latent class membership",
                  ylim=c(0,1),type="l",col=1,lwd=3)
text(50,0.18,"Other")
text(50,0.9,"Bush affinity")
text(50,0.05,"Gore affinity")


# end of file.
