

# some diagnostics of the model fit
fit_files <- c("men_fit.RData","women_fit.RData")

pdf("vaporfly_coefficients.pdf", width=8,height=3.6)
par(mfrow=c(1,2),family="serif", mar=c(3.5,4,3,1), oma = c(0,0,1.0,0))
titles <- c("Men","Women")
for(j in 1:2){
    # loads lots of objects from fitting script
    load(fit_files[j])
    n <- length(y)

    # define covariance matrix
    covmat <- 
        covparms1[1]*ZZ1 + 
        covparms1[2]*ZZ2 +
        covparms1[3]*ZZ3 +
        covparms1[4]*diag(n)

    # calculation linear combinations
    lincomb <- solve(covmat, X ) %*% solve( t(X) %*% solve(covmat,X) )[,2]

    cols <- c("blue","red")
    ord <- order(day_count)
    plot(day_count,lincomb,pch=1,cex=0.5,col=cols[x1+1], 
        axes = FALSE, ann=FALSE, ylim = c(-0.025,0.025))
    axis(1,at=(0:5)*365.25,labels=rep("",6), lwd = 0, lwd.ticks = 1)
    axis(1,at=seq(0.5,4.5,by=1)*365.25,lwd=0,lwd.ticks=0,
        labels=2015:2019)
    axis(2, lwd=0,lwd.ticks=1)
    abline(0,0,col="gray")
    mtext(titles[j], side=3, line = 0.5 )
    if(j==1){legend("topleft",legend=c("Vaporfly","Not Vaporfly"),
        col = rev(cols), pch = 1) }
    mtext("Race Date",side=1,line=2.5)
    mtext("Coefficients",side=2,line=2.5)
    box()
}
mtext("Coefficients in Estimate of Additive Vaporfly Effect",
    side=3, line = -1, outer = TRUE)
dev.off()



