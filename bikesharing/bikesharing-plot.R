## Conformal local influence statistics
CLLEV <- h/sum(h)
CLINF <- v/sum(v)
CLOUT <- CLINF/CLLEV

date.labels <- as.Date("2011-01-01") + 0:364
names(CLLEV) <- names(CLINF) <- names(CLOUT) <- date.labels

is.holiday <- aggregate(bikedata2011$holiday,
                        by=bikedata2011[, "dteday", drop=FALSE], FUN=any)$x
is.weekend <- aggregate(bikedata2011$weekend,
                        by=bikedata2011[, "dteday", drop=FALSE], FUN=any)$x

col <- rep("black", nrow(bikedata2011))
col[is.holiday] <- "red"

col2 <- rep("darkgrey", nrow(bikedata2011))
col2[is.weekend] <- "black"
col2[is.holiday] <- "red"

ticks <- as.Date(c(paste0("2011-", 1:12, "-1"), "2011-12-31"))
midmonth <- as.Date(paste0("2011-", 1:12, "-15"))
    
cairo_pdf("bikeshare-linf.pdf", width=8, height=6)
plot(date.labels, CLINF, type="h", xlab="Date", ylab="CLINF", xaxt="n")
axis(side=1, at=ticks, labels=FALSE)
mtext(side=1, at=midmonth, text=months(midmonth, abbreviate=TRUE))
dev.off()

## Not shown in the article, there are no days with outstandingly high
## leverage, but weekends and holidays have higher leverage than other
## days
plot(date.labels, CLLEV, col=col2, type="h", xlab="Date", ylab="CLLEV")
legend("topleft", lty=1, col=c("red","black","darkgrey"), legend=c("public holiday", "weekend", "weekday"), lwd=2)


cairo_pdf("bikeshare-lout.pdf", width=10, height=6)
plot(date.labels, CLOUT, type="h", xlab="Date", ylab="CLOUT", xaxt="n")
axis(side=1, at=ticks, labels=FALSE)
mtext(side=1, at=midmonth, text=months(midmonth, abbreviate=TRUE))
dev.off()

## Clean up outlier statistics with dimension reduction
Omega <- diag(1/sqrt(h)) %*% V %*% diag(1/sqrt(h)) * sum(h) / sum(diag(V))
ev <- eigen(Omega)

## Not shown in the article, a scree plot suggests informally top 7
## eigenvalues might be enough
plot(ev$values[1:50])

## Take only the top eigenvalues
m <- 7
S <- ev$vectors[,1:m]
Lambda <- diag(ev$values[1:m])
Omega.trunc <- S %*% Lambda %*% t(S)
CLOUT.trunc <- diag(Omega.trunc)

names(CLOUT.trunc) <- date.labels

cairo_pdf("bikeshare-lout-clean.pdf", width=10, height=6)
plot(date.labels, CLOUT.trunc, type="h", xlab="Date", ylab="trunc(CLOUT, 7)", xaxt="n")
axis(side=1, at=ticks, labels=FALSE)
mtext(side=1, at=midmonth, text=months(midmonth, abbreviate=TRUE))
text(as.Date("2011-04-15"), CLOUT.trunc["2011-04-15"], "Emancipation\nday weekend",
     adj=c(1.05,1))
text(as.Date("2011-07-04"), CLOUT.trunc["2011-07-04"], "Independence day",
     adj=c(1.05,1))
text(as.Date("2011-08-27"), CLOUT.trunc["2011-08-27"], "Hurricane\nIrene",
     adj=c(0.5, -0.1))
text(as.Date("2011-09-04"), CLOUT.trunc["2011-09-04"], "Labor day\nweekend",
     adj=c(0,0))# pos=4)
text(as.Date("2011-11-25"), CLOUT.trunc["2011-11-25"], "Black\nFriday",
     adj=c(1.1,1))#pos=2)
text(as.Date("2011-12-30"), CLOUT.trunc["2011-12-30"], "Holiday\nseason",
     adj=c(1.1,0))#pos=2)
dev.off()

### Highlighting days that include important outliers when analysed on an hourly scale
clout.hourly <- v.hourly*sum(h.hourly)/(sum(v.hourly) * h.hourly)
cairo_pdf("bikeshare-clout-hourly.pdf", width=10, height=6)
par(mfrow=c(1,2), las=TRUE)
plot(clout.hourly[bikedata2011$dteday=="2011-05-02"], type="b", pch=16,
     xaxt="n", xlab="time", ylab="CLOUT", main="2011-05-02", ylim=c(0, 45))
axis(side=1, at=c(1, 7,13,19, 24), labels=c("00:00", "06:00", "12:00", "18:00", "23:00"))
plot(clout.hourly[bikedata2011$dteday=="2011-08-23"], type="b", pch=16,
     xaxt="n", xlab="time", ylab="CLOUT", main="2011-08-23", ylim=c(0, 45))
axis(side=1, at=c(1, 7,13,19, 24), labels=c("00:00", "06:00", "12:00", "18:00", "23:00"))
dev.off()
