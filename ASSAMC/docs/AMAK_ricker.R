h=seq(0, 2, by=0.001)
amak_h=exp(h)/(4+exp(h))
plot(h, amak_h, pch=19,
     xlab="h'",
     ylab="h",
     xlim=c(0,2),
     ylim=c(0,2))
abline(v=0.75, lty=2, col="gray30")
abline(h=exp(0.75)/(4+exp(0.75)), lty=2, col="gray30")
