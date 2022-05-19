png('AgeSpinePlot.png')
plot(Q37~Age,data=survey.age.sub,col=rainbow(5))
title("Computer Expertise Rating by Age")
dev.off()

png('AgeHistogram.png')
x <- seq(20,70,5)
hist(survey.age.sub$Age,main="Distribution of Respondent Ages",freq=FALSE,
xlab="Age",col="blue",xlim=c(20,70),xaxt='n')
axis(1,at=x)
box()
dev.off()

