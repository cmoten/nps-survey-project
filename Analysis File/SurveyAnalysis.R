#Student Population is 1,598

#Read in Survey Data
library(gplots) #For barplot with CI

survey.analyze <- read.csv(file.choose())
survey.analyze$AgeCat <- "Younger"
survey.analyze$AgeCat[survey.analyze$AgeGroup=="41+"] <- "Older"
survey.analyze$TechConf <- "High"
survey.analyze$TechConf[survey.analyze$Q37 != "Expert"] <- "Low"
as.factor(survey.analyze$SchoolGroup <- "Other")
survey.analyze$SchoolGroup[survey.analyze$SchoolName=="Graduate School of Operational and Information Sciences"] <- "GSOIS"

likert.names <- c("Strongly \n Disagree","Somewhat \n Disagree", "Neutral", "Somewhat \n Agree", "Strongly \n Agree")

#This function will compute a simultaneous confidence interval for likert items

simul.conf <- function(n_i,n,k,alpha=0.05) {
	B <- qchisq(alpha/k,1,lower.tail=FALSE)
	tmp1 <- (n_i+(B/2))/(n+B)
	tmp2 <- B^2/4 + (B*n_i) * (1-n_i/n)
	tmp3 <- (n+B)^2
	conf.lower <- tmp1 - sqrt(tmp2/tmp3)
	conf.upper <- tmp1 + sqrt(tmp2/tmp3)
	res <- list(Mean = round(tmp1,3),StdError = round(sqrt(tmp2/tmp3),3),ConfInt = c(round(conf.lower,3),round(conf.upper,3)))
	res
}

#For your particular question just do a find and replace of the question number and all the appropriate
#text will be replaced, and just change the chart title in the main argument of the barplot
#Q28e Analysis
#If your question has any NA values use the commented out code below, if not just continue
#survey.analyze.sub <- subset(survey.analyze,Q28e != "NA")
Q28e.tab <- table(survey.analyze$Q28e) #Table of total responses
Q28e.SA <- simul.conf(n_i=sum(Q28e.tab[5]),n=sum(Q28e.tab),k=length(Q28e.tab)) #Strongly Agree CI
Q28e.A <- simul.conf(n_i=sum(Q28e.tab[4]),n=sum(Q28e.tab),k=length(Q28e.tab)) #Somewhat Agree CI
Q28e.N <- simul.conf(n_i=sum(Q28e.tab[3]),n=sum(Q28e.tab),k=length(Q28e.tab)) #Neutral CI
Q28e.D <- simul.conf(n_i=sum(Q28e.tab[2]),n=sum(Q28e.tab),k=length(Q28e.tab)) #Somewhat Disagree CI
Q28e.SD <- simul.conf(n_i=sum(Q28e.tab[1]),n=sum(Q28e.tab),k=length(Q28e.tab)) #Somewhat Disagree CI

#Create Bar Plot with standard error bars
Q28eheights <- prop.table(Q28e.tab)
Q28elower <- c(Q28e.SD$ConfInt[1],Q28e.D$ConfInt[1],Q28e.N$ConfInt[1],Q28e.A$ConfInt[1],Q28e.SA$ConfInt[1])
Q28eupper <- c(Q28e.SD$ConfInt[2],Q28e.D$ConfInt[2],Q28e.N$ConfInt[2],Q28e.A$ConfInt[2],Q28e.SA$ConfInt[2])

#Barplot with confidence interval
png('/Users/cardymoteniii/Desktop/OA4109-Survey Research Methods/Survey Project/Brief/Bar Plots/Q28ebarplot.png')
barplot2(Q28eheights,plot.ci=TRUE,ci.l=Q28elower,ci.u=Q28eupper,
ylim=c(0,0.5),ylab="Proporttion",cex.main = 0.9, cex.lab= 0.9, main="Q28e: I rarely experience connectivity problems with my laptop",names.arg=likert.names,col=rainbow(5))
dev.off()

#Create a bar plot of categorical variables
Q30.tab <- round(prop.table(table(survey.analyze$SchoolName,survey.analyze$Q30),2),3) #Proportion table across table columns
png('/Users/cardymoteniii/Desktop/OA4109-Survey Research Methods/Survey Project/Brief/Bar Plots/Q30barplot.png')
par(xpd=T, mar=par()$mar+c(0,0,0,6))
barplot(Q30.tab,col=rainbow(5),main="Q30: Would you benefit from staying in the same classroom \n with your cohort for most of your classes \n at NPS if that classroom met all of your academic needs?",xlab="Category",ylab="Proportion")
legend(4,1,legend = c("GSBPP","GSEAS","GSOIS","Provost","SIGS"),cex=0.6,fill=c(rainbow(5)[1],rainbow(5)[2],rainbow(5)[3],rainbow(5)[4],rainbow(5)[5]))
par(mar=c(5, 4, 4, 2) + 0.1)
dev.off()

survey.analyze$Q37 = factor(survey.analyze$Q37,levels(survey.analyze$Q37)[c(5,2,4,1,3)]) #Reorder factor levels
Q37.tab.school <- round(prop.table(table(survey.analyze$Q37,survey.analyze$SchoolName),2),3) #Proportion table across table columns
Q37.tab.age <- round(prop.table(table(survey.analyze$Q37,survey.analyze$AgeGroup),2),3)
png('/Users/cardymoteniii/Desktop/OA4109-Survey Research Methods/Survey Project/Brief/Bar Plots/Q37pt2_age_barplot.png')
par(xpd=T, mar=par()$mar+c(0,0,0,6))
barplot(Q37.tab.age,col=rainbow(5),main="Distribution of Technological Rating by Age Group",xlab="Category",ylab="Proportion",cex.names=0.8)
legend(6,1,legend = c("Not Like","Basic","Intermediate","Advanced","Expert"),cex=0.6,fill=c(rainbow(5)[1],rainbow(5)[2],rainbow(5)[3],rainbow(5)[4],rainbow(5)[5]))
par(mar=c(5, 4, 4, 2) + 0.1)
dev.off()

png('/Users/cardymoteniii/Desktop/OA4109-Survey Research Methods/Survey Project/Brief/Bar Plots/Q37pt2_school_barplot.png')
par(xpd=T, mar=par()$mar+c(0,0,0,6))
barplot(Q37.tab.school,col=rainbow(5),main="Distribution of Technological Rating by School",xlab="Category",ylab="Proportion",cex.names=0.8,names.arg=c("GSBPP","GSEAS","GSOIS","Provost","SIGS"))
legend(6,1,legend = c("Not Like","Basic","Intermediate","Advanced","Expert"),cex=0.6,fill=c(rainbow(5)[1],rainbow(5)[2],rainbow(5)[3],rainbow(5)[4],rainbow(5)[5]))
par(mar=c(5, 4, 4, 2) + 0.1)
dev.off()



a <- c(NewLngSvy$Q32,NewLngSvy$Q33,NewLngSvy$Q34,NewLngSvy$Q35,NewLngSvy$Q28e)
#Calculate response mean for a particular question
library(survey)
NewLngSvy <- data.frame(survey.analyze,N=rep(1598,916))
NewLngSvy.design <- svydesign(id=~1,fpc=~N,data=NewLngSvy)
svymean(NewLngSvy[,137:145],NewLngSvy.design,na.rm=TRUE)
test.sub <- subset(survey.analyze,AgeGroup != "Unknown")
test.sub2 <- subset(survey.analyze,Q37 == "Expert")
t.test(Q28e~SchoolGroup,data=survey.analyze)
t.test(test.sub2$Q33,mu=0,alternative="less")