#Read in survey Analysis file
survey <- read.csv(file.choose())

#To make the Likert Plot Data, I made a new data table in excel
a <- table(survey$SchoolName,survey$Q13f)
b <- table(survey$AgeGroup,survey$Q13f)
c <- table(survey$Q37,survey$Q13f)
write.csv(rbind(a,b,c),file="test.csv")

#Read in the Likert plot data csv file and subset it for the appropriate question to plot
survey.test <- read.csv(file.choose())
survey.test5a <- subset(survey.test,SubQuestion=="Q5a")
survey.test5a <- survey.test5a[,1:8]

#If you want to preview the plot prior to making a pdf use the dev.new command to make 
#a window large enough
#dev.new(width=8,height=6)

#Print the plot to a pdf to put in presentation for later
pdf('likertplot.pdf',width=8,height=6)
likert(Question ~ . | Subtable, survey.test5a,
       scales=list(y=list(relation="free")), layout=c(1,4),
       positive.order=TRUE,
       between=list(y=0),
       strip=FALSE, strip.left=strip.custom(bg="gray97"),
       par.strip.text=list(cex=.6, lines=5),
       main="Q5a: I like multiple writing surfaces in classrooms such as smart whiteboards, \n regular whiteboards, chalk boards, transparent writing surfaces. .",
       ylab=NULL)
dev.off()

