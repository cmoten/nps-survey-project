#Read in survey data file
survey <- read.csv(file.choose())

#To make the Likert Plot Data, I made a new data table in excel
print(levels(survey$Q36)) #Find out the current factor structure
#Change factor levels to "Strongly Disagree, Somewhat Disagree, Neutral, Somewhat Agree, Agree, NA(if necessary)"
survey$Q36 <- factor(survey$Q36,levels=c("Strongly Disagree", "Somewhat Disagree", "Neither Agree or Disagree", "Somewhat Agree", "Strongly Agree",""))
#Make the appropriate table for the subquestions you want to use in the Likert function
a <- table(survey$SchoolName,survey$Q36)
b <- table(survey$AgeGroup,survey$Q36)
c <- table(survey$Q37,survey$Q36)
write.csv(rbind(a,b,c),file="/Users/cardymoteniii/Desktop/test.csv")

#Read in the Likert plot data csv file and subset it for the appropriate question to plot
library(HH)
survey.test <- read.csv(file.choose())
survey.test28c <- subset(survey.test,SubQuestion=="Q28c" & Question != "Unknown") #Remove unknown age group for this data
survey.test28c <- survey.test28c[,-6]  # Drop the NAs
survey.test28c <- survey.test28c[,1:8] #All remaining columns

#If you want to preview the plot prior to making a pdf use the dev.new command to make 
#a window large enough
dev.new(width=8,height=6)

#Print the plot to a pdf to put in presentation for later
png('/Users/cardymoteniii/Desktop/OA4109-Survey Research Methods/Survey Project/Brief/Likert Plots/Q28clikertplot.png')
likert(Question ~ . | Subtable, survey.test28c,
       scales=list(y=list(relation="free")), layout=c(1,4),
       positive.order=TRUE,  
       as.percent=TRUE,       # Plot on a percent scale
       between=list(y=0),
       strip=FALSE, strip.left=strip.custom(bg="gray97"),
       par.strip.text=list(cex=.6, lines=5),
       main="Q28c: The speed allows me to complete necessary tasks \n in a timely manner",
       ylab=NULL)
dev.off()
   

