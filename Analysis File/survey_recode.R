#Read in survey data
survey <- read.csv(file.choose())

#The Recode function in the car package is one way of recoding data;
#I used a combination of the Rcmdr library to automate this process and some
#Manual typing of the recoding.

#Yes/ No Questions
survey$Q1 <- Recode(survey$Q1, '"Yes" = "Yes"; "No" = "No"; else = NA', as.factor.result=TRUE)
survey$Q2 <- Recode(survey$Q2, '"Yes" = "Yes"; "No" = "No"; else = NA', 
  as.factor.result=TRUE)
survey$Q4a <- Recode(survey$Q4a, '"Yes" = "Yes"; "No" = "No"; else = NA', 
  as.factor.result=TRUE)
survey$Q4b <- Recode(survey$Q4b, '"Yes" = "Yes"; "No" = "No"; else = NA', 
  as.factor.result=TRUE)
survey$Q4c <- Recode(survey$Q4c, '"Yes" = "Yes"; "No" = "No"; else = NA', 
  as.factor.result=TRUE)
survey$Q4d <- Recode(survey$Q4d, '"Yes" = "Yes"; "No" = "No"; else = NA', 
  as.factor.result=TRUE)
survey$Q4e <- Recode(survey$Q4e, '"Yes" = "Yes"; "No" = "No"; else = NA', 
  as.factor.result=TRUE)
survey$Q4f <- Recode(survey$Q4f, '"Yes" = "Yes"; "No" = "No"; else = NA', 
  as.factor.result=TRUE)
survey$Q4g <- Recode(survey$Q4g, '"Yes" = "Yes"; "No" = "No"; else = NA', 
  as.factor.result=TRUE)
survey$Q4h <- Recode(survey$Q4h, '"Yes" = "Yes"; "No" = "No"; else = NA', 
  as.factor.result=TRUE)
survey$Q4i <- Recode(survey$Q4i, '"Yes" = "Yes"; "No" = "No"; else = NA', 
  as.factor.result=TRUE)
survey$Q4j <- Recode(survey$Q4j, '"Yes" = "Yes"; "No" = "No"; else = NA', 
  as.factor.result=TRUE)
survey$Q4k <- Recode(survey$Q4k, '"Yes" = "Yes"; "No" = "No"; else = NA', 
  as.factor.result=TRUE)
survey$Q4l <- Recode(survey$Q4l, '"Yes" = "Yes"; "No" = "No"; else = NA', 
  as.factor.result=TRUE)
survey$Q4m <- Recode(survey$Q4m, '"Yes" = "Yes"; "No" = "No"; else = NA', 
  as.factor.result=TRUE)
summary(survey$Q4m)
survey$Q17 <- Recode(survey$Q17, '"Yes" = "Yes"; "No" = "No"; else = NA;', 
  as.factor.result=TRUE)
survey$Q21 <- Recode(survey$Q21, '"Yes" = "Yes"; "No" = "No"; else = NA;', 
  as.factor.result=TRUE)
survey$Q25 <- Recode(survey$Q25, '"Yes" = "Yes"; "No" = "No"; else = NA;', 
  as.factor.result=TRUE)
survey$Q30 <- Recode(survey$Q30, '"Yes" = "Yes"; "No" = "No"; else = NA;', 
  as.factor.result=TRUE)
  
#Likert Scale Questions  
survey$Q5a <- Recode(survey$Q5a, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
survey$Q5b <- Recode(survey$Q5b, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
survey$Q5c <- Recode(survey$Q5c, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
survey$Q12a <- Recode(survey$Q12a, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
survey$Q12b <- Recode(survey$Q12b, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
survey$Q13a <- Recode(survey$Q13a, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
survey$Q13b <- Recode(survey$Q13b, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
survey$Q13c <- Recode(survey$Q13c, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
survey$Q13d <- Recode(survey$Q13d, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
survey$Q13e <- Recode(survey$Q13e, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
survey$Q13f <- Recode(survey$Q13f, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
survey$Q28a <- Recode(survey$Q28a, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
survey$Q28b <- Recode(survey$Q28b, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
survey$Q28c <- Recode(survey$Q28c, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
survey$Q28d <- Recode(survey$Q28d, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
survey$Q28e <- Recode(survey$Q28e, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
survey$Q31a <- Recode(survey$Q31a, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
survey$Q31b <- Recode(survey$Q31b, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
survey$Q31c <- Recode(survey$Q31c, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
survey$Q31d <- Recode(survey$Q31d, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
survey$Q32 <- Recode(survey$Q32, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
survey$Q33 <- Recode(survey$Q33, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
survey$Q34 <- Recode(survey$Q34, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
survey$Q35 <- Recode(survey$Q35, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
survey$Q36 <- Recode(survey$Q36, 
  '"Strongly Agree" = 2; "Somewhat Agree" = 1; "Neither Agree or Disagree" = 0; "Somewhat Disagree" = -1; "Strongly Disagree" = -2; else = NA ;',
   as.factor.result=FALSE)
   
#Select all that apply questions   
survey$Q3a <- Recode(survey$Q3a, '"Lack of rooms and spaces to collaborate" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q3b <- Recode(survey$Q3b, '"Posted schedules on classrooms not accurate" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q3c <- Recode(survey$Q3c, '"Facilities are not adequate for collaboration" = 1; else = 0', 
  as.factor.result=TRUE)
str(survey$Q3c)
survey$Q6a <- Recode(survey$Q6a, 
  '"Traditional Style Classroom" = "T"; "Small Lecture Hall with Stadium Seating" = "SL"; "Large Lecture Hall" = "LL"; "Technology-Enhanced Active Learning (TEAL) Classroom with Circular Tables" = "TR"; "Innovative Customizable Classroom" = "ICR"; else = NA',
   as.factor.result=TRUE)
survey$Q6b <- Recode(survey$Q6b, 
  '"Traditional Style Classroom" = "T"; "Small Lecture Hall with Stadium Seating" = "SL"; "Large Lecture Hall" = "LL"; "Technology-Enhanced Active Learning (TEAL) Classroom with Circular Tables" = "TR"; "Innovative Customizable Classroom" = "ICR"; else = NA',
   as.factor.result=TRUE)
survey$Q6c <- Recode(survey$Q6c, 
  '"Traditional Style Classroom" = "T"; "Small Lecture Hall with Stadium Seating" = "SL"; "Large Lecture Hall" = "LL"; "Technology-Enhanced Active Learning (TEAL) Classroom with Circular Tables" = "TR"; "Innovative Customizable Classroom" = "ICR"; else = NA',
   as.factor.result=TRUE)
survey$Q7a <- Recode(survey$Q7a, 
  '"Very Well" = "Very Well"; "Somewhat Well" = "Somewhat Well"; "Very Little" = "Very Little"; "Not At All" = "Not At All"; else = NA',
   as.factor.result=TRUE)
survey$Q7b <- Recode(survey$Q7b, 
  '"Very Well" = "Very Well"; "Somewhat Well" = "Somewhat Well"; "Very Little" = "Very Little"; "Not At All" = "Not At All"; else = NA',
   as.factor.result=TRUE)
survey$Q7c <- Recode(survey$Q7c, 
  '"Very Well" = "Very Well"; "Somewhat Well" = "Somewhat Well"; "Very Little" = "Very Little"; "Not At All" = "Not At All"; else = NA',
   as.factor.result=TRUE)
survey$Q7d <- Recode(survey$Q7d, 
  '"Very Well" = "Very Well"; "Somewhat Well" = "Somewhat Well"; "Very Little" = "Very Little"; "Not At All" = "Not At All"; else = NA',
   as.factor.result=TRUE)
survey$Q7e <- Recode(survey$Q7e, 
  '"Very Well" = "Very Well"; "Somewhat Well" = "Somewhat Well"; "Very Little" = "Very Little"; "Not At All" = "Not At All"; else = NA',
   as.factor.result=TRUE)
survey$Q8a <- Recode(survey$Q8a, 
  '"Very Well" = "Very Well"; "Somewhat Well" = "Somewhat Well"; "Very Little" = "Very Little"; "Not At All" = "Not At All"; else = NA',
   as.factor.result=TRUE)
survey$Q8b <- Recode(survey$Q8b, 
  '"Very Well" = "Very Well"; "Somewhat Well" = "Somewhat Well"; "Very Little" = "Very Little"; "Not At All" = "Not At All"; else = NA',
   as.factor.result=TRUE)
survey$Q8c <- Recode(survey$Q8c, 
  '"Very Well" = "Very Well"; "Somewhat Well" = "Somewhat Well"; "Very Little" = "Very Little"; "Not At All" = "Not At All"; else = NA',
   as.factor.result=TRUE)
survey$Q8d <- Recode(survey$Q8d, 
  '"Very Well" = "Very Well"; "Somewhat Well" = "Somewhat Well"; "Very Little" = "Very Little"; "Not At All" = "Not At All"; else = NA',
   as.factor.result=TRUE)
survey$Q8e <- Recode(survey$Q8e, 
  '"Very Well" = "Very Well"; "Somewhat Well" = "Somewhat Well"; "Very Little" = "Very Little"; "Not At All" = "Not At All"; else = NA',
   as.factor.result=TRUE)
survey$Q8f <- Recode(survey$Q8f, 
  '"Very Well" = "Very Well"; "Somewhat Well" = "Somewhat Well"; "Very Little" = "Very Little"; "Not At All" = "Not At All"; else = NA',
   as.factor.result=TRUE)
survey$Q8g <- Recode(survey$Q8g, 
  '"Very Well" = "Very Well"; "Somewhat Well" = "Somewhat Well"; "Very Little" = "Very Little"; "Not At All" = "Not At All"; else = NA',
   as.factor.result=TRUE)
survey$Q8h <- Recode(survey$Q8h, 
  '"Very Well" = "Very Well"; "Somewhat Well" = "Somewhat Well"; "Very Little" = "Very Little"; "Not At All" = "Not At All"; else = NA',
   as.factor.result=TRUE)
survey$Q9a <- Recode(survey$Q9a, '"The ability to record classroom lectures" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q9b <- Recode(survey$Q9b, '"The ability to electronically write on whiteboards" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q9c <- Recode(survey$Q9c, '"The ability to project individual student
laptops or other mobile device to screens throughout the classroom" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q9d <- Recode(survey$Q9d, '"The ability to teleconference in the classroom" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q10a <- Recode(survey$Q10a, '"PowerPoint" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q10b <- Recode(survey$Q10b, '"Chalkboard/Whiteboard" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q10c <- Recode(survey$Q10c, '"Electronic Whiteboard (smart-board)" = 1; else = 0', 
  as.factor.result=TRUE)

survey$Q10e <- Recode(survey$Q10e, '"Video Tele-Conference (VTC)" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q10f <- Recode(survey$Q10f, '"Printed materials" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q11a <- Recode(survey$Q11a, '"Verbal lecture only" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q11b <- Recode(survey$Q11b, '"Lecture accompanied by 
PowerPoint presentation" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q11c <- Recode(survey$Q11c, '"Lecture accompanied by printed notes" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q11d <- Recode(survey$Q11d, '"Lecture accompanied by 
chalkboard/whiteboard notes" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q11e <- Recode(survey$Q11e, '"Professor-led group discussion" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q11f <- Recode(survey$Q11f, '"Student-led group discussion" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q11g <- Recode(survey$Q11g, '"In-class professor-led practical 
exercises (programming labs, physical labs, etc.)" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q11h <- Recode(survey$Q11h, '"Previously recorded lectures prepared
by professor" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q11i <- Recode(survey$Q11i, '"Peer mentoring (stronger students
help teach weaker students)" = 1; else = 0', 
  as.factor.result=TRUE)
  
#Ranking Questions  
survey$Q14a <- Recode(survey$Q14a, '1 = 1; 2 = 2; 3 = 3; 4 = 4; else = NA ;', 
  as.factor.result=TRUE)
survey$Q14b <- Recode(survey$Q14b, '1 = 1; 2 = 2; 3 = 3; 4 = 4; else = NA ;', 
  as.factor.result=TRUE)
survey$Q14c <- Recode(survey$Q14c, '1 = 1; 2 = 2; 3 = 3; 4 = 4; else = NA ;', 
  as.factor.result=TRUE)
survey$Q14d <- Recode(survey$Q14d, '1 = 1; 2 = 2; 3 = 3; 4 = 4; else = NA ;', 
  as.factor.result=TRUE)
survey$Q15a <- Recode(survey$Q15a, '1 = 1; 2 = 2; 3 = 3; 4 = 4; 5 = 5; else = NA ; ;', 
  as.factor.result=TRUE)
survey$Q15b <- Recode(survey$Q15b, '1 = 1; 2 = 2; 3 = 3; 4 = 4; 5 = 5; else = NA ; ;', 
  as.factor.result=TRUE)
survey$Q15c <- Recode(survey$Q15c, '1 = 1; 2 = 2; 3 = 3; 4 = 4; 5 = 5; else = NA ; ;', 
  as.factor.result=TRUE)
survey$Q15d <- Recode(survey$Q15d, '1 = 1; 2 = 2; 3 = 3; 4 = 4; 5 = 5; else = NA ; ;', 
  as.factor.result=TRUE)
survey$Q15e <- Recode(survey$Q15e, '1 = 1; 2 = 2; 3 = 3; 4 = 4; 5 = 5; else = NA ; ;', 
  as.factor.result=TRUE)  
survey$Q16 <- Recode(survey$Q16, '"Every time" = "E"; "Frequently" = "F"; 
"Seldom" = "SE"; "Never" = "N"; else = NA', 
  as.factor.result=TRUE)
  
#Select all that apply. Note that responses with an apostrophe has to have an escape
#character
  
survey$Q18a <- Recode(survey$Q18a, '"It is difficult to access" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q18b <- Recode(survey$Q18b, '"Other methods such as Youtube work better" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q18c <- Recode(survey$Q18c, '"I don\'t like watching previously recorded lectures" 
= 1; else = 0', 
  as.factor.result=TRUE)
survey$Q22a <- Recode(survey$Q22a, '"It is difficult to access" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q22b <- Recode(survey$Q22b, '"Other methods such as Google Drive
or Drop Box work better" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q22c <- Recode(survey$Q22c, '"Every professor has a different SAKAI
layout and it\'s confusing" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q24a <- Recode(survey$Q24a, '"It is difficult to access" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q24b <- Recode(survey$Q24b, '"Other methods such as Google Drive
or Drop Box work better" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q24c <- Recode(survey$Q24c, '"I didn\'t know I had a personal drive" = 1; else = 0', 
  as.factor.result=TRUE)
survey$Q26 <- Recode(survey$Q26, '"Bullard Hall" = "BH"; "Glasgow Hall" = "GH"; 
"Halligan Hall" = "HA"; "Hermann Hall" = "HE"; "Ingersoll Hall" = "IH"; 
"King Hall" = "KH"; "Library" = "DKL"; "ME Lecture Hall" = "MEL"; "Reed Hall" = "RE";
"Root Hall" = "RO"; "Spangel Hall" = "SP"; "Watkins Hall" = "WH"; else = NA', 
  as.factor.result=TRUE)
a.text27 <- "Smartphone (examples include: iPhone, Android device)"
b.text27 <- "PDA (examples include: Blackberry, Palm Pilot, etc)"
c.text27 <- "Tablet (examples include: iPad, Windows Surface, Kindle, etc)"
survey$Q27a <- Recode(survey$Q27a, 'a.text27  =  1; else = 0',
   as.factor.result=TRUE)
survey$Q27b <- Recode(survey$Q27b, 'b.text27  =  1; else = 0',
   as.factor.result=TRUE)
survey$Q27c <- Recode(survey$Q27c, 'c.text27  =  1; else = 0',
   as.factor.result=TRUE)
survey$Q27d <- Recode(survey$Q27d, '"Laptop Computer"  =  1; else = 0',
   as.factor.result=TRUE)
survey$Q29 <- Recode(survey$Q29, 
  '"All of my classes are with the same students." = "All"; "Most of my classes are with the same students." = "Most"; "Some of my classes are with the same students." = "Some"; "I seldom have class with the same students." = "Seldom" ; else = NA ; ; ; ; ; ; ; ;',
   as.factor.result=TRUE)
   
   
#For these questions it was just easier to rename the levels directly 
levels(survey$Q19a) <- c(NA,"Email")
levels(survey$Q19b) <- c(NA,"SAKAI")
levels(survey$Q19c) <- c(NA,"SharePoint")
levels(survey$Q19d) <- c(NA,"Turnitin")
levels(survey$Q19e) <- c(NA,"Wiki Page")
levels(survey$Q19f) <- c(NA,"Prof Web Site")
levels(survey$Q19g) <- c(NA,"Drop Box")
levels(survey$Q19h) <- c(NA,"Google Drive")
levels(survey$Q19i) <- c(NA,"Prof H:drive")
levels(survey$Q19j) <- c(NA,"Paper copy")

levels(survey$Q20a) <- c(NA,"Email")
levels(survey$Q20b) <- c(NA,"SAKAI")
levels(survey$Q20c) <- c(NA,"SharePoint")
levels(survey$Q20d) <- c(NA,"Turnitin")
levels(survey$Q20e) <- c(NA,"Wiki Page")
levels(survey$Q20f) <- c(NA,"Prof Web Site")
levels(survey$Q20g) <- c(NA,"Drop Box")
levels(survey$Q20h) <- c(NA,"Google Drive")
levels(survey$Q20i) <- c(NA,"Prof H:drive")
levels(survey$Q20j) <- c(NA,"Paper copy")
levels(survey$Q37)
levels(survey$Q37) <- c(NA,"Advanced","Basic","Expert","Not Like","Intermediate")
levels(survey$Q38)

#Load in the raw data and recode any previous errors
new.survey <- read.csv(file.choose())

#Use the structure function to check that each variable class is correct
str(survey[,1:15]) #Check in small batches for large data sets

survey$Q10d <- new.survey$Q10d
levels(survey$Q10d) <- c(0,1)

survey$Q11b <- new.survey$Q11b
levels(survey$Q11b) <- c(0,1)

survey$Q11d <- new.survey$Q11d
levels(survey$Q11d) <- c(0,1)

survey$Q11g <- new.survey$Q11g
levels(survey$Q11g) <- c(0,1)

survey$Q22a <- new.survey$Q22a
levels(survey$Q22a) <- c(0,1)

survey$Q22b <- new.survey$Q22b
levels(survey$Q22b) <- c(0,1)

survey$Q22c <- new.survey$Q22c
levels(survey$Q22c) <- c(0,1)

levels(survey$Q23) <- c(NA,"Often","Sometimes","Rarely","Never")
levels(survey$Q38) <- c(NA,1,'10+',2:9)


#Some of the demographic data needed to be recoded
levels(survey$MilitaryServiceName) <- c(NA,"Air Force","Army","Coast Guard","Marine Corps"  
,"National Guard","Navy","USA","USAF","USAR","USARNG","USCG","USMC","USN","USNR")

levels(survey$MilitaryRankShortName) <- c(NA,"1LT","1SG","1st Lt.","2LT","2nd Lt.","Capt",    
 "CAPT","Capt.","CDR","COL","CPT","CW2","CW3","CW4","ENS","LCDR","LT","Lt. Col.","LTC",      
"LTJG","Maj","MAJ","Maj.","Major","MSgt","PO1","SFC")

#Write the final copy of the data to a file
write.csv(survey,file="FLS_analysis_file.csv")




