MyData <- read.csv(file="Consumer_Compalints.csv", header=TRUE, sep=",")

s <- sort(MyData$Company)

compNamesList <- unique(s)

##INCREMENT FUNCTION
inc <- function(x)
{
  eval.parent(substitute(x <- x + 1))
}




##TIMELY RESPONSE
attatch(MyData)
newdata <- MyData[order(Company, State),]
fastYES <- rep(0, 4000)
fastNO <- rep(0, 4000)
index =1
for(c in newdata$Company) {
  if ( newdata$Timely.response.[index] == "Yes") {
    inc(fastYES[match(c, compNamesList)])
  }else inc(fastNO[match(c, compNamesList)])
  inc(index)
}


 responding <- data.frame(unique(newdata$Company))
 responding["Responding_ratio"] <- NA
 responding["FastYes"] <- NA
 responding$FastYes <- fastYES
fastYES <- fastYES[1:3668]
 responding$FastYes <- fastYES
 responding["FastNO"] <- NA
 fastNO <- fastNO[1:3668]
 responding$FastNO <- fastNO
 responding$Responding_ratio <- responding$FastYes / (responding$FastYes + responding$FastNO)
 responding$Responding_ratio <- responding$Responding_ratio * 100
 colnames(responding)[2] <- "Responding percentage"
 
 
 responding["Total_Issues"] <- NA
 responding$Total_Issues <- responding$FastYes + responding$FastNO
 
 ## TIMELY RESPONSE PERCENTAGE PLOT
 per <- rep(0, 4)
 colnames(responding)[2] <- "Responding_percentage"
 
 index <- 1
 for (c in responding$unique.newdata.Company.) {
   if(round(responding$Responding_percentage[index], digits = 0) <= 25)
     
     inc(per[1])
   if(round(responding$Responding_percentage[index], digits = 0) > 25 && round(responding$Responding_percentage[index], digits = 0) <= 50)
     
     inc(per[2])
   if(round(responding$Responding_percentage[index], digits = 0) > 50 && round(responding$Responding_percentage[index], digits = 0) <= 75)
     
     inc(per[3])
   if(round(responding$Responding_percentage[index], digits = 0) > 75 && round(responding$Responding_percentage[index], digits = 0) <= 100)
     
     inc(per[4])
   inc(index)
 }
 
 barplot(per,main="Timely Response",xlab = "Response Percentage", ylab="No. Of Companies", horiz=FALSE, names.arg = c("0-25%", "26-50%", "51-75%","76-100%"))
 
 
 
 
 
 ##COMPANIES WITH THIER ISSUES
 lessIssues <- data.frame(responding$unique.newdata.Company., responding$Total_Issues)
 attach(lessIssues)
  lessIssues <- lessIssues[order(responding.Total_Issues),]
  colnames(lessIssues)[1] <- "Company"
  colnames(lessIssues)[2] <- "Issues"
  s <- lessIssues$Issues
  IssuesRangeWise <- data.frame(Range = character(), No_of_Companies=integer(), stringsAsFactors=FALSE)
   a = c("1-10", "11-100", "101-1000", "1001-10000", "above 10000")
   IssuesRangeWise[nrow(IssuesRangeWise)+1,] <- c(a[1],0)
   IssuesRangeWise[nrow(IssuesRangeWise)+1,] <- c(a[2],0)
   IssuesRangeWise[nrow(IssuesRangeWise)+1,] <- c(a[3],0)
   IssuesRangeWise[nrow(IssuesRangeWise)+1,] <- c(a[4],0)
   IssuesRangeWise[nrow(IssuesRangeWise)+1,] <- c(a[5],0)
   
r <- rep(0, 5)
for (i in s) {
 if(0 < i && i < 10) 
     inc(r[1])
 if(11 < i && i < 100) 
     inc(r[2])
 if(101 < i && i < 1000) 
     inc(r[3])
 if(1001 < i && i < 10000) 
     inc(r[4])
 if(i > 10000) 
     inc(r[5])
 }
   
 IssuesRangeWise$No_of_Companies <- r
 a <- IssuesRangeWise$No_of_Companies
 barplot(a,main="ISSUES",xlab = "Issues Range", ylab="No. Of Companies", horiz=FALSE, names.arg = c("1-10", "11-100", "101-1000","10001-10000", "above 10000"))

 
 
 ##Product
 a <- summary(newdata$Product)
barplot(a,main="PRODUCT",xlab = "Product", ylab="No. of Complaints", horiz=FALSE,names.arg = c("BAS", "CL", "CC","CR", "DC", "MT", "MG","OFS", "PL","PC","SL"))






##YEAR WISE COMPLAINTS
yr <- newdata$Date.received
yr <- substring(yr,7,10)
yr <- unique(yr)
yr <- sort(yr)
y <- rep(0, 6)
for (i in newdata$Date.received) {
  i <- substring(i, 7, 10) 
  inc(y[match(i, yr)]) 
}

plot(yr,y, main="NO. OF COMPLAINTS PER YEAR",xlab = "Years", ylab="No. Of Complaints")
lines.default(yr,y,col="red")






## companies response to consumer and RATING
 crc <- unique(Consumer_Complaints$Company.response.to.consumer)
 crc_PN <- rep(0, 8)
 crc_PN[1] <- 1
 crc_PN[3] <- 1
 crc_PN[8] <- 1
i = 1
 for (c in newdata$Company.response.to.consumer) {
      newdata$CompanyResponseToConsumerValue[i] <- crc_PN[match(c, crc)]
 inc(i)
 }

crcv <- rep(0, 3668)
 index =1
 for(c in newdata$Company) {
     if ( newdata$CompanyResponseToConsumerValue[index] == "1") {
         inc(crcv[match(c, compNamesList)])
        }
      inc(index)
   }
 rating <- data.frame(responding$unique.newdata.Company., responding$Total_Issues, responding$Responding_percentage, crcv)
colnames(rating)[4] <- "CompanyResponseToConsumer"



 consumerDisputed <- rep(0, 3668)
 index =1
 for(c in newdata$Company) {
       if ( newdata$Consumer.disputed.[index] == "No") {
         inc(consumerDisputed [match(c, compNamesList)])
       }
       inc(index)
   }
rating["ConsumerNotDisputed"] <- NA

 rating$ConsumerNotDisputed <- consumerDisputed
rating <- rating[ order(rating[,2], -rating[,3], -rating[4] , -rating[5]),]

 un <- unique(rating[1:3668, 2:5])
 
 noCmp <- rep(0, 1877)
 j <- 1
 for(c in rating$Company) { for(index in 1:length(noCmp)) {
     if ( rating$responding.Total_Issues[j] == un$responding.Total_Issues[index] && rating$responding.Responding_percentage[j] == un$responding.Responding_percentage[index] && rating$CompanyResponseToConsumer[j]  == un$CompanyResponseToConsumer[index] && rating$ConsumerNotDisputed[j] == un$ConsumerNotDisputed[index]) {
      inc(noCmp[index])
 break
 }
 }
 inc(j)
 }

  un["No_Of_Companies"] <- noCmp
 
 
  mm1 <- rep(0, 20)
  j = 0
  for(i in 1:5) {
      mm1[inc(j)] <- ratingPlot$responding.Total_Issues[i]
      mm1[inc(j)] <- ratingPlot$responding.Responding_percentage[i]
      mm1[inc(j)] <- ratingPlot$CompanyResponseToConsumer[i]
      mm1[inc(j)] <- ratingPlot$ConsumerNotDisputed[i]
  }
 
  barplot(matrix(mm1,nr=4), beside=T,  col=c("aquamarine3","coral", "red", "green"),  names.arg= ratingPlot$No_Of_Companies, xlab = "No. Of Companies", main = "Companies with their features")
 
  legend("topright",inset=c(-0.4,0), border = "black", c("Issues","Responding %", "Positive Closure Of Issue", "No Dispute after Closing"), pch=15,  col=c("aquamarine3","coral", "red", "green"), bty="n")
 
 

  
  
  
   
 ## state wise
  states <- unique(newdata$State)
  statesNo <- rep(0, 63)
  index =1
  for(c in newdata$State) {
          inc(statesNo[match(c, states)])
      inc(index)
  }
  malDF["count"] <- NA
  malDF["count"] <- statesNo
 
 install.packages(googleVis)
 library(googleVis)
 
  G1 <-gvisGeoMap(malDF,locationvar='country',numvar='count',options=list(height = 600, dataMode='regions'))
  
  plot(G1)
  
  
  
  
  
  
  ##developed Countires
  
 iso_3166_2_countries <- read.csv("C:/Users/Praveenreddy/Desktop/iso_3166_2_countries.csv")

  countriesCode <- data.frame(iso_3166_2_countries$Common.Name, iso_3166_2_countries$ISO.3166.1.2.Letter.Code)

  colnames(countriesCode)[1] <- "CountryNames"
  colnames(countriesCode)[2] <- "CountryCodes"
  developed <- c("Andorra", "Australia", "Austria", "Belgium", "Bermuda", "Canada", "Denmark", "Faroe Islands",        "Finland", "France", "Germany", "Greece","Holy See",        "Iceland", "Ireland", "Israel", "Italy", "Japan", "Liechtenstein", "Luxembourg", "Malta", "Monaco", "Netherlands", "NZ", "Norway", "Portugal", "San Marino", "South Africa", "Spain", "Sweden", "Switzerland", "Turkey")
  developedCode = character(0)
   index <- 1
  for (i in developed) {
       developedCode[index] <-  toString(countriesCode$CountryCodes [match(i, countriesCode$CountryNames)])
   inc(index)
  }  
   
   developing <- c("Afghanistan", "Guatemala", "Panama", "Albania", "Guinea Papua", "New Guinea",  "Algeria", "Guinea-Bissau", "Paraguay", "American", "Samoa", "Guyana", "Peru", "Angola", "Haiti", "Philippines", "Argentina", "Honduras", "Romania ","Armenia", "India", "Russian Federation", "Azerbaijan Indonesia", "Rwanda", "Bangladesh", "Iran", "Islamic Rep. of Samoa", "Belarus", "Iraq", "Belize", "Jamaica", "Senegal", "Benin", "Jordan", "Serbia", "Bhutan", "Kazakhstan", "Seychelles", "Bolivia (Plurinational State of)", "Kenya", "Sierra", "Leone", "Bosnia and Herzegovina", "Kiribati", "Solomon Islands", "Botswana", "Korea", "Democ. P. Rep. of Somalia", "Brazil", "Kosovo", "South Africa", "Bulgaria", "Kyrgyz", "Republic South Sudan", "Burkina", "Faso", "Lao", "People's Democ. Rep.", "Sri Lanka", "Burundi", "Lebanon", "St. Lucia", "Cambodia", "Lesotho", "St. Vincent and the Grenadines Cameroon Liberia Sudan Cape Verde Libya Suriname", "Central African Republic Macedonia", "the F.Y.R. of Swaziland", "Chad", "Madagascar", "Syrian", "Arab", "Republic China", "Malawi", "Tajikistan", "Colombia", "Malaysia", "Tanzania", "United Republic of Comoros", "Maldives", "Thailand", "Congo", "Democ. Republic of the Mali Timor-Leste", "Congo", "Rep. Marshall Islands", "Togo", "Costa Rica", "Mauritania", "Tonga", "Mauritius", "Tunisia", "Cuba", "Mexico", "Djibouti ", "Micronesia", "Fed. States of Turkmenistan", "Dominica", "Moldova", "Tuvalu", "Dominican Republic", "Mongolia", "Uganda", "Ecuador", "Montenegro", "Ukraine", "Egypt", "Arab", "Rep. Morocco", "Uzbekistan", "El Salvador", "Mozambique", "Vanuatu", "Eritrea", "Myanmar", "Venezuela")
   
 developingCode = character(0)
 index <- 1
  for (i in developing) {
      developingCode[index] <-  toString(countriesCode$CountryCodes [match(i, countriesCode$CountryNames)])
      inc(index)
  } 
 
 underDeveloped <- c("Madagascar", "Malawi", "Mali", "Mauritania",     "Mozambique", "Myanmar", "Nepal", "Niger", "Rwanda", "Sao Tome And Principe", "Senegal", "Sierra Leone", "Solomon Islands", "Somalia", "South Sudan", "Sudan", "Timor-Leste", "Togo", "Tuvalu", "Uganda", "United Rep. Of Tanzania", "Vanuatu", "Yemen", "Zambia", "Afghanistan", "Angola", "Bangladesh", "Benin", "Bhutan", "Burkina Faso", "Burundi", "Cambodia", "Central African Republic", "Chad", "Como ros", "Dj ibouti", "Equatorial Guinea", "Guinea", "Guinea-Bissau", "Haiti", "Kiribati", "Lao People's Dem. Republic", "Lesotho", "Liberia")
 
  underdevelopedCode = character(0)
  index <- 1
 for (i in underDeveloped) {
        underdevelopedCode[index] <-  toString(countriesCode$CountryCodes [match(i, countriesCode$CountryNames)])
        inc(index)
 }
  
   malDF["Status"] <- NA
   index <- 1
   for(i in malDF$country) {
          if(i %in% developedCode)
             malDF$Status[index] <- "DEVELOPED COUNTRY"
          else if (i %in% developingCode)
              malDF$Status[index] <- "DEVELOPING COUNTRY"
          else 
              malDF$Status[index] <- "UNDERDEVELOPED COUNTRY"
      inc(index)}
   
  install.packages("cluster")
  library(cluster)
  dummy <- malDF
   index <- 1
   for(i in dummy$Status) {
      if(i == "DEVELOPED COUNTRY")
        dummy$Status[index] <- "3"
      if(i == "DEVELOPING COUNTRY")
        dummy$Status[index] <- "2"     
      if(i == "UNDERDEVELOPED COUNTRY")
        dummy$Status[index] <- "1"
      inc(index)
   }
   
   dummy2 <- dummy
    
    ####developed, developing
    
 index <- 1
 develSum <-rep(0, 3)
 for(i in malDF$Status) {
   if(i == "DEVELOPED COUNTRY")
        develSum[3] <- sum(develSum[3], malDF$count[index])
   if(i == "DEVELOPING COUNTRY")
       develSum[2] <- sum(develSum[2], malDF$count[index])
  if(i == "UNDERDEVELOPED COUNTRY")
       develSum[1] <- sum(develSum[1], malDF$count[index])
   inc(index)
 }
 
 StateFastYes <- rep(0, 63)
  StateFastNO <- rep(0, 63)
  index =1
  for(c in newdata$State) {
      if ( newdata$Timely.response.[index] == "Yes") {
          inc(StateFastYes[match(c, states)])
      }else inc(StateFastNO[match(c, states)])
      inc(index)
  }
  
  
  StateResponding <- data.frame(unique(newdata$State))
  StateResponding["Responding_ratio"] <- NA
  StateResponding["StateFastYes"] <- NA
  StateResponding$StateFastYes <- StateFastNO
  
  StateResponding$StateFastYes <- StateFastYes
  StateResponding["StateFastNO"] <- NA
   StateResponding$StateFastNO <- StateFastNO
  StateResponding$Responding_ratio <- StateResponding$StateFastYes / (StateResponding$StateFastYes + StateResponding$StateFastNO)
  StateResponding$Responding_ratio <- StateResponding$Responding_ratio * 100
  colnames(StateResponding)[2] <- "Responding percentage"
  
  StateResponding["Status"] <- malDF$Status
  colnames(StateResponding)[2] <- "Responding_percentage"
  index <- 1
  develTim <-rep(0, 3)
   for(i in StateResponding$Status) {
       if(i == "DEVELOPED COUNTRY")
           develTim[3] <- sum(develTim[3], StateResponding$StateFastYes [index])
       if(i == "DEVELOPING COUNTRY")
           develTim[2] <- sum(develTim[2], StateResponding$StateFastYes[index])
       if(i == "UNDERDEVELOPED COUNTRY")
           develTim[1] <- sum(develTim[1], StateResponding$StateFastYes[index])
       inc(index)
   }
  
  
   deve_mm1 <- rep(0, 6)
   j = 0
   for(i in 1:3) {
       deve_mm1[inc(j)] <- develSum[i]
       deve_mm1[inc(j)] <- develTim[i]
   }
   xx <- barplot(matrix(deve_mm1 ,nr=2), beside=T,  col=c("red", "green"),  names.arg= c("UNDERDEVELOPED", "DEVELOPING", "DEVELOPED") , xlab = "Country Types", main = "(Type of Companies) Vs (No of complaints and Timely response)",ylim=c(0,450000))
   text(x = xx, y = deve_mm1, label = deve_mm1, pos = 3, cex = 0.8, col = "red")
   
   
   
 
   
     
   ###clustering web, post
   
 
   indices <- which(newdata$Submitted.via == "Web")
   web <- rep(0, 63)
   for(i in indices) {
     inc(web[match(newdata$State[i], states)])
   }
   
   dummy3 <- data.frame(dummy2$country)
   colnames(dummy3)[1] <- "Country"
   dummy3["web"] <- NA
   dummy3$Country <- states
   dummy3$web <- web
   
   indices <- which(newdata$Submitted.via == "Postal mail")
   post <- rep(0, 63)
   for(i in indices) {
     inc(post[match(newdata$State[i], states)])
   }
   
   dummy3["postalMail"] <- post
   
   
     
   indices <- which(newdata$Submitted.via == "Email")
   email <- rep(0, 63)
   for(i in indices) {
        inc(email[match(newdata$State[i], states)])
    }
   dummy3["eMail"] <- email
   
   
   indices <- which(newdata$Submitted.via == "Fax")
   fax <- rep(0, 63)
   for(i in indices) {
     inc(fax[match(newdata$State[i], states)])
   }
   dummy3["Fax"] <- fax
   
   
   
   indices <- which(newdata$Submitted.via == "Referral")
   Referral <- rep(0, 63)
   for(i in indices) {
     inc(Referral[match(newdata$State[i], states)])
   }
   dummy3["Referral"] <- Referral
   
   
   indices <- which(newdata$Submitted.via == "Phone")
   Phone <- rep(0, 63)
   for(i in indices) {
     inc(Phone[match(newdata$State[i], states)])
   }
   dummy3["Phone"] <- Phone
   
   
   
   set.seed(123456789)
   grpProtein <- kmeans(dummy3[,-1], centers=3, nstart=10)
   o=order(grpProtein$cluster)
   data.frame(dummy3$Country[o],grpProtein$cluster[o])
   
   library(cluster)
   clusplot(dummy3[,-1], grpProtein$cluster, stand = TRUE, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=4, lines=0, plotchar = TRUE, col.p = "black")
   
   
   
    with(dummy3, pairs(dummy3[,-1], col=c(1:3)[grpProtein$cluster])) 
    
   
   