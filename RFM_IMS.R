# Purpose: Customer Loyalty analysis of customer: RFM
# Goal 1: Segment the customer based on their purchase activity;
# Goal 2: Identify which type of customer should be promoted to make profit;
# Goal 3: Calculate the customer life time value

setwd("D:/Data Science content revised/RETAIL AND MARKETING/Folder for Ishan_Retail/Ishan Reworked/Chapter 2/Case Study/19. Customer Loyalty - RFM & CLTV")

################################################################################################################################################################
# Step 1: Load data and check the data structure
################################################################################################################################################################

order_data <- read.csv("Order_Data.csv")
head(order_data)
View(order_data)
order_data[,2] <- as.Date(as.character(order_data[,2]), "%Y%m%d")
str(order_data)

UniID<- order_data[!duplicated(order_data$ID),]
nrow(UniID)

#Conclution: The data has 3 variables, ID(customer ID), Date(the date of the transaction) and Amount (the dolloar value of the transaction). 
#There are 6,919 records total and 2,357 unique customers.

################################################################################################################################################################
#Step 2: R,F,and M analysis.
#R(Recency): How recently did the customer purchase?
#F(Frequency): How often do they purchase?
#M(Monetary value): How much do they spend each time on average?
################################################################################################################################################################

startDate = min(order_data[,2])
endDate = max(order_data[,2])

df1 <- order_data[order(order_data$Date,decreasing = TRUE),]
df2 <- df1[df1$Date >= startDate & df1$Date <= endDate,]
df3 <- df2[!duplicated(df2$ID),]

Recency <- as.numeric(difftime(endDate,df3$Date,units="days"))
df3$Recency <- NULL
df3$Recency <- Recency
df3 <- df3[order(df3$ID), ]

df2$ID <- as.factor(df2$ID)
fre <- as.data.frame(table(df2$ID))
df3$Frequency <- NULL
df3$Frequency <- fre[,2]

M_df <- aggregate(df2$Amount, list(df2$ID), sum)
df3$Monetary <- NULL
df3$Monetary <- M_df$x
df3$Monetary <- df3$Monetary/df3$Frequency

head(df3)

# Conclusion: R is calculated by the different days of purchase date and enddate (2017-06-30). 
# F is caulated by the frequency each customer individual purchase during that period. 
# M is calculated by total money spent during that period divided by frequency.

################################################################################################################################################################
#	Scoring the Recency, Frequency, and Monetary in r, f, and m in aliquots independently
#	Returns a new data frame with four new columns of "R_Score","F_Score","M_Score", and "Total_Score".
################################################################################################################################################################

################################################################################

# Function
# 	scoring(df,column,r=5)
#
# Description
#	A function to be invoked by the getIndepandentScore function
#######################################

scoring <- function (df,column,r=5){
  
  #get the length of rows of df
  len <- dim(df)[1]
  
  score <- rep(0,times=len)
  
  # get the quantity of rows per 1/r e.g. 1/5
  nr <- round(len / r)
  if (nr > 0){
    
    # seperate the rows by r aliquots
    rStart <-0
    rEnd <- 0
    for (i in 1:r){
      
      #set the start row number and end row number
      rStart = rEnd+1
      
      #skip one "i" if the rStart is already in the i+1 or i+2 or ...scope.
      if (rStart> i*nr) next
      
      if (i == r){
        if(rStart<=len ) rEnd <- len else next
      }else{
        rEnd <- i*nr
      }
      
      # set the Recency score
      score[rStart:rEnd]<- r-i+1
      
      # make sure the customer who have the same recency have the same score
      s <- rEnd+1
      if(i<r & s <= len){
        for(u in s: len){
          if(df[rEnd,column]==df[u,column]){
            score[u]<- r-i+1
            rEnd <- u
          }else{
            break;
          }
        }
        
      }
      
    }
    
  }
  return(score)
  
} #end of function Scoring



#order and the score
df3 <- df3[order(df3$Recency,-df3$Frequency,-df3$Monetary),]
R_Score <- scoring(df3,"Recency",5)
df3 <- cbind(df3, R_Score)

df3 <- df3[order(-df3$Frequency,df3$Recency,-df3$Monetary),]
F_Score <- scoring(df3,"Frequency",5)
df3 <- cbind(df3, F_Score)

df3 <- df3[order(-df3$Monetary,df3$Recency,-df3$Frequency),]
M_Score <- scoring(df3,"Monetary",5)
df3 <- cbind(df3, M_Score)

#order the dataframe by R_Score, F_Score, and M_Score desc
df3 <- df3[order(-df3$R_Score,-df3$F_Score,-df3$M_Score),]

# caculate the total score
Total_Score <- c(100*df3$R_Score + 10*df3$F_Score+df3$M_Score)

df3 <- cbind(df3,Total_Score)

# Get the RFM scores
summary(df3)
head(df3)
View(df3)
# Let's further find out how many customers have a total score larger than 500 

S500<-df3[df3$Total_Score>500,]

dim(S500)

# We can consider these 471 customers are more loyal and important to get a higher response rate.

################################################################################################################################################################
#	Calculating Customer Lifetime Value (CLTV)
#	CLV = Average Order Value (AOV) * Purchase Frequency (F) * Average Customer Lifetime 
# Let's say in this case Average Customer Lifetime is 3 years
################################################################################################################################################################

df3$CLTV = df3$Amount*df3$Frequency*3
head(df3)

# Let's further find out how many customers have a CLTV larger than median CLTV & RFM Score > 500

dim(df3[df3$CLTV > median(df3$CLTV) & df3$Total_Score>500,])

# We can consider these 426 customers are more loyal and important to get a higher response rate.












