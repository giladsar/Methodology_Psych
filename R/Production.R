library(dplyr)
library(multcomp)
library(ggplot2)
library(BayesFactor)
library(coin)
library(psych)
library(reshape2)


fname <- file.choose() # Get the full path to the file

mydata <- read.csv(fname, header = TRUE) # read the file in that path
mydata <- mydata[-1,]
number_of_row <- nrow(mydata)
# dealing with empty values
if (any(is.na(mydata))){
  for ( d in colnames(mydata)){
    for (r in 1:nrow(mydata)){
      if(is.na(mydata[r,which(names(mydata) == d)]) ){
        mydata[r,which(names(mydata) == d)] <- mean(na.omit(as.numeric(mydata[[d]])))
        
      }
    }  
  }
}


# setting up the df - unused columns
cols_to_remove <- c("_FIRST_CLICK", "_LAST_CLICK", "_PAGE_SUBMIT", "_CLICK_COUNT","_2","_3","_5")
removed_cols_index <- c()
for (c in cols_to_remove){
  for (i in colnames(mydata)){
    if (grepl(c,i)){
      removed_cols_index <- append(removed_cols_index,which(names(mydata) == i))
    }
  }
}
mydata <- mydata[,-removed_cols_index]


mydata <- mydata[,-1:-18]


P_N <- c("QID1","QID110","QID111","QID112","QID113", "QID124", "QID125", "QID126","QID127", "QID128")
P_S <- c("QID134", "QID135", "QID136", "QID137", "QID138", "QID141", "QID144", "QID147", "QID150", "QID153")

NP_N<- c("QID114", "QID115", "QID116", "QID117","QID118", "QID119", "QID120","QID121", "QID122", "QID123")
NP_S <- c("QID129", "QID130", "QID131", "QID132", "QID133", "QID156", "QID159", "QID162", "QID165","QID168")







#-------> getting the columns by category <---------
NP_N_Col <- c()
NP_S_Col <- c()
P_N_Col <- c()
P_S_Col <- c()

for (i in colnames(mydata)){
  for (n in P_N){
    if (grepl(paste(n,"_",sep=""),i)){
      P_N_Col <- append(P_N_Col,i) 
    }
  }  
}
for (i in colnames(mydata)){
  for (n in P_S){
    if (grepl(paste(n,"_",sep=""),i)){
      P_S_Col <- append(P_S_Col,i) 
    }
  }  
}
for (i in colnames(mydata)){
  for (n in NP_N){
    if (grepl(paste(n,"_",sep=""),i)){
      NP_N_Col <- append(NP_N_Col,i) 
    }
  }  
}
for (i in colnames(mydata)){
  for (n in NP_S){
    if (grepl(paste(n,"_",sep=""),i)){
      NP_S_Col <- append(NP_S_Col,i) 
    }
  }  
}

NP_N_Col <- unique(NP_N_Col)
NP_S_Col <- unique(NP_S_Col)
P_N_Col <- unique(P_N_Col)
P_S_Col <- unique(P_S_Col)


#------->creating df for which category <---------

#------for P_N
P_Ndf <- data.frame(matrix(ncol = 30, nrow = number_of_row))
P_Ndf <- setNames(P_Ndf, P_N_Col)
for (n in P_N_Col){ 
  P_Ndf[[n]]<- mydata[[n]]
}
#------for P_S
P_Sdf <- data.frame(matrix(ncol = 30, nrow = number_of_row))
P_Sdf <- setNames(P_Sdf, P_S_Col)
for (n in P_S_Col){ 
  P_Sdf[[n]]<- mydata[[n]]
}

#------for NP_N
NP_Ndf <- data.frame(matrix(ncol = 30, nrow = number_of_row))
NP_Ndf <- setNames(NP_Ndf, NP_N_Col)
for (n in NP_N_Col){ 
  NP_Ndf[[n]]<- mydata[[n]]
}

#------for NP_S
NP_Sdf <- data.frame(matrix(ncol = 30, nrow = number_of_row))
NP_Sdf <- setNames(NP_Sdf, NP_S_Col)
for (n in NP_S_Col){ 
  NP_Sdf[[n]]<- mydata[[n]]
}


#-----------descriptive------------------------------------------------------




P_N_sums_vector <- c()
P_S_sums_vector <- c()



num_columns <- ncol(P_Sdf)

sums <- c()
for (start_col in seq(1, num_columns, 3)) {
  end_col <- min(start_col + 3 - 1, num_columns)
  sum_row <- rowSums(P_Sdf[, start_col:end_col])
  sums <- c(sums, sum_row)
  P_S_sums_vector <- c(P_S_sums_vector,mean(sums))
}

num_columns <- ncol(P_Ndf)

sums <- c()
for (start_col in seq(1, num_columns, 3)) {
  end_col <- min(start_col + 3 - 1, num_columns)
  sum_row <- rowSums(P_Ndf[, start_col:end_col])
  sums <- c(sums, sum_row)
  P_N_sums_vector <- c(P_N_sums_vector,mean(sums))
}


NP_N_sums_vector <- c()
NP_S_sums_vector <- c()

num_columns <- ncol(NP_Ndf)

sums <- c()
for (start_col in seq(1, num_columns, 3)) {
  end_col <- min(start_col + 3 - 1, num_columns)
  sum_row <- rowSums(NP_Ndf[, start_col:end_col])
  sums <- c(sums, sum_row)
  NP_N_sums_vector <- c(NP_N_sums_vector,mean(sums))
}

num_columns <- ncol(NP_Sdf)

sums <- c()
for (start_col in seq(1, num_columns, 3)) {
  end_col <- min(start_col + 3 - 1, num_columns)
  sum_row <- rowSums(NP_Sdf[, start_col:end_col])
  sums <- c(sums, sum_row)
  NP_S_sums_vector <- c(NP_S_sums_vector,mean(sums))
}







#Descriptive Statistics 
columns_names <- c("Variable","Mean", "Median", "Mode","SD", "25 Percentile", "75 Percentile", "Max", "Min")
des_stat <- data.frame(matrix(NA,ncol = 9 , nrow = 4))
des_stat <- setNames(des_stat, columns_names)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
NP_N_SUM <- c('NP_N',mean(NP_N_sums_vector),median(NP_N_sums_vector), getmode(NP_N_sums_vector),sd(NP_N_sums_vector), unname(quantile(NP_N_sums_vector))[2],unname(quantile(NP_N_sums_vector))[4],max(NP_N_sums_vector),min(NP_N_sums_vector))
NP_S_SUM<- c('NP_S',mean(NP_S_sums_vector),median(NP_S_sums_vector), getmode(NP_S_sums_vector),sd(NP_S_sums_vector), unname(quantile(NP_S_sums_vector))[2],unname(quantile(NP_S_sums_vector))[4],max(NP_S_sums_vector),min(NP_S_sums_vector))
P_N_SUM<- c('P_N',mean(P_N_sums_vector),median(P_N_sums_vector), getmode(P_N_sums_vector),sd(P_N_sums_vector), unname(quantile(P_N_sums_vector))[2],unname(quantile(P_N_sums_vector))[4],max(P_N_sums_vector),min(P_N_sums_vector))
P_S_SUM<- c('P_S',mean(P_S_sums_vector),median(P_S_sums_vector), getmode(P_S_sums_vector),sd(P_S_sums_vector), unname(quantile(P_S_sums_vector))[2],unname(quantile(P_S_sums_vector))[4],max(P_S_sums_vector),min(P_S_sums_vector))
des_stat[1,] <- NP_N_SUM
des_stat[2,] <- NP_S_SUM
des_stat[3,] <- P_N_SUM
des_stat[4,] <- P_S_SUM

## 1 = Male, 2 = Female, 3 = Non Binary,4 = Other , 5 = Rather not say

demographic <- mydata[,1:2 ] 
colnames(demographic) <- c("Gender", "Age")

#----summery 
dem_sum <- matrix(NA,1,7)
colnames(dem_sum) <- c("Mean Age","SD age","'Male' Percentage","'Female' Percentage","'NonBinary' Percentage","'Other' Percentage", "'Rather not say' Percentage")
dem_sum[,1]<- mean(as.numeric(demographic[,2]))
dem_sum[,2]<- sd(demographic[,2])
denominator <- number_of_row
dem_sum[,3] <- length(which(demographic[,1] == 1)) /denominator
dem_sum[,4] <- length(which(demographic[,1] == 2)) / denominator
dem_sum[,5] <- length(which(demographic[,1] == 3)) / denominator
dem_sum[,6] <- length(which(demographic[,1] == 4)) / denominator
dem_sum[,7] <- length(which(demographic[,1] == 5)) / denominator






#Two way anova 
P_N_subject_mean <-rowMeans(P_Ndf)
P_S_subject_mean<-rowMeans(P_Sdf)
NP_N_subject_mean <-rowMeans(NP_Ndf)
NP_S_subject_mean<-rowMeans(NP_Sdf)
participantID <- c(1:number_of_row)


Anova_within_subjects_mean_df <- data.frame(
  participantID =participantID,
  P_N <- P_N_subject_mean,
  P_S <- P_S_subject_mean,
  NP_N <- NP_N_subject_mean,
  NP_S <- NP_S_subject_mean
)

write_csv(Anova_within_subjects_mean_df, "./Anova_within_subjects_mean_df.csv")
Anova_within_subjects_mean_df <- setNames(Anova_within_subjects_mean_df, c("participantID","P_N", "P_S","NP_N", "NP_S"))

#in order to run the anova two way within subject we will use JAMOVI


#cohan's d of the effect between smiling politician and neutral politician
library(lsr)

PN_PS<- cohensD(Anova_within_subjects_mean_df$P_S, Anova_within_subjects_mean_df$P_N)
PN_NPS<- cohensD(Anova_within_subjects_mean_df$P_N, Anova_within_subjects_mean_df$NP_S)
PN_NPN<- cohensD(Anova_within_subjects_mean_df$P_N, Anova_within_subjects_mean_df$NP_N)
PS_NPS<- cohensD(Anova_within_subjects_mean_df$P_S, Anova_within_subjects_mean_df$NP_S)
PS_NPN<- cohensD(Anova_within_subjects_mean_df$P_S, Anova_within_subjects_mean_df$NP_N)
NPS_NPN<- cohensD(Anova_within_subjects_mean_df$NP_S, Anova_within_subjects_mean_df$NP_N)




