############ ACCESSING THE DATA #####################

# T1: 12686
# T2: 22638

library('readxl')
library(dplyr)
delay_12686 <- read_excel("MAQ_MAS_Delay.xlsx", sheet = "12686")
delay_22638 <- read_excel("MAQ_MAS_Delay.xlsx", sheet = "22638")
states <- c('1','2','3','4','5','6')

#TRAIN NO. 12686
#Total vals: 7049
# 1: Early(<=0) : 964 values
# 2: Small Delay(>0, <=5): 1946 values
# 3: Medium Delay (>5, <=15): 2030 values
# 4: Delay (>15, <=25): 961 values
# 5: Delay(>25, <=40) : 694 values
# 6: Large Delay ( >40): 454 values

#TRAIN NO. 22638
#Total vals: 7049
# 1: Early(<=0) : 964 values
# 2: Small Delay(>0, <=5): 1946 values
# 3: Medium Delay (>5, <=15): 2030 values
# 4: Delay (>15, <=25): 961 values
# 5: Delay(>25, <=40) : 694 values
# 6: Large Delay ( >40): 454 values

############ CREATE THE STATE MATRIX #####################
  

create_state_matrix <- function(data, trno){
  colnames(data) = c("Dates", colnames(data[-1]))
  df <- data[,-1]
  rownames(df) <- data$Dates
  # View(df)
  # head(df)
  state_matrix <- data.frame(matrix(ncol=0, nrow=0))
  for(j in 1:ncol(df)){
    for(i in 1:nrow(df)){
      if(df[i,j]<=0){
        state_matrix[i,j] = 1
      }else if(df[i,j] > 0 & df[i,j] <=5){
        #
        state_matrix[i,j] = 2
      }else if(df[i,j] >5 & df[i,j] <= 15){
        #
        state_matrix[i,j] = 3
      }else if(df[i,j] > 15 & df[i,j] <= 25){
        #
        state_matrix[i,j] = 4
      }else if(df[i,j] > 25 & df[i,j] <= 40){
        #
        state_matrix[i,j] = 5
      }else{
        #
        state_matrix[i,j] = 6
      }
    }
  }
  rownames(state_matrix) <- rownames(df)
  colnames(state_matrix) <- colnames(df)
  library(writexl)
  write_xlsx(state_matrix, paste0("data/",trno,"/State Matrix.xlsx"), col_names=TRUE)
  return(state_matrix)
}

############ CREATE THE TPM FOR EACH STATION #####################

tpm_func <- function(x, states, stno, trno){
  t = factor(x=x)
  tpm = createSequenceMatrix(x, toRowProbs=T, possibleStates = states, sanitize=F)
  #print(rowSums(tpm))
  rs <- rowSums(tpm)
  for(i in 1:length(rs)){
    if(rs[i] == 0){
      tpm[i,i] = 1
    }
  }
  write_xlsx(as.data.frame(tpm), paste0("data/",trno,"/",stno," Transition Probability Matrix.xlsx"), col_names=TRUE)
  return(tpm)
}

############ CREATE THE MARKOV CHAIN FOR EACH STATION #####################

mc_func <- function(x, states, stno, trno){
  library(markovchain)
  tpm = tpm_func(x, states, stno, trno) #Gets the TPM for the given col.
  mc <- new('markovchain', states= states, transitionMatrix = tpm)
  return(steadyStates(mc)[nrow(steadyStates(mc)),])
}

############ CREATE THE STATIONARY DISTRIBUTION FOR EACH STATION #####################

to_stat_dist <- function(data, states, trno){
  state_matrix <- create_state_matrix(data, trno)
  #print(head(state_matrix))
  stationary_dist <- data.frame(matrix(ncol=0, nrow=0, byrow=T))
  for(i in colnames(state_matrix)){
    stationary_dist <- rbind(stationary_dist, mc_func(state_matrix %>%pull(i), states, i, trno))
  }
  rownames(stationary_dist) <- colnames(state_matrix)
  stationary_dist <- cbind(colnames(state_matrix), stationary_dist)
  colnames(stationary_dist) <- c("Stations", states)
  View(stationary_dist)
  library(writexl)
  write_xlsx(stationary_dist, paste0("data/",trno,"/Stationary Distributions.xlsx"), col_names=TRUE)
}


############ GENERATING THE STATIONARY DISTRIBUTIONS #####################

to_stat_dist(delay_12686, states,"12686")
to_stat_dist(delay_22638, states,"22638")



# par(mfrow=c(1,1))
# hist(delay_12686$MAQ)
# library(Hmisc)
# par("mar")
# par(mar=c(1,1,1,1))
# par(mar=c(5.1, 4.1, 4.1, 2.1))
# View(delay_12686[,-1])
# hist.data.frame(delay_12686[,-1])
# par(mfrow=c(4,5))
# pdf(file="C:\\Users\\mahes\\OneDrive\\Documents\\Docs\\Projects\\DDA Sem-1\\test.pdf")
# for(col in 2:ncol(delay_12686)){
#   hist(delay_12686[,col], breaks=10, xlim=c(-5,203))
# }
# dev.off()
# library(dplyr)
# summary(delay_12686)
