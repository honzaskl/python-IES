library(readxl)
install.packages("dplyr")
library(dplyr)
install.packages("openxlsx")
library(openxlsx)
install.packages("caret")
library(caret)
install.packages("recipes")
library(recipes)
install.packages("cli")
install.packages("car")
library(car)
install.packages("stargazer")
library( stargazer)

write.xlsx(Data, file = "YourFileName17.xlsx")


Data <- read_excel("data_merged/Book1.xlsx")

set.seed(123) 


splitIndex <- createDataPartition(Data$Outcome, p = .7, list = FALSE, times = 1)
trainData <- Data[splitIndex,]
testData <- Data[-splitIndex,]
                      
trainSize <- floor(0.7 * nrow(Data))

trainIndices <- sample(seq_len(nrow(Data)), size = trainSize)


trainData <- Data[trainIndices, ]
testData <- Data[-trainIndices, ]





model_after <- glm(Outcome ~ Pl_hight + Pl_seed   + 
               Op_hight + Op_seed   + 
                 Pl_rank_points + Op_rank_points
                   + Pl_1stPro + Op_1stPro + Pl_bpsavedPro + Op_bpsavedPro + Pl_1stwonPro + Op_1stwonPro +  Pl_last10_form + Op_last10_form  ,
             family = binomial(link = "logit"),
             data = Data)

model_after_avg <- glm(Outcome ~ Pl_hight + Pl_seed   + 
                         Op_hight + Op_seed   + 
                         Pl_rank_points + Op_rank_points
                       +  Pl_last10_form + Op_last10_form  + Avg_Pl_1stPro_last5  + Avg_Op_1stPro_last5 + Avg_Pl_1stPro_won5 + Avg_Op_1stPro_won5 ,
                       family = binomial(link = "logit"),
                       data = Data)
summary(model_after) 

summary(model_after_avg) 


stargazer(model_after,model_after_avg,header=FALSE,single.row = T, type='text', column.labels=c("Probit","Logit"))


predicted_outcome_after <- predict(model_after, newdata = Data, type = "response") > 0.5
predicted_outcome_after_avg <- predict(model_after_avg, newdata = Data, type = "response") > 0.5

actual_outcome <- Data$Outcome


conf_matrix_after <- table(Predicted = predicted_outcome_after, Actual = actual_outcome)
conf_matrix_after_avg <- table(Predicted = predicted_outcome_after_avg, Actual = actual_outcome)
print(conf_matrix)

accuracy_after <- sum(diag(conf_matrix_after)) / sum(conf_matrix)
accuracy_after_avg <- sum(diag(conf_matrix_after_avg)) / sum(conf_matrix)


print(paste("Accuracy:", accuracy_after))
print(paste("Accuracy:", accuracy_after_avg))







#model1_logit <- glm(Outcome ~ Pl_hight + Pl_seed   + 
#                      Op_high + Op_seed   + 
#                      Pl_rank_points + Op_rank_points 
 #                   +Pl_Clay_spec + Pl_Hard_spec  +Pl_Grass_spec + Pl_clay_spec_played_clay + Pl_hard_spec_played_hard + Pl_grass_spec_played_grass
  #                  +Op_clay_spec_played_clay + Op_hard_spec_played_hard + Op_grass_spec_played_grass + Op_Clay_spec  + Op_Grass_spec + Op_Hard_spec + Pl_last10_form + Op_last10_form 
   #                 + as.factor(Surface),
    #                family = binomial(link = "logit"),
     #               data = Data)



model1_logit <- glm(Outcome ~ Pl_hight + Pl_seed   + 
               Op_hight + Op_seed   + 
               Pl_rank_points + Op_rank_points +
                Pl_last10_form + Op_last10_form 
               ,
             family = binomial(link = "logit"),
             data = Data)

model2_logit <- glm(Outcome ~ Pl_hight + Pl_seed   + 
                      Op_hight + Op_seed   + 
                      Pl_rank_points + Op_rank_points 
                     + Pl_clay_spec_played_clay + Pl_hard_spec_played_hard + Pl_grass_spec_played_grass
                    +Op_clay_spec_played_clay + Op_hard_spec_played_hard + Op_grass_spec_played_grass + Pl_last10_form + Op_last10_form
                      
                    ,
                    family = binomial(link = "logit"),
                    data = Data)

model2_probit <- glm(Outcome ~ Pl_hight + Pl_seed   + 
                      Op_hight + Op_seed   + 
                      Pl_rank_points + Op_rank_points 
                    +Pl_Clay_spec + Pl_Hard_spec  +Pl_Grass_spec + Pl_played_on_spec +Op_Clay_spec  + Op_Grass_spec + Op_Hard_spec+ Op_played_on_spec + Pl_last10_form + Op_last10_form 
                    + as.factor(Surface) ,
                    family = binomial(link = "probit"),
                    data = Data)

summary(model2_logit)
summary(model1_logit)
summary(model2_probit)
vif_values <- vif(model2_logit)
print(vif_values)
#######################################################################################################
predicted_log_odds <- predict(model2_logit, type = "link")

plot(Data$Pl_hight, predicted_log_odds, xlab = "Pl_hight", ylab = "Predicted Probability",
     main = "Pl_hight vs. Predicted Probability", pch = 20, cex = 0.6, col = "black")
lines(lowess(Data$Pl_hight, predicted_log_odds, f = 0.7), col = "blue", lwd = 2)

plot(Data$Op_hight, predicted_log_odds, xlab = "Pl_hight", ylab = "Predicted Probability",
     main = "Op_hight vs. Predicted Probability", pch = 20, cex = 0.6, col = "black")
lines(lowess(Data$Pl_hight, predicted_log_odds, f = 0.7), col = "blue", lwd = 2)

plot(Data$Pl_rank_points, predicted_log_odds, xlab = "Pl_hight", ylab = "Predicted Probability",
     main = "Pl_rank_points vs. Predicted Probability", pch = 20, cex = 0.6, col = "black")
lines(lowess(Data$Pl_hight, predicted_log_odds, f = 0.7), col = "blue", lwd = 2)






install.packages("pROC")
library(pROC)
roc_response <- roc(actual_outcome, predicted_prob)
auc(roc_response)
plot(roc_response)
predicted_prob <- predict(model2_logit, newdata = Data, type = "response")
###################################################################################
McFadden <- 1 - (as.numeric(logLik(model2_logit))/as.numeric(logLik(nullModel)))
nullModel <- glm(Outcome ~ 1, family = binomial(link = "logit"), data = Data)
McFadden
##########################################################################################

predicted_outcome1_logit <- predict(model1_logit, newdata = Data, type = "response") > 0.5
predicted_outcome2_logit <- predict(model2_logit, newdata = Data, type = "response") > 0.5

predicted_outcome2_probit <- predict(model2_probit, newdata = Data, type = "response") > 0.5
actual_outcome <- Data$Outcome

conf_matrix1_logit <- table(Predicted = predicted_outcome1_logit, Actual = actual_outcome)
conf_matrix2_logit <- table(Predicted = predicted_outcome2_logit, Actual = actual_outcome)
conf_matrix2_probit <- table(Predicted = predicted_outcome2_probit, Actual = actual_outcome)



accuracy1_logit <- sum(diag(conf_matrix1_logit)) / sum(conf_matrix2)
accuracy2_logit <- sum(diag(conf_matrix2_logit)) / sum(conf_matrix2)
accuracy2_probit <- sum(diag(conf_matrix2_probit)) / sum(conf_matrix2_probit)

print(paste("Accuracy:", accuracy1_logit))
print(paste("Accuracy:", accuracy2_logit))
print(paste("Accuracy:", accuracy2_probit))

##################################################################################################

Data$predicted_prob_Pl_wins = predict(model2_logit, newdata = Data, type = "response")

Data$predicted_prob_Op_wins = 1 - Data$predicted_prob_Pl_wins

Data$Pre_match_prob_Pl = (1 / Data$AvgPl) 

Data$Pre_match_prob_Op = (1 / Data$AvgOp) 


Data$model_favourite <- ifelse(Data$predicted_prob_Pl_wins > Data$predicted_prob_Op_wins, "Pl", "Op")

Data$value_bet <- ifelse(
  (Data$predicted_prob_Pl_wins > Data$predicted_prob_Op_wins & Data$predicted_prob_Pl_wins > Data$Pre_match_prob_Pl) |
    (Data$predicted_prob_Op_wins > Data$predicted_prob_Pl_wins & Data$predicted_prob_Op_wins > Data$Pre_match_prob_Op),
  TRUE, 
  FALSE
)

Data$bet_on <- ifelse(Data$value_bet, Data$model_favourite, NA)

Data$potential_return <- ifelse(Data$value_bet, fixedBetAmount * ifelse(Data$bet_on == "Pl", Data$AvgPl, Data$AvgOp), 0)
Data$actual_return <- ifelse(Data$value_bet, ifelse((Data$Outcome == 1 & Data$bet_on == "Pl") | (Data$Outcome == 0 & Data$bet_on == "Op"), Data$potential_return, -fixedBetAmount), 0)


fixedBetAmount <- 100 


Data$potential_return <- ifelse(Data$value_bet, fixedBetAmount * Data$AvgPl, 0)


Data$actual_return <- ifelse(Data$value_bet, ifelse(Data$Outcome == 1, Data$potential_return, -fixedBetAmount), 0)

total_actual_return <- sum(Data$actual_return, na.rm = TRUE)
print(paste("Total Actual Return:", total_actual_return))
total_value_bets <- sum(Data$value_bet, na.rm = TRUE)
total_value_bets










