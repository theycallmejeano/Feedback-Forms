# TODO Comparison of multiple events with same form

library(tidyverse)
library(dplyr)
library(xlsx)
library(openxlsx)

# Read in Cognito Forms download
df = read.csv("C:/Users/Intern/Downloads/2019 Investor Forum Feedback_test missing.csv")
View(df)

# Colguide development
col_guide = as.data.frame(colnames(df))
colnames(col_guide) <- "colDescLong"
col_guide <- col_guide %>% 
   mutate(analysisMarker = ifelse(grepl("PleaseIndicateWhetherYouAgreeOrDisagree",colDescLong),"agree",""),
          analysisMarker = ifelse(grepl("PleaseIndicateYourDegreeOfKnowledge",colDescLong),"learning",analysisMarker),
          analysisMarker = ifelse(grepl("Rating",colDescLong),paste(analysisMarker,"_rating", sep = ""),analysisMarker),
          colDesc = ifelse(analysisMarker == "", "", sub('.*?_', '', colDescLong)))

View(col_guide)

# Analyze completed responses only
dfCopy = df
df = df %>% filter(Entry_Status != "Incomplete")

# ANALYZE AGREE/DISAGREE STATEMENTS

# Initialize Agree/Disagree options (note these are case-sensitive)
#levAgree <- c("Strongly disagree", "Disagree", "Not sure", "Agree", "Strongly agree")
levAgree <- c("Disagree", "Not sure", "Agree")

agreeCols <- df %>%
  select(starts_with("PleaseIndicateWhetherYouAgreeOrDisagree")) %>%
  select(-ends_with("_Rating")) %>%
  rename_all(gsub, pattern = '.*?_', replacement = '')

View(agreeCols)

agreeCols[is.na(agreeCols)] <- 0

statements <- as.data.frame(colnames(agreeCols))
colnames(statements) <- "statement"
View(statements)


for(i in 1:length(levAgree)) {
  count_col = paste("count_",tolower(gsub(" ","_",levAgree[i])), sep = "")
  statements[,count_col] <- 0
  for(j in 1:nrow(statements)) 
      statements[j,count_col] <- sum(agreeCols[,j] == levAgree[i])
}

statements <- statements %>%
  mutate(responses = count_disagree + count_not_sure + count_agree,
         pct_disagree = round(count_disagree / responses,2),
         pct_not_sure = round(count_not_sure / responses,2),
         pct_agree = round(count_agree / responses,2),
         total = 1,
         pct_agree_minus_pct_disagree = pct_agree - pct_disagree,
         avg = 1 * pct_disagree + 2 * pct_not_sure + 3 * pct_agree,
         statement = gsub("([[:upper:]])", " \\1", statement)) %>%
  column_to_rownames('statement')

View(statements)

#TODO Try this part out with 5 level form
#Five level statements
#statements <- statements %>%
 # mutate(responses = count_strongly_disagree + count_disagee + count_not_sure + count_agree + count_strongly_agree,
  #       pct_disagree = round((count_disagree + count_strongly_disagree) / responses,2),
   #      pct_not_sure = round(count_not_sure / responses,2),
    #     pct_agree = round((count_agree + count_strongly_agree) / responses,2),
     #    total = 1,
      #   pct_agree_minus_pct_disagree = pct_agree - pct_disagree,
       #  avg = 1 * pct_disagree + 2 * pct_not_sure + 3 * pct_agree,
        # statement = gsub("([[:upper:]])", " \\1", statement)) %>%
#  column_to_rownames('statement')



# TODO ANALYZE BEFORE & AFTER STATEMENTS
# Get the column names
levVariables <- col_guide %>% 
  filter(analysisMarker == "learning") %>% 
  select(colDescLong)
levVariables <- as.character(unlist(levVariables, use.names = FALSE))

beforeNow <- df %>%
  select(starts_with("PleaseIndicateYourDegreeOfKnowledge")) %>%
  select(-ends_with("_Rating")) %>%
  rename_all(gsub, pattern = '.*?_', replacement = '') %>%
  rename(before = MyKnowledgeOfTheSubjectMatterBEFOREToday,
         now = MyKnowledgeOfTheSubjectMatterNOW) %>%
  mutate(change_in_knowledge = now - before)

learningGains <- beforeNow %>%
  group_by(change_in_knowledge) %>%
  summarise(count_of_respondents = n())


#TODO make this smoother
a1 = table(factor(beforeNow$before, levels = 1:(max(beforeNow$before))))
a2 = table(factor(beforeNow$now, levels = 1:(max(beforeNow$now))))
a3 = as.data.frame(bind_rows(a1,a2))
a3  = cbind(state = c('before', 'after'), a3)

a3 = a3%>%bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total"))) %>%
  column_to_rownames('state')
a3

# %>%
#   rename_all(gsub, pattern = "([[:upper:]][[:lower:]])", replacement = " \\1") %>%
#   rename_all(gsub, pattern = "BEFORE", replacement = " BEFORE") %>%
#   rename_all(gsub, pattern = "NOW", replacement = " NOW")

#Extract text statements
openEnd = df %>% select(starts_with('W')) 

# SAVE TO EXCEL
data_list = list('raw data'=df, 'agree disagree'=statements, 
                 'before after analysis'=learningGains, 'before after actuals'=a3,
                 'Open Ended statements'= openEnd )
write.xlsx(data_list, file = paste("2019 Investor Forum Feedback Summary_",Sys.Date(),".xlsx"),  
           col.names=TRUE, row.names=TRUE)



