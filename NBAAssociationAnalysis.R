library("arules")

#Load the data
df <- read.csv(file="C:\\Users\\faiza\\OneDrive\\Desktop\\NBAData\\NBADataset2019-1980Seasons.csv", header=TRUE, sep=",")
df

#Replace numerical values to factors 
df <- lapply(df, function(x){as.factor(x)})
df

df[is.na(df)] <- 0

str(df)

#Convert to a dataframe
df2 <- as.data.frame(lapply(df, unlist))


#Order the dataframe by column 
df2[order(df2[1,])]

#Make the certain columns null to avoid redundant / insignificant processing,
#since we are checking if the shot was made or not 
df2$TeamName = NULL
df2$FinalsWin = NULL


#Convert the data to be a set of transactions
transactional_data <- as(df2, "transactions")
transactional_data


#Make sure transactional_data is a set of transactions and view the first two entries
class(transactional_data)
inspect(head(transactional_data, 2))


#Generate rules with a min support and confidence 
generalRules <- apriori(transactional_data, parameter = list(support = 0.01, confidence = 0.8))

inspect(head(sort(generalRules, by = "confidence"), 100))

summary(generalRules)


#Generate rules based on if the teams reached the finals or not 

reachedFinalsRules <- apriori(transactional_data, parameter = list(supp=0.001, conf=0.8),appearance = list(default="lhs",rhs="ReachedFinals=1"))

inspect(head(sort(reachedFinalsRules, by = "confidence"), 100))

