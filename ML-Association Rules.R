# Install and load the association rules (arules) package.
install.packages("arules")
install.packages("tidyverse")
library(arules)
library(tidyverse)


#-------------------------------------------------------
#' #Part 1: Collect the Data
#-------------------------------------------------------

# Load the market basket data into a sparse matrix
basket <-
  read.transactions("https://s3.amazonaws.com/notredame.analytics.data/marketbasket.csv",
                    sep = ",")

#-------------------------------------------------------
#' #Part 2: Explore the Data
#-------------------------------------------------------

summary(basket)

#Q1-What is the density of the matrix? What does this number mean?

###Density=0.03288973
###This number means that around 3.2% of the cells are populated with non-zero values.
###Since the data is 3.2% dense, it is also 96.8% sparse which means that 96.8% of its cells are not filled with data.

#Q2-What is the median itemset length? What does this number mean?

###The median itemset length in the data is 3, which means the median basket included 3 items.

#Q3-How many items are in the dataset?

###There are 119 items in the dataset.

#Q4-How many transactions are in the dataset?

###There are 7501 Transactions in the dataset.

#Create a bar chart of the 20 least frequently bought items in the dataset in descending order of frequency.
basket.frequency <- itemFrequency(basket)

basket.frequency <- data.frame(
    Items = names(basket.frequency),
    Frequency = basket.frequency,
    row.names=NULL)

least_freq <- basket.frequency %>%
  arrange(Frequency) %>%
  slice(1:20)

ggplot(least_freq,aes(x=reorder(Items,-Frequency),y=Frequency)) + geom_bar(stat="identity") + labs(x="Item")


#-------------------------------------------------------
#' #Part 3: Generate Rules
#-------------------------------------------------------

#Calculate the support used in the model creation
#(16*7)/7501~0.015

#Generate the Association Rules
basketrules <- apriori(basket,parameter=list(support=.015, confidence=.25, minlen=2))


#-------------------------------------------------------
#' #Part 4: Examine the Rules
#-------------------------------------------------------

summary(basketrules)

#1a-How many rules did you generate?

###52 rules were generated.

#1b-How many rules have a length greater than 3?

###There are 0 rules that have a length greater than 3, but there are 9 rules with the length of 3.

#2a-What are the top 5 rules in terms of lift?
rules_lift <- sort (basketrules, by="lift", decreasing=TRUE)
inspect(head(rules_lift))
###1. {herb & pepper} => {ground beef}
###2. {mineral water,spaghetti} => {ground beef}
###3. {ground beef, mineral water} => {spaghetti}
###4. {soup} => {milk}
###5. {ground beef} => {spaghetti}

#2b-How many rules have to do with "burgers"? Call the ruleset "burgerrules".
burgerrules <- apriori (data=basket, parameter=list (supp=0.015,conf = 0.25), appearance = list (default="rhs",lhs="burgers"), control = list (verbose=F))
inspect(burgerrules)
###there are 3 rules that have to do with burgers

#2c-What are the top 10 rules in terms of lift that have "spaghetti" on the right-hand side? Call the ruleset "spaghettirules"
spaghettirules <-apriori (data=basket, parameter=list (supp=0.015,conf = 0.25), appearance = list (default="lhs",rhs="spaghetti"), control = list (verbose=F))
spaghettirules_lift <- sort (spaghettirules, by="lift", decreasing=TRUE)
inspect(spaghettirules_lift[1:10,])


#3-Association rules are typically grouped into 3 categories - "Actionable", "Trivial" and "Inexplicable". Based on these labels, how would you categorize the 10 rules from the
#previous question (spaghettirules)? For those that are actionable, what action(s) would you recommend taking?
###Rules 1 through 8 are all trivial because they are all items that would be bought to make basic pasta dishes.
###Rules 9 and 10 can be considered to be actionable.
###When you think about it, chocolate pairs well with all pasta dishes as a dessert, especially romantic dinners.
###Stores could do "Date Night" displays or deals.
###Shrip is a simple protein for many spaghetti dishes. Stores could place small displays of pastas near the seafood section.
###These items could also be thrown into the "Date Night" Ads. 