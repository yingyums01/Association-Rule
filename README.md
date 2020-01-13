# Association-Rule
Association Rule in R (apriori)
install.packages("arules")
install.packages("tidyverse")
library(arules)
library(tidyverse)

### Load the grocery data into a sparse matrix (Note the use of 'read.transactions' for this dataset).

groceries <-
  read.transactions("groceries.csv",sep = ",")
groceries
summary(groceries)


sample(groceries,100)
#plot sparse matrix
image(sample(groceries,100))

#head()
inspect(groceries[1:5])

#using itemFrequency(): the frequency of the first five items(alphabetical) 
itemFrequency(groceries[, 1:5])

### at least 10 percent support
itemFrequencyPlot(groceries, support = 0.1)

### show top 20 support item
itemFrequencyPlot(groceries, topN = 20)

### frequency of all the items
itemFrequency(groceries)

### see all the item name(alphabetic order)
names(itemFrequency(groceries))
      
### convert to dataframe(alphabetic order)
groceries.frequency<-
  data.frame(
    Items = names(itemFrequency(groceries)),
    Frequency = itemFrequency(groceries),
    #if I only enter Frequency, it also work
    row.names = NULL
  )
head(groceries.frequency)


groceries.frequency %>%
  arrange(desc(Frequency)) %>%
  slice(1:20) %>%
  ggplot() +
  geom_col(mapping=aes(x=reorder(Items,Frequency), y=Frequency), fill="lightblue", color="black") +
  labs(x="Items") +
  coord_flip() + theme_minimal() + theme(legend.position = "none")

### reorder(mydisplaycolumn, ordered by this column)


### 10 least frequency
groceries.frequency %>%
  arrange(Frequency) %>%
  slice(1:10)

# model
### support = 0.1, confidence = 0.8 and minimum required rule items = 1
groceryrules <- apriori(groceries)
groceryrules 

### got 0 
### With the minimum support threshold set at 0.1, in order for an item to be part of a rule,
### the item must have appeared in at least 983.5 (0.1 * 9835) transactions.

### These are the items that meet that requirement. 
itemFrequencyPlot(groceries, support = 0.1)

### none of the rules for these items have a confidence threshold at or above 0.8.
### We need to relax our thresholds a bit to get some rules from our data.
### Let's say we decide to include items that were purchased on average at least 5 times a day.
### Given that our data is for 30 days, we would need to set our support threshold at 0.015 -> ((5*30)/9835).


### A minimum confidence threshold of 0.8 implies that the rule has to be correct at least 80% of the time. 
### This is rather strict requirement for our data. Let's set that at 25% for now.


### Now we make another attempt at generating rules (using the 'parameter' attribute to set our thresholds).
groceryrules<-
  apriori(groceries,
          parameter=list(
            support=0.015,
            confidence=0.25,
            minlen = 2 # minimum number of itemset
          )
  )
groceryrules

summary(groceryrules)
inspect(groceryrules)

#examine the top5 lift
class(groceryrules)
groceryrules %>%
  sort(by="lift")%>% #why can't use arrange? datatype?
  head(n=5)%>%
  inspect()

### subset(): find items
### find whether tropical fruits will be purchased together with other items
groceryrules%>%
  subset(items %in% 'tropical fruit')%>% #how we know the name of 'items' # existing var in the model?
  inspect()

groceryrules %>%
  subset(lhs %in% "tropical fruit")%>%
  sort(by="lift")%>%
  head(n=5)%>%
  inspect()

### Finally, we can output our rules to a CSV file for additional analysis in a separate platform,...
write(
  groceryrules,
  file = "groceryrules.csv",
  sep = ",",
  quote = TRUE,
  row.names = FALSE
)


### ... or we can convert it to a data frame for further analysis in R.
groceryrules.data <- as(groceryrules, "data.frame")
head(groceryrules.data)
