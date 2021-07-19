# Association Rules for Market Basket Analysis

library(arules)  # association rules. ## This function basically provides the infrastructure for representing , manipulating and anlyzing transaction data and patterns using frequent itemsets and association rules.
library(arulesViz)  # data visualization of association rules ## This function is used for better visualization, as it extends the above function (arules). Includes a lot of function in the package like ggplot2 for diffrent kinds of graphs, plots, scatterplots, two key plot, m,atrix and grouped matrix.
library(RColorBrewer)  # color palettes for plots ## This library offers several color pallette for R. Sequential palletes are suited to order data that progress from low to high. Low data values indicates light collors whereas high data values indicates dark colors.
library(cluster)  # cluster analysis for market segmentation ## This library gives us the advantage to categorise/cluster the set of objects in such a way that the similar items can group together (in some sense) if we compae the other clusters(groups).

data(Groceries)  # grocery transcations object from arules package ## Gathering all the grocery data from the database of any supermarket.

# show the dimensions of the transactions object
print(dim(Groceries))  ## to print/show the values/observation the data is containing from the dataset.

print(dim(Groceries)[1])  # 9835 market baskets for shopping trips  ## there are 9835 values for the shopping trips of market baskets supermarket.
print(dim(Groceries)[2])  # 169 initial store items  ## there are 169 different variables as store items.

# examine frequency for each item with support greater than 0.025. ## for applying conditions based on the given situation, setting up the color code as dark red and then switching of the condition.
pdf(file="fig_market_basket_initial_item_support.pdf", 
  width = 8.5, height = 11)
itemFrequencyPlot(Groceries, support = 0.025, cex.names=0.8, xlim = c(0,0.3),
  type = "relative", horiz = TRUE, col = "dark red", las = 1,
  xlab = paste("Proportion of Market Baskets Containing Item",
    "\n(Item Relative Frequency or Support)"))
dev.off()    

# explore possibilities for combining similar items ## categorization and subcategorization of the data based on different level to explore and understand the data in much better way.
print(head(itemInfo(Groceries))) 
print(levels(itemInfo(Groceries)[["level1"]]))  # 10 levels... too few ## printing outputs of the levels
print(levels(itemInfo(Groceries)[["level2"]]))  # 55 distinct levels.  ## printing diferent levels (non- identical)

# aggregate items using the 55 level2 levels for food categories
# to create a more meaningful set of items.  ## categrizing and making the data meaningful and user oriented, people find it easy while looking at the isle while doing grocery shopping meaning all food items together, all diary items together, pet food together etcc.

groceries <- aggregate(Groceries, itemInfo(Groceries)[["level2"]])  

print(dim(groceries)[1])  # 9835 market baskets for shopping trips
print(dim(groceries)[2])  # 55 final store items (categories)  

pdf(file="fig_market_basket_final_item_support.pdf", width = 8.5, height = 11)
itemFrequencyPlot(groceries, support = 0.025, cex.names=1.0, xlim = c(0,0.5),
  type = "relative", horiz = TRUE, col = "blue", las = 1,                ##assigning a different color i.e. blue
  xlab = paste("Proportion of Market Baskets Containing Item",
    "\n(Item Relative Frequency or Support)"))
dev.off()   

# obtain large set of association rules for items by category and all shoppers ## applying different association rules for every single shopping user.
# this is done by setting very low criteria for support and confidence  
first.rules <- apriori(groceries, 
  parameter = list(support = 0.001, confidence = 0.05))
print(summary(first.rules))  # yields 69,921 rules... too many. ## Large outcome due to low confidence.

# select association rules using thresholds for support and confidence ## trying to make it more low to get some more yields altogether.
second.rules <- apriori(groceries, 
  parameter = list(support = 0.025, confidence = 0.05))
print(summary(second.rules))  # yields 344 rules
  
# data visualization of association rules in scatter plot.  ## scatterplot visual resprsentation R code and setting up the conditions.
pdf(file="fig_market_basket_rules.pdf", width = 8.5, height = 8.5)
plot(second.rules, 
  control=list(jitter=2, col = rev(brewer.pal(9, "Greens")[4:9])),
  shading = "lift")   
dev.off()    
  
# grouped matrix of rules ## grouping/clustering the similar data items together so as to make it more easy for shoppers.
pdf(file="fig_market_basket_rules_matrix.pdf", width = 8.5, height = 8.5)
plot(second.rules, method="grouped",   
  control=list(col = rev(brewer.pal(9, "Greens")[4:9])))
dev.off()    

# select rules with vegetables in consequent (right-hand-side) item subsets  ## selecting vegetables from the food items cluster created before.
vegie.rules <- subset(second.rules, subset = rhs %pin% "vegetables")## created a new subset to get the data related to all the vegtables in one place.
inspect(vegie.rules)  # 41 rules

# sort by lift and identify the top 10 rules. ## sorting is very essential based on different parameter like high price,low price, date expiry, date manufactured etc.
top.vegie.rules <- head(sort(vegie.rules, decreasing = TRUE, by = "lift"), 10)
inspect(top.vegie.rules) 

pdf(file="fig_market_basket_farmer_rules.pdf", width = 11, height = 8.5)
plot(top.vegie.rules, method="graph", 
  control=list(type="items"), 
  shading = "lift")
dev.off()  





