# Market Basket Data
# Market Basket Analysis _ Assocition Btw 2 Products
#
# install "arules" - Is a package for analyzing transactional data. 
# install "arulesViz" - Is a package that provides visual techniques for the arules package.
#
install.packages("arules")
install.packages("arulesViz")
# 
#
library(arules)
library(arulesViz)
#
# Uploading the ElectronidexTransactions2017
#
Electronidex_tran <- read.transactions("ElectronidexTransactions2017.csv", format = "basket", sep=",", rm.duplicates = FALSE)
summary(Electronidex_tran)
#
# Calculating the Density (The density is total # of non-empty cells "cell w/1" in the Matrix)
# 9835*125*.03506172
# Calculated as (9835 rows * 125 Columnc = 1229375*0.03506172 = 43104)
# From the calcilation above, 43104 cells of the itemMatrix are non-empty or has 1 instead of 0)
#
# Inpecting the itemMatrix to see what type of transactions it contains
# In Sparse system, use the inspect() fundtion to do this
# Inspect in batches for easier understanding
#
inspect(Electronidex_tran[1:3])
#
# SUPPORT (Looking at the % or proportion or freq. each item show up in the transactions. 
# Example 1TB Portable Ext Hard Drive is 0.002%)
# To know the number of transactions item shows in: e.g Acer Desktop (0.101881037*9835=1001 times)
itemFrequency(Electronidex_tran[ ,1:7])
#
# Plotting a graph to show items frequency or Support. How many times each item show up in transactions.
# The code below will graph items from 0.05 freq and above
itemFrequencyPlot(Electronidex_tran, support = 0.05)
#
# Lets also look at items with the top 5 high Support
itemFrequencyPlot(Electronidex_tran, topN=5)
#
# By using the image() function, you can visualize all of the transactions within your dataset. 
# It will display the 125 possible items as your x-axis (the columns in your sparse matrix) 
# and the number of transactions (the rows in your sparse matrix) as your y-axis. 
# The items that were purchased are blacked out.
image(Electronidex_tran, 5)
#
#
# visualize transactions using the sample() function with the image() function
#
image(sample(Electronidex_tran, 200))
#
#
# BUILDING OF MODEL with APRIORI
# CONFIDENCE ( If Item A,B present in transaction, how likely Item C will show. Confidence of A,B -> C)
# Calculating Confidence: Conf.([A,B]->[C]) = Support (A,B,C)/Support ([A,B])
# Note: We want to have Association Rule with high Support & high Conf.
# We're looking for Items & ItemSets with high support and Ass.Rule with high Confidence.
#
# First, start using default values for min. support and min. conf.to see what happens
ml <- apriori(Electronidex_tran)
#
# Tuning the Parameter
ml <- apriori(Electronidex_tran, parameter = list(support=0.007, confidence=0.1, minlen=2))
ml
# Use summary() to look deeper into the transaction data
summary(ml)
#
# Use inspect() function to inspect the rules.
#"lift" means how much likely an item (y-axis) will be purchase than the item (y-axis)
# being purchase by itself in general. Example, how much likely Apple MacBook Air will be purchased with
# Dell KM117 Wireless Keyboard & Mouse than it being purchased by itself or in all transactions.
# Here, it's 2.4 times more likely.
#
#Inspecting only 1-20 rules out of the total # of rules
inspect(ml [1:20])
#
# Lets also sort the rules in Lifts to analyse the top 5 lifts. We could also sort by conf. or support.
inspect(sort(ml, by="lift") [1:5])
inspect(sort(ml, by="support") [1:5])
inspect(sort(ml, by="confidence") [1:5])
inspect(sort(ml, by="lift") [6:20])
#
#
# Analysing specific item rule
ItemRules <- subset(ml, items %in% "iMac")
inspect(ItemRules)
#
# Using "is.redundant() function" to check for redundant rules. True means present and False means non.
# If there's redundant rule, will removing help?
is.redundant(ml)
#
# Since there seem to be redundant rules present in the spares matrix and preping to remove redundant rules
subset.matrix <- is.subset(ml, ml)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
# which() will list the reducdant rules in the transactions
which(redundant)
#
# remove redundant rules
rules.pruned <- ml[!redundant]
inspect(rules.pruned)
#
# VISUALIZATION OF THE RULES TO GET INSIGHT INFORMATION FOR STAKEHOLDERS
plot(ml)
#
# Specify graph type and rules
plot(ml, method="graph", control=list(verbose=TRUE))
#
plot(ml, method="paracoord", control=list(reorder=TRUE))
plot(ml, method = "graph", engine = "htmlwidget")
head(quality(ml))

#
plot(ml, method = "grouped matrix", engine = "interactive")
plot(ml, method = "graph", engine = "interactive")
#
# ## Scatterplot
# 
# ## Scatterplot with custom colors
# library(colorspace) # for sequential_hcl
# library(colorspace)
# plot(subrules, control=list(col=sequential_hcl(100)))
#
# ## graphs only work well with very few rules
# subrule2 <- sample(ml, 25)
# plot(subrule2, method="graph")
# #
# # ## igraph layout generators can be used (see ? igraph::layout_)
# plot(subrule2, method="graph", control=list(layout=igraph::in_circle()))
# plot(subrule2, method="graph", control=list(
# layout=igraph::with_graphopt(spring.const=5, mass=50)))
# # 
# # # ## The following techniques work better with fewer rules
# subrules <- subset(ml, lift>2.0)
# subrules
# # # # 
# # # # 2D matrix with shading
# plot(subrules, method="graph", measure="lift")
# plot(subrules, method="graph", measure="lift", control=list(verbose=TRUE))
# # # # 
# # # # ## 3D matrix
# plot(subrules, method="matrix3D", measure="lift")
# plot(subrules, method="matrix3D", measure="lift", control=list(reorder=TRUE))
# # # # 
# # # # ## matrix with two measures
# plot(subrules, method="matrix", measure=c("lift", "confidence"))
# plot(subrules, method="matrix", measure=c("lift", "confidence"), 
#        control=list(verbose=TRUE))
# # # 
# try: plot(subrules, method="matrix", measure="lift", interactive=TRUE, 
# 		control=list(verbose=TRUE))
# # 
# # ## grouped matrix plot
# plot(ml, method="grouped")
# try: sel <- plot(ml, method="grouped", interactive=TRUE)
# # 
# #
# # 
# plot(subrule2, method="graph", control=list(type="itemsets"))
# try: plot(subrule2, method="graph", interactive=TRUE)
# # 
# # 
# # ## parallel coordinates plot
# plot(subrule2, method="paracoord")
# plot(subrule2, method="paracoord", control=list(verbose=TRUE))
# # 
# # ## Doubledecker plot only works for a single rule
# oneRule <- sample(ml, 1)
# plot(oneRule, method="doubledecker", data = Electronidex_tran)
# # 
# # 
# # ## for itemsets
# itemsets <- eclat(Electronidex_tran, parameter = list(support = 0.02, minlen=2))
# plot(itemsets)
# plot(itemsets, method="graph")
# plot(itemsets, method="paracoord", control=list(alpha=.5, verbose=TRUE))
# 


