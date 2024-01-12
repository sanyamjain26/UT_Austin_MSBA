library(tidyverse)
library(igraph)
library(arules)  # has a big ecosystem of packages built around it
library(arulesViz)

# Association rule mining
# Adapted from code by Matt Taddy

# Read in playlists from users
# This is in "long" format -- every row is a single artist-listener pair
groceries_raw = readLines("groceries.txt")

str(groceries_raw)
summary(groceries_raw)

cart_items <- strsplit(groceries_raw, ",")

data <- readLines("groceries.txt")

# Create a list to store processed data
processed_data <- list()

cart_id <- 1
for (line in data) {
  items <- unlist(strsplit(line, ","))
  for (item in items) {
    processed_data[[length(processed_data) + 1]] <- c(cart_id, item)
  }
  cart_id <- cart_id + 1
}

cart_df <- do.call(rbind, processed_data)

cart_df = data.frame(cart_df)

colnames(cart_df) <- c("CartID", "Item")

# Turn user into a factor
cart_df$CartID = factor(cart_df$CartID)

# First create a list of baskets: vectors of items by consumer
# Analagous to bags of words

# apriori algorithm expects a list of baskets in a special format
# In this case, one "basket" of songs per user
# First split data into a list of artists for each user
carts = split(x=cart_df$Item, f=cart_df$CartID)

## Remove duplicates ("de-dupe")
carts = lapply(carts, unique)

## Cast this variable as a special arules "transactions" class.
cart_trans = as(carts, "transactions")
summary(cart_trans)

# Now run the 'apriori' algorithm
# Look at rules with support > .005 & confidence >.1 & length (# artists) <= 4
# musicrules_singles = apriori(playtrans, 
#	  parameter=list(support=.005, confidence=.1, maxlen=1))
     
shopping_rules = apriori(cart_trans, 
  parameter= list(support=.005, confidence=.1, maxlen=4))

# Look at the output... so many rules!
# inspect(musicrules_singles)
inspect(shopping_rules)

## Choose a subset
inspect(subset(shopping_rules, subset=lift > 5))
inspect(subset(shopping_rules, subset=confidence > 0.6))
inspect(subset(shopping_rules, subset=lift > 10 & confidence > 0.5))

# plot all the rules in (support, confidence) space
# notice that high lift rules tend to have low support
plot(shopping_rules)

# can swap the axes and color scales
plot(shopping_rules, measure = c("support", "lift"), shading = "confidence")

# "two key" plot: coloring is by size (order) of item set
plot(shopping_rules, method='two-key plot')

# can now look at subsets driven by the plot
inspect(subset(shopping_rules, support > 0.035))
inspect(subset(shopping_rules, confidence > 0.6))
inspect(subset(shopping_rules, lift > 20))


# graph-based visualization
# export
# associations are represented as edges
# For rules, each item in the LHS is connected
# with a directed edge to the item in the RHS. 
cart_graph = associations2igraph(subset(shopping_rules, lift>4), associationsAsNodes = FALSE)
igraph::write_graph(cart_graph, file='groceries.graphml', format = "graphml")
