# Load the libraries
library(arules)
library(datasets)

# Load the data set
data(Groceries)

# Print data
print(head(Groceries))

# Create an item frequency plot for the top 30 items
itemFrequencyPlot(Groceries, topN=30, type="absolute")

# Get the rules
rules = apriori(Groceries, parameter = list(supp = 0.001, conf = 0.5))

# Show the top 10 rules, but only 2 digits
options(digits=2)
inspect(rules[1:10])

# Print summary
print(summary(rules))

# Sort by lift
rules = sort(rules, by="lift", decreasing=TRUE)
inspect(rules[1:10])

# --- Tropical fruits ---

# Answer question #1
# What are customers likely to buy before buying X?
print("Question #1:")
rules = apriori(data=Groceries, parameter=list(supp=0.001, conf=0.08),
                appearance = list(default="lhs", rhs="tropical fruit"),
                control = list(verbose=FALSE))
rules = sort(rules, decreasing=TRUE, by="confidence")
inspect(rules[1:5])

# Answer question #2
# What are customers likely to buy if they purchase X?
print("Question #2:")
rules = apriori(data=Groceries, parameter=list(supp=0.001, conf=0.15, minlen=2),
                appearance = list(default="rhs", lhs="tropical fruit"),
                control = list(verbose=FALSE))
rules = sort(rules, decreasing=TRUE, by="confidence")
inspect(rules[1:5])

# --- Napkins ---

# Answer question #1
# What are customers likely to buy before buying X?
print("Question #1:")
rules = apriori(data=Groceries, parameter=list(supp=0.001, conf=0.08),
                appearance = list(default="lhs", rhs="napkins"),
                control = list(verbose=FALSE))
rules = sort(rules, decreasing=TRUE, by="confidence")
inspect(rules[1:5])

# Answer question #2
# What are customers likely to buy if they purchase X?
print("Question #2:")
rules = apriori(data=Groceries, parameter=list(supp=0.001, conf=0.15, minlen=2),
                appearance = list(default="rhs", lhs="napkins"),
                control = list(verbose=FALSE))
rules = sort(rules, decreasing=TRUE, by="confidence")
inspect(rules[1:5])
