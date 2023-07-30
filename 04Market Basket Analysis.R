tictoc::tic()
#--------------------------------library--------------------------------------
library(tidyverse)
library(vtable)
library(caret)
library(janitor)
library(arules)
library(arulesViz)
library(plotly)

#load data----
load("DF_Readyforanalysis.RData")


#
ars <- apriori (Data, parameter = list (support = 0.3, confidence = 0.5))




inspect (head (sort(ars, by = "confidence")))
inspect (sort(ars, by = "confidence"))
freq <- eclat (Data, parameter = list (support = 0.05, minlen = 6 , maxlen = 6))
inspect (sort(freq))

new_rules <- apriori (Data, parameter = list (support = 0.1, confidence = 0.7))

rules_conf <- sort (new_rules, by = "confidence", decreasing = T)
inspect (head(rules_conf,20))

rules_lift <- sort (new_rules, by = "lift", decreasing = T)
inspect (head(rules_lift))

rules <- apriori (Data, parameter = list (support = 0.01, confidence = 0.3), control = list (verbose = F))
rules <- apriori (Data, parameter = list (support = 0.1, confidence = 0.3),
                  appearance = list (default = "lhs", rhs = "Male"))

inspect (head(rules))


ars <- apriori (Data, parameter = list (support = 0.1, confidence = 0.5))

interestMeasure (ars, c("support", "confidence", "lift", "cosine", "chiSquare", "conviction"))

##############

data(Data)
#rules_lift <- sort(rules, by = 'lift')
#rules_pruned <- rules_lift[!is.redundant(rules_lift, measure="lift")]
rules <- apriori(Data, parameter = list(supp = 0.3, conf = 0.5, target = "rules"))

plot(rules)
plot(rules, control = list(col = rainbow(7)))
plot(rules, control = list(col = rainbow(4)))
plot(rules, method = "graph")
plot(rules, method = "two-key plot")
plot(rules, method = "paracoord")

inspectDT(rules)#great
sub_rules <- subset (rules, lift>1.2)
smpl <- sample(sub_rules,100, replace = 'T')
inspect(smpl)

smpl <- sample(sub_rules,1)
plot(smpl, method = "doubledecker", data = Data)

#
tictoc::toc()
