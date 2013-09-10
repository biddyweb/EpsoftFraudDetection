library(arules)
library(arulesViz)

patterns <- random.patterns(nItems =1000)
summary(patterns)


trans <- random.transactions(nItems=1000,nTrans=1000,method = 'agrawal',patterns = patterns)
image(trans)


data("AdultUCI");
Adult = as(AdultUCI, "transactions");

data("Adult")
rules = apriori(Adult, parameter=list(support=0.01, confidence=0.5));
rules;
inspect(head(sort(rules, by="lift"),3));

plot(rules);

head(quality(rules));

plot(rules, measure=c("support","lift"), shading="confidence");

plot(rules, shading="order", control=list(main ="Two-key plot"));

sel = plot(rules, measure=c("support","lift"), shading="confidence", interactive=TRUE);

subrules = rules[quality(rules)$confidence > 0.8];

subrules