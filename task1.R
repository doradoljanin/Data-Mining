# THIS PROJECT IS A WORK OF DORA DOLJANIN AND FILIP PAVIČIĆ

#Answares 1 - 3
# A rule is interesting when it can help us determine which mushroom is poisonous and which mushroom is edible. 
# Those rules will tell us which characteristics of a mushroom should be preferred, and which should be avoided 
# when choosing a mushroom to eat. The best rules have a high value of support. Value of support helps us identify
# the rules worth considering for further analysis. For example, we may want to consider only the item sets which
# occur at least 50 times out of a total of 1000 transactions i.e. support = 0.05. If an item set has a very low
# support, we do not have enough information on the relationship between its items and therefore we can not draw
# any conclusions from such a rule. A good rule should have a high value of confidence with a lift further away 
# from 1. A lift should be different than 1 as much as possible (the bigger the difference, the better) because
# that value shows a high association between the antecedent and the consequent. The discovered rules can be used
# by mycologists to draw useful conclusions about various types of mushrooms and their characteristics, and to create
# better presumptions about newly discovered mushroom types whose edibility may still be unknown. However, these 
# conclusions should not be taken as 100% certain when picking a mushroom to eat, because there still might be some
# faults.

# Experiments:
#1. Load libraries and data

library(arules) # association rules
library(arulesViz) # visualization of reles


#loading data
data(Mushroom)

summary(Mushroom)
#there is 8124 rows witch representance different types of mashrooms

#http://archive.ics.uci.edu/ml/datasets/Mushroom
#on this addres you can find explanation of attributes

# 1. cap-shape: bell=b,conical=c,convex=x,flat=f, knobbed=k,sunken=s
# 2. cap-surface: fibrous=f,grooves=g,scaly=y,smooth=s
# 3. cap-color: brown=n,buff=b,cinnamon=c,gray=g,green=r, pink=p,purple=u,red=e,white=w,yellow=y
# 4. bruises?: bruises=t,no=f
# 5. odor: almond=a,anise=l,creosote=c,fishy=y,foul=f, musty=m,none=n,pungent=p,spicy=s
# 6. gill-attachment: attached=a,descending=d,free=f,notched=n
# 7. gill-spacing: close=c,crowded=w,distant=d
# 8. gill-size: broad=b,narrow=n
# 9. gill-color: black=k,brown=n,buff=b,chocolate=h,gray=g, green=r,orange=o,pink=p,purple=u,red=e, white=w,yellow=y
# 10. stalk-shape: enlarging=e,tapering=t
# 11. stalk-root: bulbous=b,club=c,cup=u,equal=e, rhizomorphs=z,rooted=r,missing=?
# 12. stalk-surface-above-ring: fibrous=f,scaly=y,silky=k,smooth=s
# 13. stalk-surface-below-ring: fibrous=f,scaly=y,silky=k,smooth=s
# 14. stalk-color-above-ring: brown=n,buff=b,cinnamon=c,gray=g,orange=o, pink=p,red=e,white=w,yellow=y
# 15. stalk-color-below-ring: brown=n,buff=b,cinnamon=c,gray=g,orange=o, pink=p,red=e,white=w,yellow=y
# 16. veil-type: partial=p,universal=u
# 17. veil-color: brown=n,orange=o,white=w,yellow=y
# 18. ring-number: none=n,one=o,two=t
# 19. ring-type: cobwebby=c,evanescent=e,flaring=f,large=l, none=n,pendant=p,sheathing=s,zone=z
# 20. spore-print-color: black=k,brown=n,buff=b,chocolate=h,green=r, orange=o,purple=u,white=w,yellow=y
# 21. population: abundant=a,clustered=c,numerous=n, scattered=s,several=v,solitary=y
# 22. habitat: grasses=g,leaves=l,meadows=m,paths=p, urban=u,waste=w,woods=d

#exemple of records
inspect(head(Mushroom))

MushroomTR = Mushroom


########################################
#2. Frequent itemsets 
#setting parameters of Apriori algorithm
aParam  = new("APparameter","support" = 0.2, target ="frequent itemsets", "maxlen"= 1) 

#frequent itemsets discovery - Apriori algorithm
FI_rules <-apriori(MushroomTR,aParam)

#print 20 most frequent items, that is good way to get a feel of data
inspect(head(sort(FI_rules, by="support"),20))
# [1]  {VeilType=partial}        1.0000000 8124 
# [2]  {VeilColor=white}         0.9753816 7924 
# [3]  {GillAttached=free}       0.9741507 7914 
# [4]  {RingNumber=one}          0.9217134 7488 
# [5]  {GillSpace=close}         0.8385032 6812 
# [6]  {GillSize=broad}          0.6907927 5612 
# [7]  {SurfaceAboveRing=smooth} 0.6371246 5176 
# [8]  {SurfaceBelowRing=smooth} 0.6075825 4936 
# [9]  {Bruises=no}              0.5844412 4748 
# [10] {StalkShape=tapering}     0.5672083 4608 
# [11] {ColorAboveRing=white}    0.5494830 4464 
# [12] {ColorBelowRing=white}    0.5396356 4384 
# [13] {Class=edible}            0.5179714 4208 
# [14] {RingType=pendant}        0.4884293 3968 
# [15] {Class=poisonous}         0.4820286 3916 
# [16] {StalkRoot=bulbous}       0.4647957 3776 
# [17] {CapShape=convex}         0.4500246 3656 
# [18] {Odor=none}               0.4342688 3528 
# [19] {StalkShape=enlarging}    0.4327917 3516 
# [20] {Bruises=bruises}         0.4155588 3376

#We observe that all mushrooms in the dataset have a partial Veil Type so that attribute
#will not be useful in rules analysis

########################################
#3. Association rules discovery
aParam  = new("APparameter", "confidence" = 0.4, "support" = 0.2, "minlen"= 2) 
appearance = list(rhs = c("Class=edible", "Class=poisonous"), default="lhs")

#frequent itemsets discovery - Apriori algorithm
rules <-apriori(MushroomTR,aParam, appearance)


#how many rules we found
length(rules)

#removing reduntant rules (rules with the same consequent and confidence but with less items in the antecedent
rules <- rules[is.redundant(rules) == FALSE]

#how many non redundant rules we found
length(rules) ##226

#print 10 rules with the biggest confidence
inspect(head(sort(rules, by="confidence"),10))
# [1]  {GillColor=buff}                         => {Class=poisonous} 0.2127031 1          2.074566 1728 
# [2]  {Odor=foul}                              => {Class=poisonous} 0.2658789 1          2.074566 2160 
# [3]  {GillSize=broad,Spore=brown}             => {Class=edible}    0.2028557 1          1.930608 1648 
# [4]  {GillSpace=close,SurfaceBelowRing=silky} => {Class=poisonous} 0.2658789 1          2.074566 2160 
# [5]  {SurfaceBelowRing=silky,RingNumber=one}  => {Class=poisonous} 0.2658789 1          2.074566 2160 
# [6]  {StalkShape=tapering,Spore=white}        => {Class=poisonous} 0.2127031 1          2.074566 1728 
# [7]  {GillSpace=close,SurfaceAboveRing=silky} => {Class=poisonous} 0.2742491 1          2.074566 2228 
# [8]  {SurfaceAboveRing=silky,RingNumber=one}  => {Class=poisonous} 0.2698178 1          2.074566 2192 
# [9]  {Bruises=bruises,Habitat=woods}          => {Class=edible}    0.2245199 1          1.930608 1824 
# [10] {Odor=none,StalkShape=tapering}          => {Class=edible}    0.3072378 1          1.930608 2496 

#subsets
rulesSup0_3 <- subset(rules, subset = support > 0.3)

length(rulesSup0_3) ##75

inspect(head(sort(rules, by="confidence"),10))
# [1]  {GillColor=buff}                         => {Class=poisonous} 0.2127031 1          2.074566 1728 
# [2]  {Odor=foul}                              => {Class=poisonous} 0.2658789 1          2.074566 2160 
# [3]  {GillSize=broad,Spore=brown}             => {Class=edible}    0.2028557 1          1.930608 1648 
# [4]  {GillSpace=close,SurfaceBelowRing=silky} => {Class=poisonous} 0.2658789 1          2.074566 2160 
# [5]  {SurfaceBelowRing=silky,RingNumber=one}  => {Class=poisonous} 0.2658789 1          2.074566 2160 
# [6]  {StalkShape=tapering,Spore=white}        => {Class=poisonous} 0.2127031 1          2.074566 1728 
# [7]  {GillSpace=close,SurfaceAboveRing=silky} => {Class=poisonous} 0.2742491 1          2.074566 2228 
# [8]  {SurfaceAboveRing=silky,RingNumber=one}  => {Class=poisonous} 0.2698178 1          2.074566 2192 
# [9]  {Bruises=bruises,Habitat=woods}          => {Class=edible}    0.2245199 1          1.930608 1824 
# [10] {Odor=none,StalkShape=tapering}          => {Class=edible}    0.3072378 1          1.930608 2496 


rulesSup0_4 <- subset(rules, subset = support > 0.4)

length(rulesSup0_4) ##23

inspect(head(sort(rulesSup0_4, by="confidence"),10))
# [1]  {Odor=none}                                   => {Class=edible}    0.4194978 0.9659864  1.864941 3408 
# [2]  {GillSize=broad,SurfaceAboveRing=smooth}      => {Class=edible}    0.4155588 0.9398664  1.814514 3376 
# [3]  {Bruises=no,GillAttached=free,RingNumber=one} => {Class=poisonous} 0.4007878 0.7722960  1.602179 3256 
# [4]  {Bruises=no,RingNumber=one}                   => {Class=poisonous} 0.4007878 0.7386570  1.532393 3256 
# [5]  {Bruises=no,VeilColor=white}                  => {Class=poisonous} 0.4042344 0.7220756  1.497993 3284 
# [6]  {Bruises=no,GillAttached=free}                => {Class=poisonous} 0.4030034 0.7214632  1.496723 3274 
# [7]  {SurfaceAboveRing=smooth}                     => {Class=edible}    0.4480551 0.7032457  1.357692 3640 
# [8]  {GillSize=broad}                              => {Class=edible}    0.4825209 0.6985032  1.348536 3920 
# [9]  {Bruises=no}                                  => {Class=poisonous} 0.4052191 0.6933446  1.438389 3292 
# [10] {SurfaceBelowRing=smooth}                     => {Class=edible}    0.4185130 0.6888169  1.329836 3400 

#We discovered rules for different values of minimum support and observed the number of rules we get for each
#of these values. We can see that for a lower minimum support value we get more rules, while for a higher minimum
#support value we get less rules.



rulesLiftLess1 <- subset(rules, subset = lift < 1)
length(rulesLiftLess1)
inspect(head(sort(rulesLiftLess1, by="confidence"),10))
# [1] {StalkRoot=bulbous,RingNumber=one}  => {Class=edible}    0.2304284 0.5120350  0.9885391 1872 
# [2] {StalkRoot=bulbous}                 => {Class=edible}    0.2363368 0.5084746  0.9816653 1920 
# [3] {GillAttached=free,VeilColor=white} => {Class=edible}    0.4943378 0.5079686  0.9806885 4016 
# [4] {GillAttached=free}                 => {Class=edible}    0.4943378 0.5074551  0.9796971 4016 
# [5] {VeilColor=white}                   => {Class=edible}    0.4943378 0.5068147  0.9784608 4016 
# [6] {RingNumber=one}                    => {Class=edible}    0.4529788 0.4914530  0.9488033 3680 
# [7] {CapShape=convex}                   => {Class=poisonous} 0.2102413 0.4671772  0.9691900 1708 
# [8] {GillSpace=close}                   => {Class=edible}    0.3702610 0.4415737  0.8525059 3008 
# [9] {StalkShape=tapering}               => {Class=poisonous} 0.2481536 0.4375000  0.9076226 2016 

rulesLiftHigh1 <- subset(rules, subset = lift > 1)
length(rulesLiftHigh1)
inspect(head(sort(rulesLiftHigh1, by="confidence"),10))
# [1]  {GillColor=buff}                         => {Class=poisonous} 0.2127031 1          2.074566 1728 
# [2]  {Odor=foul}                              => {Class=poisonous} 0.2658789 1          2.074566 2160 
# [3]  {GillSize=broad,Spore=brown}             => {Class=edible}    0.2028557 1          1.930608 1648 
# [4]  {GillSpace=close,SurfaceBelowRing=silky} => {Class=poisonous} 0.2658789 1          2.074566 2160 
# [5]  {SurfaceBelowRing=silky,RingNumber=one}  => {Class=poisonous} 0.2658789 1          2.074566 2160 
# [6]  {StalkShape=tapering,Spore=white}        => {Class=poisonous} 0.2127031 1          2.074566 1728 
# [7]  {GillSpace=close,SurfaceAboveRing=silky} => {Class=poisonous} 0.2742491 1          2.074566 2228 
# [8]  {SurfaceAboveRing=silky,RingNumber=one}  => {Class=poisonous} 0.2698178 1          2.074566 2192 
# [9]  {Bruises=bruises,Habitat=woods}          => {Class=edible}    0.2245199 1          1.930608 1824 
# [10] {Odor=none,StalkShape=tapering}          => {Class=edible}    0.3072378 1          1.930608 2496 

# Lift shows us how the confidence of a rule has changed compared to the relative frequency of the consequent. 
# It can be useful if a relative frequency of the consequent is very high. In that case, if a lift is around the
# value 1, then a high confidence is misleading when choosing the best rules. When we have a very frequent
# consequent, it does not really matter what you have in the antecedent. The confidence for an association 
# rule having a very frequent consequent will always be high. So, we can use this knowledge practically when 
# choosing the best rules: we should be avoiding the rules with a lift close to 1 and should choose the rules with
# a lift further away from 1 in a positive or negative direction.

rulesSize1 <- subset(rules, subset = size(lhs) == 1)
length(rulesSize1)
inspect(head(sort(rulesSize1, by="confidence"),20)) 
#in first 20
#max conf = 1, min conf = 0.5625000


rulesSize3 <- subset(rules, subset = size(lhs) == 3)
length(rulesSize3)
inspect(head(sort(rulesSize3, by="confidence"),20))
#in first 20
#max conf = 1, min conf = 1

# We observe that the average confidence of “shorter” rules is lower than the average confidence of “longer” rules. 
# This is because the more items we have on the left side of the rule (antecedent), the rule is more specific and 
# hence it has more strength.

########################################
#4. Experiments for conclusion
#Experiments for conclusion
rulesConf1Edible <- subset(rules, subset =  confidence == 1 & rhs %in% "Class=edible")

length(rulesConf1Edible) ##20

inspect(head(sort(rulesConf1Edible, by="support"),20))
# [1]  {Odor=none,GillSize=broad,RingNumber=one}                          => {Class=edible} 0.3308715 1          1.930608 2688 
# [2]  {Odor=none,StalkShape=tapering}                                    => {Class=edible} 0.3072378 1          1.930608 2496 
# [3]  {Odor=none,GillSpace=close,SurfaceAboveRing=smooth,RingNumber=one} => {Class=edible} 0.2540620 1          1.930608 2064 
# [4]  {Odor=none,GillSpace=close,RingNumber=one,RingType=pendant}        => {Class=edible} 0.2481536 1          1.930608 2016 
# [5]  {Odor=none,GillSpace=close,SurfaceBelowRing=smooth,RingNumber=one} => {Class=edible} 0.2481536 1          1.930608 2016 
# [6]  {Bruises=bruises,Habitat=woods}                                    => {Class=edible} 0.2245199 1          1.930608 1824 
# [7]  {StalkShape=tapering,StalkRoot=bulbous,Habitat=woods}              => {Class=edible} 0.2245199 1          1.930608 1824 
# [8]  {StalkShape=tapering,RingType=pendant,Habitat=woods}               => {Class=edible} 0.2245199 1          1.930608 1824 
# [9]  {Odor=none,SurfaceAboveRing=smooth,Habitat=woods}                  => {Class=edible} 0.2186115 1          1.930608 1776 
# [10] {Odor=none,StalkRoot=bulbous,Habitat=woods}                        => {Class=edible} 0.2136878 1          1.930608 1736 
# [11] {Odor=none,RingType=pendant,Habitat=woods}                         => {Class=edible} 0.2136878 1          1.930608 1736 
# [12] {Odor=none,GillSize=broad,Habitat=woods}                           => {Class=edible} 0.2136878 1          1.930608 1736 
# [13] {GillSize=broad,RingType=pendant,Habitat=woods}                    => {Class=edible} 0.2136878 1          1.930608 1736 
# [14] {Odor=none,SurfaceBelowRing=smooth,Habitat=woods}                  => {Class=edible} 0.2127031 1          1.930608 1728 
# [15] {GillSize=broad,StalkShape=tapering,Habitat=woods}                 => {Class=edible} 0.2127031 1          1.930608 1728 
# [16] {GillSize=broad,SurfaceBelowRing=smooth,Habitat=woods}             => {Class=edible} 0.2127031 1          1.930608 1728 
# [17] {GillSize=broad,SurfaceAboveRing=smooth,Habitat=woods}             => {Class=edible} 0.2127031 1          1.930608 1728 
# [18] {Bruises=bruises,Odor=none,GillSpace=close,RingNumber=one}         => {Class=edible} 0.2127031 1          1.930608 1728 
# [19] {Odor=none,GillSpace=close,StalkRoot=bulbous,RingNumber=one}       => {Class=edible} 0.2127031 1          1.930608 1728 
# [20] {GillSize=broad,Spore=brown}                                       => {Class=edible} 0.2028557 1          1.930608 1648

rulesConf1Poisonous <- subset(rules, subset =  confidence == 1 & rhs %in% "Class=poisonous")

length(rulesConf1Poisonous) ##20

inspect(head(sort(rulesConf1Poisonous, by="support"),20))
# [1]  {GillSpace=close,SurfaceAboveRing=silky}                  => {Class=poisonous} 0.2742491 1          2.074566 2228 
# [2]  {SurfaceAboveRing=silky,RingNumber=one}                   => {Class=poisonous} 0.2698178 1          2.074566 2192 
# [3]  {Odor=foul}                                               => {Class=poisonous} 0.2658789 1          2.074566 2160 
# [4]  {GillSpace=close,SurfaceBelowRing=silky}                  => {Class=poisonous} 0.2658789 1          2.074566 2160 
# [5]  {SurfaceBelowRing=silky,RingNumber=one}                   => {Class=poisonous} 0.2658789 1          2.074566 2160 
# [6]  {GillSpace=close,GillSize=narrow,Spore=white}             => {Class=poisonous} 0.2166420 1          2.074566 1760 
# [7]  {GillSpace=close,RingNumber=one,Spore=white}              => {Class=poisonous} 0.2166420 1          2.074566 1760 
# [8]  {GillSpace=close,GillSize=narrow,RingType=evanescent}     => {Class=poisonous} 0.2166420 1          2.074566 1760 
# [9]  {Bruises=no,GillSpace=close,RingType=evanescent}          => {Class=poisonous} 0.2166420 1          2.074566 1760 
# [10] {GillSpace=close,RingNumber=one,RingType=evanescent}      => {Class=poisonous} 0.2166420 1          2.074566 1760 
# [11] {GillColor=buff}                                          => {Class=poisonous} 0.2127031 1          2.074566 1728 
# [12] {StalkShape=tapering,Spore=white}                         => {Class=poisonous} 0.2127031 1          2.074566 1728 
# [13] {GillSize=narrow,StalkShape=tapering,RingType=evanescent} => {Class=poisonous} 0.2127031 1          2.074566 1728 
# [14] {Bruises=no,GillSize=narrow,StalkShape=tapering}          => {Class=poisonous} 0.2127031 1          2.074566 1728 
# [15] {GillSpace=close,GillSize=narrow,StalkShape=tapering}     => {Class=poisonous} 0.2127031 1          2.074566 1728 
# [16] {GillSpace=close,StalkShape=tapering,RingType=evanescent} => {Class=poisonous} 0.2127031 1          2.074566 1728 
# [17] {Bruises=no,GillSpace=close,StalkShape=tapering}          => {Class=poisonous} 0.2127031 1          2.074566 1728

rulesSize1 <- subset(rules, subset = size(lhs) == 1)
length(rulesSize1)
inspect(head(sort(rulesSize1, by="confidence"),10)) 
# [1]  {GillColor=buff}         => {Class=poisonous} 0.2127031 1.0000000  2.074566 1728 
# [2]  {Odor=foul}              => {Class=poisonous} 0.2658789 1.0000000  2.074566 2160 
# [3]  {Odor=none}              => {Class=edible}    0.4194978 0.9659864  1.864941 3408 
# [4]  {SurfaceAboveRing=silky} => {Class=poisonous} 0.2742491 0.9392917  1.948623 2228 
# [5]  {SurfaceBelowRing=silky} => {Class=poisonous} 0.2658789 0.9375000  1.944906 2160 
# [6]  {Spore=brown}            => {Class=edible}    0.2146726 0.8861789  1.710864 1744 
# [7]  {GillSize=narrow}        => {Class=poisonous} 0.2737568 0.8853503  1.836718 2224 
# [8]  {Spore=black}            => {Class=edible}    0.2028557 0.8803419  1.699595 1648 
# [9]  {Bruises=bruises}        => {Class=edible}    0.3387494 0.8151659  1.573766 2752 
# [10] {RingType=pendant}       => {Class=edible}    0.3879862 0.7943548  1.533588 3152 

# Using the Mushroom dataset, we managed to get interesting association rules about the edibility of different mushrooms.
# Assuming the credibility of the dataset, we can draw useful rules for selecting edible mushrooms. For example, we found
# a rule which says that if a mushroom has no odor, a broad gill size and one ring, then it is edible, with the support 
# of 33%. Another rule we found is if a mushroom has a foul odor then it is poisonous, with support of 26.5%. However, if
# we can not trust the dataset 100%, we can still find some useful information about the edibility of certain mushroom 
# types, which can help direct us into one of the directions when making a decision about a mushroom. For example, 
# if a mushroom has no odor, then we can say that it is edible with the confidence of 96.6%. 
# 
# These kinds of analysis are very useful when we want to gain some knowledge and understanding of the data and the
# things it describes. However, we have to take possible errors of data into account and be aware of the techniques
# which the data was collected, processed and transformed, that can affect its authenticity and credibility.