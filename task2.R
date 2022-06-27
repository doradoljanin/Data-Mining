# authors: Dora Doljanin, Filip Pavičić
# EDAMI Task2: Sequential rules discovery
# The aim of the task is to find if occurrence of hypoglycemic symptoms can be predicted based on other events.

library(arules)
library(arulesSequences)

code_description <- c(
  'id_33' = 'Regular insulin dose',
  'id_34' = 'NPH insulin dose',
  'id_35' = 'UltraLente insulin dose',
  'id_48' = 'Unspecified blood glucose measurement',
  'id_57' = 'Unspecified blood glucose measurement',
  'id_58' = 'Pre-breakfast blood glucose measurement',
  'id_59' = 'Post-breakfast blood glucose measurement',
  'id_60' = 'Pre-lunch blood glucose measurement',
  'id_61' = 'Post-lunch blood glucose measurement',
  'id_62' = 'Pre-supper blood glucose measurement',
  'id_63' = 'Post-supper blood glucose measurement',
  'id_64' = 'Pre-snack blood glucose measurement',
  'id_65' = 'Hypoglycemic symptoms',
  'id_66' = 'Typical meal ingestion',
  'id_67' = 'More-than-usual meal ingestion',
  'id_68' = 'Less-than-usual meal ingestion',
  'id_69' = 'Typical exercise activity',
  'id_70' = 'More-than-usual exercise activity',
  'id_71' = 'Less-than-usual exercise activity',
  'id_72' = 'Unspecified special event'
)

# Data reading and analysis
#read data with sequences - Diabetes patient records
download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/diab_trans.data','diab_trans.data')
#reading data - into dataframe
diab.df <- read.csv("diab_trans.data", header=TRUE, stringsAsFactors = FALSE)
View(diab.df)

?is.na
View(diab.df[which(is.na(diab.df), arr.ind=TRUE)])
View(diab.df[is.na(diab.df$value),])
# we find out that there are 8 NAs, only of the attribute value for the glucose measurements
# those rows are not useful for us so we will remove them
# Apply na.omit() function
diab.df <- na.omit(diab.df)
# now we have 25137 rows

summary(diab.df)

# we will create a new column named "item" which will contain an event description 
# and the corresponding value only for insulin doses and glucose measurements
# firstly we set all values in the new column "item" to NA
diab.df$item = NA

# Regular insulin (Novolin R) is also known as short-acting insulin
# Neutral Protamine Hagedorn (NPH) insulin is an intermediate-acting insulin
# Ultralente insulin is a long-acting form of insulin

# Discretization of values associated with insulin dose and adding an event description
# discretization breakpoints are: lower outlier, 1st quartile, 3rd quartile, upper quartile
insuline_items = c('id_33','id_34','id_35')

for(item in insuline_items){
  points = boxplot(diab.df[diab.df$code == item,]$value)$stats
  lower = points[1,1]
  diab.df$item[which(diab.df$code == item & diab.df$value < lower)] = paste(code_description[item],' [<',lower,')',sep = "")
  for(i in 2:length(points)) {
    upper = points[i,1]
    diab.df$item[which(diab.df$code == item & diab.df$value >= lower & diab.df$value < upper)] = paste(code_description[item],'[',lower,',',upper,')',sep = "")
    lower = upper
  }
  diab.df$item[which(diab.df$code == item & diab.df$value >= lower)]= paste(code_description[item],'[>',lower, ']',sep = "")
}

# Discretization of values associated with glucose measurement and adding an event description
# Division into ranges based on the given borders

# The blood glucose target range for diabetics:
# 80–130 mg/dL before meals 
# less than 180 mg/dL (two hours) after meals

# Codes:
# 48 = Unspecified blood glucose measurement
# 57 = Unspecified blood glucose measurement
# Unspecified: 48, 57
# Range: 80 - 140
# 
# 58 = Pre-breakfast blood glucose measurement
# 60 = Pre-lunch blood glucose measurement
# 62 = Pre-supper blood glucose measurement
# 64 = Pre-snack blood glucose measurement
# Pre-meal: 58, 60, 62, 64
# Range: 80 - 130
# 
# 59 = Post-breakfast blood glucose measurement
# 61 = Post-lunch blood glucose measurement
# 63 = Post-supper blood glucose measurement
# Post-meal: 59, 61, 63
# Range: 80 - 180

# Discretization of values associated with glucose dose measured before a meal and adding an event description

glucose_ids = list(
  c("id_58","id_60","id_62","id_64"),
  c("id_48","id_57"),
  c("id_59","id_61","id_63")
)

glucose_names = c(
  'before a meal',
  'unspecified time',
  'after a meal'
)

glucose_ranges = list(
  c(80,130),
  c(80,140),
  c(80,180)
)

for(index in 1:length(glucose_ids)){
  ids = glucose_ids[[index]]
  points = glucose_ranges[[index]]
  lower = points[1]
  upper = points[2]
  diab.df$item[which(diab.df$code %in% ids & diab.df$value < lower)] = paste('Low glucose ',glucose_names[index],sep = "")
  diab.df$item[which(diab.df$code %in% ids & diab.df$value >= lower & diab.df$value < upper)] = paste('Normal glucose ',glucose_names[index],sep = "")
  diab.df$item[which(diab.df$code %in% ids & diab.df$value >= upper)]= paste('High glucose ',glucose_names[index],sep = "")
}

# for other events we add an event description

other_items = c('id_65','id_66','id_67','id_68','id_69','id_70','id_71','id_72')

for(item in other_items){
  diab.df$item[which(diab.df$code == item & diab.df$value == 0)] = code_description[item]
}

unique(diab.df$item)
# we observe rows in the data with code values "id_56" and "id_36"
# there are no description for these codes in the dataset description so we do not know what they represent
# we decide to remove those rows
diab.df = diab.df[diab.df$code != "id_56" & diab.df$code != "id_36",]
unique(diab.df$item)
View(diab.df[is.na(diab.df$item),])

# after discretization of values and the creation of a new column "item", we do not need the columns "code" and "value"
# we delete them
diab.df$code = NULL
diab.df$value = NULL
# we sort the data based on patient_id,time_sek
diab.df <- diab.df[order(diab.df$patient_id,diab.df$time_sek),]

summary(diab.df)
head(diab.df)
View(diab.df)
# we have 25017 rows total

# saving data into a file  - removing the header line
write.table(diab.df, "diab_trans2.data", sep = ";" , row.names = FALSE, col.names = FALSE, quote = FALSE )

?read_baskets
#reading data in transactional form
diabSeq <- read_baskets(con = "diab_trans2.data", sep =";", info = c("sequenceID","eventID"))
View(as(diabSeq,"data.frame"))
View(unique(as(diabSeq,"data.frame")))
str(diabSeq)

#information about data concerning times 
?timeFrequency
timeSeq  = as(diabSeq,"timedsequences")
freqT = timeFrequency(timeSeq, "times")
head(freqT)

spanT= timeFrequency(timeSeq, "span")
spanT

#calculation of frequency of items
?itemFrequency
freqItem = itemFrequency(diabSeq)
freqItem = sort(freqItem, decreasing = TRUE )
head(freqItem,20)

# Frequent sequences discovery - finding frequent sequential patterns with the cSPADE algorithm
?cspade
# time(eventid) in the diab_trans.data set is given as a number of seconds from some date
# we set the following parameters:
# support 30%
# maxsize 4
# maxgap 604800 seconds (7 days)
# maxlen 5

seqParam = new ("SPparameter",support = 0.3, maxsize = 4,mingap = 1, maxgap =172800, maxlen = 6 )
patSeq= cspade(diabSeq,seqParam, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))
#information about discoverd sequences
summary(patSeq)
inspect(head(patSeq,100))
#patterns with more than one element
seq2elem <- patSeq[size(patSeq)>1]
length(seq2elem)
inspect(head(seq2elem, 20))

#discovery of sequential rules
seqRules = ruleInduction(patSeq,confidence = 0.8)
seqRules <- seqRules[is.redundant(seqRules) == FALSE]

length(seqRules)
#summary for the set of rules
summary(seqRules)
#view of of rules
inspect(head(seqRules,10))

# we want to find if occurrence of hypoglycemic symptoms can be predicted based on other events
# we want to find patterns with the item "Hypoglycemic symptoms"

#selecting interesting rules
rulesI = subset(seqRules, rhs(seqRules) %in% c('Hypoglycemic symptoms') & !(lhs(seqRules) %in% c("Hypoglycemic symptoms")))
inspect(head(sort(rulesI, by="confidence"),30))

# Medical data show that people with diabetes who take too much medication (insulin) 
# or take their usual amount but then eat less ("Less-than-usual meal ingestion")
# or exercise more than usual "More-than-usual exercise activity" - can develop hypoglycemia,
# so these situations will be particularly interesting to us

# the following code lines produce an error for which we have not figured out the reason so we comment them
# rulesI = subset(seqRules, rhs(seqRules) %in% c('Low glucose before a meal'))
# inspect(rulesI)
# 
# rulesI = subset(seqRules, rhs(seqRules) %in% c('Low glucose after a meal'))
# inspect(rulesI)
#
# rulesI = subset(seqRules, rhs(seqRules) %in% c("Hypoglycemic symptoms") & lhs(seqRules) %in% c("Regular insulin dose[3,6)"))
# inspect(rulesI)
# 
# rulesI = subset(seqRules, rhs(seqRules) %in% c("Hypoglycemic symptoms") & lhs(seqRules) %in% c("Regular insulin dose[>15]"))
# inspect(rulesI)
# 
# rulesI = subset(seqRules, rhs(seqRules) %in% c("Hypoglycemic symptoms") & lhs(seqRules) %in% c("NPH insulin dose[>36]"))
# inspect(rulesI)
# 
# rulesI = subset(seqRules, rhs(seqRules) %in% c("Hypoglycemic symptoms") & lhs(seqRules) %in% c("UltraLente insulin dose [>30]"))
# inspect(rulesI)
# 
# rulesI = subset(seqRules, rhs(seqRules) %in% c("Hypoglycemic symptoms") & lhs(seqRules) %in% c("Less-than-usual meal ingestion"))
# inspect(rulesI)

rulesI = subset(seqRules, lhs(seqRules) %in% c('More-than-usual exercise activity') && rhs(seqRules) %in% c('Hypoglycemic symptoms'))
inspect(rulesI)

# the following rules were interesting to us:

# <{Normal glucose before a meal},                                        
# {Normal glucose before a meal},                                        
# {Regular insulin dose[0,3)},                                           
# {NPH insulin dose[1,6)},                                               
# {NPH insulin dose[6,12)}>       => <{Hypoglycemic symptoms}>        0.3030303  0.8695652 1.551116
# 
# we can see that taking several doses of insulin (short-acting insulin and intermediate-acting insulin) after measuring 
# a normal level of glucose in the blood leads to Hypoglycemic symptoms
# with the support of 30%, confidence of almost 87% and lift of 1.55
# 
# <{Normal glucose before a meal},                                        
# {Low glucose before a meal},                                           
# {NPH insulin dose[1,6)},                                               
# {High glucose before a meal},                                          
# {Regular insulin dose[0,3)}>    => <{Hypoglycemic symptoms}>        0.3181818  0.8400000 1.498378
#
# we can also see that Hypoglycemic symptoms occur when taking several insulin doses (short-acting insulin and intermediate-acting insulin)
# after measuring normal and low levels of glucose
# with the support of 31%, confidence of 84% and lift of 1.49

# we expected to find some rules that show the effect of "More-than-usual exercise activity" and "Less-than-usual meal ingestion"
# on the occurrence of Hypoglycemia, however the
# results did not contain such rules
# we try adjusting the parameters to find some more interesting rules

seqParam = new ("SPparameter",support = 0.1, maxsize = 4,mingap = 1, maxgap =172800, maxlen = 4 )
patSeq= cspade(diabSeq,seqParam, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))
summary(patSeq)
inspect(head(patSeq,100))
seqRules = ruleInduction(patSeq,confidence = 0.8)
seqRules <- seqRules[is.redundant(seqRules) == FALSE]
length(seqRules)
summary(seqRules)
inspect(head(seqRules,10))

#selecting interesting rules
rulesI = subset(seqRules, rhs(seqRules) %in% c('Hypoglycemic symptoms') & !(lhs(seqRules) %in% c("Hypoglycemic symptoms")))
inspect(head(sort(rulesI, by="confidence"),30))

rulesII = subset(seqRules, lhs(seqRules) %in% c('More-than-usual exercise activity') & rhs(seqRules) %in% c('Hypoglycemic symptoms'))
inspect(rulesII)

# we found some interesting rules:
# 
# <{More-than-usual exercise activity},                                             
# {NPH insulin dose[18,36)}>           => <{Hypoglycemic symptoms}>             0.1060606  1.0000000 1.783784
#
# we can see that More-than-usual exercise activity followed by a higher dose of intermediate-acting insulin leads to Hypoglycemic symptoms
# with the support of 10%, confidence of 100% and lift of 1.78

# <{Hypoglycemic symptoms},                                                         
# {More-than-usual exercise activity},                                             
# {Normal glucose before a meal}>      => <{Hypoglycemic symptoms}>             0.1212121     0.8000 1.427027
#
# we can also see that having More-than-usual exercise activity after having experienced Hypoglycemic symptoms
# leads to Hypoglycemic symptoms with the support of 12%, confidence of 80% and lift of 1.42
# even if the pre-meal glucose level was normal after that

rulesIII = subset(seqRules, rhs(seqRules) %in% c("Hypoglycemic symptoms") & lhs(seqRules) %in% c("Less-than-usual meal ingestion"))
inspect(rulesIII)

# we still did not manage to find rules that could show us the effect of "Less-than-usual meal ingestion" on developing "Hypoglycemic symptoms"

# Conclusion

# We conclude that the occurrence of hypoglycemic symptoms can be predicted based on other events,
# for example taking several insuline doses after measuring normal or low glucose levels 
# or having a more-than-usual exercise activity followed by taking an insulin dose
# These findings may be helpful to people with diabetes because they can help them avoid the sequences
# of events that may lead to experiencing hypoglycemic symptoms.