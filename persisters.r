load("~/Desktop/Research/opsci08_15.rda")
names(opsci)

#library added by me for filtering rows in table
library(dplyr)

library(car)
library(psych)
library(bitops)
library(igraph)
library(httr)
library(RCurl)
library(memoise)
library(whisker)
library(devtools)
library(rCharts)
library(rms)
library(pscl)

################################################################################
######################PERSISTERS VS QUITTERS ANALYSIS###########################

#total of students that chose physics in college
sum(opsci$q1colphys==1)

#counting total amount of NA that chose physics 
table(opsci$q1colphys, opsci$q26gender, useNA="always")

#Vectorizing number of students that chose physics in midddleschool or beggining of highschool or end of highschool 
precol <- opsci$q1msphys==1 | opsci$q1bhsphys==1 | opsci$q1ehsphys==1

#Vectorizing number of students that chose physics in college and in midddleschool or beggining of highschool or end of highschool
persister <- opsci$q1colphys==1 & precol==TRUE
#Total number of persisters/ The equation adds everytime the persister is True (no NA)
sum(persister, na.rm=TRUE) 



#Vectorizing Total number of students that chose physics before college at least once and did not chose physics in college
quitter <- opsci$q1colphys==0 & precol==TRUE 
#Total number of quitter(no NA)
sum(quitter, na.rm=TRUE) 

#matrix of quitters
quitter[1:500]




#############################MAKING SURE THE CODE ABOVE IS CORRECT##########################

#Total number of students that chose physics in middleschool 
sum(opsci$q1msphys,na.rm=1) 
#Total number of students that chose physics in beginning of highschool
sum(opsci$q1bhsphys,na.rm=1) 
#Total number of students that chose physics in middleschool 
sum(opsci$q1ehsphys,na.rm=1) 
#Total number of students that chose physics in collegue 
sum(opsci$q1colphys)

#### numbers are incorrect in comparison with doc files


##############################FEMALE BASIC ANALYSIS ######################

#Total female that chose physics in collegue 
femcol <-opsci$q26gender==0 & opsci$q1colphys==1
sum(femcol== TRUE, na.rm =TRUE) 
#double check above info
table(opsci$q1colphys,opsci$q26gender)

#Total female persisters (persister definition above)
fempersister <-opsci$q26gender==0 & persister==1
sum(fempersister ==TRUE, na.rm =TRUE) 

#Total female quitters (quitter definition above)
femquitter <-opsci$q26gender==0 & quitter==1
sum(femquitter==TRUE, na.rm=TRUE) 


############################FEMALE PERSISTER################

#ID of females that chose physics in college 
subset(opsci, opsci$q26gender==0 & opsci$q1colphys==1,id)

#creating a new table with only females that chose physics in college 
table<- filter(opsci, opsci$q26gender ==0 & opsci$q1colphys==1 )


#creating a new table with female persisters
table1 <- filter(opsci, fempersister)



#Total of english not as primary language
sum(table$q29lang==0, na.rm=TRUE)



#total number of support for science/ very supportive
sum(table$q31homesup==5)
#total number of support for science/ not supportive
sum(table$q31homesup==1)

########################FEMALE QUITTER#################

#creating a new table of female quitters ( they took physics at some point but not in college) 
table2<- filter(opsci, femquitter)

#########################NEWCOMERS (college physics but not previous physics) #######

#newcomer (females and males and NA)
newcom<-opsci$q1colphys==1 & precol==FALSE
sum(newcom)

#variable newcomer female
femnew <- femcol & precol==FALSE 
#sum of newcomer female, excluding NA
sum(femnew==TRUE,na.rm=TRUE )

print(femcol)

#variable newcomer male
malnew<- opsci$q26gender==1 & newcom==1
#sum of newcomer male, excluding NA
sum(malnew, na.rm=TRUE)

#variable newcomer NA
nanew<- is.na(opsci$q26gender) & newcom==1 
sum(nanew)

###############################COMPARISONS#############

#dtem identity by question 21
idstem<- table$q21sinterest+table$q21spersonself+table$q21scurious+table$q21spersonteach+table$q21spersonfam+table$q21senjoy+table$q21shelp+table$q21spersonfriend 
print(idstem)

#creating a variable to defined persisters vs non persisters 
persistance <- opsci$q1msphys==1   



########SOME TEST I WAS TRYING TO DO#########




#home support comparison female in college physics vs quitters
wilcox.test(table$q31homesup,table2$q31homesup)

wilcox.test(table$q34edmoth,table2$q34edmoth)


# referring back to a column based on its position
column<- table[,3]

#testing wilcox test based in position
test<- wilcox.test(table[,2], table2[,2])
print(test)

# trying to name columns in a different file in a loop maner in each column in table 1 
tab1col<- lapply (4:6, function(x){prop.paste0(table1[,x])})
print(tab1col)

##call each column by position in table2 by creating a loop in its position

tab2col<- lapply (4:6, function(x){paste0(table2[,x])})
print(tab2col)




prop.table(table(tab1col,tab2col),2)
wilcox.test(tab1col,tab2col)
print(test)

# apply the test in a loop to each set of cotlumns 
apply( )


#################NOTES/ QUESTIONS##############




#number of NA anserws in gender/ why is there so many how this affects the data?
sum(is.na(opsci$q26gender))
#the gender NA is clearly seen 
print(opsci$q26gender)
#how many of NA are quitters/persisters? 









