library(readxl)
library(stringr)
library(naniar)
library(plyr)
library(RColorBrewer)
library(randomcoloR)
library(tidyverse)

mainrn <- read.csv("surveydata.csv") ## save coconut survey as csv and input data here
mainrn <- mainrn %>% select(-contains("version"))
mainrn$X_submission_time <- gsub("T.*", "\\1", mainrn$X_submission_time)
mainrn$X_submission_time <-as.Date(mainrn$X_submission_time)

colnames(mainrn) <- make.names(colnames(mainrn))
colname <- read.csv("name2.csv")
col <- colname$short_name
colnames(mainrn) <- col

### 90 means not recovered from the shock
mainrn <- mainrn %>% 
  mutate(ShockRecoverOrNot=
           ifelse(is.na(V4011.month.recover.shock),NA,
                  ifelse(V4011.month.recover.shock==90,"No","Yes")))
mainrn$V4011.month.recover.shock[mainrn$V4011.month.recover.shock ==90] <- NA
mainrn <- mutate(mainrn, enum_hhid = paste0(V1102Interviewer.Name,"/",V1108hhID))

### correction point----
#62194822/Journie Mae M. Vicentino/DMM 004 How important is coconut crop to your household income? V2214 Second most important crop -> First most important crop
which(mainrn$X_id==62194822)
mainrn$enum_hhid[mainrn$X_id==62194822]
mainrn$V2214coco.importance.to.income[mainrn$X_id==62194822]
mainrn$V2214coco.importance.to.income[mainrn$X_id==62194822] <- "First most important crop"

#62335442 Rizalde Jr. P. Valendez/DMM001 -> less than adequate
mainrn$enum_hhid[mainrn$X_id==62335442]
mainrn$V3117income.adequate.consumption[mainrn$X_id==62335442]
mainrn$V3117income.adequate.consumption[mainrn$X_id==62335442] <- "It was less than adequate for your household's needs"


#####mutation and adding variable-----

mainrn <- mainrn %>% mutate(V2220coco_output_kg= 
                          ifelse(V2221coco.output.unit=="Kg",V2220coco.output,
                                 ifelse(V2221coco.output.unit=="Tons",V2220coco.output*1000,NA)))

mainrn <- mainrn %>% mutate(V2220coco_output_nut=
                          ifelse(V2221coco.output.unit=="Nuts",V2220coco.output,NA))

mainrn <- mainrn %>% mutate(V2220coco_output_others=
                          ifelse(V2221coco.output.unit=="Other, specify",V2220coco.output,NA))    

mainrn <- mainrn %>% mutate(V2227cal_coco_kg_per_tree= V2220coco_output_kg/(V2225coco.productive.percent*(V2226no.coco.tree/100)))
mainrn <- mainrn %>% mutate(V2227cal_coco_nut_per_tree= V2220coco_output_nut/(V2225coco.productive.percent*(V2226no.coco.tree/100)))
mainrn <- mainrn %>% mutate(V2227cal_coco_others_per_tree= V2220coco_output_others/(V2225coco.productive.percent*(V2226no.coco.tree/100)))
mainrn$V2227cal_coco_nut_per_tree

###HH Satified with cacao has reverse questions compared to other 2 groups -> recode them and combine with other 2 groups
table(mainrn$V2404struggle.satisfied.w.cacao)
table(mainrn$V2209coco.cacao.crop.system) #Intercrop coconut with non-cacao crops only + Monocrop coconut, no cacao = no cacao
mainrn <- mainrn %>% mutate(cacao_Satisfy.Strg.NoCC=
                    ifelse(V2404struggle.satisfied.w.cacao=="Satisfied","Satisfied",
                           ifelse(V2404struggle.satisfied.w.cacao=="Struggling","Struggling","No Cacao")))
table(mainrn$cacao_Satisfy.Strg.NoCC)

CC.satify.reverse.ans <- mainrn %>% select(X_id,starts_with("Reverse.")) 
names(CC.satify.reverse.ans) <- gsub("Reverse\\.","",names(CC.satify.reverse.ans))
table(CC.satify.reverse.ans$Farmops.not.enough.land.intercr)
table(CC.satify.reverse.ans$Farmops.difficulty.machinery)

#test
#CC.satify.reverse.ans <- data.frame(
#  ID= c("a","b","c"),
#  V1 = c("Strongly agree", "Strongly disagree","No answer"),
#  V2 = c(NA, "Agree","Disagree"))
#CC.satify.reverse.ans

idata <- CC.satify.reverse.ans
idata <- as.data.frame(idata)
old_dat <- idata # just for check data
nsample <- nrow(idata) 
rev_ques <- seq(2,ncol(idata))
#rev_ques <- c(1,3) # select columns (i.e. questions for reversing)

for (iques in c(rev_ques)){
  for (isample in seq(nsample)){
    flag <- 0
    if (is.na(idata[isample,iques])==FALSE){
      #print(idata[isample,iques])
      if (flag == 0){if (idata[isample,iques]=="Agree"){idata[isample,iques]<-"Disagree";flag <-1}}
      if (flag == 0){if (idata[isample,iques]=="Disagree"){idata[isample,iques]<-"Agree";flag <-1}}
      if (flag == 0){if (idata[isample,iques]=="Strongly agree"){idata[isample,iques]<-"Strongly disagree";flag <-1}}
      if (flag == 0){if (idata[isample,iques]=="Strongly disagree"){idata[isample,iques]<-"Strongly agree";flag <-1}}
    } 
  }
}
CC.satify.reverse.ans <- idata
#row <- 1
#col <- 2
#for (row in seq(1,nrow(CC.satify.reverse.ans))){
#for (col in seq(2,ncol(CC.satify.reverse.ans))){
#  if (CC.satify.reverse.ans[row,col]=="Strongly agree"){
#    CC.satify.reverse.ans[row,col] <- "Strongly dis"
#  } 
#  if(CC.satify.reverse.ans[row,col]=="Agree"){
#    CC.satify.reverse.ans[row,col] <- "Dis"
#  }
#  if(CC.satify.reverse.ans[row,col]=="Disagree"){
#  CC.satify.reverse.ans[row,col] <- "Agree"
#  }
#  if(CC.satify.reverse.ans[row,col]=="Strongly disagree"){
#    CC.satify.reverse.ans[row,col] <- "Strongly agree"
#  }
#  else{
#    CC.satify.reverse.ans[row,col] <- CC.satify.reverse.ans[row,col]
#  }
#}}
#CC.satify.reverse.ans[] <-
#  lapply(CC.satify.reverse.ans, function(x) gsub("Dis", "Disagree", x)) 
#CC.satify.reverse.ans[] <-
#  lapply(CC.satify.reverse.ans, function(x) gsub("Strongly dis", "Strongly disagree", x)) 


table(CC.satify.reverse.ans$Farmops.not.enough.land.intercr)
table(CC.satify.reverse.ans$Farmops.difficulty.machinery)

CC.strg <- mainrn %>% select(X_id,starts_with("Strg.")) 
names(CC.strg) <- gsub("Strg\\.","",names(CC.strg)) 
CC.none <- mainrn %>% select(X_id,starts_with("NCC.")) 
names(CC.none) <- gsub("NCC\\.","",names(CC.none))

cacaogroup <- plyr::rbind.fill(CC.strg,CC.satify.reverse.ans,CC.none)
cacaogroup <- cacaogroup %>% replace_with_na_all(condition = ~.x == "") 
cacaogroup <- cacaogroup %>% na.omit()
cacaogroup$X_id <- as.numeric(cacaogroup$X_id)
#names(cacaogroup) <- gsub("^","Cb.",names(cacaogroup))
mainrn <- mainrn %>% left_join(cacaogroup)


#### Recode comments V2804

mainrn$V2804reason.not.agree.neighbor.rent.land[mainrn$V2804reason.not.agree.neighbor.rent.land==""] <- NA
mainrn$V2804reason.not.agree.neighbor.rent.land <- tolower(mainrn$V2804reason.not.agree.neighbor.rent.land)
sum(is.na(mainrn$V2804reason.not.agree.neighbor.rent.land))

mainrn <- mainrn %>% 
  mutate(V2804reasonNotRentToNeighbor=
           ifelse(is.na(V2804reason.not.agree.neighbor.rent.land)|V2804reason.not.agree.neighbor.rent.land=="",NA,
           ifelse(grepl("nt enough|n't enough|not enough|little|small|low|yield more than|agree at|farmer can yield more than|not satisfied",V2804reason.not.agree.neighbor.rent.land)&
                  grepl("amount|20,000|20000|offer|at 50,000 pesos|from its crops if payment is per year",V2804reason.not.agree.neighbor.rent.land),"20000peso is not enough",
                  ifelse(grepl("land|soil|area.*rockie",V2804reason.not.agree.neighbor.rent.land)&grepl("not suitable|nt suitable|n't suitable",V2804reason.not.agree.neighbor.rent.land),"Soil not suitable",
                  ifelse(grepl("plant.*self|himself|themselves|herself|own use|own cacao|own plant|own crop|needs capital to finance cacao production|i will be the one to|cultivate the land himself, and for him, it is better to have a share of income from cacao|depends on the agreement|i will be the one|am the one to|plant.*on my own|intercrop.*n my own|want.*land.*for his own",
                               V2804reason.not.agree.neighbor.rent.land),"Rather plant/use land themselves",
                  ifelse(grepl("already.*cacao",V2804reason.not.agree.neighbor.rent.land),"Already has cacao",
                         ifelse(grepl("banana",V2804reason.not.agree.neighbor.rent.land),"Already has banana/ Prefer banana",
                                ifelse(grepl("area.*small|land.*small|no.*space|land.*given by|sell.*land|land.*not.*to their name",V2804reason.not.agree.neighbor.rent.land),"No space/ small land/ Not their land",
                                       ifelse(grepl(".*ifficult|.*omplicat|.*igh.* maintenance|chemicals",V2804reason.not.agree.neighbor.rent.land),
                                       "Afraid of dificulty for coconut, maintenance, chemicals",
                                       ifelse(grepl("son|children|relative",V2804reason.not.agree.neighbor.rent.land),"Rather leave to children/ relatives",
                                              ifelse(grepl("not allow|ont allow|oesn.*like|don.*like|on't want|ont want|as of this time no|not for rent|don’t want anybody to manage her area|find.*joy in.* farm|doesn't want others to plant|contented|no need|insignificant compared.*income|rather stay as it is",V2804reason.not.agree.neighbor.rent.land),
                                                     "Just don't want to rent/ already contented","Other")))))))))))

test <- select(mainrn, X_id, contains("V2804"))
table(test$V2804reasonNotRentToNeighbor)

#mainrn$cacao_Satisfy.Strg.NoCC[which(mainrn$V2804reason.not.agree.neighbor.rent.land=="Its insignificant compared to my income")]
##V2805 too few categories

mainrn$V2805reason.not.agree.trader.rent.land <- tolower(mainrn$V2805reason.not.agree.trader.rent.land)
mainrn <- mainrn %>% 
  mutate(V2805reasonNotRentToTrader=
           ifelse(is.na(V2805reason.not.agree.trader.rent.land)|V2805reason.not.agree.trader.rent.land=="",NA,
                  ifelse(grepl("neighbor than.*cacao trader",V2805reason.not.agree.trader.rent.land),"Prefer/trust neighbor more than trader",
                         ifelse(grepl("chemicals|deteriorate|destroy",V2805reason.not.agree.trader.rent.land),"Afraid of chemicals used on land",
                                ifelse(grepl("want.*a higher offer|should offer more than 20,000",V2805reason.not.agree.trader.rent.land),"20000peso is not enough",
                                       "Other")))))
                  
test <- select(mainrn, X_id, contains("V2805")) 
table(test$V2805reasonNotRentToTrader)                  
#### Recode comments at the end of file
sum(is.na(mainrn$V6006notes.or.comments.from.interview.))
mainrn$V6006notes.or.comments.from.interview. <- tolower(mainrn$V6006notes.or.comments.from.interview.)

mainrn <- mainrn %>% 
  mutate(V6006FinalComment=
           ifelse(is.na(V6006notes.or.comments.from.interview.)|V6006notes.or.comments.from.interview.=="",NA,
                  ifelse(grepl("none|no comment",V6006notes.or.comments.from.interview.),"No comment",
                         ifelse(grepl("drought|typhoon|drouhgt|extreme event|flood|calamity|loss of asset due to shock",V6006notes.or.comments.from.interview.),
                                "Drought/typhoon/flood affect/ destroy crop",
                                ifelse(grepl("not willing to intercrop cacao|hesitant to intercrop cacao",V6006notes.or.comments.from.interview.),
                                      "Unwilling to intercrop cacao",
                                      ifelse(grepl("remittance|senior citizen",V6006notes.or.comments.from.interview.),"Has remittance/ senior citizen benefit",
                                             ifelse(grepl("own.*business|has.*business|own.*store|ha.*sari-sari|business is trucking services|respondent has gasoline station|hus store",V6006notes.or.comments.from.interview.),"Own business",
                                                    ifelse(grepl("retire|they are old|oldies|old and weak",V6006notes.or.comments.from.interview.),"They are old/ retired people",
                                                           ifelse(grepl("labor work|employee|work.*as|teacher|work.*government|works in a construction site|fisherman|farmer got a.*job.*left the farm|sell of copra, charcoal and dehusk|sell coconut wine",V6006notes.or.comments.from.interview.),"Work other jobs",
                                                                  ifelse(grepl("disease",V6006notes.or.comments.from.interview.),"Diseases in the crops",
                                                                         ifelse(grepl("banana|mango",V6006notes.or.comments.from.interview.),"Plant banana/ mango",
                                                                                ifelse(grepl("cacao.*not.*productive|few of the farmer's cacao trees are productive|few of the farmer's cacao trees are bearing pods|cacao.*no output",V6006notes.or.comments.from.interview.),"Cacao not/low productive",
                                                                                       ifelse(grepl("coconut.*not.*productive|only.*of.*coconut.*productive|50% of coconut trees are productive|low production of coconut",V6006notes.or.comments.from.interview.),"Coconut not/low productive",
                                                                                              ifelse(grepl("land|soil",V6006notes.or.comments.from.interview.)&grepl("not suitable|nt suitable|n't suitable",V6006notes.or.comments.from.interview.),"Soil not suitable",
                                                                                                     ifelse(grepl("is a tenant.*coconut|as tenant of coconut farm|coconut.*as tenant|only 50% of coconut income was  accrued by the respondent|she has 30% share|as tenant.*only receive",V6006notes.or.comments.from.interview.),"Are tenants and receive only <1/2 coconut income",
                                                                                                            ifelse(grepl("other crops.*consumption only|other crops.*no income|other crops.*n't.*income|durian fruits are for consumption only",V6006notes.or.comments.from.interview.),"Other crop not a source of income",
                                                                                                                   ifelse(grepl("former prov pca director|municipal councilor",V6006notes.or.comments.from.interview.),"Local gov officer",
                                                                                                                          ifelse(grepl("33.*laborer|50% share of the tenant/laborer|high expenses because he has a regular labor|received only 60% of gross income of coconut|only get 60% from gross sales",V6006notes.or.comments.from.interview.),"High share 1/2-1/3 coconut income to laborer/tenants",
                                                                                                                                 ifelse(grepl("willing.*cacao|willing.*rent|will actually plant cacao|plant a cacao.*if|will to rent out|will continue intercropping cacao",V6006notes.or.comments.from.interview.),"Willing plant cacao/rent out land under certain condition",
                                                                                                                                        ifelse(grepl("credit",V6006notes.or.comments.from.interview.),"Credit problem/ worry about credit",
                                                                                                                                               "Other"))))))))))))))))))))
table(mainrn %>% filter(V6006FinalComment=="Other") %>% .$V6006notes.or.comments.from.interview.)
table(mainrn$V6006FinalComment)



### Write cleaned file-----
readr::write_excel_csv(mainrn,"surveydataclean.csv",na="")








