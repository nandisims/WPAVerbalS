library(tidyverse) 
library(brms)
library(lme4)
library(lmerTest)

this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# READ DATA
vS <- read_csv("verbalS.csv")

vS = vS %>% 
  mutate_if(is.character, as.factor)

# reset outcome variable to predict null instead of s
contrasts(vS$Verbal_S) = c(0,1)
contrasts(vS$Verbal_S)

#Remove Levels
#Remove Discourse Markers in Aspect
vSnoDis = vS %>% 
  filter(Aspect != "discourse") %>% 
  droplevels()

#Remove null subjects in Type of Subject
vSnoDisNull = vSnoDis %>% 
  filter(Type_of_Subject != "Null") %>% 
  droplevels()

#Remove pause in Following Segment
vSnoDisNullP = vSnoDisNull %>% 
  filter(Following_Segment != "P") %>% 
  droplevels()

#mean centering
vSnoDisNullP = vSnoDisNullP %>% 
  mutate(cThirdSingular = as.numeric(vSnoDisNullP$ThirdSingular) - mean(as.numeric(vSnoDisNullP$ThirdSingular)),
         cAdjacency = as.numeric(vSnoDisNullP$Adjacency) - mean(as.numeric(vSnoDisNullP$Adjacency)),
         cAspect = as.numeric(vSnoDisNullP$Aspect)-mean(as.numeric(vSnoDisNullP$Aspect)),
         cVerb_Type = as.numeric(vSnoDisNullP$Verb_Type)-mean(as.numeric(vSnoDisNullP$Verb_Type)),
         cType_of_Subject = as.numeric(vSnoDisNullP$Type_of_Subject)-mean(as.numeric(vSnoDisNullP$Type_of_Subject)))
summary(vSnoDisNullP)

#Contrasts
# reset reference level of Region to Mountain (most likely a priori to have s)
contrasts(vSnoDisNullP$Region) = cbind(c(1,0,0,0),c(0,1,0,0),c(0,0,0,1))
contrasts(vSnoDisNullP$Region)

contrasts(vSnoDisNullP$Preceding_Segment) = cbind(c(1,0,0,0),c(0,1,0,0),c(0,0,1,0))
contrasts(vSnoDisNullP$Preceding_Segment)

contrasts(vSnoDisNullP$Following_Segment) = cbind(c(1,0,0),c(0,1,0))
contrasts(vSnoDisNullP$Following_Segment)

#Region Subsets
UplandnoDisNullP = vSnoDisNullP %>%
  filter(Region == "Mountain")

UpperCoastnoDisNullP = vSnoDisNullP %>%
  filter(Region == "Upper Coast")

GulfnoDisNullP = vSnoDisNullP %>%
  filter(Region == "Gulf")

LowerCoastnoDisNullP = vSnoDisNullP %>%
  filter(Region == "Lower Coast")

#mean centering within region
UplandnoDisNullP = UplandnoDisNullP %>% 
  mutate(cThirdSingular = as.numeric(UplandnoDisNullP$ThirdSingular) - mean(as.numeric(UplandnoDisNullP$ThirdSingular)),
         cAdjacency = as.numeric(UplandnoDisNullP$Adjacency) - mean(as.numeric(UplandnoDisNullP$Adjacency)),
         cAspect = as.numeric(UplandnoDisNullP$Aspect)-mean(as.numeric(UplandnoDisNullP$Aspect)),
         cVerb_Type = as.numeric(UplandnoDisNullP$Verb_Type)-mean(as.numeric(UplandnoDisNullP$Verb_Type)),
         cType_of_Subject = as.numeric(UplandnoDisNullP$Type_of_Subject)-mean(as.numeric(UplandnoDisNullP$Type_of_Subject)))
UpperCoastnoDisNullP = UpperCoastnoDisNullP %>% 
  mutate(cThirdSingular = as.numeric(UpperCoastnoDisNullP$ThirdSingular) - mean(as.numeric(UpperCoastnoDisNullP$ThirdSingular)),
         cAdjacency = as.numeric(UpperCoastnoDisNullP$Adjacency) - mean(as.numeric(UpperCoastnoDisNullP$Adjacency)),
         cAspect = as.numeric(UpperCoastnoDisNullP$Aspect)-mean(as.numeric(UpperCoastnoDisNullP$Aspect)),
         cVerb_Type = as.numeric(UpperCoastnoDisNullP$Verb_Type)-mean(as.numeric(UpperCoastnoDisNullP$Verb_Type)),
         cType_of_Subject = as.numeric(UpperCoastnoDisNullP$Type_of_Subject)-mean(as.numeric(UpperCoastnoDisNullP$Type_of_Subject)))
LowerCoastnoDisNullP = LowerCoastnoDisNullP %>% 
  mutate(cThirdSingular = as.numeric(LowerCoastnoDisNullP$ThirdSingular) - mean(as.numeric(LowerCoastnoDisNullP$ThirdSingular)),
         cAdjacency = as.numeric(LowerCoastnoDisNullP$Adjacency) - mean(as.numeric(LowerCoastnoDisNullP$Adjacency)),
         cAspect = as.numeric(LowerCoastnoDisNullP$Aspect)-mean(as.numeric(LowerCoastnoDisNullP$Aspect)),
         cVerb_Type = as.numeric(LowerCoastnoDisNullP$Verb_Type)-mean(as.numeric(LowerCoastnoDisNullP$Verb_Type)),
         cType_of_Subject = as.numeric(LowerCoastnoDisNullP$Type_of_Subject)-mean(as.numeric(LowerCoastnoDisNullP$Type_of_Subject)))
GulfnoDisNullP = GulfnoDisNullP %>% 
  mutate(cThirdSingular = as.numeric(GulfnoDisNullP$ThirdSingular) - mean(as.numeric(GulfnoDisNullP$ThirdSingular)),
         cAdjacency = as.numeric(GulfnoDisNullP$Adjacency) - mean(as.numeric(GulfnoDisNullP$Adjacency)),
         cAspect = as.numeric(GulfnoDisNullP$Aspect)-mean(as.numeric(GulfnoDisNullP$Aspect)),
         cVerb_Type = as.numeric(GulfnoDisNullP$Verb_Type)-mean(as.numeric(GulfnoDisNullP$Verb_Type)),
         cType_of_Subject = as.numeric(GulfnoDisNullP$Type_of_Subject)-mean(as.numeric(GulfnoDisNullP$Type_of_Subject)))

#Statistics
m1 = glmer(Verbal_S ~ Region + (1|Verb) + (1|Interviewee),
                     data=vSnoDisNullP, family="binomial")
summary(m1)

m2 = glmer(Verbal_S ~ ThirdSingular + cAdjacency + Preceding_Segment + Following_Segment + cType_of_Subject + cAspect + cVerb_Type + (1|Verb) + (1|Interviewee),
                     data=vSnoDisNullP, family="binomial")
summary(m2)

mGulf = glmer(Verbal_S ~  ThirdSingular + cAdjacency + Preceding_Segment + Following_Segment + cType_of_Subject + cAspect + cVerb_Type + (1|Verb) + (1|Interviewee),
                        data=GulfnoDisNullP, family="binomial")
summary(mGulf)

mUpland = glmer(Verbal_S ~ ThirdSingular + cAdjacency + Preceding_Segment + Following_Segment + cType_of_Subject + cAspect  + cVerb_Type + (1|Verb) + (1|Interviewee),
                          data=UplandnoDisNullP, family="binomial")
summary(mUpland)

mUpperCoast = glmer(Verbal_S ~ ThirdSingular + cAdjacency + Preceding_Segment + Following_Segment + cType_of_Subject + cAspect + cVerb_Type + (1|Verb) + (1|Interviewee),
                              data=UpperCoastnoDisNullP, family="binomial")
summary(mUpperCoast)

mLowerCoast = glmer(Verbal_S ~ cThirdSingular + cAdjacency + Preceding_Segment + Following_Segment + cType_of_Subject + cAspect +  cVerb_Type + (1|Verb) + (1|Interviewee),
                              data=LowerCoastnoDisNullP, family="binomial")
summary(mLowerCoast)
