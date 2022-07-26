## Using R Code to Export Fixation Duration Data from Tobii ##

##########################################################################################
## Import Dataset, Create Output File, Filter for Fixations and Real Trials Only ##
##########################################################################################

###########################################################
      ## Import All Data Files ##
###########################################################

FullEdiData <- read.table(file = 'Ingroup-Outgroup_FrekLouis_Data_Export.tsv', sep = '\t', header = TRUE)

FullPNDData <- read.table(file = 'Ingroup-Outgroup_Planckendael_Data_Export.tsv', sep = '\t', header = TRUE)
  
KSBonoboData <- read.table(file = 'KS Bonobo Ingroup-Outgroup Test_Data_Export.tsv', sep = '\t', header = TRUE)

MissedKSBonoboData <- read.table(file = 'Missed_KS Bonobo Ingroup-Outgroup Test_Data_Export.tsv', sep = '\t', header = TRUE)
  
KSChimpData <- read.table(file = 'KS Chimp Ingroup-Outgroup Test_Data_Export.tsv', sep = '\t', header = TRUE)

MissedKSChimpData <- read.table(file = 'MISSED_KS Chimp Ingroup-Outgroup Test 2_Data_Export.tsv', sep = '\t', header = TRUE)

###### Delete and Add Columns to KSData to Match all other Dataframes (this can be avoided with new export) #####
KSChimpData <- KSChimpData[-5]

KSBonoboData <- KSBonoboData[-6]

###########################################################
        ## Merge All Data Files ##
###########################################################

EdiPNDData <- rbind(FullEdiData, FullPNDData)

EdiPNDData <- EdiPNDData[-6]

levels(EdiPNDData$ParticipantName)

ALLKSData1 <- rbind(KSBonoboData, KSChimpData)

MissingKSData <- rbind(MissedKSBonoboData, MissedKSChimpData)

MissingKSData <- MissingKSData[-5]

COMPLETEKSData <- rbind(ALLKSData1, MissingKSData)

###########################################################
     ## Combine all Datasets Into a Single Dataframe ##
###########################################################

file.names <- rbind(EdiPNDData, COMPLETEKSData)

###########################################################
      ## Remove all Unused Participants and Trials ##
###########################################################

## Subset partipants
new_df <- subset(file.names, file.names$ParticipantName == "Edith" | file.names$ParticipantName == "Eva" 
                 | file.names$ParticipantName == "Frek" | file.names$ParticipantName == "Kilimi" 
                 | file.names$ParticipantName == "Liberius 02" | file.names$ParticipantName == "Louis 02" 
                 | file.names$ParticipantName == "Paul" | file.names$ParticipantName == "Qafzeh" 
                 |  file.names$ParticipantName == "Rene 2" |  file.names$ParticipantName == "Sophie" 
                 | file.names$ParticipantName == "Nayoki" | file.names$ParticipantName == "Kikongo"
                 | file.names$ParticipantName == "Habari 2" | file.names$ParticipantName == "Djanoa"
                 | file.names$ParticipantName == "Lina 2" | file.names$ParticipantName == "Rubani"
                 | file.names$ParticipantName == "connielenore" | file.names$ParticipantName == "ikela"
                 | file.names$ParticipantName == "junior" | file.names$ParticipantName == "lolita"
                 | file.names$ParticipantName == "louise" | file.names$ParticipantName == "vijay"
                 | file.names$ParticipantName == "hatsuka" | file.names$ParticipantName == "iroha"
                 | file.names$ParticipantName == "misaki" | file.names$ParticipantName == "mizuki"
                 | file.names$ParticipantName == "natsuki" | file.names$ParticipantName == "zamba"
                 | file.names$ParticipantName == "Velu")
## Look at frequency distribution
table(new_df$ParticipantName)
## Drop un-used levels of ParticipantName
new_df$ParticipantName <- droplevels(new_df$ParticipantName)
## Check levels
levels(new_df$ParticipantName)

############## Remove all rows that are not fixations ############

FixationData <- subset(new_df, new_df$GazeEventType == "Fixation")
table(FixationData$GazeEventType)
FixationData$GazeEventType <- droplevels(FixationData$GazeEventType)
levels(FixationData$GazeEventType)

############# Keep only real trials (not mouse calibration screen or fixation cross screen) #################

RealFixData <- FixationData[ !(FixationData$MediaName %in% c("Mouse Calibration Screen.png", "Fixation.jpg", "")), ]
table(RealFixData$MediaName)
RealFixData$MediaName <- droplevels(RealFixData$MediaName)
levels(RealFixData$MediaName)

head(RealFixData)

############## Change Participant and StudioTestName columnns names to match with later dataframes ##############

colnames(RealFixData)[4] = "Subject"
colnames(RealFixData)[3] = "Cluster"

############## Change "MediaName" image names to only the image name, without .jpeg ####################

RealFixData$MediaName <- sapply(strsplit(RealFixData$MediaName, split='.', fixed=TRUE), function(x) (x[1]))

RealFixData<-mutate(RealFixData,MediaName=as.character(MediaName))
RealFixData<-mutate(RealFixData,MediaName=sapply(strsplit(RealFixData$MediaName, split='.', fixed=TRUE),function(x) (x[1])))

RealFixData<-mutate(RealFixData,MediaName=as.data.frame.vector(MediaName))
table(RealFixData$MediaName)

############ ****Create a new variable to label individual trials for each participant #################

RealFixData$TrialName <- (paste(RealFixData$Subject, RealFixData$Cluster, RealFixData$MediaName, sep = ""))
RealFixData$TrialName

length(unique(RealFixData$TrialName))

###################################################################
##Use only trials where Validity for at least one eye = 0##
##(A value of 0 for Validity means the eye was captured)##
###################################################################

########### Fill empty rows with 4, which Tobii  uses to mark when eyes were not captured ############
RealFixData$ValidityLeft[is.na(RealFixData$ValidityLeft)] <- 4
RealFixData$ValidityRight[is.na(RealFixData$ValidityRight)] <- 4

table(RealFixData$ValidityRight)

######### Set Invalid Fixations to 9999, which is outside of our AOI's and therefore will not be counted #########
for (i in 1:nrow(RealFixData)){
  if (RealFixData$ValidityLeft[i] != 0 & RealFixData$ValidityRight[i] != 0) {
    RealFixData$GazePointX..ADCSpx.[i] = 9999
    RealFixData$GazePointY..ADCSpx.[i] = 9999
  }
}


########################################################################################################################
  ### Assign Actual Trial Numbers, AOI Labels, Trial Order, Population, Model Sex, Subject Sex, and Model ID's ####
########################################################################################################################

## Fill these columns by Merging RealFixData dataframe with ALLInOutData_ALLDV's excel sheet (original, hand-extracted datasheet):

############ Import Original Datasheet ############
ALLInOutData_ALLDV = read.csv("ALLInOutData_ALLDVs.csv")

########## Create smaller dataframe with only columns needed in final output ##########
Smalltable <- ALLInOutData_ALLDV[,c("Subject","Subject_Sex", "Population", "Cluster", "WithinCluster_Trial_Number", 
                                    "Actual_Total_Trial_Number", "Model_Sex", "Fam_Unfam_Location", "Fam_ID", "Unfam_ID")]

############ Merge Tables and Rename Columns if Needed ############

Mergedtable <- merge(RealFixData,Smalltable, by = c("Subject","Cluster","WithinCluster_Trial_Number"))

RealFixData <- Mergedtable

View(RealFixData)

###########################################################
## Sum Up AOIs for all trials and all participants ##
###########################################################

##Make New Columns for "Ingroup" and "Outgroup" Total Fixation Duration (summing fixation durations within Left and Right AOI's):

RealFixData$IngroupSum <- 0

RealFixData$OutgroupSum <- 0

##################################################################
## Create index of rows deeming whether Fixation occurs in Left or Right AOI ##
##################################################################

RealFixData$FixationSide <- 0

#Left Image
LXStart = 136
LXEnd = 836
LYStart = 175
LYEnd = 875

#Right Image
RXStart = 1083
RXEnd = 1783
RYStart = 175
RYEnd = 875

leftside_idx <- which(RealFixData$GazePointX..ADCSpx. >= LXStart & RealFixData$GazePointX..ADCSpx. <= LXEnd & RealFixData$GazePointY..ADCSpx. >= LYStart & RealFixData$GazePointY..ADCSpx. <= LYEnd)
RealFixData$FixationSide[leftside_idx] <- "Left"

rightside_idx <- which(RealFixData$GazePointX..ADCSpx. >= RXStart & RealFixData$GazePointX..ADCSpx. <= RXEnd & RealFixData$GazePointY..ADCSpx. >= RYStart & RealFixData$GazePointY..ADCSpx. <= RYEnd)
RealFixData$FixationSide[rightside_idx] <- "Right"

View(RealFixData)

table(RealFixData$FixationSide)

##################################################################


##### Delete Duplicate Rows (multiple lines of fixations durations for the same fixation) #####

##### Create New Dataframe with only Trial Name, Fixation Index, Fixation Side, Model Sex, and Fixation Duration####
df2 <- subset(RealFixData, select = c("Subject", "Cluster", "TrialName", "FixationIndex", "FixationSide", "Model_Sex", "GazeEventDuration", "WithinCluster_Trial_Number"))

####### Rename "Trial Name" to include subject, cluster, and within cluster trial number to be used in merge later ######

df2$TrialName <- (paste(df2$Subject, df2$Cluster, df2$WithinCluster_Trial_Number, sep = ""))

#####Create New Variable that ties Fixation Index to Trial Name#####
df2$TrialName_FixIndex <- (paste(RealFixData$FixationIndex, df2$TrialName, sep = ""))

###### Sebastian ########
### groupby all variables by df2, AND AOI
#### then summarize[length of trial_fixindex]

##### Delete all duplicates of fixation indices, keeping only a single row for each fixation#########
df2 <- df2[ !duplicated(df2$TrialName_FixIndex), ]

###### Sum Fixation Durations per trial, separated by left and right sides#######

agg_table <- aggregate(GazeEventDuration ~ TrialName + FixationSide, df2, sum, all.x = T)

View(agg_table)
######## Subset Dataframe by Left and Right Sums, Then Merge! #######

attach(agg_table)

LeftTable <- as.data.frame(subset(agg_table,agg_table$FixationSide =="Left")) 
RightTable <- as.data.frame(subset(agg_table,agg_table$FixationSide =="Right"))

detach(agg_table)

########## Merge Subsetted Dataframes ################

SingleRow <- merge(LeftTable, RightTable, all = TRUE, by = c("TrialName"))

View(SingleRow)

SingleRow[is.na(SingleRow)] <- 0

View(SingleRow)

#### Download Plyr Package if  Don't Already Have It #####
library(plyr)

#### Change Final Column Names, Take Out Extra Columns #####

SingleRow <- rename(SingleRow, replace = c("GazeEventDuration.x" = "Left_Sum"))

SingleRow <- rename(SingleRow, replace = c("GazeEventDuration.y" = "Right_Sum"))

SingleRow <- within(SingleRow, rm(FixationSide.x, FixationSide.y))

View(SingleRow)


############## Merge SingleRow Simple Datasheet with Datasheet containing all relevant info ##########

################ Create Matching Trial Names in Original Datasheet and SingleRow Datasheet to be able to Merge ###########
View(ALLInOutData_ALLDV)

Smalltable <- ALLInOutData_ALLDV[,c("Subject","Subject_Sex", "Population", "Cluster", "WithinCluster_Trial_Number", 
                                    "Actual_Total_Trial_Number", "Model_Sex", "Fam_Unfam_Location", "Fam_ID", "Unfam_ID")]

Smalltable$TrialName <- (paste(Smalltable$Subject, Smalltable$Cluster, Smalltable$WithinCluster_Trial_Number, sep = ""))

############# Merge Trial Sums with Original Datasheet ###########

FinalData <- merge(Smalltable, SingleRow, by = c("TrialName"))

View(FinalData)

############ Put Sums into Ingroup and Outgroup Columns ###############

FinalData$IngroupSum <- 0
FinalData$OutgroupSum <- 0

left_idx <- which(FinalData$Fam_Unfam_Location == "LR")
right_idx <- which(FinalData$Fam_Unfam_Location == "RL")

FinalData$IngroupSum[left_idx] <- FinalData$Left_Sum[left_idx]
FinalData$OutgroupSum[left_idx] <- FinalData$Right_Sum[left_idx]

FinalData$IngroupSum[right_idx] <- FinalData$Right_Sum[right_idx]
FinalData$OutgroupSum[right_idx] <- FinalData$Left_Sum[right_idx]


######## Create OutPut File #########

write.csv(FinalData, file = "~/Desktop/In_Out_Extracted_Data.csv", row.names=FALSE)

###########################################################################################
##### If Needed, Use Below Code to Assign Population, Model Sex, and Subject Sex ####
###########################################################################################


##########  Assign Populations ##############
#For Edi Chimps:

#Edinburgh <- RealFixData$StudioProjectName == "Ingroup-Outgroup_FrekLouis"
#RealFixData[Edinburgh, "Population"] <- "Edinburgh"

##########  Assign Subject Sex ##############

##For Edi Chimps:

#levels(RealFixData$ParticipantName)

#Female <- RealFixData$ParticipantName == "Eva" | RealFixData$ParticipantName == "Edith" | RealFixData$ParticipantName == "Kilimi" | RealFixData$ParticipantName == "Sophie"
#RealFixData[Female, "Subject_Sex"] <- "Female"

#Male <- RealFixData$ParticipantName == "Frek" | RealFixData$ParticipantName == "Liberius 02" | RealFixData$ParticipantName == "Louis 02" | RealFixData$ParticipantName == "Paul"| RealFixData$ParticipantName == "Qafzeh"| RealFixData$ParticipantName == "Rene 2"| RealFixData$ParticipantName == "Velu"
#RealFixData[Male, "Subject_Sex"] <- "Male"

#unique(RealFixData$Subject_Sex)


##########  Assign Model Sex ##############

#FemaleTrials <- grepl("Female",RealFixData$StudioTestName)

#RealFixData[FemaleTrials, "Model_Sex"] <- "Female_Trials"

#MaleTrials <- grepl("Male",RealFixData$StudioTestName)

#RealFixData[MaleTrials, "Model_Sex"] <- "Male_Trials"

###########################################################################################
##### If Needed, Use Below Code to Assign AOI Labels and Trial Order####
###########################################################################################

###########################################################
##***Mark which AOI is Ingroup, which is Outgroup***##
###########################################################

#For Edi Chimps:

#RealFixData$Ingroup_side <- 0

#group_idx <- substr(as.character(RealFixData$MediaName[28000]), 5, 5)

#for (i in 1:nrow(RealFixData)){
#group_idx <- substr(as.character(RealFixData$MediaName[i]), 5, 5)
#if ((group_idx == "A")) {
#RealFixData$Ingroup_side[i] <- "Left"

#}
#if ((group_idx == "B")) {
#RealFixData$Ingroup_side[i] <- "Right"
#}
#if ((group_idx == "")) {
#RealFixData$Ingroup_side[i] <- "N/A"
#}
#}

#table(RealFixData$Ingroup_side)

###########################################################
##Create Within Cluster Trial Order##
###########################################################

##For Edi Chimps:

#RealFixData$WithinCluster_Trial_Number <- 0

#Order <- substr(as.character(RealFixData$MediaName[28000]), 4, 4)

#for (i in 1:nrow(RealFixData)){
#Order <- substr(as.character(RealFixData$MediaName[i]), 4, 4)
#if ((Order == "1")) {
#RealFixData$WithinCluster_Trial_Number[i] <- "1"

#}
#if ((Order == "2")) {
#RealFixData$WithinCluster_Trial_Number[i] <- "2"
#}
#if ((Order == "3")) {
#RealFixData$WithinCluster_Trial_Number[i] <- "3"
#}
#}

#table(RealFixData$WithinCluster_Trial_Number)






