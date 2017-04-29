# Derive Psytools CSV datasets downloaded from the Delosis server
#
#  Copyright (C) 2017 Delosis
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#NB These functions expect AgeGroup and rowIndex columns to be present in the supplied df
## The DFs supplied are expected to be as they come out of the csv PER TASK other than this I am not removing the extra columns (pause duration etc) which came from merging all tasks together into one DF
## However you do NOT need to have filtered to the correct iteration as iteration will be supplied back in the summary DF
## In short:
##        Load the csv using read.csv
##        get age group off the User code
##        compute a row index
##        supply the DF to one of the derive functions.
##
##        repeat for each task seperately

## Happy to remove reliance on the Age group and row index if you prefer! Some additional flexibility can be worked in

library(car)
library(plyr)


#########
##SST
#########
deriveSST <- function(df) {
    df <- subset(df, df$Block !='SST_Practice')
    df <-df[order(df$User.code, df$Iteration, df$rowIndex, df$Trial),]
    #Split the result column
    options(stringsAsFactors=FALSE)
    df <- cbind(df, data.frame(do.call('rbind', strsplit(as.character(df$Trial.result),':',fixed=TRUE))))
    names(df)[names(df) == 'X1'] <- 'TrialType'
    names(df)[names(df) == 'X2'] <- 'Stimulus'
    names(df)[names(df) == 'X3'] <- 'StopDelay'
    names(df)[names(df) == 'X4'] <- 'TrialResult'
    names(df)[names(df) == 'X5'] <- 'StopHitRate'
    names(df)[names(df) == 'X6'] <- 'TrialDuration'
    df$StopDelay <- as.numeric(df$StopDelay)
    df$StopHitRate <- as.numeric(df$StopHitRate)
    #write.csv(df, "cVEDA_SST_RAW.csv", row.names=FALSE, na='')

    #Summaries - not 100% sure what you need here?
    dfsums <- do.call(data.frame, aggregate(cbind(TrialResult)~AgeGroup+User.code+Iteration+Language+Completed+Completed.Timestamp+Processed.Timestamp, function(x)
      c(GO_SUCCESS = length(which(x == "GO_SUCCESS")),
        GO_TOO_LATE = length(which(x == "GO_TOO_LATE")),
        GO_WRONG_KEY_RESPONSE = length(which(x == "GO_WRONG_KEY_RESPONSE")),
        STOP_TOO_EARLY = length(which(x == "STOP_TOO_EARLY")),
        STOP_FAIL = length(which(x == "STOP_FAIL")),
        STOP_SUCCESS = length(which(x == "STOP_SUCCESS"))), data=df))
    dfsums <- merge(dfsums, do.call(data.frame, aggregate(cbind(StopDelay, StopHitRate)~User.code+Iteration, function(x) 
      c(mean = mean(x), sd = sd(x), final = tail(x,1)), data=subset(df, df$TrialType == 'STOP_VAR'))), by=c("User.code", "Iteration"))
    return (dfsums)
}
#Clearly there are some people who are simply not responding for big chunks of this task - this gives them a good stop hit rate but very bad go_success rate - they will probably need to be excluded - really this should not be happening in a supervised admin situation?


#########
##MID
#########
deriveMID <- function(df) {
    df <- subset(df, df$Block !='MID_PRACTICE' & df$Block !='midNRCHECK')
    df <- df[order(df$User.code, df$Iteration, df$rowIndex, df$Trial),]
    #Split the result column
    options(stringsAsFactors=FALSE)
    df <- cbind(df, data.frame(do.call('rbind', strsplit(as.character(df$Trial.result),':',fixed=TRUE))))
    names(df)[names(df) == 'X1'] <- 'TrialResult'
    names(df)[names(df) == 'X2'] <- 'TargetDuration'
    names(df)[names(df) == 'X3'] <- 'TargetHitRate'
    df <- cbind(df, data.frame(do.call('rbind', strsplit(as.character(df$Trial),'_',fixed=TRUE))))
    names(df)[names(df) == 'X1'] <- 'TrialNum'
    names(df)[names(df) == 'X4'] <- 'TargetPosition'
    df$TrialType <- paste(df$X2, df$X3)
    df$TrialNum <- as.numeric(df$TrialNum)
    df<-subset(df, select = -c(X2, X3))
    ##Fix for coding bug with Failure trials (hit rate and target duration not output in early version of task)
    df$TargetDuration[df$TrialResult=="'FAILURE"] <- df$TargetDuration[df$TrialResult[-1]=="'FAILURE"]
    df$TargetHitRate[df$TrialResult=="'FAILURE"] <- round(as.numeric(df$TargetHitRate[df$TrialResult[-1]=="'FAILURE"]) * as.numeric(df$TrialNum[df$TrialResult[-1]=="'FAILURE"]) / (as.numeric(df$TrialNum[df$TrialResult[-1]=="'FAILURE"]) +1), digits = 2)
    df$TrialResult[df$TrialResult=="'FAILURE"] <- "FAILURE"

    df$TargetDuration <- as.numeric(df$TargetDuration)
    df$TargetHitRate <- as.numeric(df$TargetHitRate)
    #write.csv(df, "cVEDA_MID_RAW.csv", row.names=FALSE, na='')

    #Summaries - not 100% sure what you need here?
    dfsums <- do.call(data.frame, aggregate(cbind(TrialResult)~AgeGroup+User.code+Iteration+Language+Completed+Completed.Timestamp+Processed.Timestamp, function(x)
      c(NO_RESPONSE = length(which(x == "NO_RESPONSE")),
        TOO_LATE = length(which(x == "TOO_LATE")),
        TOO_EARLY = length(which(x == "TOO_EARLY")),
        SUCCESS = length(which(x == "SUCCESS")),
        FAILURE = length(which(x == "FAILURE"))), data=df))
    dfsums <- merge(dfsums, do.call(data.frame, aggregate(cbind(TargetDuration, TargetHitRate)~User.code+Iteration, function(x) 
      c(mean = mean(x), sd = sd(x), final = tail(x,1)), data=df)), by=c("User.code", "Iteration"))
    return (dfsums)
}


#########
##WCST
#########
deriveWCST <- function(df) {
    df <- df
    #Split the result column
    options(stringsAsFactors=FALSE)
    df <- cbind(df, data.frame(do.call('rbind', strsplit(as.character(df$Trial.result),'_',fixed=TRUE))))
    names(df)[names(df) == 'X2'] <- 'SortCategory'
    df$Perseverations[df$X3=='PERSEV'] <- 1
    df$Corrects[df$X1=='PASS'] <- 1
    df <- subset(df, select = -c(X1, X3))
    df <- df[order(df$User.code, df$Iteration, df$rowIndex),]
    #write.csv(df, "cVEDA_WCST_RAW.csv", row.names=FALSE, na='')

    # Flag each switch for summing
    df$Switches <- 0
    df$Switches[df$SortCategory != df$SortCategory[-1] & df$User.code == df$User.code[-1] & df$Iteration == df$Iteration[-1]] <- 1

    #Summaries
    dfsums <- do.call(data.frame, aggregate(cbind(Corrects, Switches, Perseverations)~AgeGroup+User.code+Iteration+Language+Completed+Completed.Timestamp+Processed.Timestamp, FUN=sum, na.rm=TRUE, na.action=NULL, data=df))
    dfsums <- merge(dfsums, do.call(data.frame, aggregate(cbind(Response.time..ms.)~User.code+Iteration, function(x) 
      c(mean = mean(x), sd = sd(x)), data=df)), by=c("User.code", "Iteration"))
    return (dfsums)
}


#########
##DS
#########
deriveDS <- function(df) {
    df <- subset(df, df$Block !='SST_Practice')
    #Create a numerical score to sum later
    df$Corrects[df$Trial.result=="PASS"] <- 1
    #write.csv(df, "cVEDA_DS_RAW.csv", row.names=FALSE, na='')

    #Summaries
    dfsums<-do.call(data.frame, aggregate(cbind(Corrects)~AgeGroup+User.code+Iteration+Language+Completed+Completed.Timestamp+Processed.Timestamp+Block, FUN=sum, na.rm=TRUE, na.action=NULL, data=df))
    dfsums<-reshape(dfsums, direction = "wide", idvar = c("AgeGroup", "User.code", "Iteration", "Language", "Completed", "Completed.Timestamp", "Processed.Timestamp"), timevar = "Block")
    dfsums$SpanF[dfsums$Corrects.F_2>1] <- 2
    dfsums$SpanF[dfsums$Corrects.F_3>1] <- 3
    dfsums$SpanF[dfsums$Corrects.F_4>1] <- 4
    dfsums$SpanF[dfsums$Corrects.F_5>1] <- 5
    dfsums$SpanF[dfsums$Corrects.F_6>1] <- 6
    dfsums$SpanF[dfsums$Corrects.F_7>1] <- 7
    dfsums$SpanF[dfsums$Corrects.F_8>1] <- 8
    dfsums$SpanF[dfsums$Corrects.F_9>1] <- 9
    dfsums$SpanF[dfsums$Corrects.F_10>1] <- 10

    dfsums$SpanB[dfsums$Corrects.B_2>1] <- 2
    dfsums$SpanB[dfsums$Corrects.B_3>1] <- 3
    dfsums$SpanB[dfsums$Corrects.B_4>1] <- 4
    dfsums$SpanB[dfsums$Corrects.B_5>1] <- 5
    dfsums$SpanB[dfsums$Corrects.B_6>1] <- 6
    dfsums$SpanB[dfsums$Corrects.B_7>1] <- 7
    dfsums$SpanB[dfsums$Corrects.B_8>1] <- 8
    dfsums$SpanB[dfsums$Corrects.B_9>1] <- 9
    dfsums$SpanB[dfsums$Corrects.B_10>1] <- 10
    return (dfsums)
}


#########
##CORSI
#########

deriveCORSI <- function(df) {
    df <- subset(df, df$Block !='P2')

    #Create a numerical score to sum later
    df$Corrects[df$Trial.result=="PASS"] <- 1
    #write.csv(df, "cVEDA_CORSI_RAW.csv", row.names=FALSE, na='')

    #Summaries
    dfsums <- do.call(data.frame, aggregate(cbind(Corrects)~AgeGroup+User.code+Iteration+Language+Completed+Completed.Timestamp+Processed.Timestamp+Block, FUN=sum, na.rm=TRUE, na.action=NULL, data=df))
    dfsums <- reshape(dfsums, direction = "wide", idvar = c("AgeGroup", "User.code", "Iteration", "Language", "Completed", "Completed.Timestamp", "Processed.Timestamp"), timevar = "Block")
    dfsums$SpanF[dfsums$Corrects.F2>0] <- 2
    dfsums$SpanF[dfsums$Corrects.F3>0] <- 3
    dfsums$SpanF[dfsums$Corrects.F4>0] <- 4
    dfsums$SpanF[dfsums$Corrects.F5>0] <- 5
    dfsums$SpanF[dfsums$Corrects.F6>0] <- 6
    dfsums$SpanF[dfsums$Corrects.F7>0] <- 7
    dfsums$SpanF[dfsums$Corrects.F8>0] <- 8
    dfsums$SpanF[dfsums$Corrects.F9>0] <- 9
    dfsums$SpanF[dfsums$Corrects.F10>0] <- 10

    dfsums$SpanB[dfsums$Corrects.B2>0] <- 2
    dfsums$SpanB[dfsums$Corrects.B3>0] <- 3
    dfsums$SpanB[dfsums$Corrects.B4>0] <- 4
    dfsums$SpanB[dfsums$Corrects.B5>0] <- 5
    dfsums$SpanB[dfsums$Corrects.B6>0] <- 6
    dfsums$SpanB[dfsums$Corrects.B7>0] <- 7
    dfsums$SpanB[dfsums$Corrects.B8>0] <- 8
    dfsums$SpanB[dfsums$Corrects.B9>0] <- 9
    dfsums$SpanB[dfsums$Corrects.B10>0] <- 10

    return (dfsums)
}

#########
##TMT
#########
#Drop the practice
deriveTMT <- function(df) {
    df <- subset(df, !grepl("Practice", Block, ignore.case=TRUE))

    #Simplify the block names
    df$Block <- gsub("TMT_", "", df$Block)
    df$Block <- gsub("_Test[1]?", "", df$Block)
    #write.csv(dfsums, "cVEDA_TMT_RAW.csv", row.names=FALSE, na='')
    #Summaries
    dfsums <- do.call(data.frame, aggregate(cbind(Response.time..ms., Incorrect.responses, Wild.responses)~AgeGroup+User.code+Iteration+Language+Completed+Completed.Timestamp+Processed.Timestamp+Block, FUN=sum, na.rm=TRUE, na.action=NULL, data=df))
    dfsums <- reshape(dfsums, direction = "wide", idvar = c("AgeGroup", "User.code", "Iteration", "Language", "Completed", "Completed.Timestamp", "Processed.Timestamp"), timevar = "Block")
    return (dfsums)
}

                                                        
##########
##SOCRATIS
##########
## NB this is essentially just a questionnaire - other questionnaires could be similarly processed!
deriveSOCRATIS <- function(df) {
    df <- subset(df, !grepl("FEEDBACK|js", Block, ignore.case=TRUE))

    #remove unneeded columns and any skip back control markers
    df <- subset(df, df$Response != 'skip_back', select=c(AgeGroup, User.code, Iteration,Language,Completed,Completed.Timestamp,Processed.Timestamp,Trial, Trial.result))
    #Select just the LAST response on each question - note that this means repeating a task will update the results - but it also takes the most recent response if they navigate backwards and then change their mind 
    df <- df[!duplicated(subset(df, select=c(User.code, Iteration, Trial)), fromLast=TRUE),]
    #write.csv(dfsums, "cVEDA_SOCRATIS_RAW.csv", row.names=FALSE, na='')

    #Summaries - currently just showing those calculated in task - let me know if there are any other ones
    df <- subset(df, grepl("INDEX", Trial, ignore.case=TRUE))
    df <-reshape(df, direction = "wide", idvar = c("AgeGroup", "User.code", "Iteration", "Language", "Completed", "Completed.Timestamp", "Processed.Timestamp"), timevar = "Trial")
    names(df) <- gsub("Trial.result.", "", names(df))
    df$SOCRATIS_TOM_1_INDEX <- as.numeric(df$SOCRATIS_TOM_1_INDEX)
    df$SOCRATIS_TOM_2_INDEX <- as.numeric(df$SOCRATIS_TOM_2_INDEX)
    df$SOCRATIS_FAUS_PAS_INDEX <- as.numeric(df$SOCRATIS_FAUS_PAS_INDEX)
    return (df)
}


##########
##BART
##########
deriveBART <- function(df) {
    #Split the result column
    options(stringsAsFactors=FALSE)
    df <- cbind(df, data.frame(do.call('rbind', strsplit(as.character(df$Trial.result),'_',fixed=TRUE))))
    names(df)[names(df) == 'X1'] <- 'TrialResult'
    names(df)[names(df) == 'X2'] <- 'PumpsMade'
    df$PumpsMade<-as.numeric(df$PumpsMade)
    #remove the index fromteh trial column so it can serve as the Colour factor
    df$BalloonColour <- toupper(gsub("[0-9]", "", df$Trial))
    #remove unneeded columns 
    df <- subset(df, select=c(AgeGroup, User.code, Iteration, Language, Completed, Completed.Timestamp, Processed.Timestamp, BalloonColour, TrialResult, PumpsMade))
    #write.csv(df, "cVEDA_BART_RAW.csv", row.names=FALSE, na='')

    #Summaries
    dfsums <- do.call(data.frame, aggregate(cbind(PumpsMade)~AgeGroup+User.code+Iteration+Language+Completed+Completed.Timestamp+Processed.Timestamp+BalloonColour+TrialResult, FUN=sum, na.rm=TRUE, na.action=NULL, data=df))
    dfsums <- reshape(dfsums, direction = "wide", idvar = c("AgeGroup", "User.code", "Iteration", "Language", "Completed", "Completed.Timestamp", "Processed.Timestamp", "BalloonColour"), timevar = "TrialResult")
    dfsums <- merge(do.call(data.frame, aggregate(cbind(TrialResult)~User.code+Iteration+BalloonColour, function(x)
      c(NumPopped = length(which(x == "POPPED"))), data=df)), dfsums, by=c("User.code", "Iteration", "BalloonColour"))
    names(dfsums)[names(dfsums) == 'TrialResult'] <- 'NumPopped'
    dfsums <- reshape(dfsums, direction = "wide", idvar = c("AgeGroup", "User.code", "Iteration", "Language", "Completed", "Completed.Timestamp", "Processed.Timestamp"), timevar = "BalloonColour")
    
    return (dfsums)
}


##########
##ERT
##########
deriveERT <- function(df) {
    df <- subset(df, df$Block=='MAIN')

    #Split the Trial column
    options(stringsAsFactors=FALSE)
    df <- cbind(df, data.frame(do.call('rbind', strsplit(as.character(df$Trial),'_',fixed=TRUE))))
    names(df)[names(df) == 'X1'] <- 'TrialEmotion'
    names(df)[names(df) == 'X2'] <- 'TrialEmotionIndex'
    df$TrialEmotionIndex <- as.numeric(df$TrialEmotionIndex)

    #Mark the response
    df$Correct <- 0
    df$Correct[df$TrialEmotion==df$Response] <- 1

    #Make an RTC and RTI (correct / Incorrect)
    df$RTcorrect[df$Correct==1] <- df$Response.time..ms.[df$Correct==1]
    df$RTincorrect[df$Correct==0] <- df$Response.time..ms.[df$Correct==0]


    #remove unneeded columns 
    df <- subset(df, select=c(AgeGroup, User.code, Iteration, Language, Completed, Completed.Timestamp, Processed.Timestamp, Trial, Response, Response.time..ms., TrialEmotion, TrialEmotionIndex, Correct, RTcorrect, RTincorrect))
    #write.csv(df, "cVEDA_ERT_RAW.csv", row.names=FALSE, na='')

    #Summaries
    dfsums <- do.call(data.frame, aggregate(cbind(RTcorrect, RTincorrect)~AgeGroup+User.code+Iteration+Language+Completed+Completed.Timestamp+Processed.Timestamp+TrialEmotion, FUN=mean, na.rm=TRUE, na.action=NULL, data=df))
    dfsums <- merge(do.call(data.frame, aggregate(cbind(Correct)~User.code+Iteration+TrialEmotion, FUN=sum, na.rm=TRUE, na.action=NULL, data=df)), dfsums, by=c("User.code", "Iteration", "TrialEmotion"))
    dfsums$RTcorrect <- recode(dfsums$RTcorrect, 'NaN=NA')
    dfsums$RTincorrect <- recode(dfsums$RTincorrect, 'NaN=NA')
    dfsums<-reshape(dfsums, direction = "wide", idvar = c("AgeGroup", "User.code", "Iteration", "Language", "Completed", "Completed.Timestamp", "Processed.Timestamp"), timevar = "TrialEmotion")
    
    return (dfsums)
}

###############
##MCQ / KIRBY
##############
##NB again just a questionnaire
deriveKIRBY<-function(df) {
    df <- subset(df, !grepl("FEEDBACK|js|KIRBY_PCDELAY", Block, ignore.case=TRUE) & df$Trial.result !='skip_back')
    #Select just the LAST response on each question - note that this means repeating a task will update the results - but it also takes the most recent response if they navigate backwards and then change their mind 
    df <- df[!duplicated(subset(df, select=c(User.code, Iteration, Trial)), fromLast=TRUE),]

    #Add the computed Kind values
    df$Kind[df$Block=='KIRBY01'] <- 0.000158277936055715
    df$Kind[df$Block=='KIRBY02'] <- 0.00596125186289121
    df$Kind[df$Block=='KIRBY03'] <- 0.00595829195630586
    df$Kind[df$Block=='KIRBY04'] <- 0.248847926267281
    df$Kind[df$Block=='KIRBY05'] <- 0.0413533834586466
    df$Kind[df$Block=='KIRBY06'] <- 0.000398936170212766
    df$Kind[df$Block=='KIRBY07'] <- 0.102564102564103
    df$Kind[df$Block=='KIRBY08'] <- 0.1
    df$Kind[df$Block=='KIRBY09'] <- 0.000158277936055713
    df$Kind[df$Block=='KIRBY10'] <- 0.00604838709677419
    df$Kind[df$Block=='KIRBY11'] <- 0.246753246753247
    df$Kind[df$Block=='KIRBY12'] <- 0.00100338642919854
    df$Kind[df$Block=='KIRBY13'] <- 0.00595829195630586
    df$Kind[df$Block=='KIRBY14'] <- 0.0405643738977072
    df$Kind[df$Block=='KIRBY15'] <- 0.00254817646121994
    df$Kind[df$Block=='KIRBY16'] <- 0.00252235725750975
    df$Kind[df$Block=='KIRBY17'] <- 0.000398089171974522
    df$Kind[df$Block=='KIRBY18'] <- 0.0158045977011494
    df$Kind[df$Block=='KIRBY19'] <- 0.101731601731602
    df$Kind[df$Block=='KIRBY20'] <- 0.000399042298483639
    df$Kind[df$Block=='KIRBY21'] <- 0.0156862745098039
    df$Kind[df$Block=='KIRBY22'] <- 0.0025
    df$Kind[df$Block=='KIRBY23'] <- 0.0414634146341463
    df$Kind[df$Block=='KIRBY24'] <- 0.001001001001001
    df$Kind[df$Block=='KIRBY25'] <- 0.0160493827160494
    df$Kind[df$Block=='KIRBY26'] <- 0.00100267379679144
    df$Kind[df$Block=='KIRBY27'] <- 0.25
    #Ad the LDR scale
    df$LDRscale[df$Block=='KIRBY01'] <- 2
    df$LDRscale[df$Block=='KIRBY02'] <- 3
    df$LDRscale[df$Block=='KIRBY03'] <- 1
    df$LDRscale[df$Block=='KIRBY04'] <- 3
    df$LDRscale[df$Block=='KIRBY05'] <- 1
    df$LDRscale[df$Block=='KIRBY06'] <- 2
    df$LDRscale[df$Block=='KIRBY07'] <- 1
    df$LDRscale[df$Block=='KIRBY08'] <- 2
    df$LDRscale[df$Block=='KIRBY09'] <- 3
    df$LDRscale[df$Block=='KIRBY10'] <- 2
    df$LDRscale[df$Block=='KIRBY11'] <- 1
    df$LDRscale[df$Block=='KIRBY12'] <- 3
    df$LDRscale[df$Block=='KIRBY13'] <- 1
    df$LDRscale[df$Block=='KIRBY14'] <- 2
    df$LDRscale[df$Block=='KIRBY15'] <- 3
    df$LDRscale[df$Block=='KIRBY16'] <- 2
    df$LDRscale[df$Block=='KIRBY17'] <- 3
    df$LDRscale[df$Block=='KIRBY18'] <- 1
    df$LDRscale[df$Block=='KIRBY19'] <- 3
    df$LDRscale[df$Block=='KIRBY20'] <- 1
    df$LDRscale[df$Block=='KIRBY21'] <- 2
    df$LDRscale[df$Block=='KIRBY22'] <- 1
    df$LDRscale[df$Block=='KIRBY23'] <- 3
    df$LDRscale[df$Block=='KIRBY24'] <- 2
    df$LDRscale[df$Block=='KIRBY25'] <- 3
    df$LDRscale[df$Block=='KIRBY26'] <- 1
    df$LDRscale[df$Block=='KIRBY27'] <- 2


    #write.csv(df, "cVEDA_KIRBY_RAW.csv", row.names=FALSE, na='')

    #NB this analysis only works for completed attempts - remove any early teminations  
    df <- df[order(df$User.code, df$Iteration, df$LDRscale, df$Kind),]
    df <- subset(df, df$Completed =="t")


    ####RECODE refuse to 0 - the calculations will fail otherwise - this is a slight biasing move but hard to see how else to avoid removing them completely?
    df$Trial.result <- recode(df$Trial.result, '\'refuse\'=0')

    ##First work out Kest by LDRscale
    df$TrialOrderIdx <- 1
    for (i in 1:nrow(df)) {
        if (i >1) {
            if (df[i,]$User.code ==df[i-1,]$User.code & df[i,]$Iteration ==df[i-1,]$Iteration & df[i,]$LDRscale ==df[i-1,]$LDRscale) {
                df$TrialOrderIdx[i] <- df$TrialOrderIdx[i-1] +1
            } else {
                df$TrialOrderIdx[i] <- 1
            }
        }
        #Sum of higher and equal k choices which are 1 (LDR)
        df$Consistency[i] <- sum(as.numeric(df$Trial.result[i:(i+9-df$TrialOrderIdx[i])]))
        #Add on the sum of all the lower K ones which are 0 (SIR)
        if ( df$TrialOrderIdx[i] > 1) {
            df$Consistency[i] <- df$Consistency[i] + ((df$TrialOrderIdx[i] -1)-sum(as.numeric(df$Trial.result[(i - (df$TrialOrderIdx[i] -1) ):(i-1)])))
        }
    }
    #Add a max consistency field
    df <- merge(df, aggregate(Consistency~User.code+Iteration+LDRscale, function(x) c(mean = max(as.numeric(x))), data=df), by=c("User.code", "Iteration", "LDRscale"),suffixes = c("",".max"), all.x=TRUE)
    df$Kest <- NA
    # Calculate the Kest field for each max consistency - based on the geo mean of the max and preceding
    for (i in 1:nrow(df)) {
        if (df$Consistency[i] == df$Consistency.max[i]) {
            df$Kest[i]<-exp(mean(log(c(df$Kind[i], df$Kind[i-1]))))
        }
    }

    #Finally make a geomean of all the max consistencies geomeans as their final outcome
    dfsums <- do.call(data.frame, aggregate(cbind(Kest)~User.code+Iteration+LDRscale, function(x)
      c(geomean=exp(mean(log(x)))), data=df))
    dfsums <- reshape(dfsums, direction = "wide", idvar = c("User.code", "Iteration"), timevar = "LDRscale")

    ##Next overall
    df <- df[order(df$User.code, df$Iteration, df$Kind),]
    df <- subset(df, select=-c(TrialOrderIdx, Consistency, Consistency.max))
    df$TrialOrderIdx <- 1
    for (i in 1:nrow(df)) {
        if (i >1) {
            if (df[i,]$User.code ==df[i-1,]$User.code & df[i,]$Iteration ==df[i-1,]$Iteration) {
                df$TrialOrderIdx[i] <- df$TrialOrderIdx[i-1] + 1
            } else {
                df$TrialOrderIdx[i] <- 1
            }
        }
        #Sum of higher and equal k choices which are 1 (LDR)
        df$Consistency[i] <- sum(as.numeric(df$Trial.result[i:(i+27-df$TrialOrderIdx[i])]))
        #Add on the sum of all the lower K ones which are 0 (SIR)
        if (df$TrialOrderIdx[i] > 1) {
            df$Consistency[i] <- df$Consistency[i] + ((df$TrialOrderIdx[i] -1)-sum(as.numeric(df$Trial.result[(i - (df$TrialOrderIdx[i] -1) ):(i-1)])))
        }
    }
    #Add a max consistency field
    df <- merge(df, aggregate(Consistency~User.code+Iteration, function(x) c(mean = max(as.numeric(x))), data=df), by=c("User.code", "Iteration"),suffixes = c("",".max"), all.x=TRUE)
    df$Kest <- NA
    # Calculate the Kest field for each max consistency - based on the geo mean of the max and preceding
    for (i in 1:nrow(df)) {
        if (df$Consistency[i] == df$Consistency.max[i]) {
            df$Kest[i] <- exp(mean(log(c(df$Kind[i], df$Kind[i-1]))))
        }
    }

    #Finally make a geomean of all the max consistencies geomeans as their final outcome - Merge it into the Kest by LDRscale from earlier
    dfsums <- merge(do.call(data.frame, aggregate(cbind(Kest)~AgeGroup+User.code+Iteration+Language+Completed+Completed.Timestamp+Processed.Timestamp, function(x)
      c(geomean=exp(mean(log(x)))), data=df)), 
      dfsums,
      by=c("User.code", "Iteration"))

    return (dfsums)
}
