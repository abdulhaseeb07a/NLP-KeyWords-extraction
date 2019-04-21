#Install Libraries
install.packages("devtools")
library(devtools)
install_github("C://Downloads/pluralize-master.zip")

install.packages("tidytext")
install.packages("udpipe")
install.packages("qdapDictionaries")

#Include Libraries
#Github Packages
devtools::install("C:\\Downloads\\pluralize-master\\pluralize-master")

library(tidyverse)
library(tidytext)
library(udpipe)
library(qdapDictionaries)
library(pluralize)


#################################################################

NFMLExperimentMSM <- read.csv("N:/ML/NLP/KeywordExtract/DataStore/USER_COMMENT_VISIT_OBS.csv")

##### Filter Data Set
NFMLExperimentMSM01 <- NFMLExperimentMSM

#Remove 1 and 2 character string
NFMLExperimentMSM01 <- NFMLExperimentMSM01 %>% filter(nchar(as.character(USER_COMMENT)) != 1)
NFMLExperimentMSM01 <- NFMLExperimentMSM01 %>% filter(nchar(as.character(USER_COMMENT)) != 2)

#Remove N/A And Empty string
NFMLExperimentMSM01 <- NFMLExperimentMSM01 %>% filter(as.character(USER_COMMENT) != "N/A")
NFMLExperimentMSM01 <- NFMLExperimentMSM01 %>% filter(as.character(USER_COMMENT) != "...")
NFMLExperimentMSM01 <- NFMLExperimentMSM01 %>% filter(as.character(USER_COMMENT) != "")

head(NFMLExperimentMSM01)

fix.newlinetabs <- function(doc) {
    # "won't" is a special case as it does not expand to "wo not"
    doc <- gsub("\n", "", doc)
    doc <- gsub("\t", "", doc)
    doc <- gsub("-", "", doc)
    doc <- gsub(";", "", doc)
    doc <- gsub(":", " ", doc)
    doc <- gsub("$", "", doc)
    doc <- gsub("#", "", doc)
    doc <- gsub("%", "", doc)
    doc <- gsub("*", "", doc)
    doc <- gsub("\\(", "", doc)
    doc <- gsub("\\)", "", doc)
    doc <- gsub("_", "", doc)
    doc <- gsub("=", "", doc)
    doc <- gsub("!", "", doc)
    doc <- gsub("@", "", doc)
    doc <- gsub("/", "", doc)
    doc <- gsub("\\\\", "", doc)
    return(doc)
}

#Drop existing process columns
#MLExperimentMSM01[, "processed_comment"] = NULL
#MLExperimentMSM01[, "Key.Phrases"] = NULL


fix.contractions <- function(doc) {
    # "won't" is a special case as it does not expand to "wo not"
    doc <- gsub("won't", "will not", doc)
    doc <- gsub("can't", "can not", doc)
    doc <- gsub("n't", " not", doc)
    doc <- gsub("'ll", " will", doc)
    doc <- gsub("'re", " are", doc)
    doc <- gsub("'ve", " have", doc)
    doc <- gsub("'m", " am", doc)
    doc <- gsub("'d", " would", doc)
    # 's could be 'is' or could be possessive: it has no expansion
    doc <- gsub("'s", "", doc)
    doc <- gsub("&", "and", doc)
    return(doc)
}
NFMLExperimentMSM01$OBS_INITIAL_COMMENT <- sapply(NFMLExperimentMSM01$USER_COMMENT, fix.newlinetabs) #This is additional becuase removeSpecialChars will do this
NFMLExperimentMSM01$OBS_INITIAL_COMMENT <- sapply(NFMLExperimentMSM01$USER_COMMENT, fix.contractions)

#remove dublicates
head(MLExperimentMSM01, 30)
NFMLExperimentMSM01 <- NFMLExperimentMSM01[!duplicated(NFMLExperimentMSM01$USER_COMMENT),]

# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
# remove special characters
NFMLExperimentMSM01$OBS_INITIAL_COMMENT <- sapply(NFMLExperimentMSM01$USER_COMMENT, removeSpecialChars)

# convert everything to lower case
NFMLExperimentMSM01$OBS_INITIAL_COMMENT <- sapply(NFMLExperimentMSM01$USER_COMMENT, tolower)

#Remove some meaningless words manually
meaninglessWords <- c("test", "tttt", "hhyj", "gghf", "nhjh", "cvcv", "vbvb", "bvbv", "kjkj", "dfdf", "fgfg", "sfss", "jghg", "gdgg", "tedt", "ynop", "vvvv", "bgbf", "xcxc", "hjhj", "ghgh", "yyyuuuu", "fddf")
NFMLExperimentMSM01 <- NFMLExperimentMSM01 %>% filter(!str_detect(USER_COMMENT, paste(meaninglessWords, collapse = "|")))

#############################################################################################

###############################################################################
#UDPipe - Basic Analytics
NFMLExperimentMSM02 <- NFMLExperimentMSM01
str(MLExperimentMSM02)
NFMLExperimentMSM02$RowID <- seq.int(nrow(NFMLExperimentMSM02))

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
nfx <- udpipe_annotate(ud_model, x = NFMLExperimentMSM02$USER_COMMENT, doc_id = NFMLExperimentMSM02$RowID)
nfx <- as.data.frame(nfx)

#Filter only Noun adjective and verbs
NFfilterNNADJVERB001 <- x %>% filter(x$upos %in% c("NOUN", "ADJ", "VERB", "PART") | (x$upos == "ADP" & x$xpos == "IN" & (x$lemma == "out" | x$lemma == "of")))

NFfilterNNADJVERB001 <- NFfilterNNADJVERB001 %>% filter(!NFfilterNNADJVERB001$xpos %in% c("TO", "POS"))

#Singularize Keywords
NFfilterNNADJVERB001$lemma <- sapply(NFfilterNNADJVERB001$lemma, singularize)

#Removes Keywords which is not word
is.word <- function(x) x %in% GradyAugmented
NFfilterNNADJVERB002 <- NFfilterNNADJVERB001 %>% filter(is.word(lemma))

#Combine "Not" with next word and make negative word and remove "Not" word alone rows
NFfilterNNADJVERB002$KeyWords <- NFfilterNNADJVERB002$lemma
for (i in 1:nrow(NFfilterNNADJVERB002)) {
    if (NFfilterNNADJVERB002[i, "xpos"] == "RB") {
        if (!is.na(NFfilterNNADJVERB002[i + 1, "KeyWords"])) {
            NFfilterNNADJVERB002[i + 1, "KeyWords"] <- paste(NFfilterNNADJVERB002[i, "lemma"], NFfilterNNADJVERB002[i + 1, "token"], sep = " ")
            #paste(cat(i,filterNNADJVERB[i, "lemma"],filterNNADJVERB[i + 1, "lemma"],"\n"))
        }
    }
}

NFfilterNNADJVERB002 <- NFfilterNNADJVERB002 %>% filter(NFfilterNNADJVERB002$KeyWords != "not")
NFfilterNNADJVERB002 <- NFfilterNNADJVERB002 %>% filter(NFfilterNNADJVERB002$KeyWords != "not of")

#Combine "out of with next word"

NFfilterNNADJVERB003 <- NFfilterNNADJVERB002
for (i in 1:nrow(NFfilterNNADJVERB003)) {
    if (NFfilterNNADJVERB003[i, "KeyWords"] == "out") {
        if (NFfilterNNADJVERB003[i + 1, "KeyWords"] == "of") {
            if (!is.na(NFfilterNNADJVERB003[i + 2, "KeyWords"])) {
                NFfilterNNADJVERB003[i + 2, "KeyWords"] <- paste(NFfilterNNADJVERB003[i, "lemma"], NFfilterNNADJVERB003[i + 1, "lemma"], NFfilterNNADJVERB003[i + 2, "token"], sep = " ")
                #paste(cat(i,filterNNADJVERB[i, "lemma"],filterNNADJVERB[i + 1, "lemma"],"\n"))
            }
        }
    }
}
NFfilterNNADJVERB003 <- NFfilterNNADJVERB003 %>% filter(NFfilterNNADJVERB003$KeyWords != "out")
NFfilterNNADJVERB003 <- NFfilterNNADJVERB003 %>% filter(NFfilterNNADJVERB003$KeyWords != "of")

#Remove AUX verb
NFfilterNNADJVERB003 <- NFfilterNNADJVERB003 %>% filter(NFfilterNNADJVERB003$KeyWords != "is")
NFfilterNNADJVERB003 <- NFfilterNNADJVERB003 %>% filter(NFfilterNNADJVERB003$KeyWords != "be")
NFfilterNNADJVERB003 <- NFfilterNNADJVERB003 %>% filter(NFfilterNNADJVERB003$KeyWords != "have")

#Remove stop words
customStopWords <- c("find", "use", "several", "advise", "completion", "renovation", "only", "month")
NFfilterNNADJVERB004 <- NFfilterNNADJVERB003 %>% filter(!str_detect(KeyWords, paste(customStopWords, collapse = "|")))

#Custom words manipulation match the exact key word else part of string will replace
#But not functioning is not working replce manually 
fix.customeWordManipulation <- function(doc) {
    doc <- gsub("damaged", "damage", doc)
    doc <- gsub("\\<not functioning\\> ", "not functional", doc)
    doc <- gsub("\\<not function\\>", "not functional", doc)
    doc <- gsub("\\<miss\\>", "missing", doc)
    doc <- gsub("\\<not look\\>", "not looking", doc)
    doc <- gsub("\\<not looked\\>", "not looking", doc)
    return(doc)
}
NFfilterNNADJVERB004$KeyWords <- sapply(NFfilterNNADJVERB004$KeyWords, fix.customeWordManipulation)
NFfilterNNADJVERB004[NFfilterNNADJVERB004$KeyWords == "not functioning", "KeyWords"] <- "not functional"

NFMLExperimentMSM03 <- data.frame(NFfilterNNADJVERB004$doc_id, NFfilterNNADJVERB004$sentence, NFfilterNNADJVERB004$KeyWords)
colnames(NFMLExperimentMSM03) <- c("RowID", "USER_COMMENT", "keyWords")

#Remove the single character after NLP apply
NFMLExperimentMSM03 <- NFMLExperimentMSM03 %>% filter(nchar(as.character(keyWords)) != 1)

#Remove duplicate if same keyword repeat for same comments
NFMLExperimentMSM04 <- NFMLExperimentMSM03 %>% count(RowID, keyWords, OBS_INITIAL_COMMENT, sort = TRUE)
NFMLExperimentMSM04[, "n"] <- NULL
head(MLExperimentMSM05)

#Convert tibble to dataframe
NFMLExperimentMSM05 <- as.data.frame(NFMLExperimentMSM04)
NFMLExperimentMSM05[, 'RowID'] <- as.numeric(as.character(NFMLExperimentMSM05[, 'RowID']))


############ Merge keyword and original data set and generate csv ###########################

mergeDF <- merge(x = NFMLExperimentMSM05, y = NFMLExperimentMSM02, by = "RowID")
mergeDF[, "USER_COMMENT.y"] <- NULL

write.table(mergeDF, file = "USER_COMMENT_EXTRACTEDKEYWORD_OBS001.csv", sep = ",", qmethod = "double" , row.names = FALSE)
