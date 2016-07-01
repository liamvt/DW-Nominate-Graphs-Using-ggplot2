library(openxlsx) # Note that read.xlsx from the xlsx package will yield an error
                  # when reading in this data on many systems
library(ggplot2)
library(foreign)
library(dplyr)

### Download the house and senate nominate score data

if(!file.exists("data")){dir.create("data")}
fileUrl <- "http://voteview.uga.edu/ftp/junkord/Sl01113d21_bsse.xlsx"
download.file(fileUrl,destfile="./data/senate.xlsx", mode='wb')

if(!file.exists("data")){dir.create("data")}
fileUrl <- "http://voteview.uga.edu/ftp/junkord/hl01113d21_bsse.xlsx"
download.file(fileUrl,destfile="./data/house.xlsx", mode='wb')


senate <- read.xlsx("./data/senate.xlsx", colNames = FALSE)
house <- read.xlsx("./data/house.xlsx", colNames = FALSE)

varNames = c("CongressNumber", "id", "StateCode", "DistrictNumber", 
             "State", "PartyCode", "Name", "DimOneCoord", "DimTwoCoord", 
             "DimOneSE", "DimTwoSE", "CorrBetwDimSEs", "LogLikelihood", 
             "NumVotes", "NumofClassErrors", "GeomMeanProb")

names(house) <- varNames
names(senate) <- varNames


## It is possible for nominate scores to be < -1 and > 1 in certain congresses
## though not over the entire career of a congressman
## Use the following to set -1 and 1 as the min and max score

house$DimOneCoord[house$DimOneCoord > 1] <- 1
house$DimOneCoord[house$DimOneCoord < -1] <- -1
house$DimTwoCoord[house$DimTwoCoord > 1] <- 1
house$DimTwoCoord[house$DimTwoCoord < -1] <- -1

senate$DimOneCoord[senate$DimOneCoord > 1] <- 1
senate$DimOneCoord[senate$DimOneCoord < -1] <- -1
senate$DimTwoCoord[senate$DimTwoCoord > 1] <- 1
senate$DimTwoCoord[senate$DimTwoCoord < -1] <- -1


### Nominate Scores for the 111th House by Party

house111 <- subset(house, CongressNumber==111)

house111$Party <- ifelse(house111$PartyCode==100, "Dem", "Rep")
house111$Party <- as.factor(house111$Party)

jpeg(file = "house111.jpeg")

ggplot(house111, aes(x = DimOneCoord, y = DimTwoCoord, col = Party)) + 
        geom_point() +
        scale_color_manual(values=c("blue", "red")) +
        labs(title = "House Nomimate scores: 111th Congress (2009-11)",
             x = "1st Dimension (Liberal-Conservative)", 
             y = "2nd Dimension (Region)") +
        theme(plot.title = element_text(size = 20), 
              axis.title = element_text(size = 16),
              panel.background = element_rect(fill = "grey85")) +
        scale_x_continuous(limits = c(-1, 1))

dev.off()

### Nominate Scores for the 99th House by Party

house99 <- subset(house, CongressNumber==99)

house99$Party <- ifelse(house99$PartyCode==100, "Dem", "Rep")
house99$Party <- as.factor(house99$Party)

jpeg(file = "house99.jpeg")

ggplot(house99, aes(x = DimOneCoord, y = DimTwoCoord, col = Party)) + 
        geom_point() +
        scale_color_manual(values=c("blue", "red")) +
        labs(title = "House Nomimate scores: 99th Congress (1985-87)",
             x = "1st Dimension (Liberal-Conservative)", 
             y = "2nd Dimension (Region)") +
        theme(plot.title = element_text(size = 20), 
              axis.title = element_text(size = 16),
              panel.background = element_rect(fill = "grey85")) +
        scale_x_continuous(limits = c(-1, 1))

dev.off()


### Nominate Scores for the 89th House by Party

house89 <- subset(house, CongressNumber==89)
house89$Party <- ifelse(house89$PartyCode==100, "Dem", "Rep")
house89$Party <- as.factor(house89$Party)

jpeg(file = "house89.jpeg")

ggplot(house89, aes(x = DimOneCoord, y = DimTwoCoord, col = Party)) + 
        geom_point() +
        scale_color_manual(values=c("blue", "red")) +
        labs(title = "House Nomimate scores: 89th Congress (1965-67)",
             x = "1st Dimension (Liberal-Conservative)", 
             y = "2nd Dimension (Region)") +
        theme(plot.title = element_text(size = 20), 
              axis.title = element_text(size = 16),
              panel.background = element_rect(fill = "grey85")) +
        scale_x_continuous(limits = c(-1, 1))

dev.off()


### Combined graph with 89th, 99th and 111th congresses

houseComb <- rbind(house89, house99, house111)
houseComb$CongressNumber <- as.character(houseComb$CongressNumber)
houseComb$CongressNumber <- factor(houseComb$CongressNumber,
                                   levels = c("89", "99", "111"))

jpeg(file = "houseComb.jpeg")

ggplot(houseComb, aes(x = DimOneCoord, y = DimTwoCoord, col = Party)) + 
        geom_point() +
        scale_color_manual(values=c("blue", "red")) +
        labs(title = "House Nomimate scores: 89th, 99th & 111th Congresses",
             x = "1st Dimension (Liberal-Conservative)", 
             y = "2nd Dimension (Region)") +
        theme(plot.title = element_text(size = 20), 
              axis.title = element_text(size = 16),
              panel.background = element_rect(fill = "grey85")) +
        facet_grid(CongressNumber ~ .) +
        scale_x_continuous(limits = c(-1, 1))

dev.off()


### Download Roll-Call data for the 111th house 

if(!file.exists("data")){dir.create("data")}
fileUrl <- "ftp://voteview.com/wf1/hou111kh.dta"
download.file(fileUrl,destfile="./data/hou111kh.dta", mode='wb')

houseRC111 <- read.dta("./data/hou111kh.dta")

### House Obamacare vote by Party

OcareRCHouse <- select(houseRC111, id, V885) #Select the Obamacare roll call vote

merged111 <- merge(house111, OcareRCHouse)
table(merged111$V885)
merged111 <- merged111[merged111$V885 > 0, ] # Only include Senators sitting at 
# the time of the vote
table(merged111$V885)

merged111$V885 <- ifelse(merged111$V885==1, "Y", "N")
merged111 <- rename(merged111, Vote = V885)

jpeg(file = "houseObamacare.jpeg")

ggplot(merged111, aes(x = DimOneCoord, y = DimTwoCoord, col = Party, label = Vote)) + 
        geom_text(size = 6) +
        scale_color_manual(values=c("blue", "red")) +
        labs(title = "Roll Call Vote for the PPACA 'House Bill', Nov 7, 2009: 220-215",
             x = "1st Dimension (Liberal-Conservative)", 
             y = "2nd Dimension (Region)") +
        theme_set(theme_grey()) +
        theme(plot.title = element_text(size = 20), 
              axis.title = element_text(size = 16),
              legend.position = "none",
              panel.background = element_rect(fill = "grey85")) +
        scale_x_continuous(limits = c(-1, 1))

dev.off()


## Extract the 111th Senate from the senate DF

senate111 <- subset(senate, CongressNumber==111)

senate111$Party <- ifelse(senate111$PartyCode==100, "Dem", "Rep")
x <- which(senate111$Name == "SANDERS") # Change Sanders to Socialist
senate111$Party[x] <- "Soc"
senate111$Party <- as.factor(senate111$Party)


### Download Roll-Call data for the 111th house 

if(!file.exists("data")){dir.create("data")}
fileUrl <- "ftp://voteview.com/wf1/sen111kh.dta"
download.file(fileUrl,destfile="./data/sen111kh.dta", mode='wb')

senateRC111 <- read.dta("./data/sen111kh.dta")

### Senate Obamacare vote by party with pivots

OcareRCSenate <- select(senateRC111, id, V396) #Select the Obamacare vote

mergedS111 <- merge(senate111, OcareRCSenate)
table(mergedS111$V396)
mergedS111 <- subset(mergedS111, StateCode < 99)

mergedS111 <- mergedS111[mergedS111$V396 > 0, ]
table(mergedS111$V396)

mergedS111$V396 <- as.character(mergedS111$V396)
mergedS111$V396[mergedS111$V396 == "1"] <- "Y"
mergedS111$V396[mergedS111$V396 == "6"] <- "N"
mergedS111$V396[mergedS111$V396 == "9"] <- "A"
table(mergedS111$V396)

mergedS111 <- rename(mergedS111, Vote = V396)
mergedS111 <- arrange(mergedS111, DimOneCoord)
FP <- mergedS111$DimOneCoord[60] #Find the filibuster pivot (60th most left Senator)

jpeg(file = "senateObamacare.jpeg")

ggplot(mergedS111, aes(x = DimOneCoord, y = DimTwoCoord, col = Party, label = Vote)) + 
        geom_text(size = 6) +
        scale_color_manual(values=c("blue", "red", "green")) +
        geom_vline(xintercept = FP, linetype = 2) +
        geom_vline(xintercept = -0.364, linetype = 2) +
        labs(title = "Roll Call Vote for the PPACA 'Senate Bill' with Filibuster Pivot and President Ideal Points",
             x = "1st Dimension (Liberal-Conservative)", 
             y = "2nd Dimension (Region)") +
        theme_set(theme_grey()) +
        theme(plot.title = element_text(size = 20), 
              axis.title = element_text(size = 16),
              legend.position = "none",
              panel.background = element_rect(fill = "grey85")) +
        scale_x_continuous(limits = c(-1, 1))

dev.off()
