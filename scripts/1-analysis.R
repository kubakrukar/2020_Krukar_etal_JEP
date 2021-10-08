source(here("scripts", "options.R"))
source(here("scripts", "checks.R"))

df <- read_excel(here("data", "my_modf_data.xlsx"), na = "empty")
df.ind <- select(df, Code, Gender, Age, GlobalSelfConfidence, SurveyStrtegy, CardinalDirKnow, MRT1SCORE, MRT2SCORE, PTError_circle, PTError_map, EstDist)
df <- select(df, Code, Gender, Age, GlobalSelfConfidence, SurveyStrtegy, CardinalDirKnow, MRT1SCORE, MRT2SCORE)

# MRT1SCORE       - easier instance of objective test (Muenzer is subjective)
# MRT2SCORE       - harder instance
# PTError	        - pointing error from goal to destination (0-180 deg)   (only visual condition)
# EstDist(km)     - estimated distance from goal to desination            (only visual condition)
# Repeat#         - how many times they repeated the verbal instructions  (only verbal condition)

df      <- df[1:84,] # filter out some NAs from the end
df$Code <- gsub(" ", "", df$Code, fixed = TRUE)
df      <- rename(df, part.ID = Code)

df.ind            <- df.ind[1:84,] # filter out some NAs from the end
df.ind$Code       <- gsub(" ", "", df.ind$Code, fixed = TRUE)
df.ind            <- rename(df.ind, part.ID = Code)
df.ind[grep("TbT", df.ind$part.ID), "instr.type"] <- "TbT"
df.ind[grep("SC", df.ind$part.ID), "instr.type"]  <- "SC"
df.ind[grep("OI", df.ind$part.ID), "instr.type"]  <- "OI"
df.ind$part.ID    <- as.factor(df.ind$part.ID)
df.ind$Gender     <- as.factor(df.ind$Gender)
df.ind$instr.type <- as.factor(df.ind$instr.type)

# for merging 'landmark name' with 'landmark type'
ldnm.type            <- read_excel(here("data", "my_modf_data.xlsx"), sheet = "landmark_types", na = "empty")
ldnm.type            <- unique(ldnm.type)
ldnm.type[ldnm.type$ldnm.type == "LLAR", "ldnm.type"] <- "LAR"
ldnm.type$ldnm.type  <- as.factor(ldnm.type$ldnm.type)

# load each sheet...
OIVer_incl          <- read_excel(here("data", "my_modf_data.xlsx"), sheet = "OIVer_incl", na = "empty")
OIVer_incl$part.ID  <- paste0("OI", OIVer_incl$part.ID)
OIVer_incl          <- gather(OIVer_incl[1:28, 1:14], key = landmark, value = included, c(2:14))
OIVer_incl          <- mutate(OIVer_incl, instr.type = "OI", instr.media = "verbal")  
OIVer_incl          <- left_join(OIVer_incl, ldnm.type, by="landmark") 
OIVer_incl$landmark <- paste0(OIVer_incl$landmark, ".OIVer")

OIVer_corr          <- read_excel(here("data", "my_modf_data.xlsx"), sheet = "OIVer_corr", na = "empty")
OIVer_corr$part.ID  <- paste0("OI", OIVer_corr$part.ID)
OIVer_corr          <- gather(OIVer_corr[1:28, 1:14], key = landmark, value = correct, c(2:14))
OIVer_corr          <- mutate(OIVer_corr, instr.type = "OI", instr.media = "verbal")  
OIVer_corr          <- left_join(OIVer_corr, ldnm.type, by="landmark") 
OIVer_corr$landmark <- paste0(OIVer_corr$landmark, ".OIVer")

OIVis_incl          <- read_excel(here("data", "my_modf_data.xlsx"), sheet = "OIVis_incl", na = "empty")
OIVis_incl$part.ID  <- paste0("OI", OIVis_incl$part.ID)
OIVis_incl          <- gather(OIVis_incl[1:28, 1:16], key = landmark, value = included, c(2:16))
OIVis_incl          <- mutate(OIVis_incl, instr.type = "OI", instr.media = "visual")  
OIVis_incl          <- left_join(OIVis_incl, ldnm.type, by="landmark") 
OIVis_incl$landmark <- paste0(OIVis_incl$landmark, ".OIVis")

OIVis_corr          <- read_excel(here("data", "my_modf_data.xlsx"), sheet = "OIVis_corr", na = "empty")
OIVis_corr$part.ID  <- paste0("OI", OIVis_corr$part.ID)
OIVis_corr          <- gather(OIVis_corr[1:28, 1:16], key = landmark, value = correct, c(2:16))
OIVis_corr          <- mutate(OIVis_corr, instr.type = "OI", instr.media = "visual")  
OIVis_corr          <- left_join(OIVis_corr, ldnm.type, by="landmark") 
OIVis_corr$landmark <- paste0(OIVis_corr$landmark, ".OIVis")

SCVer_incl          <- read_excel(here("data", "my_modf_data.xlsx"), sheet = "SCVer_incl", na = "empty")
SCVer_incl$part.ID  <- paste0("SC", SCVer_incl$part.ID)
SCVer_incl          <- gather(SCVer_incl[1:28, 1:6], key = landmark, value = included, c(2:6))
SCVer_incl          <- mutate(SCVer_incl, instr.type = "SC", instr.media = "verbal")  
SCVer_incl          <- left_join(SCVer_incl, ldnm.type, by="landmark") 
SCVer_incl$landmark <- paste0(SCVer_incl$landmark, ".SCVer")

SCVer_corr          <- read_excel(here("data", "my_modf_data.xlsx"), sheet = "SCVer_corr", na = "empty")
SCVer_corr$part.ID  <- paste0("SC", SCVer_corr$part.ID)
SCVer_corr          <- gather(SCVer_corr[1:28, 1:6], key = landmark, value = correct, c(2:6))
SCVer_corr          <- mutate(SCVer_corr, instr.type = "SC", instr.media = "verbal")  
SCVer_corr          <- left_join(SCVer_corr, ldnm.type, by="landmark") 
SCVer_corr$landmark <- paste0(SCVer_corr$landmark, ".SCVer")

SCVis_incl          <- read_excel(here("data", "my_modf_data.xlsx"), sheet = "SCVis_incl", na = "empty")
SCVis_incl$part.ID  <- paste0("SC", SCVis_incl$part.ID)
SCVis_incl          <- gather(SCVis_incl[1:28, 1:13], key = landmark, value = included, c(2:13))
SCVis_incl          <- mutate(SCVis_incl, instr.type = "SC", instr.media = "visual")  
SCVis_incl          <- left_join(SCVis_incl, ldnm.type, by="landmark") 
SCVis_incl$landmark <- paste0(SCVis_incl$landmark, ".SCVis")

SCVis_corr          <- read_excel(here("data", "my_modf_data.xlsx"), sheet = "SCVis_corr", na = "empty")
SCVis_corr$part.ID  <- paste0("SC", SCVis_corr$part.ID)
SCVis_corr          <- gather(SCVis_corr[1:28, 1:13], key = landmark, value = correct, c(2:13))
SCVis_corr          <- mutate(SCVis_corr, instr.type = "SC", instr.media = "visual")  
SCVis_corr          <- left_join(SCVis_corr, ldnm.type, by="landmark") 
SCVis_corr$landmark <- paste0(SCVis_corr$landmark, ".SCVis")

TbTVis_incl         <- read_excel(here("data", "my_modf_data.xlsx"), sheet = "TbTVis_incl", na = "empty")
TbTVis_incl$part.ID <- paste0("TbT", TbTVis_incl$part.ID)
TbTVis_incl         <- gather(TbTVis_incl[1:28, 1:13], key = landmark, value = included, c(2:13))
TbTVis_incl         <- mutate(TbTVis_incl, instr.type = "TbT", instr.media = "visual")  
TbTVis_incl         <- left_join(TbTVis_incl, ldnm.type, by="landmark") 
TbTVis_incl$landmark<- paste0(TbTVis_incl$landmark, ".TbTVis")

TbTVis_corr         <- read_excel(here("data", "my_modf_data.xlsx"), sheet = "TbTVis_corr", na = "empty")
TbTVis_corr$part.ID <- paste0("TbT", TbTVis_corr$part.ID)
TbTVis_corr         <- gather(TbTVis_corr[1:28, 1:13], key = landmark, value = correct, c(2:13))
TbTVis_corr         <- mutate(TbTVis_corr, instr.type = "TbT", instr.media = "visual")  
TbTVis_corr         <- left_join(TbTVis_corr, ldnm.type, by="landmark") 
TbTVis_corr$landmark<- paste0(TbTVis_corr$landmark, ".TbTVis")
# ...and merge it with the data
OIVer <- left_join(OIVer_corr, OIVer_incl)
OIVis <- left_join(OIVis_corr, OIVis_incl)
SCVer <- left_join(SCVer_corr, SCVer_incl)
SCVis <- left_join(SCVis_corr, SCVis_incl)
TbTVis <- left_join(TbTVis_corr, TbTVis_incl)
df2 <- rbind(OIVer, OIVis)
df2 <- rbind(df2, SCVer)
df2 <- rbind(df2, SCVis)
df2 <- rbind(df2, TbTVis)
df3 <- left_join(df2, df)
rm(OIVer_incl, OIVer_corr, OIVis_incl, OIVis_corr, SCVer_incl, SCVer_corr, SCVis_incl, SCVis_corr, TbTVis_incl, TbTVis_corr)

df3 <- df3 %>% select(part.ID, instr.type, instr.media, Gender, Age, GlobalSelfConfidence, SurveyStrtegy, CardinalDirKnow, 
                      MRT1SCORE, MRT2SCORE, landmark, ldnm.type, included, correct)

df3$part.ID       <- as.factor(df3$part.ID)
df3$instr.type    <- as.factor(df3$instr.type)
df3$instr.media   <- as.factor(df3$instr.media)
df3$Gender        <- as.factor(df3$Gender)
df3$landmark      <- as.factor(df3$landmark)
df3$ldnm.type     <- as.factor(df3$ldnm.type)
df3$included      <- as.logical(as.numeric(df3$included))
df3$correct       <- as.logical(df3$correct)

# work on split data (two models, instead of one)
vis <- filter(df3, instr.media == "visual")
vis <- droplevels(vis)

ver <- filter(df3, instr.media == "verbal")
ver <- droplevels(ver)

rm(df, df2, ldnm.type, OIVer, OIVis, SCVer, SCVis, TbTVis)

# surveyness/routeness data ####
df.sr              <- read_excel(here("data", "mySurvRout.xlsx"))
df.sr$part.ID      <- gsub(" ", "", df.sr$part.ID, fixed = TRUE)
df.sr$part.ID      <- as.factor(df.sr$part.ID)
df.sr$instr.media  <- as.factor(df.sr$instr.media)
df.sr$instr.type   <- as.factor(df.sr$instr.type)

df.sr.vis <- filter(df.sr, instr.media=="visual")
df.sr.ver <- filter(df.sr, instr.media=="verbal")

df.sr.ord <- df.sr
df.sr.ord$routeness  <- as.ordered(df.sr.ord$routeness)
df.sr.ord$surveyness <- as.ordered(df.sr.ord$surveyness)
df.sr.ord <- left_join(df.sr.ord, df.ind)
df.sr <- left_join(df.sr, df.ind)
dfs <- df.sr %>% group_by(instr.media) %>% summarise(mean.routeness = mean(routeness), mean.surveyness = mean(surveyness))


warning("landmarks named uniquely across conditions")
levels(df3$landmark)
warning("no responses for condition ---verbal / TbT---")
table(df3$instr.media, df3$instr.type)

