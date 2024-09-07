# FOR ANALYZING A .out FILE
# First: read in an outputs file: 
rm(list=ls()) # get rid of prior variables
require(tidyverse)
require(ggplot2)
options(scipen=100) # turns off scientific notation so that reading in file names goes smoothly. =
all.outputs.saved <- FALSE # if this is "true" it will read in all the output csv files that were saved (if they were saved) and it takes way longer. 
thresh.forticks <- 0.5
num.updates.ops.list <- list(c(15000,23000,32900),
                        c(5000,10000,17200))
example.names <- c("2021-01-18_8t6i6oexamples",
                   "2021-01-19_14t4i2oexamples")
ex.name.shorts <- c("Impoverished","Intermediate")
prime.df.filenames <- c(paste("primetestpairsdf_",example.names[1],".txt",sep=""),
                        paste("primetestpairsdf_",example.names[2],".txt",sep=""),
                        paste("primetestpairsdf_encbestunrel_",example.names[3],".txt",sep=""))
# language <- strsplit(strsplit(example.name,"examples")[[1]][1],"_")[[1]][2]
prime.durs <- c("0.2prime0.0ISI4.0start",# "0.4prime0.0ISI4.0start",
                "0.6prime0.0ISI4.0start",# "0.8prime0.0ISI4.0start",
                "1.0prime0.0ISI4.0start",# "1.2prime0.0ISI4.0start",
                "1.4prime0.0ISI4.0start",# "1.6prime0.0ISI4.0start",
                "1.8prime0.0ISI4.0start")# ,"2.0prime0.0ISI4.0start")
paths <- c("~/Users/username/Desktop/recurnet_morphproc/2_testing/14t4i2o_language",
           "~/Users/username/Desktop/recurnet_morphproc/2_testing/14t4i2o_language")
net.name <- "dynamorph"
targ.time <- 6.0
thresh.df <- data.frame() 
se <- function(x){sd(x)/(sqrt(length(x)))}

# for each language 
for (lang in 1:length(example.names)) {
  prime.df.filename <- prime.df.filenames[lang]
  # read in example info 
  setwd(paths[lang])
  primes.df <- read.table(prime.df.filename,header=TRUE)
  primes.df$example <- 0:(nrow(primes.df)-1)
  # if (max(primes.df$example) != max(outs.toplot$example)) {print("Example mismatch!")}
  colnames(primes.df)[1:2] <- c("target.word","prime.word")
  
  ### *** Plot RTs for meeting "yes" output threshold of 0.5 *** ###
  print("...now plotting RTs")
  num.updates.ops <- num.updates.ops.list[[lang]]
  example.name <- example.names[lang]
  # for each time point in training... 
  for (num.updates in num.updates.ops) {
    for (prime.dur in prime.durs) {
      pd <- as.numeric(strsplit(prime.dur,split="prime")[[1]][1])
      nu <- read.table(paste("RTs_",num.updates,"_",example.name,
                             "_",prime.dur,"_",thresh.forticks,
                             "thresh.txt",sep=""),header=TRUE)
      nu.bound <- cbind(primedur=rep(pd,nrow(nu)),
                        timepoint = rep(which(num.updates.ops == num.updates),nrow(nu)),
                        nu)
      thresh.df <- rbind(thresh.df, cbind(primedur=rep(pd,nrow(nu)),
                                          timepoint = rep(which(num.updates.ops == num.updates),nrow(nu)),
                                          language = rep(ex.name.shorts[lang]),
                                          nu))
    }
  }
}
thresh.df <- left_join(thresh.df,primes.df,by="example")

sum(thresh.df$timepoint == 1 & thresh.df$language == 'Intermediate')

print("...plotting RT diffs")
# match up unrelated pairs with corresponding related pairs
thresh.match <- thresh.df %>%
  select(primedur,RT,target.word,condition,pos,relation,timepoint,language) %>%
  pivot_wider(names_from="relation",values_from="RT")
# get unrelated vals for Identity condition from transparent
for (i in which(thresh.match$condition=="Identity")){
  ind <- which(thresh.match$primedur==thresh.match$primedur[i] & 
                 thresh.match$target.word==thresh.match$target.word[i] & 
                 thresh.match$condition=="Transparent" & 
                 thresh.match$pos == thresh.match$pos[i] & 
                 thresh.match$timepoint==thresh.match$timepoint[i] & 
                 thresh.match$language==thresh.match$language[i])
  if (length(ind)==1) {
    thresh.match$Unrelated[i] <- thresh.match$Unrelated[ind]
  } else if (length(ind) > 1) {print(paste("problem!",ind))}
}

setwd("~/Users/username/Desktop/recurnet_morphproc/2_testing") # plot across languages requires stepping back in directory. 

# relevel priming condition labels 
thresh.match$condition[which(thresh.match$condition == "Form")] <- "Orthographic"
thresh.match$condition <- factor(thresh.match$condition)
thresh.match$condition <- relevel(thresh.match$condition, "Semantic")
thresh.match$condition <- relevel(thresh.match$condition, "Opaque")
thresh.match$condition <- relevel(thresh.match$condition, "Intermediate")
thresh.match$condition <- relevel(thresh.match$condition, "Transparent")
thresh.match$condition <- relevel(thresh.match$condition, "Identity")

# plot
time.labs = c("Time Point 1", "Time Point 2","Time Point 3")
names(time.labs) = c(1,2,3)
thresh.match$primedur.ticks <- as.integer(thresh.match$primedur * 5)
missing.pairs <- sum(is.na(thresh.match$Unrelated) | is.na(thresh.match$Related))
theme_set(theme_light(base_size = 20))
thresh.match.norich <- filter(thresh.match,language %in% c("Impoverished","Intermediate"))
thresh.match.norich$language[which(thresh.match.norich$language == "Intermediate")] <- "Morph. Rich Language"
thresh.match.norich$language[which(thresh.match.norich$language == "Impoverished")] <- "Morph. Impov. Language"
thresh.match.norich %>% 
  mutate(RT.diff = Unrelated-Related) %>%
  drop_na() %>% 
  group_by(primedur.ticks,condition,timepoint,language) %>% 
  summarise(mn.RTdiff = mean(RT.diff),se.RTdiff = se(RT.diff)) %>%
  ggplot(.,aes(x=primedur.ticks,y=mn.RTdiff,col=condition)) + 
  geom_pointrange(aes(ymin=mn.RTdiff-se.RTdiff,ymax=mn.RTdiff+se.RTdiff)) + 
  scale_x_continuous(breaks = c(5,10)) + 
  geom_line() + facet_grid(language~timepoint,labeller=labeller(timepoint = time.labs))  + 
  labs(title = "Simulated priming effects for both languages, over training", # instead of "across languages" which I used for three langs. 
       x = "Prime duration (ticks)",y="Simulated priming magnitude",col="Condition")
ggsave(paste("RTdiffs_justtwo_","thresh",thresh.forticks,"_bypdurcondlangeps.png",sep=""), plot = last_plot(),
       device="png",width = 12, height = 10)
print("done")

options(scipen=0)

# Stats analysis for thresh.match
  
ggplot(thresh.df,aes(x=RT)) + geom_histogram() # can probably be sufficiently approximated as gaussian.

require(lme4)
require(lmerTest)
thresh.df.sm <- thresh.df %>% filter(primedur %in% c(0.2, 0.8,1.4,2.0) & language %in% c("Impoverished","Intermediate")) %>% mutate(primedur.fact = factor(primedur))
thresh.df.sm$language[which(thresh.df.sm$language == "Intermediate")] <- "Rich"
id.unrel <- filter(thresh.df.sm, condition == "Transparent" & relation == "Unrelated")
id.unrel$condition <- "Identity"
thresh.df.sm <- rbind(thresh.df.sm,id.unrel)
RTs.thwy.simple <- lmer(RT ~ language*condition*primedur.fact*relation + (1|target.word), #  + (1|pos),
                      data = filter(thresh.df.sm, timepoint == 3))

anova(RTs.thwy.simple) # F = 7.98, p < 0.0001

RTs.thwy.simple.tp1 <- lmer(RT ~ language*condition*primedur.fact*relation + (1|target.word), #  + (1|pos),
                        data = filter(thresh.df.sm, timepoint == 1))
anova(RTs.thwy.simple.tp1) #  F = 1.74, p  = 0.037

RTs.thwy.simple.tp2 <- lmer(RT ~ language*condition*primedur.fact*relation + (1|target.word), #  + (1|pos),
                            data = filter(thresh.df.sm, timepoint == 2))
anova(RTs.thwy.simple.tp2) # F = 5.93, p < 0.0001

thresh.df.sm$tid <- "no"
thresh.df.sm$tid[which(thresh.df.sm$condition %in% c("Transparent", "Identity"))] <- "yes"

summary(lmer(RT ~ tid*relation + (1|target.word), data=filter(thresh.df.sm,timepoint == 3 & language == "Impoverished")))
summary(lmer(RT ~ tid*relation + (1|target.word), data=filter(thresh.df.sm,timepoint == 3 & language == "Rich")))

summary(lmer(RT ~ relation*primedur.fact + (1|target.word), data=filter(thresh.df.sm,timepoint == 3 & language == "Impoverished" & condition == "Semantic" & primedur.fact %in% c("0.8","2"))))
summary(lmer(RT ~ relation*primedur.fact  + (1|target.word), data=filter(thresh.df.sm,timepoint == 3 & language == "Rich" & condition == "Semantic" & primedur.fact %in% c("0.8","2"))))

summary(lmer(RT ~ relation + (1|target.word), data=filter(thresh.df.sm,timepoint == 3 & language == "Impoverished" & condition %in% c("Opaque","Form") & primedur.fact == "2")))
summary(lmer(RT ~ relation + (1|target.word), data=filter(thresh.df.sm,timepoint == 3 & language == "Rich" & condition %in% c("Opaque","Form") & primedur.fact == "2")))

# vis of french results (quemart 2011)


# now planned contrasts, for all timepoint and language combinations: 
########################### RICH LANGUAGE ####################################################################

# For identity
I.r.1 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 1 & 
                                                                               language == "Rich" & 
                                                                               condition == "Identity"))
I.r.2 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 2 & 
                                                                               language == "Rich" & 
                                                                               condition == "Identity"))
I.r.3 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 3 & 
                                                                               language == "Rich" & 
                                                                               condition == "Identity"))
summary(I.r.1) # 5.50, 9.71, 14.17, all p < 0.0001
summary(I.r.2) # 7.26, 12.31, 16.21, all p < 0.0001
summary(I.r.3) # 8.37, 13.03, 16.24, all p < 0.0001

# For transparent
T.r.1 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 1 & 
                                                                                          language == "Rich" & 
                                                                                          condition == "Transparent"))
T.r.2 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 2 & 
                                                                               language == "Rich" & 
                                                                               condition == "Transparent"))
T.r.3 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 3 & 
                                                                               language == "Rich" & 
                                                                               condition == "Transparent"))
summary(T.r.1) # 5.14, 8.55, 12.48, all p < 0.0001
summary(T.r.2) # 6.62, 10.90, 14.10, all p < 0.0001
summary(T.r.3) # 7.17, 11.25, 14.37, all p < 0.0001

# For intermediate
Int.r.1 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 1 & 
                                                                               language == "Rich" & 
                                                                               condition == "Intermediate"))
Int.r.2 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 2 & 
                                                                               language == "Rich" & 
                                                                               condition == "Intermediate"))
Int.r.3 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 3 & 
                                                                               language == "Rich" & 
                                                                               condition == "Intermediate"))
summary(Int.r.1) # 3.64, 6.32, 7.91, all p < 0.0001
summary(Int.r.2) # 5.06, 6.48, 6.44, all p < 0.0001
summary(Int.r.3) # 4.47, 3.72, 3.64, all p < 0.0001

# For opaque
Op.r.1 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 1 & 
                                                                               language == "Rich" & 
                                                                               condition == "Opaque"))
Op.r.2 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 2 & 
                                                                               language == "Rich" & 
                                                                               condition == "Opaque"))
Op.r.3 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 3 & 
                                                                               language == "Rich" & 
                                                                               condition == "Opaque"))
summary(Op.r.1) # 2.04 (yes), 2.20 (yes), 2.69 (yes)
summary(Op.r.2) # 2.78 (yes), 1.89 (yes), 0.73 (no)
summary(Op.r.3) # 2.81 (yes), -0.060 (no), -1.09 (no)

# For semantic
Se.r.1 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 1 & 
                                                                              language == "Rich" & 
                                                                              condition == "Semantic"))
Se.r.2 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 2 & 
                                                                              language == "Rich" & 
                                                                              condition == "Semantic"))
Se.r.3 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 3 & 
                                                                              language == "Rich" & 
                                                                              condition == "Semantic"))
summary(Se.r.1) # 2.33, 5.46, 9.66 (all p < 0.0001)
summary(Se.r.2) # 3.34, 7.00, 10.68 (all p < 0.0001)
summary(Se.r.3) # 1.40 (no), 5.52 (yes), 10.69 (yes)

Or.r.1 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 1 & 
                                                                              language == "Rich" & 
                                                                              condition == "Form"))
Or.r.2 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 2 & 
                                                                              language == "Rich" & 
                                                                              condition == "Form"))
Or.r.3 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 3 & 
                                                                              language == "Rich" & 
                                                                              condition == "Form"))
summary(Or.r.1) # 1.39, 1.38, 1.58 (all p < 0.0001)
summary(Or.r.2) # 0.29 (no), -1.22 (no), -1.59 (yes)
summary(Or.r.3) # -0.95 (no), -0.42 (no), -0.89 (no)

########################### IMPOVERISHED LANGUAGE ####################################################################

# For identity
I.i.1 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 1 & 
                                                                             language == "Impoverished" & 
                                                                             condition == "Identity"))
I.i.2 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 2 & 
                                                                             language == "Impoverished" & 
                                                                             condition == "Identity"))
I.i.3 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 3 & 
                                                                             language == "Impoverished" & 
                                                                             condition == "Identity"))
summary(I.i.1) # 6.25, 9.57, 11.80, all p < 0.0001
summary(I.i.2) # 6.15, 11.69, 16.02, all p < 0.0001
summary(I.i.3) # 9.73, 14.32, 19.17, all p < 0.0001

# For transparent
T.i.1 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 1 & 
                                                                             language == "Impoverished" & 
                                                                             condition == "Transparent"))
T.i.2 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 2 & 
                                                                             language == "Impoverished" & 
                                                                             condition == "Transparent"))
T.i.3 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 3 & 
                                                                             language == "Impoverished" & 
                                                                             condition == "Transparent"))
summary(T.i.1) # 3.98, 6.02, 7.79, all p < 0.0001
summary(T.i.2) # 3.86, 7.31, 10.87, all p < 0.0001
summary(T.i.3) # 5.79, 8.73, 15.94, all p < 0.0001

# For intermediate
Int.i.1 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 1 & 
                                                                               language == "Impoverished" & 
                                                                               condition == "Intermediate"))
Int.i.2 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 2 & 
                                                                               language == "Impoverished" & 
                                                                               condition == "Intermediate"))
Int.i.3 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 3 & 
                                                                               language == "Impoverished" & 
                                                                               condition == "Intermediate"))
summary(Int.i.1) # 5.37, 5.56, 3.89, all p < 0.0001
summary(Int.i.2) # 4.78, 5.66, 5.60, all p < 0.0001
summary(Int.i.3) # 4.95, 6.49, 6.03, all p < 0.0001

# For opaque
Op.i.1 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 1 & 
                                                                              language == "Impoverished" & 
                                                                              condition == "Opaque"))
Op.i.2 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 2 & 
                                                                              language == "Impoverished" & 
                                                                              condition == "Opaque"))
Op.i.3 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 3 & 
                                                                              language == "Impoverished" & 
                                                                              condition == "Opaque"))
summary(Op.i.1) # 2.03 (yes), -0.98 (no), 0.53 (no)
summary(Op.i.2) # 3.16 (yes), 2.69 (yes), 1.89 (yes)
summary(Op.i.3) # 3.07 (yes), 2.68 (yes), 1.94 (yes)

# For semantic
Se.i.1 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 1 & 
                                                                              language == "Impoverished" & 
                                                                              condition == "Semantic"))
Se.i.2 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 2 & 
                                                                              language == "Impoverished" & 
                                                                              condition == "Semantic"))
Se.i.3 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 3 & 
                                                                              language == "Impoverished" & 
                                                                              condition == "Semantic"))
summary(Se.i.1) # 2.71, 4.54, 6.54 (all p < 0.0001)
summary(Se.i.2) # 0.59 (no), 3.21 (yes), 8.76 (no)
summary(Se.i.3) # 2.16, 2.32, 8.88 (all p < 0.0001)

Or.i.1 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 1 & 
                                                                              language == "Impoverished" & 
                                                                              condition == "Form"))
Or.i.2 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 2 & 
                                                                              language == "Impoverished" & 
                                                                              condition == "Form"))
Or.i.3 <- lmer(RT ~ relation*primedur.fact + (1|target.word), data = filter(thresh.df.sm, timepoint == 3 & 
                                                                              language == "Impoverished" & 
                                                                              condition == "Form"))
summary(Or.i.1) # 1.10 (no), 1.34 (no), -0.31 (no)
summary(Or.i.2) # 1.95 (yes), 1.46 (no), -0.32 (no)
summary(Or.i.3) # 2.53 (yes), 1.88 (yes), 1.23 (no)

################## comparing conditions. 
# For id / transp
IdT.r.1 <- lmer(RT ~ relation*primedur.fact*condition + (1|target.word), data = filter(thresh.df.sm, timepoint == 1 & 
                                                                               language == "Rich" & 
                                                                               condition %in% c("Identity","Transparent")))
IdT.r.2 <- lmer(RT ~ relation*primedur.fact*condition + (1|target.word), data = filter(thresh.df.sm, timepoint == 2 & 
                                                                               language == "Rich" & 
                                                                               condition %in% c("Identity","Transparent")))
IdT.r.3 <- lmer(RT ~ relation*primedur.fact*condition + (1|target.word), data = filter(thresh.df.sm, timepoint == 3 & 
                                                                               language == "Rich" & 
                                                                               condition %in% c("Identity","Transparent")))
summary(IdT.r.1) # -0.41 (no) -1.13 (no) -1.56 (yes)
summary(IdT.r.2) # -0.61 (no), -1.33 (no), -1.99 (yes)
summary(IdT.r.3) # -1.12 (no), -1.69 (no), -1.81 (yes)

IdT.i.1 <- lmer(RT ~ relation*primedur.fact*condition + (1|target.word), data = filter(thresh.df.sm, timepoint == 1 & 
                                                                               language == "Impoverished" & 
                                                                               condition %in% c("Identity","Transparent")))
IdT.i.2 <- lmer(RT ~ relation*primedur.fact*condition + (1|target.word), data = filter(thresh.df.sm, timepoint == 2 & 
                                                                               language == "Impoverished" & 
                                                                               condition %in% c("Identity","Transparent")))
IdT.i.3 <- lmer(RT ~ relation*primedur.fact*condition + (1|target.word), data = filter(thresh.df.sm, timepoint == 3 & 
                                                                               language == "Impoverished" & 
                                                                               condition %in% c("Identity","Transparent")))
summary(IdT.i.1) # -2.35 (no), -3.57 (yes), -4.06 (yes)
summary(IdT.i.2) # -2.00 (no), -3.93 (yes), -4.41 (yes)
summary(IdT.i.3) # -4.13 (yes), -6.17 (yes), -3.77 (yes)

# only sig diff for rich language at longest prime dur - transp closer to identity because of boost from morph info

# For transp / sem 
TrS.r.1 <- lmer(RT ~ relation*primedur.fact*condition + (1|target.word), data = filter(thresh.df.sm, timepoint == 1 & 
                                                                                         language == "Rich" & 
                                                                                         condition %in% c("Transparent","Semantic")))
TrS.r.2 <- lmer(RT ~ relation*primedur.fact*condition + (1|target.word), data = filter(thresh.df.sm, timepoint == 2 & 
                                                                                         language == "Rich" & 
                                                                                         condition %in% c("Transparent","Semantic")))
TrS.r.3 <- lmer(RT ~ relation*primedur.fact*condition + (1|target.word), data = filter(thresh.df.sm, timepoint == 3 & 
                                                                                         language == "Rich" & 
                                                                                         condition %in% c("Transparent","Semantic")))
summary(TrS.r.1) # 2.66 (yes), 2.87 (yes), 2.62 (yes)
summary(TrS.r.2) # 3.23 (yes), 3.99 (yes), 3.57 (yes)
summary(TrS.r.3) # 5.73 (yes), 5.66 (yes), 3.59 (yes)

TrS.i.1 <- lmer(RT ~ relation*primedur.fact*condition + (1|target.word), data = filter(thresh.df.sm, timepoint == 1 & 
                                                                                         language == "Impoverished" & 
                                                                                         condition %in% c("Transparent","Semantic")))
TrS.i.2 <- lmer(RT ~ relation*primedur.fact*condition + (1|target.word), data = filter(thresh.df.sm, timepoint == 2 & 
                                                                                         language == "Impoverished" & 
                                                                                         condition %in% c("Transparent","Semantic")))
TrS.i.3 <- lmer(RT ~ relation*primedur.fact*condition + (1|target.word), data = filter(thresh.df.sm, timepoint == 3 & 
                                                                                         language == "Impoverished" & 
                                                                                         condition %in% c("Transparent","Semantic")))
summary(TrS.i.1) # 1.38 (no), 1.39 (no), 0.56 (no)
summary(TrS.i.2) # 3.18 (yes), 4.03 (yes), 1.90 (no)
summary(TrS.i.3) # 3.27 (no), 5.84 (yes), 6.59 (yes)

# transparent and semantic are closer for impoverished language - less of an advantage of morphological info, especially early on. 

# # For transp / op
# TrOp.r.1 <- lmer(RT ~ relation*primedur.fact*condition + (1|target.word), data = filter(thresh.df.sm, timepoint == 1 & 
#                                                                                          language == "Rich" & 
#                                                                                          condition %in% c("Transparent","Opaque")))
# TrOp.r.2 <- lmer(RT ~ relation*primedur.fact*condition + (1|target.word), data = filter(thresh.df.sm, timepoint == 2 & 
#                                                                                          language == "Rich" & 
#                                                                                          condition %in% c("Transparent","Opaque")))
# TrOp.r.3 <- lmer(RT ~ relation*primedur.fact*condition + (1|target.word), data = filter(thresh.df.sm, timepoint == 3 & 
#                                                                                          language == "Rich" & 
#                                                                                          condition %in% c("Transparent","Opaque")))
# summary(TrOp.r.1) # 
# summary(TrOp.r.2) # 
# summary(TrOp.r.3) # 
# 
# TrOp.i.1 <- lmer(RT ~ relation*primedur.fact*condition + (1|target.word), data = filter(thresh.df.sm, timepoint == 1 & 
#                                                                                          language == "Impoverished" & 
#                                                                                          condition %in% c("Transparent","Opaque")))
# TrOp.i.2 <- lmer(RT ~ relation*primedur.fact*condition + (1|target.word), data = filter(thresh.df.sm, timepoint == 2 & 
#                                                                                          language == "Impoverished" & 
#                                                                                          condition %in% c("Transparent","Opaque")))
# TrOp.i.3 <- lmer(RT ~ relation*primedur.fact*condition + (1|target.word), data = filter(thresh.df.sm, timepoint == 3 & 
#                                                                                          language == "Impoverished" & 
#                                                                                          condition %in% c("Transparent","Opaque")))
# summary(TrOp.i.1)
# summary(TrOp.i.2)
# summary(TrOp.i.3) # opaque transp differences are weaker for rich than for impoverished ? 

# For op / orth
OpOrth.r.1 <- lmer(RT ~ relation*primedur.fact*condition + (1|target.word), data = filter(thresh.df.sm, timepoint == 1 & 
                                                                                          language == "Rich" & 
                                                                                          condition %in% c("Opaque","Form")))
OpOrth.r.2 <- lmer(RT ~ relation*primedur.fact*condition + (1|target.word), data = filter(thresh.df.sm, timepoint == 2 & 
                                                                                          language == "Rich" & 
                                                                                          condition %in% c("Opaque","Form")))
OpOrth.r.3 <- lmer(RT ~ relation*primedur.fact*condition + (1|target.word), data = filter(thresh.df.sm, timepoint == 3 & 
                                                                                          language == "Rich" & 
                                                                                          condition %in% c("Opaque","Form")))
summary(OpOrth.r.1) # 0.72 (no), 0.82 (no), 1.02 (no)
summary(OpOrth.r.2) # 2.48 (yes), 3.03 (yes), 2.24 (yes)
summary(OpOrth.r.3) # 3.65 (yes), 0.36 (no), -0.18 (no)

OpOrth.i.1 <- lmer(RT ~ relation*primedur.fact*condition + (1|target.word), data = filter(thresh.df.sm, timepoint == 1 & 
                                                                                          language == "Impoverished" & 
                                                                                          condition %in% c("Opaque","Form")))
OpOrth.i.2 <- lmer(RT ~ relation*primedur.fact*condition + (1|target.word), data = filter(thresh.df.sm, timepoint == 2 & 
                                                                                          language == "Impoverished" & 
                                                                                          condition %in% c("Opaque","Form")))
OpOrth.i.3 <- lmer(RT ~ relation*primedur.fact*condition + (1|target.word), data = filter(thresh.df.sm, timepoint == 3 & 
                                                                                          language == "Impoverished" & 
                                                                                          condition %in% c("Opaque","Form")))
summary(OpOrth.i.1) # 1.04 (no), -0.24 (no), 0.81 (no)
summary(OpOrth.i.2) # 1.25 (no), 1.29 (no), 2.41 (yes)
summary(OpOrth.i.3) # 0.50 (no), 0.77 (no), 0.58 (no)

#############################################################
################ RT generation example figure ###############
#############################################################

# Ok now read in the output file for one of the languages I generated them for... 
outs.df <- read.csv("~/Users/username/Desktop/recurnet_morphproc/2_testing/outdf_train_10000epochs_2021-01-19_14t4i2oexamples_1.0prime0.0ISI4.0start.csv")

# ex.no = sample(unique(outs.df$example),1)
setwd("~/Users/username/Desktop/recurnet_morphproc/2_testing/") # plot across languages requires stepping back in directory. 

outs.df$target <- factor(outs.df$target)
outs.df.oneex <- filter(outs.df,example==ex.no)
outs.df.oneex$future.target  <- rep(outs.df.oneex$target[which(outs.df.oneex$tick == 30)],length(unique(outs.df.oneex$tick)))
outs.df.oneex %>% 
  group_by(tick,future.target) %>%
  summarise(mn.out=mean(output),se.out = se(output))%>% # calculate mean output
  ggplot(.,aes(x=tick,y=mn.out,col=future.target,group=future.target)) + 
  geom_pointrange(aes(ymin=mn.out - se.out,ymax=mn.out + se.out)) + geom_line() + 
  geom_hline(yintercept = 0.5,col="black",linetype="dashed") + geom_vline(xintercept=20,col="black") + geom_vline(xintercept=25,col="black") + 
  labs(title = "Determining reaction time from outputs",x = "Example time (ticks)",y="Mean output unit activation",col="Unit Target")
ggsave("ReactionTime_fromexampleoutput.png",plot=last_plot(),device = "png",width = 7, height = 5.5)
# if (all.outputs.saved) {
#   ### *** Plot trajectory of lexical decision units' outputs *** ###
#   outs.toplot <- data.frame()
#   for (prime.dur in prime.durs) {
#     print(prime.dur)
#     pd <- as.numeric(strsplit(prime.dur,split="prime")[[1]][1])
#     nu <- read.csv(paste(paste("outdf_train",paste(num.updates,"epochs",sep=""),example.name,prime.dur,sep="_"),".csv",sep=""))
#     outs.toplot <- rbind(outs.toplot, cbind(primedur=rep(pd,nrow(nu)),nu))
#   }
#   
#   # plot means by target 
#   outs.toplot %>% 
#     group_by(target,tick,primedur) %>% 
#     summarize(mn.out=mean(output),se.out=se(output)) %>% 
#     ggplot(.,aes(x=tick,y=mn.out,col=factor(target))) + 
#     geom_pointrange(aes(ymin=mn.out-se.out,ymax=mn.out+se.out)) + geom_line() + 
#     facet_grid(.~primedur)
#   ggsave(paste(ex.name.short,"outputactiv_bytargetandprimedur",paste(num.updates,"epochs",".png",sep=""),sep="_"), plot = last_plot(),
#          device="png",width = 9, height = 6)
#   
#   # match example info to data. 
#   outs.toplot <- left_join(outs.toplot,primes.df,by="example")
#   
#   # plot means by condition & related / unrelated
#   outs.toplot %>% 
#     filter(target==1) %>%
#     group_by(tick,condition,relation,primedur) %>% 
#     summarize(mn.out=mean(output),se.out=se(output)) %>%
#     ggplot(.,aes(x=tick,y=mn.out,col=condition)) + 
#     geom_point() + theme_light() + 
#     geom_line() + facet_grid(primedur~relation)
#   
#   ggsave(paste(ex.name.short,"outputactiv_byrelandcond",paste(num.updates,"epochs",".png",sep=""),sep="_"), plot = last_plot(),
#          device="png",width = 9, height = 6)
# }

# # matching up unrelated-related trials - not really necessary, takes a while .
# outs.match <- outs.toplot %>%
#   select(primedur,tick,unit,output,target.word,condition,pos,relation) %>%
#   pivot_wider(names_from="relation",values_from="output")
# # plug in unrel from transparent condition for identity examples
# outs.match$Unrelated[which(outs.match$condition=="Identity")] <- 
#   outs.match$Unrelated[which(outs.match$condition=="Transparent")]
# # Calculate & plot activation diffs
# outs.match$out.diff <- outs.match$Unrelated - outs.match$Related
# outs.match %>% 
#   filter(unit==2) %>%
#   group_by(tick,condition,primedur) %>% 
#   summarize(mn.diff=mean(out.diff),se.diff=se(out.diff)) %>%
#   ggplot(.,aes(x=tick,y=mn.diff,col=condition)) + 
#   geom_pointrange(aes(ymin=mn.diff-se.diff,ymax=mn.diff+se.diff)) + 
#   geom_line() + facet_grid(.~primedur)
# ggsave(paste(ex.name.short,"outputdiffs_bypdurandcond",paste(num.updates,"epochs",".png",sep=""),sep="_"), plot = last_plot(),
#        device="png",width = 9, height = 6)


# # plot the base RTs, before getting RT diffs
# thresh.df %>% drop_na() %>% 
#   group_by(primedur,condition,relation) %>%
#   summarise(mn.RT = mean(RT),se.RT = se(RT)) %>%
#   ggplot(.,aes(x=primedur,y=mn.RT,col=condition)) + 
#   geom_pointrange(aes(ymin=mn.RT-se.RT,ymax=mn.RT+se.RT)) + 
#   geom_line() + theme_light() + facet_grid(.~relation)
# ggsave(paste(ex.name.short,paste("RTs_","thresh",thresh.forticks,"_bypdurcondrel",sep=""),paste(num.updates,"epochs",".png",sep=""),sep="_"), plot = last_plot(),
#        device="png",width = 9, height = 6)


