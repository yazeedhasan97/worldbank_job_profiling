rm(list = ls())
graphics.off()

# 1| Preparation ----------------------------------------------------------
# 1.1| Libraries ----------------------------------------------------------
myPackages   = c('haven', 'mclust', 'poLCA', 'nlsem', 'depmixS4', 'survival', 
                 'rpart.plot', 'survMisc', 'ggplot2', 'ggparty','partykit', 
                 'treeClust', 'flexmix', 'openxlsx', 'fitdistrplus', 
                 'tidyverse', 'reshape2', 'factoextra', 'ggpubr', 'tidyquant',
                 'DescTools')
notInstalled = myPackages[!(myPackages %in% rownames(installed.packages()))]
if(length(notInstalled)) {
  install.packages(notInstalled)
}

library(haven)        # Reads .dta files.
library(openxlsx)     # Exports the results to excel.
library(partykit)     # Tree-structured regression and classification models.
library(treeClust)    # Produces dissimilarities for tree-based clustering.
library(ggparty)      # Extends ggplot2 functionality to the partykit package. Customizable visualizations for trees. 
library(factoextra)   # Dendogram.
library(rpart.plot)   # Plots rpart trees.
library(flexmix)      # Fits discrete mixtures of regression models.
library(fitdistrplus) # Fits of a parametric distribution to non-censored or censored data.
library(poLCA)        # Estimation of latent class and latent class regression models for polytomous outcome variables.
library(nlsem)        # Estimation of Structural Equation Mixture Modeling (SEMM).
library(depmixS4)     # Estimation of dependent mixture models.
library(mclust)       # Model-based clustering, classification, and density estimation (based on finite normal mixture modelling).
library(survival)     # Survival analysis.
library(survMisc)     # Analysis of right censored survival data. Extends the methods available in 'survival'.
library(tidyverse)    # Data manipulation and graph generation.
library(reshape2)     # Data manipulation for graphs in ggplot and export data.
library(extrafont)    # Font according to Word.
library(tidyquant)    # For statistical moments.
library(DescTools)    # To calculate entropy.
library(ggpubr)       # Nice trees.
library(fastDummies)

loadfonts(device = 'win') # Load fonts from windows.
options(scipen = 999) # Disable scientific notation.


data <- read_csv("E:/Project/WorldBank/job_profiling/job_profiling/modeling/data/final_outputs.csv")
colnames(data)
dummyVariables=c("governorate","disability","education")
bucketSize=200

##Male
miniData=data%>%mutate(governorate=factor(governorate),
                       disability=factor(disability),
                       education=factor(education),
                       nodes=paste0("Male",nodes))%>%
  #  dummy_cols(select_columns =dummyVariables, remove_first_dummy = T ,remove_selected_columns = T)%>%
  filter(gender=="male")


controlBasic = rpart.control(minbucket = bucketSize, minsplit = 2, cp = 0.0002, maxsurrogate = 0, usesurrogate = 0, xval = 10)
controlBasic_ot = rpart.control(minbucket = bucketSize, minsplit = 2, cp = 0.0002, maxsurrogate = 0, usesurrogate = 0, xval = 10)

tr_UnemploymentSpell = rpart(formula = Surv(unemployment_spell, last_job_c) ~ .,
                             data = miniData%>%
                                mutate(age=factor(age),
                                                       experience=factor(experience))%>%
                               dplyr::select(-c("gender", "clusters", "nodes", "durations")), control = controlBasic)

rpart.plot(tr_UnemploymentSpell)
rp = tr_UnemploymentSpell
newdata = miniData%>%mutate(age=factor(age),experience=factor(experience))
type = 'where'
rp$frame$yval <- 1:nrow(rp$frame)
should.be.leaves <- which(rp$frame[, 1] == "<leaf>")

leaves <- predict(rp, newdata = newdata, type = "vector")
should.be.leaves <- which (rp$frame[,1] == "<leaf>")
bad.leaves <- leaves[!is.element (leaves, should.be.leaves)]
if (length (bad.leaves) > 0){
  u.bad.leaves <- unique (bad.leaves)
  u.bad.nodes <- row.names (rp$frame)[u.bad.leaves]
  all.nodes <- row.names (rp$frame)[rp$frame[,1] == "<leaf>"]
  is.descendant <- function (all.leaves, node) {
    if (length (all.leaves) == 0) return (logical (0))
    all.leaves <- as.numeric (all.leaves); node <- as.numeric (node)
    if (missing (node)) return (NA)
    result <- rep (FALSE, length (all.leaves))
    for (i in 1:length (all.leaves)) {
      result=node<all.leaves
    }
    return (result)
  }
  where.tbl <- table (rp$where)
  names (where.tbl) <- row.names (rp$frame)[as.numeric (names (where.tbl))]
  for (u in 1:length (u.bad.nodes)) {
    desc.vec <- is.descendant (all.nodes, u.bad.nodes[u])
    me <- where.tbl[all.nodes][desc.vec]
    winner <- names (me)[me == max(me)][1]
    leaves[leaves == u.bad.leaves[u]] <- which (row.names (rp$frame) == winner)
  }
  leaves[leaves%in%u.bad.leaves] <- NA
}

miniData=miniData %>%mutate(durations=unemployment_spell)
miniData$durations

sM=paste0("Mal",leaves)
miniData=miniData %>%mutate(nodes=sM)
miniDataM=cbind(miniData,sM)
statMoments = miniData %>% group_by(nodes) %>%drop_na(durations) %>% summarise(min     = min(durations ),
                                                         max     = max(durations ),
                                                         mean     = mean(durations ),
                                                         variance = sd(durations ),
                                                         skew     = skewness(durations ),
                                                         kurtosis = kurtosis(durations ),
                                                         p25      = quantile(durations , 0.25),
                                                         p50      = quantile(durations , 0.50),
                                                         p75      = quantile(durations , 0.75),
                                                         entropy  = Entropy(durations ))


statMoments = statMoments %>% mutate(min     = scale(min),
                                       max     = scale(max),
                                       mean     = scale(mean),
                                       variance = scale(variance),
                                       skew     = scale(skew),
                                       kurtosis = scale(kurtosis),
                                       p25      = scale(p25),
                                       p50      = scale(p50),
                                       p75      = scale(p75),
                                       entropy  = scale(entropy)) %>%
  as.data.frame()

rownames(statMoments) = statMoments[,1]
statMoments = statMoments %>% dplyr::select(-nodes)
d = statMoments %>% dist()

hc1 = hclust(d, method = 'ward.D')
plot(hc1)
cut_avg <- cutree(hc1, k = 3)
t=names(cut_avg)
cut_avg=as.data.frame(cut_avg)
cut_avg=cut_avg%>%mutate(nodes=t)
miniDataM=miniDataM%>%left_join(cut_avg)

temp=miniDataM%>%group_by(cut_avg)%>%
  summarise(unemployment_spell=mean(durations, na.rm=T))%>%
  arrange(unemployment_spell)%>%select(-c("unemployment_spell"))
temp$order=c(0,1,2)
miniDataM=miniDataM%>%left_join(temp)%>%
  mutate(cut_avg=order)%>%select(-c("order"))



##Female
miniData=data%>%mutate(governorate=factor(governorate),
                       disability=factor(disability),
                       education=factor(education),
                       nodes=paste0("Female",nodes))%>%
  #  dummy_cols(select_columns =dummyVariables, remove_first_dummy = T ,remove_selected_columns = T)%>%
  filter(gender=="female")


controlBasic = rpart.control(minbucket = bucketSize, minsplit = 2, cp = 0.0002, maxsurrogate = 0, usesurrogate = 0, xval = 10)
controlBasic_ot = rpart.control(minbucket = bucketSize, minsplit = 2, cp = 0.0002, maxsurrogate = 0, usesurrogate = 0, xval = 10)

tr_UnemploymentSpell = rpart(formula = Surv(unemployment_spell, last_job_c) ~ .,
                             data = miniData%>%
                                mutate(age=factor(age),
                                                       experience=factor(experience))%>%
                               dplyr::select(-c("gender", "clusters", "nodes", "durations")), control = controlBasic)
rpart.plot(tr_UnemploymentSpell)
rp = tr_UnemploymentSpell
newdata = miniData%>%mutate(age=factor(age),experience=factor(experience))
type = 'where'
rp$frame$yval <- 1:nrow(rp$frame)
should.be.leaves <- which(rp$frame[, 1] == "<leaf>")

leaves <- predict(rp, newdata = newdata, type = "vector")
should.be.leaves <- which (rp$frame[,1] == "<leaf>")
bad.leaves <- leaves[!is.element (leaves, should.be.leaves)]
if (length (bad.leaves) > 0){
  u.bad.leaves <- unique (bad.leaves)
  u.bad.nodes <- row.names (rp$frame)[u.bad.leaves]
  all.nodes <- row.names (rp$frame)[rp$frame[,1] == "<leaf>"]
  is.descendant <- function (all.leaves, node) {
    if (length (all.leaves) == 0) return (logical (0))
    all.leaves <- as.numeric (all.leaves); node <- as.numeric (node)
    if (missing (node)) return (NA)
    result <- rep (FALSE, length (all.leaves))
    for (i in 1:length (all.leaves)) {
      result=node<all.leaves
    }
    return (result)
  }
  where.tbl <- table (rp$where)
  names (where.tbl) <- row.names (rp$frame)[as.numeric (names (where.tbl))]
  for (u in 1:length (u.bad.nodes)) {
    desc.vec <- is.descendant (all.nodes, u.bad.nodes[u])
    me <- where.tbl[all.nodes][desc.vec]
    winner <- names (me)[me == max(me)][1]
    leaves[leaves == u.bad.leaves[u]] <- which (row.names (rp$frame) == winner)
  }
  leaves[leaves%in%u.bad.leaves] <- NA
}

miniData=miniData %>%mutate(durations=unemployment_spell)
sM=paste0("Fem",leaves)
miniData=miniData %>%mutate(nodes=sM)
miniDataF=cbind(miniData,sM)
statMoments = miniData %>%  group_by(nodes) %>%drop_na(durations)%>% summarise(min     = min(durations ),
                                                          max     = max(durations ),
                                                          mean     = mean(durations ),
                                                          variance = sd(durations ),
                                                          skew     = skewness(durations ),
                                                          kurtosis = kurtosis(durations ),
                                                          p25      = quantile(durations , 0.25),
                                                          p50      = quantile(durations , 0.50),
                                                          p75      = quantile(durations , 0.75),
                                                          entropy  = Entropy(durations ))

statMoments = statMoments %>% mutate(min     = scale(min),
                                       max     = scale(max),
                                       mean     = scale(mean),
                                       variance = scale(variance),
                                       skew     = scale(skew),
                                       kurtosis = scale(kurtosis),
                                       p25      = scale(p25),
                                       p50      = scale(p50),
                                       p75      = scale(p75),
                                       entropy  = scale(entropy)) %>%
  as.data.frame()

rownames(statMoments) = statMoments[,1]
statMoments = statMoments %>% dplyr::select(-nodes)
d = statMoments %>% dist()

hc1 = hclust(d, method = 'ward.D')
plot(hc1)
cut_avg <- cutree(hc1, k = 3)
t=names(cut_avg)
cut_avg=as.data.frame(cut_avg)
cut_avg=cut_avg%>%mutate(nodes=t)
miniDataF=miniDataF%>%left_join(cut_avg)
temp=miniDataF%>%group_by(cut_avg)%>%
  summarise(unemployment_spell=mean(durations, na.rm=T))%>%
  arrange(unemployment_spell)%>%select(-c("unemployment_spell"))
temp$order=c(0,1,2)
miniDataF=miniDataF%>%left_join(temp)%>%
  mutate(cut_avg=order)%>%select(-c("order"))



fullData=rbind(miniDataF,miniDataM)
table(fullData$nodes,fullData$sM)

library("FactoMineR")
library("factoextra")
t=CA(table(fullData$nodes,fullData$sM))

t$eig



#########Block from here

data=fullData
colnames(data)

##Male
miniData=data%>%mutate(governorate=factor(governorate),
                       disability=factor(disability),
                       education=factor(education),
                       age=factor(age),
                       experience=factor(experience),
                       gender=factor(gender),
                       nodes=factor(nodes),
                       clusters=factor(cut_avg))

wb  = openxlsx::createWorkbook()

rowLine    = 1
jumpOfRows = 20
jumpOfCols = 20
colPlots   = 1
colTables  = 10
# - By plot:
base_exp     = 1
heightExp    = 1
widthExp     = 1
scale_factor = base_exp/widthExp
sheetName="Results"
newSheet      = addWorksheet(wb, sheetName = sheetName, gridLines = F)

# Agreggate of cluster:
p = ggplot(miniData, aes(x = durations, colour = clusters)) +
  facet_wrap(.~gender)+
  geom_density() +
  labs(title = 'Cluster Distribution',
       y     = 'Density',
       x     = 'Unemployment Spell') +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, .05))) +
  theme(legend.position = 'right',
        text            = element_text(family = 'Georgia'),
        axis.text.x     = element_text(size = scale_factor * 10, family = 'Georgia'),
        axis.text.y     = element_text(size = scale_factor * 10, family = 'Georgia'),
        axis.title.x    = element_text(hjust = 0.5, size = scale_factor * 10, family = 'Georgia'),
        axis.title.y    = element_text(hjust = 0.5, size = scale_factor * 10, family = 'Georgia'),
        legend.text     = element_text(size = scale_factor * 10, family = 'Georgia'),
        strip.text      = element_text(size = scale_factor * 10, family = 'Georgia'),
        plot.title      = element_text(face = 'bold', size = 10, family = 'Georgia'),
        plot.subtitle   = element_text(size = 10, family = 'Georgia'),
        legend.key.size = unit(0.5 * scale_factor, 'cm'),
        plot.margin = unit(c(0,0,0,0), 'lines'))

print(p)

fileName = paste0('g1.png')
ggsave(fileName, width = 6 * widthExp, height = 4 * heightExp * widthExp, scale = scale_factor)
insertImage(wb, 
            file = fileName, sheet = "Results", 
            startRow = rowLine, startCol = colPlots,
            width = 6 * widthExp, height = 4 * heightExp * widthExp)

# Looking at the nodes:
p = miniData %>% 
  ggplot(aes(x = durations, colour = nodes)) +
  facet_wrap(gender+clusters~.) +
  geom_density() +
  labs(title = 'Nodes Distribution',
       y     = 'Density',
       x     = 'Unemployment Spell') +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, .05))) +
  theme(legend.position = 'right',
        text            = element_text(family = 'Georgia'),
        axis.text.x     = element_text(size = scale_factor * 10, family = 'Georgia'),
        axis.text.y     = element_text(size = scale_factor * 10, family = 'Georgia'),
        axis.title.x    = element_text(hjust = 0.5, size = scale_factor * 10, family = 'Georgia'),
        axis.title.y    = element_text(hjust = 0.5, size = scale_factor * 10, family = 'Georgia'),
        legend.text     = element_text(size = scale_factor * 10, family = 'Georgia'),
        strip.text      = element_text(size = scale_factor * 10, family = 'Georgia'),
        plot.title      = element_text(face = 'bold', size = 10, family = 'Georgia'),
        plot.subtitle   = element_text(size = 10, family = 'Georgia'),
        legend.key.size = unit(0.5 * scale_factor, 'cm')) +
  guides(colour = guide_legend('Nodes', override.aes = list(size = scale_factor * 1)))

print(p)
fileName = paste0('g2.png')
ggsave(fileName, width = 6 * widthExp, height = 4 * heightExp * widthExp, scale = scale_factor)
insertImage(wb, 
            file = fileName, sheet = "Results", 
            startRow = rowLine, startCol = colPlots + jumpOfCols,
            width = 6 * widthExp, height = 4 * heightExp * widthExp)

rowLine = rowLine + jumpOfRows

exportData = miniData %>% group_by(clusters, gender) %>% summarise(one = mean(durations, na.rm=T)) %>% 
  ungroup() 

p = exportData %>% ggplot(aes(x = '', y = one, fill = clusters)) +
  facet_wrap(gender~.) +
  geom_bar(stat = 'identity', position =position_dodge(), colour = 'black') +
  labs(title = 'Cluster Distribution', 
       y     = 'Unemployment spell', 
       x     = ' ') +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, .05)),
                     labels = scales::percent_format(scale = 1, suffix = '', accuracy = 1)) +
  theme(legend.position = 'right',
        text            = element_text(family = 'Georgia'),
        axis.text.x     = element_text(size = scale_factor * 10, family = 'Georgia'),
        axis.text.y     = element_text(size = scale_factor * 10, family = 'Georgia'),
        axis.title.x    = element_text(hjust = 0.5, size = scale_factor * 10, family = 'Georgia'),
        axis.title.y    = element_text(hjust = 0.5, size = scale_factor * 10, family = 'Georgia'),
        axis.ticks.x    = element_blank(),
        legend.text     = element_text(size = scale_factor * 10, family = 'Georgia'),
        strip.text      = element_text(size = scale_factor * 10, family = 'Georgia'),
        plot.title      = element_text(face = 'bold', size = 10, family = 'Georgia'),
        plot.subtitle   = element_text(size = 10, family = 'Georgia'),
        legend.key.size = unit(0.5 * scale_factor, 'cm')) +
  guides(fill   = guide_legend('Cluster'))

fileName = paste0('g3.png')
ggsave(fileName, width = 6 * widthExp, height = 4 * heightExp * widthExp, scale = scale_factor)
print(p)
insertImage(wb, 
            file = fileName, 
            sheet = "Results", 
            startRow = rowLine, startCol = colPlots, 
            width = 6 * widthExp, height = 4 * heightExp * widthExp)
writeDataTable(wb, 
               sheet    = "Results", 
               x        = exportData%>% spread(gender,one), 
               startRow = rowLine, startCol = colTables)
rowLine = rowLine + jumpOfRows

rowLineSave = rowLine

exportData = miniData %>%mutate(one=1)%>% group_by(clusters, gender) %>% summarise(one = sum(one)) %>% 
  ungroup() 

p = exportData %>% ggplot(aes(x = '', y = one, fill = clusters)) +
  facet_wrap(gender~.) +
  geom_bar(stat = 'identity', position = position_fill(), colour = 'black') +
  labs(title = 'Cluster Distribution', 
       y     = 'Percentage', 
       x     = ' ') +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, .05)),
                     labels = scales::percent_format(scale = 100, suffix = '', accuracy = 1)) +
  theme(legend.position = 'right',
        text            = element_text(family = 'Georgia'),
        axis.text.x     = element_text(size = scale_factor * 10, family = 'Georgia'),
        axis.text.y     = element_text(size = scale_factor * 10, family = 'Georgia'),
        axis.title.x    = element_text(hjust = 0.5, size = scale_factor * 10, family = 'Georgia'),
        axis.title.y    = element_text(hjust = 0.5, size = scale_factor * 10, family = 'Georgia'),
        axis.ticks.x    = element_blank(),
        legend.text     = element_text(size = scale_factor * 10, family = 'Georgia'),
        strip.text      = element_text(size = scale_factor * 10, family = 'Georgia'),
        plot.title      = element_text(face = 'bold', size = 10, family = 'Georgia'),
        plot.subtitle   = element_text(size = 10, family = 'Georgia'),
        legend.key.size = unit(0.5 * scale_factor, 'cm')) +
  guides(fill   = guide_legend('Cluster'))

fileName = paste0('g31.png')
ggsave(fileName, width = 6 * widthExp, height = 4 * heightExp * widthExp, scale = scale_factor)
print(p)
insertImage(wb, 
            file = fileName, 
            sheet = "Results", 
            startRow = rowLine, startCol = colPlots, 
            width = 6 * widthExp, height = 4 * heightExp * widthExp)
writeDataTable(wb, 
               sheet    = "Results", 
               x        = exportData%>% spread(gender,one), 
               startRow = rowLine, startCol = colTables)
rowLine = rowLine + jumpOfRows

rowLineSave = rowLine

# * Other Graphs ----------------------------------------------------------

##Experience
exportData = miniData %>%mutate(one=1)%>% group_by(clusters, gender,experience) %>% summarise(one = sum(one)) %>% 
  mutate(one = one/sum(one)*100)%>%ungroup() 

p = exportData %>% ggplot(aes(x = clusters, y = one, fill = experience)) +
  facet_wrap(gender~.) +
  geom_bar(stat = 'identity', position = position_fill(), colour = 'black') +
  labs(title = 'Experience distribution', 
       y     = 'Percentage', 
       x     = 'Clusters') +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, .05)),
                     labels = scales::percent_format(scale = 100, suffix = '', accuracy = 1)) +
  theme(legend.position = 'right',
        text            = element_text(family = 'Georgia'),
        axis.text.x     = element_text(size = scale_factor * 10, family = 'Georgia'),
        axis.text.y     = element_text(size = scale_factor * 10, family = 'Georgia'),
        axis.title.x    = element_text(hjust = 0.5, size = scale_factor * 10, family = 'Georgia'),
        axis.title.y    = element_text(hjust = 0.5, size = scale_factor * 10, family = 'Georgia'),
        axis.ticks.x    = element_blank(),
        legend.text     = element_text(size = scale_factor * 10, family = 'Georgia'),
        strip.text      = element_text(size = scale_factor * 10, family = 'Georgia'),
        plot.title      = element_text(face = 'bold', size = 10, family = 'Georgia'),
        plot.subtitle   = element_text(size = 10, family = 'Georgia'),
        legend.key.size = unit(0.5 * scale_factor, 'cm')) +
  guides(fill   = guide_legend('Cluster'))

fileName = paste0('g4.png')
ggsave(fileName, width = 6 * widthExp, height = 4 * heightExp * widthExp, scale = scale_factor)
print(p)
insertImage(wb, 
            file = fileName, 
            sheet = "Results", 
            startRow = rowLine, startCol = colPlots, 
            width = 6 * widthExp, height = 4 * heightExp * widthExp)
writeDataTable(wb, 
               sheet    = "Results", 
               x        = exportData%>%spread(experience,one), 
               startRow = rowLine, startCol = colTables)
rowLine = rowLine + jumpOfRows

rowLineSave = rowLine

##Age
exportData = miniData %>%mutate(one=1)%>% group_by(clusters, gender,age) %>% summarise(one = sum(one)) %>% 
  mutate(one = one/sum(one)*100)%>%ungroup() 

p = exportData %>% ggplot(aes(x = clusters, y = one, fill = age)) +
  facet_wrap(gender~.) +
  geom_bar(stat = 'identity', position = position_fill(), colour = 'black') +
  labs(title = 'Age distribution', 
       y     = 'Percentage', 
       x     = 'Clusters') +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, .05)),
                     labels = scales::percent_format(scale = 100, suffix = '', accuracy = 1)) +
  theme(legend.position = 'right',
        text            = element_text(family = 'Georgia'),
        axis.text.x     = element_text(size = scale_factor * 10, family = 'Georgia'),
        axis.text.y     = element_text(size = scale_factor * 10, family = 'Georgia'),
        axis.title.x    = element_text(hjust = 0.5, size = scale_factor * 10, family = 'Georgia'),
        axis.title.y    = element_text(hjust = 0.5, size = scale_factor * 10, family = 'Georgia'),
        axis.ticks.x    = element_blank(),
        legend.text     = element_text(size = scale_factor * 10, family = 'Georgia'),
        strip.text      = element_text(size = scale_factor * 10, family = 'Georgia'),
        plot.title      = element_text(face = 'bold', size = 10, family = 'Georgia'),
        plot.subtitle   = element_text(size = 10, family = 'Georgia'),
        legend.key.size = unit(0.5 * scale_factor, 'cm')) +
  guides(fill   = guide_legend('Cluster'))

fileName = paste0('g5.png')
ggsave(fileName, width = 6 * widthExp, height = 4 * heightExp * widthExp, scale = scale_factor)
print(p)
insertImage(wb, 
            file = fileName, 
            sheet = "Results", 
            startRow = rowLine, startCol = colPlots, 
            width = 6 * widthExp, height = 4 * heightExp * widthExp)
writeDataTable(wb, 
               sheet    = "Results", 
               x        = exportData%>%spread(age,one), 
               startRow = rowLine, startCol = colTables)
rowLine = rowLine + jumpOfRows

rowLineSave = rowLine

##Governorate
exportData = miniData %>%mutate(one=1)%>% group_by(clusters, gender,governorate) %>% summarise(one = sum(one)) %>% 
  mutate(one = one/sum(one)*100)%>%ungroup() 

p = exportData %>% ggplot(aes(x = clusters, y = one, fill = governorate)) +
  facet_wrap(gender~.) +
  geom_bar(stat = 'identity', position = position_fill(), colour = 'black') +
  labs(title = 'Governorate distribution', 
       y     = 'Percentage', 
       x     = 'Clusters') +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, .05)),
                     labels = scales::percent_format(scale = 100, suffix = '', accuracy = 1)) +
  theme(legend.position = 'right',
        text            = element_text(family = 'Georgia'),
        axis.text.x     = element_text(size = scale_factor * 10, family = 'Georgia'),
        axis.text.y     = element_text(size = scale_factor * 10, family = 'Georgia'),
        axis.title.x    = element_text(hjust = 0.5, size = scale_factor * 10, family = 'Georgia'),
        axis.title.y    = element_text(hjust = 0.5, size = scale_factor * 10, family = 'Georgia'),
        axis.ticks.x    = element_blank(),
        legend.text     = element_text(size = scale_factor * 10, family = 'Georgia'),
        strip.text      = element_text(size = scale_factor * 10, family = 'Georgia'),
        plot.title      = element_text(face = 'bold', size = 10, family = 'Georgia'),
        plot.subtitle   = element_text(size = 10, family = 'Georgia'),
        legend.key.size = unit(0.5 * scale_factor, 'cm')) +
  guides(fill   = guide_legend('Cluster'))

fileName = paste0('g6.png')
ggsave(fileName, width = 6 * widthExp, height = 4 * heightExp * widthExp, scale = scale_factor)
print(p)
insertImage(wb, 
            file = fileName, 
            sheet = "Results", 
            startRow = rowLine, startCol = colPlots, 
            width = 6 * widthExp, height = 4 * heightExp * widthExp)
writeDataTable(wb, 
               sheet    = "Results", 
               x        = exportData%>%spread(governorate,one), 
               startRow = rowLine, startCol = colTables)
rowLine = rowLine + jumpOfRows

rowLineSave = rowLine


##Disability
exportData = miniData %>%mutate(one=1)%>% group_by(clusters, gender,disability) %>% summarise(one = sum(one)) %>% 
  mutate(one = one/sum(one)*100)%>%ungroup() 

p = exportData %>% ggplot(aes(x = clusters, y = one, fill = disability)) +
  facet_wrap(gender~.) +
  geom_bar(stat = 'identity', position = position_fill(), colour = 'black') +
  labs(title = 'Disability distribution', 
       y     = 'Percentage', 
       x     = 'Clusters') +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, .05)),
                     labels = scales::percent_format(scale = 100, suffix = '', accuracy = 1)) +
  theme(legend.position = 'right',
        text            = element_text(family = 'Georgia'),
        axis.text.x     = element_text(size = scale_factor * 10, family = 'Georgia'),
        axis.text.y     = element_text(size = scale_factor * 10, family = 'Georgia'),
        axis.title.x    = element_text(hjust = 0.5, size = scale_factor * 10, family = 'Georgia'),
        axis.title.y    = element_text(hjust = 0.5, size = scale_factor * 10, family = 'Georgia'),
        axis.ticks.x    = element_blank(),
        legend.text     = element_text(size = scale_factor * 10, family = 'Georgia'),
        strip.text      = element_text(size = scale_factor * 10, family = 'Georgia'),
        plot.title      = element_text(face = 'bold', size = 10, family = 'Georgia'),
        plot.subtitle   = element_text(size = 10, family = 'Georgia'),
        legend.key.size = unit(0.5 * scale_factor, 'cm')) +
  guides(fill   = guide_legend('Cluster'))

fileName = paste0('g7.png')
ggsave(fileName, width = 6 * widthExp, height = 4 * heightExp * widthExp, scale = scale_factor)
print(p)
insertImage(wb, 
            file = fileName, 
            sheet = "Results", 
            startRow = rowLine, startCol = colPlots, 
            width = 6 * widthExp, height = 4 * heightExp * widthExp)
writeDataTable(wb, 
               sheet    = "Results", 
               x        = exportData%>%spread(disability,one), 
               startRow = rowLine, startCol = colTables)
rowLine = rowLine + jumpOfRows

rowLineSave = rowLine

##Disability
exportData = miniData %>%mutate(one=1)%>% group_by(clusters, gender,education) %>% summarise(one = sum(one)) %>% 
  mutate(one = one/sum(one)*100)%>%ungroup() 

p = exportData %>% ggplot(aes(x = clusters, y = one, fill = education)) +
  facet_wrap(gender~.) +
  geom_bar(stat = 'identity', position = position_fill(), colour = 'black') +
  labs(title = 'Education distribution', 
       y     = 'Percentage', 
       x     = 'Clusters') +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, .05)),
                     labels = scales::percent_format(scale = 100, suffix = '', accuracy = 1)) +
  theme(legend.position = 'right',
        text            = element_text(family = 'Georgia'),
        axis.text.x     = element_text(size = scale_factor * 10, family = 'Georgia'),
        axis.text.y     = element_text(size = scale_factor * 10, family = 'Georgia'),
        axis.title.x    = element_text(hjust = 0.5, size = scale_factor * 10, family = 'Georgia'),
        axis.title.y    = element_text(hjust = 0.5, size = scale_factor * 10, family = 'Georgia'),
        axis.ticks.x    = element_blank(),
        legend.text     = element_text(size = scale_factor * 10, family = 'Georgia'),
        strip.text      = element_text(size = scale_factor * 10, family = 'Georgia'),
        plot.title      = element_text(face = 'bold', size = 10, family = 'Georgia'),
        plot.subtitle   = element_text(size = 10, family = 'Georgia'),
        legend.key.size = unit(0.5 * scale_factor, 'cm')) +
  guides(fill   = guide_legend('Cluster'))

fileName = paste0('g8.png')
ggsave(fileName, width = 6 * widthExp, height = 4 * heightExp * widthExp, scale = scale_factor)
print(p)
insertImage(wb, 
            file = fileName, 
            sheet = "Results", 
            startRow = rowLine, startCol = colPlots, 
            width = 6 * widthExp, height = 4 * heightExp * widthExp)
writeDataTable(wb, 
               sheet    = "Results", 
               x        = exportData%>%spread(education,one), 
               startRow = rowLine, startCol = colTables)
rowLine = rowLine + jumpOfRows

rowLineSave = rowLine


saveWorkbook(wb, 
             file = paste0('Results from R.xlsx'), 
             overwrite = TRUE)


