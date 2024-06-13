# https://www.clres.com/ca/pdepca01a.html
library(tm) #load text mining library

path <- readline() #D:\\Research\\PAPERS\\big data\\social-environmental
# D:\Research\PAPERS\big data\social-environmental-30 April 2024\data\for-analysis
pathch <- gsub('\\\\','//', path)
setwd(path)
getwd()

bigse <- read.csv(file.choose())
names(bigse)
bigseabs <- bigse["Abstract"]
length(t(bigseabs)) # 62

bigsecorp <- VCorpus((VectorSource(t(bigseabs))))

abstracts_corp <- tm_map(bigsecorp, stripWhitespace)
abstracts_corp <- tm_map(abstracts_corp, removePunctuation)
abstracts_corp <- tm_map(abstracts_corp, content_transformer(tolower))                          
abstracts_corp <- tm_map(abstracts_corp, removeWords, stopwords("english"))
abstracts_corp <- tm_map(abstracts_corp, removeNumbers)

# abstracts_corp <- tm_map(hc_corp, stemDocument, language = "english")

path <- file.path(path, 'R')
setwd(path)
getwd()
dir.create('outputs')
# list.dirs()

pathch <- file.path(path, 'outputs')
setwd(pathch)
getwd()

write.csv(summary(abstracts_corp), 'corpus-summary.csv')
# data.frame(text=unlist(sapply(mycorpus, `[`, "content")), 
           # stringsAsFactors=F)
# data.frame(text = sapply(myCorpus, as.character), stringsAsFactors = FALSE)

adtm <- DocumentTermMatrix(abstracts_corp)
tm::inspect(adtm[10:16, ] )
dim(adtm) # [1]   62 2644
adtm$dimnames

bigse_ <- adtm[, -c(1:10)]
dim(bigse_) # [1]   62 2634

bigsedf <- data.frame(as.matrix(bigse_), stringsAsFactors=FALSE)
bigsedfdense <- data.frame(as.matrix(removeSparseTerms(bigse_, 0.90)))
dim(bigsedfdense) #200
names(bigsedfdense)

path <- readline()
pathch <- gsub('\\\\', '//', path)

write.csv(bigsedfdense, file.path(pathch, 'tm-data.csv'))
bigsefinaldf <- subset(bigsedfdense, select = -c(across, addition, additional, address, aims, also,
                                                 among, areas, article, articles, aspects, authors, 
                                                 available, based, better, bias, biases, can, conclusions,
                                                 construct, continuous, control, criteria, current, 
                                                 detailed, develop, developed, development, develops, different, 
                                                 especially, even, existing, explore, factors, features, finally, 
                                                 findings, first, follows, furthermore, future, general, high, 
                                                 highlight, however, human, identify, impact, implementation, 
                                                 implemented, important, improve, improving, including, 
                                                 increasing, initial, issue, key, limited, mainly, 
                                                 many, may, method, methods, model, moreover, multiple, need, 
                                                 needed, new, novel, number, one, order, paper, potential, practical, 
                                                 problems, process, processing, projects, propose, proposed, 
                                                 protection, provide, public, purpose, quality, reference, related, 
                                                 reporting, required, results, review, second, set, several, show, 
                                                 shows, significant, significantly, smart, solve, standards, studies, study, theory, 
                                                 third, three, time, two, understanding, use, used, using, various, verification, ways, well, 
                                                 whole, will, within, work, year))
length(names(bigsefinaldf)) # 75

# D:\Research\PAPERS\big data\social-environmental\data\from_analysis
pathdata <- readline()
pathdata <- gsub('\\\\', '//', pathanal)
write.csv(bigsefinaldf, file.path(pathanal, 'df-for-analysis.csv'))

library(FactoMineR)

fit <- CA(data.frame(bigsefinaldf), 37)

pathanal <- readline()
pathanal <- gsub('\\\\', '//', pathanal)

write.csv(fit$row$coord, file.path(pathanal, 'row-coord.csv'))
write.csv(fit$row$cos2, file.path(pathanal, 'row-cos2.csv'))
write.csv(fit$row$contrib, file.path(pathanal, 'row-contrib.csv'))
write.csv(fit$row$inertia, file.path(pathanal, 'row-inertia.csv'))

write.csv(fit$col$coord, file.path(pathanal, 'col-coord.csv'))
write.csv(fit$col$cos2, file.path(pathanal, 'col-cos2.csv'))
write.csv(fit$col$contrib, file.path(pathanal, 'col-contrib.csv'))
write.csv(fit$col$inertia, file.path(pathanal, 'col-inertia.csv'))

plot.CA(fit, xlim = c(-2, -1), ylim = c(-2, -1))
plot.CA(fit, xlim = c(-1, 0), ylim = c(-1, 0))
plot.CA(fit, xlim = c(0, 1), ylim = c(-1, 0))
plot.CA(fit, xlim = c(1, 2), ylim = c(-1, 0))
plot.CA(fit, xlim = c(0, 1), ylim = c(0, 1)) #q3 -1
plot.CA(fit, xlim = c(0.5, 1.5), ylim = c(0, 0.5)) 
plot.CA(fit, xlim = c(-1.5, 0), ylim = c(0, 1.5))
plot.CA(fit, xlim = c(-1.5, 0), ylim = c(2, 4))

# summary(bigsefinaldf)

desc_anal <- sapply(bigsefinaldf, function(x) c(summary(x), type = class(x)))
write.csv(desc_anal, file = file.path(pathanal, 'summary-df.csv'))

# jpeg(file.path(pathanal, 'plot.jpg'))
# plot(bigsefinaldf)

names(bigsefinaldf)

# regression analysis (modeling)
# http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/#:~:text=The%20stepwise%20regression%20(or%20stepwise,model%20that%20lowers%20prediction%20error.


# FA

bigsefadf <- subset(bigsedfdense, select = c(environmental, cloud, storage, 
                                                big, data, financial, blockchain, sustainability, 
                                                social, technology, information, system, 
                                                transparency, accountability, methodology, auditing, 
                                                machine, learning, governance, network, management))

library(psych)

fafit <- fa(bigsefadf, 2)
structure.diagram(fafit)

write.csv(loadings(fafit), 'D:/Research/PAPERS/big data/social-environmental-30 April 2024/analysis/2fa-loadings.csv')
write.csv(fafit$r.scores, 'D:/Research/PAPERS/big data/social-environmental-30 April 2024/analysis/2fa-r-scores.csv')
structure.diagram(fit)
