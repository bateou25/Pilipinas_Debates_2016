# The recent Philippines' Presidential Debate (Pilipinas Debates 2016) drew unprecedented amounts of viewers and the
# usual lot of controversies and soundbites in the media. In our polarized media landscape, ensuing political analysis
# always suffer from political bias. Whether you trust the media, you will get a very different take on what was said.

# So far, the candidates have spoken thousands of words in the two (2) debates held by two TV networks.
# In this simple textual analysis, I turn all of the words into data and make inferences about the candidates'
# characteristics that would otherwise be too difficult to discover.


# NLP tools and methods can help bring some objectivity to better understand the current political discourse.
# Using different state-of-the-art NLP libraries and packages, we will try to answer questions about:
#   Debate Dynamics - What can be inferred about the debaters performances? (FINISHED)
#   Sentiment Analysis - How do the candidates feel about certain issues? (WIP)
#   Topic Modelling - What did the candidates really talked about? (WIP)
#                   - What was the most important subject for each candidate?


#set working directory
getwd()
setwd("Z:/Github/Data Projects/Pilipinas_Debates_2016/transcripts")

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)


################################################################################################
#First let's answer the following question: how often does a certain candidate get mentioned
#by name by the other candidates VS how often do the other candidates mention each other's name.
#On the first debate, the candidates really didn't refer to each other much at all.
################################################################################################
ref_matrix = function (date){
  
  #read character data
  text = scan(paste0('debate_', date, '_full', '.txt'), what='x', quote=NULL)
  
  speakers =
    c(  BINAY    = '',
        MAR   = '',
        DUTERTE  = '',
        MIRIAM  = '',
        POE = ''
    )
  
  #assign text to the right speaker
  for(word in text){
    #if word ends with :
    if(substr(word,nchar(word),nchar(word))==':'){
      #if word corresponds to one of the speakers of interest
      if(word %in% paste0(names(speakers), ':')){
        #set current speaker
        currentSpeaker = substr(word,1,nchar(word)-1)
      }
      else{
        #if the current speaker is not one of the speakers of interest, set it to NA
        currentSpeaker = NA
      }
    }
    else if(!is.na(currentSpeaker)){
      #if the current speaker is of interest, save what he is saying
      speakers[currentSpeaker] = paste(speakers[currentSpeaker], word)
    }
  }
  
  #preprocess text
  prez = Corpus(VectorSource(speakers))
  prez = tm_map(prez, tolower)
  prez = tm_map(prez, removeWords,stopwords('english'))
  #remove additional unwanted words
  prez = Corpus(VectorSource(speakers))
  prez = tm_map(prez, tolower)
  prez = tm_map(prez, removeWords,stopwords('english'))
  #remove additional unwanted words
  prez = tm_map(prez, removeWords, c('laughter', 'applause', 'intermission', 'and', 'the', 'all',
                                     'ang','mga','you','hindi','that','kung','ating','said','naman',
                                     'namin','natin','yung','pero','sila','nila','ako','yan','para',
                                     'will','isang','ito','doon','alam','may','kanilang','dahil',
                                     'lahat','have','tayo','kami','for','because','rin','ngayon',
                                     'kaya','kaya','hong','walang','but','kahit','dapat','lang',
                                     'mas','pwedeng','saan','yong','buong','yong','ayaw','lang',
                                     'not','wala','there','are','was','would','ano','well','can',
                                     'kasi','dito','our','akong','with','dun','gusto','ninyo',
                                     'bakit','niya','like','ibang','talagang','did','yes','yun','this',
                                     'anong','man','diyan','tulad','pwede','talaga','give','bilang','isa',
                                     'itong','what','nasa','sinasabi','just','kayo','lamang','nga','luchi',
                                     'mismo','other','akin','kanya','ikaw','cannot','aking','ginawa',
                                     'kanila','ginawa','know','really','sapagkat','din','maraming','they',
                                     'meron','mong','uulitin','yon',
                                     'yon','yo','tony','below','anyway','eto','aba','when','madam','amin',
                                     'nandun','pagka','mahigit','senadora','sya','inyong','parang','salamat',
                                     'senador','nagiging','yeah','look','how','goes','huwag','see','say',
                                     'kang','actually','sinabi','about','mayor','already','nung','importante',
                                     'oras','tama','here','very','into','pati','why','sabi','those','always',
                                     'has','first','ganoon','atin','dalawang','kailangan','papaano','sabihin',
                                     'nilang','hanggang','nating','from','siguro','tayong','naging','after',
                                     'siya','namang','nagsabi','nang','lagi','niyo','grace','nagsasabi','nyo',
                                     'pong','basta','dyan','iyan','pang','tayong','which','should','sino','these',
                                     'without','di','yang','ilang','noon','noong','pag','senator','iyon','kong',
                                     'mag','maging','nandiyan','nang','per','pumunta','iyan','let','muna','each',
                                     'aming','sana','also','basta','ganito'))
  prez = tm_map(prez, removePunctuation,preserve_intra_word_dashes=FALSE)
  prez = tm_map(prez, stemDocument)
  prez = tm_map(prez, stripWhitespace)
  prez = tm_map(prez, removeNumbers)
  prez = tm_map(prez, PlainTextDocument)
  #make document term matrix
  dtm = DocumentTermMatrix(prez) 
  #reassign row names (each row is a speaker)
  rownames(dtm) = names(speakers)
  
  #how many times was VP Jejomar Binay referred to by other candidates
  binay_names = character()
  if('binay' %in% colnames(dtm)){binay_names = c(binay_names, 'binay')}
  if('jejomar' %in% colnames(dtm)){binay_names = c(binay_names, 'jejomar')}
  if('vp' %in% colnames(dtm)){binay_names = c(binay_names, 'vp')}
  if('vice president' %in% colnames(dtm)){binay_names = c(binay_names, 'vice president')}
  dtm_binay = dtm[,binay_names]
  BINAY = apply(dtm_binay, 1, sum)
  
  #how many times was Mar Roxas referred to by other candidates
  mar_names = character()
  if('mar' %in% colnames(dtm)){mar_names = c(mar_names, 'mar')}
  if('roxas' %in% colnames(dtm)){mar_names = c(mar_names, 'roxas')}
  if('secretary' %in% colnames(dtm)){mar_names = c(mar_names, 'secretary')}
  dtm_mar = dtm[,mar_names]
  MAR = apply(dtm_mar, 1, sum)
  
  #how many times was Rodrigo Duterte referred to by other candidates
  duterte_names = character()
  if('mayor' %in% colnames(dtm)){duterte_names = c(duterte_names, 'mayor')}
  if('duterte' %in% colnames(dtm)){duterte_names = c(duterte_names, 'duterte')}
  if('rudy' %in% colnames(dtm)){duterte_names = c(duterte_names, 'rudy')}
  if('rodrigo' %in% colnames(dtm)){duterte_names = c(duterte_names, 'rodrigo')}
  dtm_duterte = dtm[,duterte_names]
  DUTERTE = apply(dtm_duterte, 1, sum)
  
  #how many times was Sen. Miriam Defensor-Santiago referred to by other candidates
  miriam_names = character()
  if('santiago' %in% colnames(dtm)){miriam_names = c(miriam_names, 'santiago')}
  if('miriam' %in% colnames(dtm)){miriam_names = c(miriam_names, 'miriam')}
  dtm_miriam = dtm[,miriam_names]
  MIRIAM = apply(dtm_miriam, 1, sum)
  
  #how many times was Sen. Grace Poe referred to by other candidates
  poe_names = character()
  if('grace' %in% colnames(dtm)){poe_names = c(poe_names, 'grace')}
  if('poe' %in% colnames(dtm)){poe_names = c(poe_names, 'poe')}
  dtm_poe = dtm[,poe_names]
  POE = apply(dtm_poe, 1, sum)
  
  #summary matrix
  data.frame(BINAY=BINAY, MAR=MAR, DUTERTE=DUTERTE, MIRIAM=MIRIAM, POE=POE)
}



dates = c(20160221,20160320)

ref_list = lapply(dates, ref_matrix)

names(ref_list) = dates

###########################
# First, let's answer the question: How does Binay get mentioned by name by the other four candidates during a debate,
# versus how often do the other candidates mention each other's name?
###########################
binay = sapply(ref_list, function(df) sum(df[rownames(df) != 'BINAY', 'BINAY']))
the_rest = sapply(ref_list, function(df) sum(df[rownames(df) == 'MAR', 'DUTERTE'], df[rownames(df) == 'DUTERTE', 'MAR'],
                                             df[rownames(df) == 'MAR', 'MIRIAM'],df[rownames(df) == 'MIRIAM', 'MAR'],
                                             df[rownames(df) == 'MAR', 'POE'],df[rownames(df) == 'POE', 'MAR'],
                                             df[rownames(df) == 'DUTERTE', 'MIRIAM'],df[rownames(df) == 'MIRIAM', 'DUTERTE'],
                                             df[rownames(df) == 'DUTERTE', 'POE'],df[rownames(df) == 'POE', 'DUTERTE'],
                                             df[rownames(df) == 'MIRIAM', 'POE'],df[rownames(df) == 'POE', 'MIRIAM']))
m = t(as.matrix(data.frame(binay, the_rest)))
#             20160221 20160320
# binay           3        6
# the_rest       17       32



barplot(m,main='NUmber of times the other candidates mention Binay vs each other',
        beside=TRUE,col=c('darkblue','lightblue'),
        #legend=c('# of times the other candidates mention Binay','# of times the other candidates mention each other'),
        legend.text = FALSE)


##### Who mentioned Binay's name?
binay_mar = sapply(ref_list, function(df) sum(df[rownames(df) == 'MAR', 'BINAY']))
# 20160221 20160320 
# 3        2
duterte_binay = sapply(ref_list, function(df) sum(df[rownames(df) == 'DUTERTE', 'BINAY']))
# 20160221 20160320 
# 0        0 
poe_binay = sapply(ref_list, function(df) sum(df[rownames(df) == 'POE', 'BINAY']))
# 20160221 20160320 
# 0        4 
miriam_binay = sapply(ref_list, function(df) sum(df[rownames(df) == 'MIRIAM', 'BINAY']))
# 20160221 20160320 
# 0        0 


###########################
# How does Mar get mentioned by name by the other four candidates during a debate,
# versus how often do the other candidates mention each other's name?
###########################
mar = sapply(ref_list, function(df) sum(df[rownames(df) != 'MAR', 'MAR']))
the_rest = sapply(ref_list, function(df) sum(df[rownames(df) == 'BINAY', 'DUTERTE'], df[rownames(df) == 'DUTERTE', 'BINAY'],
                                             df[rownames(df) == 'BINAY', 'MIRIAM'],df[rownames(df) == 'MIRIAM', 'BINAY'],
                                             df[rownames(df) == 'BINAY', 'POE'],df[rownames(df) == 'POE', 'BINAY'],
                                             df[rownames(df) == 'DUTERTE', 'MIRIAM'],df[rownames(df) == 'MIRIAM', 'DUTERTE'],
                                             df[rownames(df) == 'DUTERTE', 'POE'],df[rownames(df) == 'POE', 'DUTERTE'],
                                             df[rownames(df) == 'MIRIAM', 'POE'],df[rownames(df) == 'POE', 'MIRIAM']))
n = t(as.matrix(data.frame(mar, the_rest)))
#             20160221 20160320
# mar             8       45
# the_rest       16       13


barplot(n,main='NUmber of times the other candidates mention Mar vs each other',
        beside=TRUE,col=c('darkblue','lightblue'),
        #legend=c('# of times the other candidates mention Mar','# of times the other candidates mention each other'),
        legend.text = FALSE)



##### Who mentioned Mar's name?
mar_binay = sapply(ref_list, function(df) sum(df[rownames(df) == 'BINAY', 'MAR']))
# 20160221 20160320 
# 5       21
mar_duterte = sapply(ref_list, function(df) sum(df[rownames(df) == 'DUTERTE', 'MAR']))
# 20160221 20160320 
# 0       14 
mar_poe = sapply(ref_list, function(df) sum(df[rownames(df) == 'POE', 'MAR']))
# 20160221 20160320 
# 2       10 
mar_miriam = sapply(ref_list, function(df) sum(df[rownames(df) == 'MIRIAM', 'MAR']))
# 20160221 20160320 
# 1        0


###########################
# How does Duterte get mentioned by name by the other four candidates during a debate,
# versus how often do the other candidates mention each other's name?
###########################
duterte = sapply(ref_list, function(df) sum(df[rownames(df) != 'DUTERTE', 'DUTERTE']))
the_rest = sapply(ref_list, function(df) sum(df[rownames(df) == 'BINAY', 'MAR'], df[rownames(df) == 'MAR', 'BINAY'],
                                             df[rownames(df) == 'BINAY', 'MIRIAM'],df[rownames(df) == 'MIRIAM', 'BINAY'],
                                             df[rownames(df) == 'BINAY', 'POE'],df[rownames(df) == 'POE', 'BINAY'],
                                             df[rownames(df) == 'MAR', 'MIRIAM'],df[rownames(df) == 'MIRIAM', 'MAR'],
                                             df[rownames(df) == 'MAR', 'POE'],df[rownames(df) == 'POE', 'MAR'],
                                             df[rownames(df) == 'MIRIAM', 'POE'],df[rownames(df) == 'POE', 'MIRIAM']))
o = t(as.matrix(data.frame(duterte, the_rest)))
#             20160221 20160320
# duterte         6       11
# the_rest       20       39

barplot(o,main='NUmber of times the other candidates mention Duterte vs each other',
        beside=TRUE,col=c('darkblue','lightblue'),
        #legend=c('# of times the other candidates mention Duterte','# of times the other candidates mention each other'),
        legend.text = FALSE)




###########################
# How does Poe get mentioned by name by the other four candidates during a debate,
# versus how often do the other candidates mention each other's name?
###########################
poe = sapply(ref_list, function(df) sum(df[rownames(df) != 'POE', 'POE']))
the_rest = sapply(ref_list, function(df) sum(df[rownames(df) == 'BINAY', 'MAR'], df[rownames(df) == 'MAR', 'BINAY'],
                                             df[rownames(df) == 'BINAY', 'MIRIAM'],df[rownames(df) == 'MIRIAM', 'BINAY'],
                                             df[rownames(df) == 'BINAY', 'DUTERTE'],df[rownames(df) == 'DUTERTE', 'BINAY'],
                                             df[rownames(df) == 'MAR', 'MIRIAM'],df[rownames(df) == 'MIRIAM', 'MAR'],
                                             df[rownames(df) == 'MAR', 'DUTERTE'],df[rownames(df) == 'DUTERTE', 'MAR'],
                                             df[rownames(df) == 'MIRIAM', 'DUTERTE'],df[rownames(df) == 'DUTERTE', 'MIRIAM']))
p = t(as.matrix(data.frame(poe, the_rest)))

#             20160221 20160320
# poe             2        1
# the_rest       18       45



###########################
# How does Miriam get mentioned by name by the other four candidates during a debate,
# versus how often do the other candidates mention each other's name?
###########################
miriam = sapply(ref_list, function(df) sum(df[rownames(df) != 'MIRIAM', 'MIRIAM']))
the_rest = sapply(ref_list, function(df) sum(df[rownames(df) == 'BINAY', 'MAR'], df[rownames(df) == 'MAR', 'BINAY'],
                                             df[rownames(df) == 'BINAY', 'POE'],df[rownames(df) == 'POE', 'BINAY'],
                                             df[rownames(df) == 'BINAY', 'DUTERTE'],df[rownames(df) == 'DUTERTE', 'BINAY'],
                                             df[rownames(df) == 'MAR', 'POE'],df[rownames(df) == 'POE', 'MAR'],
                                             df[rownames(df) == 'MAR', 'DUTERTE'],df[rownames(df) == 'DUTERTE', 'MAR'],
                                             df[rownames(df) == 'POE', 'DUTERTE'],df[rownames(df) == 'DUTERTE', 'POE']))
q = t(as.matrix(data.frame(miriam, the_rest)))
#             20160221 20160320
# miriam         10        1
# the_rest       16       63



#######################################################
#Let's now turn our attention to the words themselves.
#######################################################

dates = c(20160221,20160320)
#collect all the transcripts in a single character string
read_transcript = function(date){scan(paste0('debate_', date, '_full', '.txt'), what='x', quote=NULL)}
#read and collate all transcripts
text = unlist(sapply(dates, read_transcript))


speakers =
  c(  BINAY    = '',
      MAR   = '',
      DUTERTE  = '',
      MIRIAM  = '',
      POE = ''
  )

#assign text to the right speaker
for(word in text){
  #if word ends with :
  if(substr(word,nchar(word),nchar(word))==':'){
    #if word corresponds to one of the speakers of interest
    if(word %in% paste0(names(speakers), ':')){
      #set current speaker
      currentSpeaker = substr(word,1,nchar(word)-1)
    }
    else{
      #if the current speaker is not one of the speakers of interest, set it to NA
      currentSpeaker = NA
    }
  }
  else if(!is.na(currentSpeaker)){
    #if the current speaker is of interest, save what he is saying
    speakers[currentSpeaker] = paste(speakers[currentSpeaker], word)
  }
}


##############################################################################
# The candidates' vocabularies are further simplified by removing common words
# that convey little meaning (e.g.,prepositions,pronouns) and stemming the
# remaining, more substantive words.
##############################################################################

#preprocess text
prez = Corpus(VectorSource(speakers))
prez = tm_map(prez, tolower)
prez = tm_map(prez, removeWords,stopwords('english'))
#remove additional unwanted words
prez = Corpus(VectorSource(speakers))
prez = tm_map(prez, tolower)
prez = tm_map(prez, removeWords,stopwords('english'))
#remove additional unwanted words
prez = tm_map(prez, removeWords, c('laughter', 'applause', 'intermission', 'and', 'the', 'all',
                                   'ang','mga','you','hindi','that','kung','ating','said','naman',
                                   'namin','natin','yung','pero','sila','nila','ako','yan','para',
                                   'will','isang','ito','doon','alam','may','kanilang','dahil',
                                   'lahat','have','tayo','kami','for','because','rin','ngayon',
                                   'kaya','kaya','hong','walang','but','kahit','dapat','lang',
                                   'mas','pwedeng','saan','yong','buong','yong','ayaw','lang',
                                   'not','wala','there','are','was','would','ano','well','can',
                                   'kasi','dito','our','akong','with','dun','gusto','ninyo',
                                   'bakit','niya','like','ibang','talagang','did','yes','yun','this',
                                   'anong','man','diyan','tulad','pwede','talaga','give','bilang','isa',
                                   'itong','what','nasa','sinasabi','just','kayo','lamang','nga','luchi',
                                   'mismo','other','akin','kanya','ikaw','cannot','aking','ginawa',
                                   'kanila','ginawa','know','really','sapagkat','din','maraming','they',
                                   'meron','roxas','duterte','poe','mar','miriam',
                                   'mong','uulitin','yon','yo','tony','below','anyway','eto','aba',
                                   'when','madam','amin','nandun','pagka','mahigit','senadora','sya',
                                   'inyong','parang','salamat','nagiging','yeah','look','how','goes','huwag','see','say',
                                   'kang','actually','sinabi','about','already','nung','importante',
                                   'oras','tama','here','very','into','pati','why','sabi','those','always',
                                   'has','first','ganoon','atin','dalawang','kailangan','papaano','sabihin',
                                   'nilang','hanggang','nating','from','siguro','tayong','naging','after',
                                   'siya','namang','nagsabi','nang','lagi','niyo','grace','nagsasabi','nyo',
                                   'pong','basta','dyan','iyan','pang','tayong','which','should','sino','these',
                                   'without','di','yang','ilang','noon','noong','pag','iyon','kong',
                                   'mag','maging','nandiyan','nang','per','pumunta','iyan','let','muna','each',
                                   'aming','sana','also','basta','ganito'))
prez = tm_map(prez, removePunctuation,preserve_intra_word_dashes=FALSE)
prez = tm_map(prez, stemDocument)
prez = tm_map(prez, stripWhitespace)
prez = tm_map(prez, removeNumbers)
prez = tm_map(prez, PlainTextDocument)
#make document term matrix
dtm = DocumentTermMatrix(prez) 
#reassign row names (each row is a speaker)
rownames(dtm) = names(speakers)

inspect(dtm)#visualize the document-term matrix


#######################################
#For each candidate, what are the top 100 most frequent words ?
#######################################
# get indexes of 100 most frequent words
indexes = apply(dtm, 1, function(v) head(order(v, decreasing=TRUE), 100))
# find the 100 most frequent words
freq_words = apply(indexes, 2, function(v) colnames(dtm)[v])
freq_words

#find the number of times each word appears in the matrix
word_count_freq = apply(freq_words, c(1,2), function(x) sum(x == freq_words))
#
unique_words_freq = word_count_freq >= 1

l_freq = lapply(colnames(freq_words), function(name) freq_words[unique_words_freq[,name], name])
names(l_freq) = colnames(freq_words)
l_freq

#WORDCLOUD
freq = dtm['BINAY', l_freq$BINAY]
wordcloud(l_freq$BINAY, as.vector(freq),random.order=FALSE,scale=c(2,.1),
          rot.per=.15,colors=brewer.pal(9,'Blues'))


freq = dtm['DUTERTE', l_freq$DUTERTE]
wordcloud(l_freq$DUTERTE, as.vector(freq),random.order=FALSE,scale=c(2,.1),
          rot.per=.15,colors=brewer.pal(9,'Greens'))

freq = dtm['POE', l_freq$POE]
wordcloud(l_freq$POE, as.vector(freq),random.order=FALSE,scale=c(2,.1),
          rot.per=.15, colors=brewer.pal(9,'Oranges'))

freq = dtm['MAR', l_freq$MAR]
wordcloud(l_freq$MAR, as.vector(freq),random.order=FALSE,scale=c(2,.1),
          rot.per=.15, colors=brewer.pal(9,'Greens'))

freq = dtm['MIRIAM', l_freq$MIRIAM]
wordcloud(l_freq$MIRIAM, as.vector(freq),random.order=FALSE,scale=c(2,.1),
          rot.per=.15, colors=brewer.pal(9,'Blues'))



#####################
#It would actually be interesting to see for each candidate, the words in his top-100
#that are unique to him, i.e. that are not in the other candidates' top-100

#find the number of times each word appears in the matrix
word_count = apply(freq_words, c(1,2), function(x) sum(x == freq_words))
#keep those words that appear only once
unique_words = word_count == 1

l = lapply(colnames(freq_words), function(name) freq_words[unique_words[,name], name])
names(l) = colnames(freq_words)
l

################

#For each candidate, what is the average word length ?

#number of words by speaker
nb_words = apply(dtm, 1, sum)
# BINAY     MAR DUTERTE  MIRIAM     POE 
# 1171    2297    1160     347    1713


#word lengths
word_lengths = sapply(colnames(dtm), nchar)
#transform word count into total character count matrix
character_counts = t(apply(dtm, 1, function(v) v * word_lengths))
#total character count by speaker
total_character_counts = apply(character_counts, 1, function(v) sum(v))
#divide total character count by numer of words
round(total_character_counts / nb_words, digits=1)
# BINAY     MAR DUTERTE  MIRIAM     POE 
# 7.3     7.1     6.5     6.8     7.1 

################

#for each candidate, how diversified is their vocabulary ?

# count the number of unique words per 1000 words
apply(dtm, 1, function(v) round(sum(v != 0)/sum(v)*1000, digits=1))

#number of unique words per 1000 words
# BINAY     MAR DUTERTE  MIRIAM     POE 
# 617.4   553.3   622.4   821.3   598.4 

#number of words by speaker
# BINAY     MAR DUTERTE  MIRIAM     POE 
# 1171    2297    1160     347    1713




##################
#### SUMMARY #####
##################

# The recent Philippines' Presidential Debate (Pilipinas Debates 2016) drew unprecedented amounts of viewers and the
# usual lot of controversies and soundbites in the media. In our polarized media landscape, ensuing political analysis
# always suffer from political bias. Whether you trust the media, you will get a very different take on what was said.

# So far, the candidates have spoken thousands of words in the two (2) debates held by two TV networks.
# In this simple textual analysis, I turn all of the words into data and make inferences about the candidates'
# characteristics that would otherwise be too difficult to discover.


#First let's answer the following question: how often does a certain candidate get mentioned
#by name by the other candidates VS how often do the other candidates mention each other's name.
#On the first debate, the candidates really didn't refer to each other much at all.

#Things change around the second debate, where the candidates refer to each other significantly more.
#Three candidates stood out that got mentioned by name by the other candidates.
#Out of these trio, the other candidates collectively referred to Mar more often than they refer to each other.
#In particular, they throw jabs to each other 13 times in total (red bar),
#where as they ganged-up on Mar 45 times! (blue bar).





#Let's now turn our attention to the words themselves. For each candidate, what are the top 100 most words?

#Poe and Mar were the most active for the first and second debates (60% of the total words thrown)
#They also both used longer words (around 7 letters per word).With Binay included, these three candidates
#used longer words than the rest of the candidates.

#In terms of the diversity of words used (unique words per 1000 words), this same set of candidates + Duterte used fewer words
#except Miriam. She was the least active and she have the most diversified vocabulary among
#the five candidates. It doesn't came as a surprise, considering that she was not around during the 2nd run of the debate.

