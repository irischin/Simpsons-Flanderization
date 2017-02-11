Flanderization of Homer Simpsons
================

Mean Length of Utterance
========================

One particular analysis that I'm interested in is examining Homer's mean length of utterance (MLU). MLU is a common measure used in language acquisition research that is thought to reflect children's linguistic complexity and productivity. It is computed by first calculating the number of morphemes (i.e., the smallest meaningful unit) produced by a child in a speech sample and then dividing that by the total number of utterances produced. This may not be completely appropriate to use for *adult* speech, but just as a first pass analysis, may nonetheless provide some insight. To calculate a character's MLU, I used the **Computerized Language ANanlysis (CLAN)** program. **CLAN** is a tool commonly used by language acquisitionist to analyze children's speech. To learn more about CLAN (and CHILDES), visit this website: <http://childes.psy.cmu.edu/>.

To use CLAN, the speech samples needed to be in organized into individual text files and in a format required by CLAN (i.e., CHAT). Thus, the following steps describe the method through which I generated individual transcript files that were later passed through CLAN.

I've already extracted the Simpsons corpus from the FXX Simpsons World website (see my Extracting Simpsons Corpus script) so here, I just need to load the corpus:

``` r
Simpsons.corpus<-read.table("~/Desktop/Iris Chin/Whole_Simpsons_corpus.txt", header=T, sep="\t")
```

Here, I've created a `Character.corpus` function that allows you to extract all utterances of a particular character and in the meantime, also further cleans the transcripts so that they are "CLAN-ready" (e.g., making sure that for each entry, there is only one utterance while still maintaining speaker information. Thus, an entry such as "Homer Simpson: That's my boy. Everything on Santa's Little Helper." was split into two entries (1) "Homer Simpson: That's my boy." (2) "Homer Simpson: Everything on Santa's Little Helper."). This is done for each episode, of each season. Season and Episode information for each entry is still maintained despite the splits. You'll also notice that I have loaded the `magrittr` library as the function utilizes pipes.

The `Character.corpus` function takes three arguments:

1. `corpus`: the corpus that you are working with 
2. `simps.char`: the character you are interested in extracting all the utterances for (e.g., "Homer Simpson" or "Lisa Simpson") 
3. `simps.char.tag`: the character/speaker tag you would like to use in the transcripts. The CHAT format allows for only one-word name tags. Thus, if you were interested in extracting Homer Simpson utterances, your character tag can be named Homer or HM (anything as long as it is one-word not involving any special characters).

``` r
library(magrittr)

Character.corpus<-function(corpus,simps.char,simps.char.tag) {
  Char.fn.ln<-vector()
  Season.H.n<-vector()
  Episode.H.n<-vector()
  for (cur.season.num in 1:length(unique(corpus$Season))) {
  Season.sub<-subset(corpus, Season==cur.season.num)
    for (cur.ep.num in 1:length(unique(Season.sub$Episode))) {
      Episode.sub<-subset(Season.sub, Episode==cur.ep.num)
      Char.ln<-Episode.sub$Lines[grep(paste("^", simps.char, sep=""), ignore.case = T, Episode.sub$Lines)]
      Char.ln.1<-Char.ln %>% 
        gsub("(\\?)+", "\\?___", .) %>%
        gsub("(\\!)+", "\\!___", .) %>%
        gsub("(\\.)+", "\\.___", .) %>%
        strsplit("___", perl=T) %>%
        unlist()
      for (z in 1:length(Char.ln.1)) {
        if (length(grep(paste("^", simps.char, ":", sep=""), ignore.case=T, Char.ln.1[z]))==0) {
          Char.ln.1[z]<-paste(simps.char, ":\t", Char.ln.1[z], sep="")
        }
      }
      Char.ln.2<-Char.ln.1 %>% 
        gsub("\t\\s+", "\t", .) %>%
        gsub("([^\\.\\?\\!\t]$)", "\\1\\.", .) %>%
        gsub(paste(simps.char, ":", sep=""), paste("*", simps.char.tag, ":", sep=""), ignore.case=T, .)
      Char.ln.3<-Char.ln.2[grep(paste("(^\\*", simps.char.tag, ":)\t\\w+.*", sep=""), Char.ln.2)]
      Char.fn.ln<-c(Char.fn.ln, Char.ln.3)
      Season.H.n<-c(Season.H.n, rep(cur.season.num, length(Char.ln.3)))
      Episode.H.n<-c(Episode.H.n, rep(cur.ep.num, length(Char.ln.3)))
    }
  }
  Final.corpus<-data.frame(Season=Season.H.n, Episode=Episode.H.n, Lines=Char.fn.ln)
}
```

From the `Character.corpus` function, we can then extract specifically Homer's corpus:

``` r
Homer.corpus<-Character.corpus(corpus = Simpsons.corpus, simps.char = "Homer Simpson", simps.char.tag = "HOMER")
head(Homer.corpus)
```

    ##   Season Episode                                   Lines
    ## 1      1       1 *HOMER:\tThere's no time to be careful.
    ## 2      1       1                    *HOMER:\tWe're late.
    ## 3      1       1            *HOMER:\tPardon my galoshes.
    ## 4      1       1                 *HOMER:\tOh, it's Lisa.
    ## 5      1       1                   *HOMER:\tThat's ours.
    ## 6      1       1                         *HOMER:\tMarge!

Some exploratory analysis looking at how often Homer speaks across the seasons can be done. This will also give us an idea of how much speech we have to work with:

``` r
library(plyr)
ddply(Homer.corpus, .(Season), summarise, Num.lines=length(Lines))
```

    ##    Season Num.lines
    ## 1       1       901
    ## 2       2      1370
    ## 3       3      1208
    ## 4       4       924
    ## 5       5      1063
    ## 6       6      1037
    ## 7       7       766
    ## 8       8       854
    ## 9       9      1170
    ## 10     10      1304
    ## 11     11      1177
    ## 12     12       999
    ## 13     13       897
    ## 14     14      1032
    ## 15     15       966
    ## 16     16      1038
    ## 17     17       699
    ## 18     18       652
    ## 19     19       662
    ## 20     20       698
    ## 21     21       617
    ## 22     22       590
    ## 23     23       745
    ## 24     24       881
    ## 25     25       754
    ## 26     26       668

If you look at the number of utterances in total that is spoken by Homer per season, we actually find that there isn't that much speech to work with. This is especially the case given that, within a season, there is usually between 20-22 episodes. For example, imagine a season had about 700 lines. This would suggest that Homer speaks about 30-35 lines per episode. Although this might be completely reasonable for a 22-minute episode, having such a small speech sample per episode would potentially distort the MLU calculation. Thus, I instead created 100-utterance samples for each season (rather than dividing the speech samples by their respective episodes).

To generate individual transcription files for each season, where each transcription file contains 100 utterances as well as the beginning and ending headers specified by the CHAT format, I created the `transcript.create` function.

The function contains a for-loop that essentially first tries to calculate how many 100-utterance "chunks" there are in each season and then randomly removes *n* utterances, where *n* is number of utterances per season mod 100. The `trans.hding` string essentially contains the beginning header information (e.g., who the speaker is, what role they have, who the "transcriber" is -- here I have used the initials MG) that is required for CHAT and CLAN.

The function then creates 100-utterance chunks for each season and outputs them into text files in a format that is compatible for CLAN. It takes four arguments:

1. `corpus`: the structure containing your corpus / dialogue lines for the particular character 
2. `character.name`: the name of the character you are currently analyzing. This should be in title case (e.g., "Homer") 
3. `character.role`: this is the role of the character. CHAT has a specified list of possible roles. The relevant ones that you can use include Father, Mother, Child, Sibling, Brother, Sister, Female, and Male. 
4. `location`: the location / directory in which you want the transcripts to be outputted.

``` r
transcript.create<-function(corpus, character.name, character.role, location) {
  for (x in 1:length(unique(corpus$Season))) {
    Season.a<-subset(corpus, Season==x)
    if ((length(Season.a$Lines)%%100)==0) {
      Season.b<-Season.a
   } else {
      Season.b<-Season.a[-sample(1:length(Season.a$Lines),length(Season.a$Lines)%%100),]
    }
    Season.split<-split(Season.b$Lines, rep(1:(length(Season.b$Lines)/100), each=100))
    for (y in 1:(length(Season.b$Lines)/100)) {
      trans.hding<-paste("@Begin", "@Languages:\teng", paste("@Participants:\t", toupper(character.name), " ", character.role, sep=""), paste("@ID: eng|MG|", character.name, "|||||", character.role, "||", sep=""), sep="\n")
      Season.split.1<-c(trans.hding, as.character(Season.split[[y]]), "@End")
      writeLines(Season.split.1, paste(location, "Season", x, "Part", y, ".txt", sep=""))
    }
  }
}
transcript.create(Homer.corpus, "Homer", "Father", "~/Transcripts/")
```

Thus far, I've been only focused on Homer's corpus. However, we might want to investigate whether a change occurs with the other family members as well. They can serve as controls -- in that we don't typically talk about, for example, Lisa's Flanderization in terms of her intelligence. Thus, we might expect Lisa's linguistic complexity to stay relatively the same across seasons. It might be also interesting to just compare Homer's linguistic complexity to the other family members to investigate whether he is, at least linguistically speaking, less intelligent.

We therefore would want to create individual transcripts for the other family members (e.g., Marge, Bart, and Lisa) as well. We can reuse our `Character.corpus` function to first extract all the relevant dialogue lines for each of the characters as well as our `transcript.create` function to create the individual 100-utterance transcripts.

``` r
Marge.corpus<-Character.corpus(corpus = Simpsons.corpus, simps.char = "Marge Simpson", simps.char.tag = "MARGE")

Bart.corpus<-Character.corpus(corpus = Simpsons.corpus, simps.char = "Bart Simpson", simps.char.tag = "BART")

Lisa.corpus<-Character.corpus(corpus = Simpsons.corpus, simps.char = "Lisa Simpson", simps.char.tag = "LISA")
```

``` r
transcript.create(Marge.corpus, "Marge", "Mother", "~/Transcripts/")
transcript.create(Bart.corpus, "Bart", "Child", "~/Transcripts/")
transcript.create(Lisa.corpus, "Lisa", "Child", "~/Transcripts/")
```

For each character, their transcripts were ran through CLAN and a table was generated containing the MLU of each of those transcripts. We can import these tables, create summary tables of the MLU of each season for each character, and then concatentate the four tables together.

``` r
Homer.mlu<-read.table("~/Desktop/Iris Chin/MLU tables/Homer_MLU.txt", header=T, sep="\t")
Homer.mlu.sum<-ddply(Homer.mlu, .(Season), summarize, Mean_mlu=mean(MLU))

Marge.mlu<-read.table("~/Desktop/Iris Chin/MLU tables/Marge_MLU.txt", header=T, sep="\t")
Marge.mlu.sum<-ddply(Marge.mlu, .(Season), summarize, Mean_mlu=mean(MLU))

Bart.mlu<-read.table("~/Desktop/Iris Chin/MLU tables/Bart_MLU.txt", header=T, sep="\t")
Bart.mlu.sum<-ddply(Bart.mlu, .(Season), summarize, Mean_mlu=mean(MLU))

Lisa.mlu<-read.table("~/Desktop/Iris Chin/MLU tables/Lisa_MLU.txt", header=T, sep="\t")
Lisa.mlu.sum<-ddply(Lisa.mlu, .(Season), summarize, Mean_mlu=mean(MLU))

MLU.sum.all<-join_all(list(Homer.mlu.sum, Marge.mlu.sum, Bart.mlu.sum, Lisa.mlu.sum), by="Season")
names(MLU.sum.all)[2:5]<-c("Homer", "Marge", "Bart", "Lisa")
print(MLU.sum.all)
```

    ##    Season    Homer    Marge     Bart     Lisa
    ## 1       1 6.005556 6.647500 5.390000 5.640000
    ## 2       2 6.180769 7.105000 5.478333 6.790000
    ## 3       3 6.573333 7.334000 6.661667 8.106667
    ## 4       4 6.062222 6.996667 6.272500 6.430000
    ## 5       5 6.454000 7.773333 6.572500 7.605000
    ## 6       6 7.136000 7.376000 6.425000 7.218000
    ## 7       7 6.770000 6.953333 6.687500 6.735000
    ## 8       8 6.622500 8.062500 6.375000 6.713333
    ## 9       9 6.074545 6.664000 5.550000 6.990000
    ## 10     10 6.357692 6.640000 5.727500 6.802500
    ## 11     11 6.175455 6.960000 5.670000 6.760000
    ## 12     12 5.977778 6.203333 5.610000 6.346667
    ## 13     13 6.540000 6.977500 6.260000 7.006667
    ## 14     14 6.954000 7.525000 7.156667 7.513333
    ## 15     15 7.314444 7.885000 6.822500 7.956667
    ## 16     16 6.988000 8.364000 6.836667 8.103333
    ## 17     17 7.646667 7.735000 7.300000 7.900000
    ## 18     18 6.891667 7.870000 7.160000 7.645000
    ## 19     19 7.590000 8.286667 7.150000 7.260000
    ## 20     20 7.341667 7.910000 6.493333 7.243333
    ## 21     21 7.556667 7.355000 6.665000 7.306667
    ## 22     22 7.362000 7.610000 7.476667 8.330000
    ## 23     23 7.854286 7.983333 7.620000 8.326667
    ## 24     24 7.490000 7.745000 7.513333 7.826667
    ## 25     25 7.171429 7.330000 6.696667 7.223333
    ## 26     26 7.010000 7.160000 7.260000 7.550000

We can now plot the average MLU of the different characters across the seasons. <img src="Flanderization_of_the_Simpsons_files/figure-markdown_github/Plotting MLU-1.png" style="display: block; margin: auto;" />

Additional Measures of Linguistic Complexity
============================================

In addition to MLU, we can also use other measures of linguistic complexity. Here, we'll include four additional measures:

1. **Type-Token ratio**, where higher TTR indicates larger amount of lexical variation 
2. **Average Word Length** 
3. **Average number of syllables per word** 
4. **Flesch reading-ease Score** indicates how difficult it is to understand a passage or text. The higher the score, the easier it is to understand the text/passage.

I've written a function called `Char.ling.complex` that calculates these four measures for the character of interest. The function takes two arguments:

1. `char.corpus`: the corpus containing all of the character's lines 
2. `character.name`: the name of the character (e.g., "Homer")

To execute the function properly, you will need the `quanteda` package (which helps calculate the number of syllables) and a list of stop words (roughly, common words like 'the', 'of', 'and' which carry little meaning on their own).

This will generate the four measures for each episode, of each season.

``` r
library(quanteda)
stop.list<-scan("~/Desktop/Iris Chin/stop_list.txt", what="character", sep=",")

Char.ling.complex<-function(char.corpus, character.name) {
  Season<-vector()
  Episode<-vector()
  Flesch.reading.score<-vector()
  type.token<-vector()
  word.length<-vector()
  num.syll<-vector()
  #num.syll.na<-vector()
  for (season.num in 1:length(unique(char.corpus$Season))) {
    corpus.1<-subset(char.corpus, Season==season.num)
    corpus.1$Lines.lw<-tolower(gsub(paste("\\*", toupper(character.name), ":\t", sep=""), "", corpus.1$Lines))
    for (episode.num in 1:length(unique(corpus.1$Episode))) {
      if (length(subset(corpus.1, Episode==episode.num)$Lines.lw) > 0) {
        corpus.2<-subset(corpus.1, Episode==episode.num)
        words.list<-unlist(strsplit(corpus.2$Lines.lw, "(\\s|\\?|\\.|\\!|\\,)+"))
        words.by.sentences<-length(words.list)/length(corpus.2$Lines.lw)
        syll.by.words<-(sum(nsyllable((words.list)), na.rm = T))/length(words.list)
        #num.syll.na.1<-sum(is.na(nsyllable(words.list)))
        Flesch.reading<-206.835-1.015*words.by.sentences-84.6*syll.by.words
        
        #this below looks at the type-token ratio -- here, I've filtered out stopwords (the list was complied mostly from the NLTK package in Python)
        words.list.2<-words.list[!words.list%in%stop.list]
        word.table.2<-sort(table(words.list.2), decreasing = T)
        type.token.1<-length(word.table.2)/length(words.list.2)
        #this calculates average word length
        word.length.1<-mean(nchar(words.list.2))
        #this calculates average number of syllables
        num.syll.1<-mean(nsyllable(words.list.2), na.rm=T)
        #to concatenate all the pieces of information
        Season<-c(Season, season.num)
        Episode<-c(Episode, episode.num)
        Flesch.reading.score<-c(Flesch.reading.score, Flesch.reading)
        type.token<-c(type.token, type.token.1)
        word.length<-c(word.length, word.length.1)
        num.syll<-c(num.syll, num.syll.1)
        #num.syll.na<-c(num.syll.na, num.syll.na.1)
      }
    }
  } 
  Character.complexity<-data.frame(Season, Episode, Flesch.reading.score, type.token, word.length, num.syll)
}
```

We can now calculate these additional four measures for Homer (as well as the other Simpsons family members).

``` r
Homer.complexity<-Char.ling.complex(char.corpus = Homer.corpus, character.name = "Homer")
Marge.complexity<-Char.ling.complex(char.corpus = Marge.corpus, character.name = "Marge")
Bart.complexity<-Char.ling.complex(char.corpus = Bart.corpus, character.name = "Bart")
Lisa.complexity<-Char.ling.complex(char.corpus = Lisa.corpus, character.name = "Lisa")
```

For each measure, I'm also going to create a mean score for each season. Then I'll create a plot of each of the those measures.

First, for Homer:

``` r
library(plyr)
Homer.complex.sum<-ddply(Homer.complexity, .(Season), summarise,
                         Flesch_mean=mean(Flesch.reading.score),
                         TTR.mean=mean(type.token),
                         Word.length.mean=mean(word.length),
                         Num.syll.mean=mean(num.syll))
print(Homer.complex.sum)
```

    ##    Season Flesch_mean  TTR.mean Word.length.mean Num.syll.mean
    ## 1       1    94.97652 0.7283142         4.958238      1.428880
    ## 2       2    94.89422 0.7538930         4.935587      1.431659
    ## 3       3    91.86510 0.7823693         5.050111      1.479777
    ## 4       4    95.83179 0.8152818         4.878755      1.413516
    ## 5       5    91.79756 0.7901983         5.031723      1.473652
    ## 6       6    90.58009 0.7977537         5.041005      1.497560
    ## 7       7    90.81342 0.8368755         4.990487      1.497775
    ## 8       8    91.43611 0.8416593         5.047073      1.480777
    ## 9       9    93.80383 0.7984738         4.886134      1.454392
    ## 10     10    91.77097 0.8060744         5.053138      1.482205
    ## 11     11    91.22157 0.8179810         5.090686      1.497436
    ## 12     12    91.67221 0.8352745         5.050446      1.489519
    ## 13     13    91.07006 0.8262813         5.015600      1.499121
    ## 14     14    91.46209 0.8151923         5.082164      1.482319
    ## 15     15    90.20489 0.8235249         5.119910      1.503682
    ## 16     16    88.53867 0.8230823         5.215151      1.536782
    ## 17     17    88.48497 0.8521128         5.261096      1.531361
    ## 18     18    89.44660 0.8754591         5.184087      1.525087
    ## 19     19    90.16204 0.8536595         5.119849      1.506266
    ## 20     20    87.23116 0.8454396         5.292274      1.575862
    ## 21     21    89.05108 0.8522934         5.219571      1.532170
    ## 22     22    90.75813 0.8712858         5.100767      1.491175
    ## 23     23    86.98354 0.8616647         5.285099      1.557194
    ## 24     24    87.82281 0.8553215         5.279981      1.561284
    ## 25     25    90.40953 0.8330431         5.148877      1.512454
    ## 26     26    90.39830 0.8354631         5.146018      1.504109

<img src="Flanderization_of_the_Simpsons_files/figure-markdown_github/Homer complexity graph-1.png" style="display: block; margin: auto;" />

Then for Marge:

``` r
Marge.complex.sum<-ddply(Marge.complexity, .(Season), summarise,
                         Flesch_mean=mean(Flesch.reading.score),
                         TTR.mean=mean(type.token),
                         Word.length.mean=mean(word.length),
                         Num.syll.mean=mean(num.syll))
print(Marge.complex.sum)
```

    ##    Season Flesch_mean  TTR.mean Word.length.mean Num.syll.mean
    ## 1       1    90.58331 0.7654806         5.125617      1.514425
    ## 2       2    88.39854 0.7890017         5.196294      1.542104
    ## 3       3    90.19035 0.8470960         5.097524      1.503821
    ## 4       4    90.38563 0.8523582         5.081853      1.508330
    ## 5       5    86.69047 0.8973333         5.287956      1.587226
    ## 6       6    87.41895 0.8355449         5.229242      1.566035
    ## 7       7    89.79149 0.8966334         5.245303      1.539252
    ## 8       8    87.25289 0.8972204         5.319461      1.552202
    ## 9       9    90.48216 0.8634883         5.207480      1.530344
    ## 10     10    90.48137 0.8857448         5.237917      1.538745
    ## 11     11    87.93954 0.8940481         5.185421      1.565419
    ## 12     12    91.12631 0.8877741         5.051124      1.509650
    ## 13     13    86.10193 0.8780682         5.410215      1.606288
    ## 14     14    86.32744 0.8902037         5.240539      1.559123
    ## 15     15    88.38124 0.8791300         5.209242      1.528295
    ## 16     16    86.63630 0.8678174         5.333380      1.544064
    ## 17     17    86.59380 0.8885294         5.347410      1.571184
    ## 18     18    86.58212 0.9017159         5.290263      1.563796
    ## 19     19    85.32405 0.9062347         5.370875      1.599853
    ## 20     20    89.21580 0.8939728         5.172873      1.504469
    ## 21     21    88.98242 0.9129964         5.133213      1.522679
    ## 22     22    87.77600 0.9145990         5.270223      1.544082
    ## 23     23    87.59624 0.9256129         5.208524      1.543309
    ## 24     24    81.87504 0.8993748         5.507298      1.637251
    ## 25     25    87.28046 0.8818586         5.286706      1.552904
    ## 26     26    91.81757 0.8758937         5.216743      1.510826

<img src="Flanderization_of_the_Simpsons_files/figure-markdown_github/Marge complexity graph-1.png" style="display: block; margin: auto;" />

Then for Bart:

``` r
Bart.complex.sum<-ddply(Bart.complexity, .(Season), summarise,
                         Flesch_mean=mean(Flesch.reading.score),
                         TTR.mean=mean(type.token),
                         Word.length.mean=mean(word.length),
                         Num.syll.mean=mean(num.syll))
print(Bart.complex.sum)
```

    ##    Season Flesch_mean  TTR.mean Word.length.mean Num.syll.mean
    ## 1       1    95.91231 0.7456913         4.700021      1.392992
    ## 2       2    93.81993 0.8431595         4.845472      1.427751
    ## 3       3    94.71927 0.8442017         4.814606      1.409842
    ## 4       4    93.23047 0.8614580         4.980022      1.462918
    ## 5       5    90.93262 0.8751249         5.077013      1.504186
    ## 6       6    87.99535 0.8686375         5.148776      1.558758
    ## 7       7    91.34090 0.8670282         5.186877      1.495934
    ## 8       8    93.67141 0.8936871         4.966068      1.434336
    ## 9       9    93.76341 0.8771758         4.963505      1.454210
    ## 10     10    92.36692 0.9032728         5.018390      1.458456
    ## 11     11    93.57950 0.9142895         5.036201      1.472397
    ## 12     12    90.39920 0.9111851         5.069079      1.494517
    ## 13     13    93.89628 0.9095497         5.009814      1.437068
    ## 14     14    92.54714 0.9000084         4.860495      1.448386
    ## 15     15    89.07770 0.8904136         5.105424      1.528567
    ## 16     16    91.85790 0.9077342         5.087549      1.470236
    ## 17     17    92.19746 0.9251920         5.015103      1.468428
    ## 18     18    92.46740 0.9155008         5.039854      1.471678
    ## 19     19    90.69386 0.8892087         5.056836      1.469753
    ## 20     20    93.92635 0.9298663         4.875232      1.421377
    ## 21     21    91.23555 0.8843095         5.182570      1.480419
    ## 22     22    91.06721 0.9000454         5.042810      1.478492
    ## 23     23    87.60343 0.9126066         5.227176      1.559084
    ## 24     24    88.57172 0.8867709         5.074631      1.523119
    ## 25     25    88.61663 0.9165938         5.231704      1.539835
    ## 26     26    90.96760 0.9267000         4.953184      1.453134

<img src="Flanderization_of_the_Simpsons_files/figure-markdown_github/Bart complexity graph-1.png" style="display: block; margin: auto;" />

Finally, for Lisa:

``` r
Lisa.complex.sum<-ddply(Lisa.complexity, .(Season), summarise,
                         Flesch_mean=mean(Flesch.reading.score),
                         TTR.mean=mean(type.token),
                         Word.length.mean=mean(word.length),
                         Num.syll.mean=mean(num.syll))
print(Lisa.complex.sum)
```

    ##    Season Flesch_mean  TTR.mean Word.length.mean Num.syll.mean
    ## 1       1    90.96742 0.8796796         5.113562      1.477453
    ## 2       2    82.82886 0.8724731         5.404656      1.627776
    ## 3       3    89.03804 0.8944719         5.263117      1.528365
    ## 4       4    87.77389 0.8686630         5.202534      1.555776
    ## 5       5    86.08777 0.8727979         5.260418      1.558785
    ## 6       6    84.86003 0.8645440         5.303876      1.594476
    ## 7       7    87.88900 0.8660068         5.325942      1.566108
    ## 8       8    86.77521 0.9075856         5.242584      1.567847
    ## 9       9    91.13845 0.8758000         5.134525      1.516456
    ## 10     10    88.77057 0.9047569         5.194902      1.526049
    ## 11     11    91.22458 0.9156921         5.183977      1.504994
    ## 12     12    88.55667 0.9040134         5.307834      1.548498
    ## 13     13    87.33859 0.9005601         5.330017      1.557337
    ## 14     14    85.44443 0.9007745         5.366578      1.577871
    ## 15     15    85.05257 0.8881077         5.396372      1.592420
    ## 16     16    80.81671 0.9205767         5.578538      1.677394
    ## 17     17    85.48396 0.9193483         5.472985      1.563029
    ## 18     18    87.14102 0.8922098         5.425174      1.575477
    ## 19     19    86.19115 0.9193094         5.303584      1.594631
    ## 20     20    85.69694 0.8851884         5.267166      1.585234
    ## 21     21    85.88061 0.8882249         5.350872      1.604861
    ## 22     22    81.67436 0.8912076         5.505697      1.633066
    ## 23     23    83.51487 0.9164199         5.410738      1.608740
    ## 24     24    84.89728 0.8963953         5.390196      1.602974
    ## 25     25    88.41529 0.8957776         5.258753      1.558469
    ## 26     26    87.25051 0.9099069         5.339178      1.558404

<img src="Flanderization_of_the_Simpsons_files/figure-markdown_github/Lisa complexity graph-1.png" style="display: block; margin: auto;" />

We can also create a single group of plots containing all members simultaneously <img src="Flanderization_of_the_Simpsons_files/figure-markdown_github/Complexity all members-1.png" style="display: block; margin: auto;" />
