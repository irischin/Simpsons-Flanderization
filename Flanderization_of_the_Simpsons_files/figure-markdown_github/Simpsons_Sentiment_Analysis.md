Sentiment Analysis of the Simpsons
================

Change in the Simpsons' Sentiment
=================================

A potentially interesting question that we might be able to examine in our Simpsons corpus is whether the general tone of the show has changed over time. For example, one might think that during the "classic" Simpsons era, there was much more of a sweet, touching tone (what we might think of as more of a "positive" sentiment) that might be less evident in the newer seasons. (Some examples from the classic era like *Lisa's substitute*, *Mother Simpson*, *Duffless*, *A Fish Called Selma*, for example, comes to mind). One way to investigate this question is to perform a sentiment analysis on our corpus. `Tidytext` ([Silge and Robinson, 2016](https://github.com/juliasilge/tidytext)) is a neat R package that provides the tool for us to do this.

First, let's load the relevant packages, `tidytext`, `tidyr`, `dplyr`, and `ggplot2`.

``` r
library(tidytext)
library(tidyr)
library(dplyr)
library(ggplot2)
```

I can then look at the general sentiment of the Simpsons corpus to investigate whether it has changed overtime with regards to how "positive" or "negative" it is.

As mentioned before, I'll be using the Simpsons corpus that I have previously collected (see `Extracting_Simpsons_Corpus.md` under my `Simpsons-Flanderization` project).

``` r
Simpsons.corpus<-read.table("~/Google Drive/Projects/Flanderization of Homer/Whole_Simpsons_corpus.txt", header=T, sep="\t")
```

To maintain some contextual information, I might want to calculate a sentiment score for each sentence rather than simply calculating the total number of "positive" and "negative" words in an episode. For example, in the utterance, "yes, not very bright at all," the word "bright" would be counted as a positive sentiment even though with the negation "not," it should be counted more of a negative sentiment.

Thus, we want to first tokenize our corpus into sentences rather than unigrams.

``` r
sentence.tokens<-Simpsons.corpus %>%
  separate(Lines, c("Speaker", "Dialogue"), ":\t") %>%
  mutate(linenumber=row_number()) %>%
  unnest_tokens(sentence, Dialogue, token = "sentences")
```

I have then subsequently written a loop in which ***each sentence*** is first tokenized into unigrams. The unigrams were then matched to a list of negation words to check whether it contains a negation word.

**If the group of unigrams contained any negation words:** - unigrams were first assigned sentiment scores via the `AFINN` lexicon [Finn Årup Nielsen](http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010). Here, we are using a dictionary-based approach, where the unigrams of interest are checked through a dictionary that already contains words that have been assigned sentiment scores. `AFINN` in particular contains a dictionary of words that are scored between -5 (negative sentiment) to 5 (positive sentiment). To access the complete dictionary, simply call `get_sentiments("afinn")`. - then, the sentiment score of a word that was preceded by a negation word was reversed (e.g., "bright", which has a sentiment score of 1 was now turned to -1) - to avoid inflating the sentiment score for the sentence to be too negative, the sentiment score of each negation word was assigned as NA - occurrences of consecutive no's (e.g., no no no) are special cases since typically the presence of a negation word does not involve the change of direction of the next word's sentiment (e.g., "no" preceding "no" does not mean the second "no" should now be reversed from a negative sentiment to a positive sentiment). Thus, cases with consecutive "no's" did not have their sentiments reversed

**If the group of unigrams *did not* contain any negation words:** - the unigrams were simply assigned the sentiment scores using the `AFINN` dictionary.

After these steps, the sentiment score for each sentence was computed by simply adding the sentiment scores of each word

``` r
corp.sentiment<-data.frame()
negation.words<-readLines("~/Google Drive/Projects/Sentiment Analysis of the Simpsons/negation_words.txt")

for (sentence.num in 1:length(sentence.tokens$sentence)) {
  line<-sentence.tokens[sentence.num,]
  unigrams<-unnest_tokens(line, word, sentence, token="words")
  
  if (sum(unigrams$word %in% negation.words) > 0) {
    afinn.scores<-unigrams %>%
      left_join(get_sentiments("afinn"), by="word")
    positions<-which(unigrams$word %in% negation.words) 
    
    #double check whether one of the negated words fall at the end of the utterance:
    if (!tail(positions, n=1)==length(afinn.scores$word)) {
      #then to capture instances in which there are two conseq. negation words
      
      #this removes the location of any (second) negation from the position vector
      sec.neg.positions<-positions[!afinn.scores$word[positions+1] %in% negation.words]
      
      #check if the 2nd negation is "no": these are unique cases and should be removed
      #checking for remaing positions, how many are preceded by "no"
      no.sec.pos<-sec.neg.positions[afinn.scores$word[sec.neg.positions]=="no"]
      no.sec.pos.b<-no.sec.pos[afinn.scores$word[no.sec.pos-1]=="no"]
      sec.neg.positions.2<-sec.neg.positions[!sec.neg.positions %in% no.sec.pos.b]
      
      #adjusts the score for the word following a negation (so that it becomes flipped)
      afinn.scores$score[sec.neg.positions.2+1]<-afinn.scores$score[sec.neg.positions.2+1]*-1
      
      #we don't want to double count the negation so turn that negation word into NA
      afinn.scores$score[sec.neg.positions.2]<-NA
      line$utterance.afinn<-sum(afinn.scores$score, na.rm=T)
      corp.sentiment<-rbind(corp.sentiment, line)
    } else {
      positions.b<-head(positions, n=-1)
      sec.neg.positions<-positions.b[!afinn.scores$word[positions.b+1] %in% negation.words]
      
      no.sec.pos<-sec.neg.positions[afinn.scores$word[sec.neg.positions]=="no"]
      no.sec.pos.b<-no.sec.pos[afinn.scores$word[no.sec.pos-1]=="no"]
      sec.neg.positions.2<-sec.neg.positions[!sec.neg.positions %in% no.sec.pos.b]
      
      afinn.scores$score[sec.neg.positions.2+1]<-afinn.scores$score[sec.neg.positions.2+1]*-1
      afinn.scores$score[sec.neg.positions.2]<-NA
      line$utterance.afinn<-sum(afinn.scores$score, na.rm=T)
      corp.sentiment<-rbind(corp.sentiment, line)
    }
    
  } else {
    afinn.scores<-unigrams %>%
      inner_join(get_sentiments("afinn"), by="word")
    line$utterance.afinn<-sum(afinn.scores$score, na.rm=T)
    corp.sentiment<-rbind(corp.sentiment, line)
  }
  print(sentence.num)
}
```

We can now sum the individual sentiment scores for each sentence to create a cumulative sentiment score for each episode. Then, we can average those scores to create a mean sentiment score for each season. <img src="Simpsons_Sentiment_Analysis_files/figure-markdown_github/Season sentiment-1.png" style="display: block; margin: auto;" />

As we can see, overall, the show has a pretty positive sentiment (e.g., it never falls below zero). However, there does seem to be a steep drop in positive sentiment after season 3. I'm not quite sure yet what might account for this change. Initially I had wondered if it was because a change of showrunners, but the switch to Al Jean and Mike Reiss (from Matt Groening, James L. Brooks, and Sam Simon) occurs between season 2 and 3. If other people might know more about the Simpsons history that might account for such a change (e.g., change of writers), I'm open to ideas!

I've also included markers to the plot indicating when there was a change in showrunner(s). Again, I don't think a particularly revealing pattern emerges here.

Evaluating Homer's "short-temper"
=================================

Another analysis that I was interested in was looking at the sentiment of the individual Simpsons family members. In particular, I was curious to see if there was a change in Homer's "angry" sentiment over the years. I think one characteristic of Homer's is his short-temper. This quality thus might be "Flanderized" over time.

To perform this analysis, I used a different sentiment dictionary, namely the `NRC` dictionary from [Saif Mohammad and Peter Turney](http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm). Here, words are categorized in a binary fashion (yes/no) as to whether it falls into 10 categories: positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust. As mentioned previously, here I'm particularly interested in anger. However, I want to also include a contrasting, positive emotion as well. In the case that we do see an increase in Homer's anger sentiment, this will provide us with information as to whether this increase is unique to anger or is there just a general increase in more "sentiment" (or "emotion"). Here, I've chosen joy to be my contrasting, positive sentiment because fewer words are in both anger and joy categories, than anger and other positive categories.

Here, I have subsetted the `NRC` dictionary, into one (i.e., `nrc.sub`) that contains only anger and joy words. As one can see, there are only 37 unique words that fall in both anger and joy categories).

``` r
nrc.sub<-get_sentiments("nrc") %>%
  filter(sentiment=="anger"|sentiment=="joy")

joy.words<-unique(subset(nrc.sub, sentiment=="joy")$word)
anger.words<-unique(subset(nrc.sub, sentiment=="anger")$word)
length(which(joy.words %in% anger.words))
```

    ## [1] 37

Given that in a particular season the number of utterances that a character speaks can vary, we might want to take this into account when creating a sum sentiment score for a particular character for the season. A lower score in angry sentiment, for example, might just be from the character speaking just less in that particular season and less so about him/her being less angry. Similarly, if we find that one character's "angry" sentiment is higher than another's, this again might be because the first character just had more lines/had more opportunity to express anger. Thus, I've also created a table containing the number of utterances spoken by each character per season. We can then calculate sentiment scores given the number of utterances spoken, for that season, thus allowing us to control for differences in how much a character speaks season to season as well as in comparison to other characters.

``` r
character.num.utter<-sentence.tokens %>%
  group_by(Season, Speaker) %>%
  summarise(Num_utterance=length(sentence))
```

We can now calculate the number of "angry" and "joy" words spoken by each character, per episode, controlling for how much the character has spoken in the individual episodes.

``` r
corpus.sent.nrc<-sentence.tokens %>%
  unnest_tokens(word, sentence) %>%
  inner_join(nrc.sub, by="word") %>%
  group_by(Season, Speaker, sentiment) %>%
  summarise(Sent_count=length(sentiment)) %>%
  inner_join(character.num.utter, by=c("Season", "Speaker")) %>%
  mutate(Sent_prop=Sent_count/Num_utterance)

print(corpus.sent.nrc)
```

    ## Source: local data frame [5,901 x 6]
    ## Groups: Season, Speaker [4,050]
    ## 
    ##    Season                Speaker sentiment Sent_count Num_utterance
    ##     <int>                  <chr>     <chr>      <int>         <int>
    ## 1       1             Adil Hoxha     anger          2            19
    ## 2       1             Adil Hoxha       joy          3            19
    ## 3       1          Agnes Skinner       joy          1             4
    ## 4       1              Announcer     anger          2             5
    ## 5       1              Announcer       joy          5             5
    ## 6       1 Apu Nahasapeemapetilon     anger          4            12
    ## 7       1 Apu Nahasapeemapetilon       joy          2            12
    ## 8       1          Barney Gumble     anger          5            30
    ## 9       1          Barney Gumble       joy          3            30
    ## 10      1           Bart Simpson     anger         44           655
    ## # ... with 5,891 more rows, and 1 more variables: Sent_prop <dbl>

And now we can subsequently pull out just the Simpsons family members' sentiment scores. Here, I'm including the 3 other family members so that they can serve as controls for Homer.

``` r
Simpsons.members<-c("Homer Simpson", "Marge Simpson", "Bart Simpson", "Lisa Simpson")

Simpsons.mems.sent<-subset(corpus.sent.nrc, Speaker %in% Simpsons.members) 
Simpsons.mems.sent$Speaker<-factor(Simpsons.mems.sent$Speaker, levels=c("Homer Simpson", "Marge Simpson", "Bart Simpson", "Lisa Simpson"))
Simpsons.mems.sent$sentiment<-gsub("anger", "Anger", Simpsons.mems.sent$sentiment)
Simpsons.mems.sent$sentiment<-gsub("joy", "Joy", Simpsons.mems.sent$sentiment)
```

<img src="Simpsons_Sentiment_Analysis_files/figure-markdown_github/Simpsons anger-joy sentiment-1.png" style="display: block; margin: auto;" /> As we can see, there isn't much change to Homer's "angry" sentiment over the course of the seasons. Moreover, in comparison to his other family members, he doesn't seem particularly angry. Overall, the four family members usually tend to express more joy sentiment than angry sentiment. I initially found this pattern to be a little surprising (especially with regards to Homer). However, upon further reflection, I think another characteristic of Homer is that he is sometimes a bit oblivious to his surroundings and can be over-optimistic (think of his catchphrase, "Woo hoo!"). Given that, maybe it's not actually that surprising that he would use many joyous words as well.

<img src="https://frinkiac.com/gif/S05E18/41957/46962.gif?b64lines=IENPTkdSQVRVTEFUSU9OUywgSE9NRVIKIFNJTVBTT04hIFlPVSdWRSBKVVNUIFdPTgogVEhFIEVNUExPWUVFIFJBRkZMRS4KIFdIT08tSE9PIQ==" width="342" height="257"/><img src="https://frinkiac.com/gif/S05E18/47380/53002.gif?b64lines=LSBXSEFUIERPIEkgR0VUPwotIFRIRSBKT0IgT0YgSU5EVVNUUklBTCAKQ0hJTU5FWSBTV0VFUCBGT1IgQSBEQVkhCi0gV29vLWhvbyE=" width="342" height="257"/>
