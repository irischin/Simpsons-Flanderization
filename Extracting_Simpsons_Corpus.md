Extracting the Simpsons Corpus
================

To begin my analyses, I needed a collection of the Simpsons scripts/transcripts. There are several places that have some of the scripts/transcripts available (e.g., Simpsons Archive), but they tend to be incomplete. The FXX, Simpsons World website, however, has most of the scripts/transcripts up on their website and thus, I've used that as my source of data for these analyses.

From Simpsons World, I needed a method to gather all relevant hyperlinks (i.e., hyperlinks for only the episodes and not any short clips or interviews). To do this, I first went to the *Episodes* page -- as it provides links to all the episodes across the different seasons.

Using the rvest package, I downloaded the html for the *Episodes* page to further parse and extract the relevant hyperlinks.

``` r
library(rvest)
simpsoneplist <-read_html("https://www.simpsonsworld.com/browse/episodes")
```

Inspecting the html of the *Episodes* page, the relevant information appears to be embedded in a &lt;div&gt; tag, under the class attribute "share-button", with a &lt;li&gt; parent. Thus, I first extracted all html code within the &lt;li&gt; tags, followed by those in the &lt;div&gt; tag, then those containing the attribute "share-button."

``` r
eplink<-simpsoneplist %>% 
  html_nodes("li") %>%
  html_nodes("div") %>%
  html_nodes(".share-button")
```

For each episode, I extracted information regarding: a) the **Season number** it is in, b) its **Episode Number** (here I'm just assigning the episode number sequentially rather than using a production code), c) its **Episode name**, and d) the hyperlink to the episode itself. I then concatenated the information into a data.frame:

``` r
Season.num<-as.numeric(gsub(".+data-season-number=\"(\\d+)\".+", "\\1", eplink))
Ep.num<-as.numeric(gsub(".+data-episode-number=\"(\\d+)\".+", "\\1", eplink))
Ep.name<-gsub(".+data-video-name=\"(.+)\" data-video-description.+", "\\1", eplink)
Ep.link.ad<-gsub(".+data-video-link=\"(.+)\" data-season-number=.+", "\\1", eplink)
ep.df.link<-data.frame(Season=Season.num, Episode=Ep.num, Ep.Name=Ep.name, Ep.link=Ep.link.ad)
head(ep.df.link)
```

After obtaining the relevant information for each episode, I then did the following for each episode: \* access and import the episode's html file \* extract relevant html code containing only dialogue information (they are located under the "script-message" attributes, in &lt;p&gt; tags) \* extract the actual dialogue embedded in the &lt;p&gt; tags \* do some preliminary cleaning, in which only dialogue containing speech information is included (so anything that is sung, which is delineated with "(SINGING)" in the script/transcription, is excluded) \* after extracting the relevant dialogue for the particular episode, I concatenated all those lines into a larger vector containing all the dialogue that has thus far been extracted from other episodes \* to ensure that information regarding what season and episode number the lines come from is not lost, I also updated vectors with the Season and Episode information for each line of dialogue extracted (i.e., if the episode contained 100 lines of dialogue, the Season and Episode number will each be repeated 100 times and concatenated with larger vectors containing those pieces of information for all the episodes that have been analyzed thus far)

This was all implemented with a loop, where each episode underwent the described above extraction and cleaning. After the loop completed, there were three vectors containing the relevant information for each line of dialogue:
<div>
1.  `Season.n`: containing the season number information
2.  `Episode.n`: containing the episode number information
3.  `Lines`: containing the actual line of dialogue (which included the speaker information)

``` r
Season.n<-vector()
Episode.n<-vector()
Lines<-vector()

for (i in 1:length(ep.df.link$Ep.link)) {
  episode<-read_html(as.character(ep.df.link$Ep.link[i]))
  lines.1<-episode %>%
    html_nodes(".script-message") %>%
    html_nodes("p")
  lines.2<-gsub("<p><span>(.+) </span> (.+)</p>", "\\1\t\\2", lines.1)
  lines.3<-lines.2[grep(".+:\t[^(SINGING)].+", lines.2)]
  Lines<-c(Lines, lines.3)
  Season.n<-c(Season.n, rep(ep.df.link$Season[i], length(lines.3)))
  Episode.n<-c(Episode.n, rep(ep.df.link$Episode[i], length(lines.3)))
}
```

The following is not a necessary step, but as I had planned to use a variety of other tools to analyze some of the scripts/transcripts, I also did some additional cleaning to standardized some of the formatting (e.g., getting rid of any extra spaces or special characters; making sure that punctuation marks are only present at the end of the utterances). I also converted words back from their abbreviations. I suspect this was not an exhaustive list of abbreviated words in the transcripts -- but those were common things that stuck out when I looked initially at a couple transcripts. Feel free to skip this step.

``` r
Lines.clean<-Lines %>% 
  gsub("\\(.+?\\)", "", .) %>%
  gsub("--(-)*", "", .) %>%
  gsub("\\.\\.\\.", "", .) %>%
  gsub("\"", "", .) %>%
  gsub("Mr\\.", "Mister", ignore.case=F, .) %>%
  gsub("Mrs\\.", "Missus", ignore.case=F, .) %>%
  gsub("Ms\\.", "Miss", ignore.case=F, .) %>%
  gsub("Dr\\.", "Doctor", ignore.case=F, .) %>%
  gsub("a\\.m\\.", "am", ignore.case=T, .) %>%
  gsub("p\\.m\\.", "pm", ignore.case=T, .) %>%
  gsub("U\\.S\\.A\\.", "USA", ignore.case=F, .) %>%
  gsub("\t\\s+", "\t", .)
```

After cleaning the lines of dialogue, I combined the three vectors created from the previous for-loop into a dataframe that contains the whole Simpsons corpus across the seasons. Note: if you skipped the above step, you would use `Lines` rather than `Lines.clean`. I then exported the data frame for subsequent analyses.

``` r
Simpsons.corpus<-data.frame(Season=Season.n, Episode=Episode.n, Lines=Lines.clean)

write.table(Simpsons.corpus, sep="\t", row.names=F, col.names=T, "Whole_Simpsons_corpus.txt")
```
