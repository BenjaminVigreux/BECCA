##  Code chunks for data loading

```{r Load Datasets, include = FALSE, echo = FALSE}
knitr::opts_knit$set(root.dir = normalizePath('./'))
# print(opts_knit$get("root.dir"))

##  Load All Datasets

wd <- getwd()
dataset <- c("kyrgyzstan", "moldova", "unicef", "serbia", "tajikistan", "yemen")
# print(dataset)

for (d in 1:length(dataset)){
  load(paste0(wd,"/clean_data/",dataset[d],"_clean.RData"))
  dataset <- c("kyrgyzstan", "moldova", "unicef", "serbia", "tajikistan", "yemen")
  assign(paste0(dataset[d]),clean)
}

##  Choose Dataset

dataset <- "moldova" ## Pick one!
load(paste0(wd,"/clean_data/",dataset,"_clean.RData"))
load(paste0(wd,"/clean_data/texts/",dataset,"_translated_texts.RData"))
load(paste0(wd,"/clean_data/subsets/",dataset,"_subsets.RData"))
save("dataset", file = "dataset.txt")

##  Attach to search dir

search <- search()
if (!is.na(match("clean", search))) {detach(clean)}
if (!is.na(match("data", search))) {detach(data)}
search <- search()
if (is.na(match("clean", search))) {attach(clean)}
# knitr::opts_knit$set(root.dir = normalizePath('../'))
# print(opts_knit$get("root.dir"))
```

```{r Load All Corpuses and Texts}
## Load all Corpuses
load(paste0(wd,"/clean_data/corpus/moldova_corpus.RData"))
moldova_corpus <- corpus
load(paste0(wd,"/clean_data/corpus/kyrgyzstan_corpus.RData"))
kyrgyzstan_corpus <- corpus
load(paste0(wd,"/clean_data/corpus/serbia_corpus.RData"))
serbia_corpus <- corpus
load(paste0(wd,"/clean_data/corpus/tajikistan_corpus.RData"))
tajikistan_corpus <- corpus
load(paste0(wd,"/clean_data/corpus/yemen_corpus.RData"))
yemen_corpus <- corpus
load(paste0(wd,"/clean_data/corpus/unicef_corpus.RData"))
unicef_corpus <- corpus

# names(summary(yemen_corpus))
# names(summary(tajikistan_corpus))
load(paste0(wd,"/clean_data/texts/moldova_translated_texts.RData"))
moldova_texts_eng <- texts_eng
moldova_texts_org <- texts_org
moldova_titles_eng <- titles_eng
moldova_titles_org <- titles_org

load(paste0(wd,"/clean_data/texts/kyrgyzstan_translated_texts.RData"))
kyrgyzstan_texts_eng <- texts_eng
kyrgyzstan_texts_org <- texts_org
kyrgyzstan_titles_eng <- titles_eng
kyrgyzstan_titles_org <- titles_org

load(paste0(wd,"/clean_data/texts/serbia_translated_texts.RData"))
serbia_texts_eng <- texts_eng
serbia_texts_org <- texts_org
serbia_titles_eng <- titles_eng
serbia_titles_org <- titles_org

load(paste0(wd,"/clean_data/texts/tajikistan_translated_texts.RData"))
tajikistan_texts_eng <- texts_eng
tajikistan_texts_org <- texts_org
tajikistan_titles_eng <- titles_eng
tajikistan_titles_org <- titles_org

load(paste0(wd,"/clean_data/texts/yemen_translated_texts.RData"))
yemen_texts_eng <- texts_eng
yemen_texts_org <- texts_org
yemen_titles_eng <- titles_eng
yemen_titles_org <- titles_org

load(paste0(wd,"/clean_data/texts/unicef_translated_texts.RData"))
unicef_texts_eng <- texts_eng
unicef_texts_org <- texts_org
unicef_titles_eng <- titles_eng
unicef_titles_org <- titles_org
```