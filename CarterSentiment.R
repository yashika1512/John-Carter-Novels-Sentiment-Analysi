#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

# Initial set up.
## This is the script used to download the Project Gutenberg text files
library(remotes)
install_version("gutenbergr", "0.2.1") 
#choose 3 for installing options
#would have to update few packages version seperately in further steps
library(gutenbergr)
install.packages("readr")
library(readr)

#to run the below make sure to have open network connection

twinmystery <- gutenberg_download(65783)$text
twinmystery <- twinmystery[14:length(twinmystery)]
stolenname <- gutenberg_download(64147)$text
hiddenfoes <- gutenberg_download(62860)$text
hiddenfoes <- iconv(hiddenfoes, "latin1", "UTF-8")

## First, read the plain text files from Project Gutenberg
## Skip lines at the beginning to remove Project Gutenberg header information
## Remove lines at the end to get rid of Project Gutenberg footer information
## A few of these files ended up with NA lines

twinmystery <- read_lines("https://www.gutenberg.org/files/65783/65783-0.txt", skip = 33)
twinmystery <- twinmystery[1:(length(twinmystery) - 370)]
twinmystery <- twinmystery[!is.na(twinmystery)]

stolenname <- read_lines("https://www.gutenberg.org/files/64147/64147-0.txt", skip = 30)
stolenname <- stolenname[1:(length(stolenname) - 366)]
stolenname <- stolenname[!is.na(stolenname)]

hiddenfoes <- read_lines("https://www.gutenberg.org/files/62860/62860-0.txt", skip = 29)
hiddenfoes <- hiddenfoes[1:(length(hiddenfoes) - 367)]
hiddenfoes <- hiddenfoes[!is.na(hiddenfoes)]

# Tidy data frame of nicholas carter's 3 completed, published novels
#'
#' Returns a tidy data frame of nicholas carter's 3 completed, published novels with
#' two columns: \code{text}, which contains the text of the novels divided into
#' elements of up to about 70 characters each, and \code{book}, which contains the titles of
#' the novels as a factor in order of publication.
#'
#' @details Users should be aware that there are some differences in usage
#' between the novels as made available by Project Gutenberg. For example,
#' "anything" vs. "any thing", "Mr" vs. "Mr.", and using underscores vs. all
#' caps to indicate italics/emphasis.
#'
#' @return A data frame with two columns: \code{text} and \code{book}
#'
#' @name carter_books
#'
#' @examples
#'
#' library(dplyr)
#'
#' carter_books() %>% group_by(book) %>%
#'      summarise(total_lines = n())
#'
#' @export

#create a function to named carter_books

carter_books <- function(){
  books <- list(
    "The Twin Mystery" = twinmystery,
    "A Stolen Name" = stolenname,
    "The Hidden Foes" = hiddenfoes
  )
  ret <- data.frame(text = unlist(books, use.names = FALSE),
                    stringsAsFactors = FALSE)
  ret$book <- factor(rep(names(books), sapply(books, length)))
  ret$book <- factor(ret$book, levels = unique(ret$book))
  structure(ret, class = c("tbl_df", "tbl", "data.frame"))
}

globalVariables(c("twinmystery", "stolenname", "hiddenfoes",
                  "book"))

#' The text of nicholas carter's novel "The Twin Mystery"
#'
#' A dataset containing the text of nicholas carter's novel "The
#' Twin Mystery". The UTF-8 plain text was sourced from Project Gutenberg
#' and is divided into elements of up to about 70 characters each.
#' (Some elements are blank.)
#'
#' @source \url{https://www.gutenberg.org/ebooks/65783}
#' @format A character vector with 12262 elements
"twinmystery"

#' The text of nicholas carter's novel "A Stolen Name"
#'
#' A dataset containing the text of nicholas carter's novel "A Stolen
#' Name". The UTF-8 plain text was sourced from Project Gutenberg
#' and is divided into elements of up to about 70 characters each.
#' (Some elements are blank.)
#'
#' @source \url{https://www.gutenberg.org/ebooks/64147}
#' @format A character vector with 12447 elements
"stolenname"

#' The text of nicholas carter's novel "Hidden foes"
#'
#' A dataset containing the text of nicholas carter's novel "Hidden
#' Foes". The UTF-8 plain text was sourced from Project Gutenberg
#' and is divided into elements of up to about 70 characters each.
#' (Some elements are blank.)
#'
#' @source \url{https://www.gutenberg.org/ebooks/62860}
#' @format A character vector with 14768 elements
"hiddenfoes" 

#We will now start from first scenario to test sentimental analysis

#Following packages need to installed 

#Note: This script will take a while to run. About 10-12 minutes on a system

install.packages('Rtools')
install.packages('tidyverse') # importing, cleaning, visualising 
install.packages('stringr')
install.packages('tidytext') # working with text
install.packages('tidyr') 
install.packages('ggplot2')
install.packages('reshape2')
install.packages('gridExtra') # extra plot options
install.packages('grid') # extra plot options
install.packages('keras') # deep learning with keras
install.packages('RColorBrewer')
install.packages('dplyr')
install.packages('wordcloud') # visualising text
install.packages("textdata")
#if the mentiond packages under tidyverse are not updated to latest version
#then tidyverse might fail to load
#so please update the said packages ggplot2, dplyr, readr, purrr, tibble, stringr, forcats.
library(tidyverse) #would have to update rlang and vctrs version if not already
library(stringr)
library(gridExtra)
library(dplyr)
library(grid) # extra plot options
library(keras) 
library(tidytext)
library(tidyr)
library(ggplot2) #would have to update vctrs version if not already
library(reshape2)
library(RColorBrewer)
library(wordcloud)

##### III. Results
#a. Word Frequencies 

#the total number of words per novel 

carter_words <- carter_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

total_words <- carter_words %>%
  group_by(book) %>%
  summarize(total = sum(n))
total_words

# “n” is the total count of each word 

carter_words <- left_join(carter_words, total_words)
carter_words

#plot the repeating words
ggplot(carter_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE, bins = 30) +
  xlim(NA, 0.0009) +
  scale_fill_brewer() +
  theme_light() +
  ggtitle("Figure 1: Word Count across Total Words of each Jane Austen Novel") +
  facet_wrap(~book, ncol = 2)

#To find more useful word frequencies. Usully the words that are used most are no significance to the story context
#So we will look for words that are not used much often by finding tf-idf values

book_tf_idf <- carter_words %>%
  bind_tf_idf(word, book, n)
book_tf_idf

#tf = n/total
#idf = ln(1/tf)
#tf-idf = tf * idf

#Now of we arrange the dataset in descending order, we will find words that are used not much often

book_tf_idf %>%
  arrange(desc(tf_idf))

#Lets plot this observation to get much better insight

book_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_y") +
  labs(x = "tf-idf", y = NULL) +
  scale_fill_brewer() +
  theme_light() +
  ggtitle("Figure 2. Top 15 TF-IDF Values for each Carter Books")


##### III. Results
#b. Sentimental Analysis

#We will first try extracting some sentences to check if the books formatting is valid

#extracting random sentence from books

carter_sentences <- carter_books() %>% group_by(book) %>% unnest_tokens(sentence, text, token = "sentences") %>% ungroup()
carter_sentences$sentence[30]
    
#get sentiments
sentiments 
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

#Read chapter numbers and divide the whole string into single elements of words based on grouping of books

tidy_data <- carter_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

tidy_data

#Let's start by checking if sentiments works fine.
#We will look for most joy words in the book twin mystery
#get sentiment ncr - joy
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

#let’s filter() the data frame with the text from the books for the words from twin mystery and then count the most joy words in the book
tidy_data %>%
  filter(book == "The Twin Mystery") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

#find a sentiment score for each index using the Bing lexicon and inner_join().
carter_sentiment <- tidy_data %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

#count up how many positive and negative words there are in defined sections of each book and plot in a graph
ggplot(carter_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")
#We see how the plot of each novel changes toward more positive or negative sentiment over the trajectory of the story.

#To understand the better purpose of all three sentiment lexicons, 
#let's compare the three and examine how the sentiment changes across the 
#narrative arc of stolen name

#use filter() to choose only the words from the one novel we are interested in
astolenname <- tidy_data %>% 
  filter(book == "A Stolen Name")

#use inner_join() to calculate the sentiment in different ways.
afinn <- astolenname %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

#use integer division (%/%) to define larger sections of text that span multiple lines, and we can use the same pattern with count(), pivot_wider(), and mutate() to find the net sentiment in each of these sections of text.
bing_and_nrc <- bind_rows(
  astolenname %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  astolenname %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

#bind the net sentiment and visualize them
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

#We see similar dips and peaks in sentiment at about the same places in the 
#novel, but the absolute values are slightly different. The NRC sentiment is 
#high, the Bing et al. sentiment has more variance, the Affin sentiment appears 
#to find longer stretches of similar text, but all three agree roughly on the
#overall trends in the sentiment through a narrative arc.

#From this it look clear that Bing et al. has more systematic sentiments. 
#This analysis will help us in choosing a correct lexicon sentiment.

#find no of rows in carter book's 3 completed novels
carter_books() %>% group_by(book) %>%
  summarise(total_lines = n())

#save positive sentiments and negavtive sentiments

positive_senti <- get_sentiments("bing") %>%
  filter(sentiment == "positive")

negative_senti <- get_sentiments("bing")%>%
  filter(sentiment == "negative")

#count the positive sentiments in the book by words count in book twinmystery

tidy_data %>%
  filter(book == "The Twin Mystery") %>%
  semi_join(positive_senti) %>%
  count(word, sort = TRUE)

#We will look same for negative sentiment and then compare the both sentiments usage

#count the negative sentiments in the book by words count in book twinmystery

tidy_data %>%
  filter(book == "The Twin Mystery") %>%
  semi_join(negative_senti) %>%
  count(word, sort = TRUE)

#Twinmystery sentiment difference graph

#seperate data into positive and negative elements and then difference of 
#positive and negative elements

bing <- get_sentiments("bing")
twinmystery_sentiment <- tidy_data %>%
  inner_join(bing) %>%
  count(book = "The Twin Mystery" , index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
twinmystery_sentiment

# To see the data, we plot a bar graph of the difference
# we see starting chapter starts with more on negative sentiments
#otherwise rest chapters are balanced on sentiments
#plot
ggplot(twinmystery_sentiment, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

#Stolen name sentiment difference graph

#seperate data into positive and negative elements and then difference of positive and negative elements
bing <- get_sentiments("bing")
stolenname_sentiment <- tidy_data %>%
  inner_join(bing) %>%
  count(book = "A Stolen Name" , index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
stolenname_sentiment

# To see the data, we plot a bar graph of the difference
# we see starting chapter starts with more on negative sentiments
#otherwise rest chapters are balanced on sentiments
#plot
ggplot(stolenname_sentiment, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

#We can see the sentiments are equally divided among both books, 
#which means despite the different in negative and positive sentiments usage for both books.
#Nicholas carter is able to manage an same balance of sentiments trajectory across all his books
#Which is a trace of good writing style

##### III. Results
#c. Word Cloud

#We are trying a new plot style to have more clear idea of the common
#words usage by carter in his books

#wordcloud for max pos and neg words
tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "dark green"),
                   max.words = 100)

#Now we will try another scenario fo same sentimental analysis approach

#most common words
counting_words <- tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)
head(counting_words)


#plot1
counting_words %>%
  filter(n > 50) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment))+
  geom_col() +
  coord_flip() +
  labs(y = "Sentiment Score")

#Now let's try a different approach to sentimental analysis for the carterbooks
#By removing stop words and sorting books chapters wise

#for this we would need two packages
#installing packages below
install.packages("widyr")
install.packages("viridis")
library(viridis)
library(widyr)

#sort books by chapter wise
sorted_books <- carter_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

#show sorted books
sorted_books

#tidy the sorted books
tidy_data <- sorted_books %>% unnest_tokens(word,text)
tidy_data

#remove stop words from the books
data("stop_words")
tidy_data <- tidy_data %>% anti_join(stop_words)

#count the times words repeating in the books
tidy_data %>% count(word, sort = TRUE)

#getting sentiments
bing <- get_sentiments("bing")
bing

#setting sentiment score in carter books by chapter index wise
cartersentiment <- tidy_data %>%
  inner_join(bing) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
cartersentiment


#plotting sentiments on basis of sentiment score
cartersentiment %>% ggplot(aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x") +
  labs(title = "Sentiments in carter books", y = "sentiments") +
  theme_minimal(base_size = 13) +
  scale_fill_viridis(end = 0.75, discrete = TRUE, direction = -1)+
  scale_x_discrete(expand = c(0.02,0))+
  theme(strip.text = element_text(hjust = 0))+
  theme(strip.text = element_text(face = "italic"))+
  theme(axis.title.x =  element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.x = element_blank())

#Based on the plot graph trajectory, every book has distinct sentiment score range. 
#Hidden Foes is bend more toward negative score with little positive difference.
#Twin Mystery and Stolen Name has a equal balance of sentiment words both negative and positive.
#Negative sentiments also include thrilling or scary emotions or mystery elements. 
#This tells Carter is good at keeping his readers on edge with thrilled mystery books. 
#Giving some positive elements too to keep readers engaged.


##### III. Results
#d. Naive Prediction

#The below script borrows heavily from the fantastic book ‘Deep Learning with R’ by Francois Chollet and J.J. Allaire

#We will be using some test train data, deep learning approach to try our last approach to sentimental analysis

#for this we would need grid base packages
library(grid)

#sort books
sorted_books <- carter_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

#Let’s have a look at some high level comparisons between the train and test set. 
#We’ll use tidytext to split phrases into n-grams and see what we find both with (left) 
#and without (right) stop words.

#Prepare test and train data
traindata <- sorted_books %>% filter(book == "The Hidden Foes")
testdata <- sorted_books %>% filter(book == "The Twin Mystery")

#Combine

train = traindata %>% mutate(Split = "train")
test = testdata %>% mutate(Split = "test")
train
test
full = data.frame(rbind(train %>% select(-book), test %>% select( -book)))
head(full)

# Top words ---------------------------------------------------------------

# Have a look at the most common words (having removed stop words)

#top_words_train
top_words_train = full %>% 
  filter(Split == "train") %>% 
  unnest_tokens(output = word, input = text) %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))
top_words_train

#top_words_test
top_words_test = full %>% 
  filter(Split == "test") %>% 
  unnest_tokens(output = word, input = text) %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))
top_words_test

# Plot the top 10 words for train/test with and without stop words

grobs = list(
  tableGrob(head(top_words_train,10), theme = ttheme_minimal()),
  tableGrob(head(top_words_train %>% anti_join(stop_words),10), theme = ttheme_minimal()),
  tableGrob(head(top_words_test,10), theme = ttheme_minimal()),
  tableGrob(head(top_words_test %>% anti_join(stop_words),10), theme = ttheme_minimal())
)

lg <- tableGrob(c("", "Train", "Test"), theme= ttheme_minimal())
rg <- arrangeGrob(grobs = grobs, ncol=2,
                  top = textGrob("Top 10 Words",gp=gpar(fontsize=18)))
grid.newpage()
grid.draw(cbind(lg, rg, size = "last"))


#Let’s make some changes and remove some of the weird “lrb” words.
# Adjustments -----------------------------------------------------

# There are a few odd things in the data based on the above that I want to make adjustments for


full = full %>% mutate(
  text = gsub(" n't"," not", tolower(text)), 
  text = gsub("he 's","he is", tolower(text)), 
  text = gsub("she 's","she is", tolower(text)), 
  text = gsub("what 's","what is", tolower(text)), 
  text = gsub("that 's","that is", tolower(text)), 
  text = gsub("there 's","there is", tolower(text)), 
  text = gsub("-lrb-"," ", tolower(text)),
  text = gsub("-rrb-"," ", tolower(text)),
  # Going to remove all instances of "'s" that remain (nearly always possession)
  # This way we retain the immediate connection between the possession and possessor in our sequence
  # Otherwise we will end up padding it with zeros and lose some information
  
  text = gsub(" 's "," ", tolower(text))
)


# Visualise -----------------------------------------------------

# Create some summary statistics

chapter_summaries = full %>% 
  unnest_tokens(output = word, input = text) %>% 
  group_by(chapter) %>% 
  summarise(
    words_per_chapter = n_distinct(word),
    lines_per_chapter = n_distinct(linenumber)
  )
chapter_summaries

lines_summaries = full %>% 
  unnest_tokens(output = word, input = text) %>% 
  group_by(linenumber) %>% 
  summarise(
    words_per_line = n_distinct(word)
  )
lines_summaries

full_summaries = full %>% 
  left_join(chapter_summaries, c("chapter" = "chapter")) %>% 
  left_join(lines_summaries, c("linenumber" = "linenumber")) %>% 
  mutate(
    characters_per_line = nchar(text)
  )
full_summaries


a = ggplot(full_summaries, aes(words_per_line,fill = Split)) +
  geom_density(position = "stack")

b = ggplot(full_summaries, aes(characters_per_line,fill = Split)) +
  geom_density(position = "stack")
grid.arrange(ncol = 2, nrow = 2, a, b)

#Distributions look sufficiently similar with respect to these features,
#so I’m happy to say that our training set is representative.

##### III. Conclusion
#a. Cross Validation with confusion Matrix

library(RTextTools)
library(tm)
library(textutils)

# Initial set up.

prepdata <- tidy_data %>%
  inner_join(bing, by = "word") %>% 
  mutate(sentiment = ifelse(is.na(sentiment), "neutral", sentiment)) %>%
  group_by(book) %>%
  ungroup()

#We will mutate the data by giving 0-1 score to sentiment type
carter_data <- prepdata[,!names(prepdata) %in% c("linenumber", "chapter")]
carter_data <- carter_data %>% mutate(score = ifelse(sentiment == "positive",1, 0))%>%
  group_by(book) %>%
  ungroup()

# Set seed
set.seed(1985) 

#Shuffle the data
carter_data <- carter_data[sample(nrow(carter_data)),]

set.seed(1985) 
carter_data <- carter_data[sample(nrow(carter_data)),]


#data pre-processing.
mat <- create_matrix(carter_data$word, language="english", 
                     removeStopwords=TRUE, removeNumbers=TRUE, 
                     stemWords=TRUE, weightTfIdf)


container <- create_container(mat, carter_data$score,
                              trainSize=1:2000,virgin=FALSE)
cvres <- cross_validate(container, nfold=10, algorithm="SVM", seed=1985)

trainids <- seq(1, floor(nrow(carter_data)*0.8))
testids <- seq(floor(nrow(carter_data)*0.8)+1, nrow(carter_data))

container <- create_container(mat, carter_data$score,
                              trainSize=trainids,virgin=FALSE)

models <- train_models(container, algorithms="SVM")

texts <- c("sad", "happy")
newmatrix <- create_matrix(texts, language="english", 
                           removeStopwords=TRUE, removeNumbers=TRUE, 
                           stemWords=TRUE, weightTfIdf, originalMatrix = mat)
predict(models[[1]], newmatrix)

texts <- carter_data$word[testids]
trueclass <- carter_data$score[testids]
testmatrix <- create_matrix(texts, language="english", 
                            removeStopwords=TRUE, removeNumbers=TRUE, 
                            stemWords=TRUE, weightTfIdf, originalMatrix = mat)
results = predict(models[[1]], testmatrix)
table(trueclass, results)
sum(trueclass==results)/length(results)
sum(trueclass==results & results==1)/sum(results==1)
sum(trueclass==results & trueclass==1)/sum(trueclass==1)

#As you see, the precision is high but the recall is low. 
#The model has learned well from the examples in its training data


