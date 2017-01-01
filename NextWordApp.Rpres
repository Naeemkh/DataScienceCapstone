Smart prediction application for easier typing
========================================================
author: Naeem Khoshnevis
date: December 22, 2016
autosize: true
font-family: 'Helvetica'

Application Overview
========================================================

Typing on touchscreen could be slow and frustrating. I developed an application to predict the next probable word and save considerable amount of time for the user.    
Here are the main features of the application. 

- Predicting the next word based on the previous sequence of words
- Reporting the probability of the most probable words
- Creating a word cloud for the most probable words
- Simple and effective application design
- Fast and accurate results



Work Flow
========================================================

Prediction of the next word is mainly based on the frequency of the observed words for each sequence of words in the corpus. Two main steps in developing the application are:

**Data Generation**  
Word-frequency data (ngrams) is the essential part of the application. I used corpus of formal and informal contemporary American English (including news, blogs, and twitter) to generate the ngrams. Here are the steps for generating the ngrams:
 
 * Remove numbers, punctuation, urls, and profanity words.
 * Translate characters from upper case to lower case.
 * Generate 1-5 grams word-frequency table as an input to the prediction model.

**Prediction Model**  
Prediction model uses the user input and ngrams to predict the most probable words.

Prediction Model
========================================================

The prediction model is based on Katz back-off algorithm. The application first receive the input from the user and put it in an appropriate format, then feed the preprocessed data into the prediction function, and finally get the results and report it to the user. The model assign  probability to seen and unseen words. The word prediction steps are discussed below:

* Count the number of words at the sequence (n). If it is more than 4, choose the last 4 words.
* Look up the whole seen words that complete the input sequence at n-grams.
* Compute the word probability and total discount value.
* Remove the first word and redo the mentioned steps for (n-1)grams.
* Repeat these steps up to unigrams.
* Sort the words based on probability of occurrence.
* Report the top m words.


The Application 
========================================================

The application is accessible at the following link:   
https://naeem.shinyapps.io/shinyapp-NLP

![](app_environment.png)

Data and source code:   
https://github.com/Naeemkh/DataScienceCapstone
