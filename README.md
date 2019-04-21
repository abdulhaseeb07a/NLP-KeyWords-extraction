# NLP-KeyWords-extraction
Extract the Negative keywords from User comments

Journey start from some cleaning the data and remove the meaning less words and remove stop words for my custome vector, that is extendable according to business logic

Using 'udpipe' Library for tokenize the User comments the best this for this library is it will nto tokenize your sentence but also give you the lemma , part of speech and lot more

Then use the 'GradyAugmented' dictionary to identify the extracted key word is a actul word in dictonary or not remove all words which is not the dictionary words

and also used 'pluralize' github library for check the words sigularity.
