# Exploring the Topics of Banned Books

![BANNED_BOOKS](https://github.com/RodNSS/banned_book_exploration/assets/92232804/004d293c-aaed-4dc1-99d7-e5a2120a0b12)
Just show me the [app] already!

[app]: https://roderick.shinyapps.io/Banned_Book_Directory/

# Summary
In recent years, there has been a strong uptick in the number of books being banned in schools and prisons across the United States. While the reasons for book banning in schools largely seem ideologically motivated, the reasons for book banning in prisons sometimes seem arbitrary or unclear in comparison. This project attempts to analyze banned book data to uncover patterns, trends and identify topics that lead to censorship in each state. 

# Motivation
I chose this project due to the prevalence of book banning in today's society. Tennessee ranks in the top 5 by amount of books banned in schools (mostly coming from Collierville). While being a very interesting topic in and of itself, the data will be excellent for various visualizations, geographical mapping and provide an opportunity to combine machine learning with natural language processing (topic modeling). Also, there has not been much analysis on the prison dataset yet which peaked my interest. I did not know what to expect.

# Issues and Challenges
This project utilizes the Google Books API to grab book descriptions. Sometimes the description may not match the book title. I've tried to ensure most are correct, especially for the school dataset, but it is very difficult to check thousands of descriptions. For the prison dataset, I was only able to grab about 16k descriptions out of a little over 55k publications. A lot of the banned publications in prison are adult magazines which do not require topic modeling (due to the title being descriptive enough all by itself!)🙈

This project uses [BERTopic] for topic modeling. I've chosen to leave the topics in their raw output form as given by the model so the topics given may not be as accurate as a human labeling them all one by one. A lot of books consist of several topics so when you see "love_school_life_friends" as the top topic banned in schools and think "Well that doesn't sound like a topic that would be banned!" it's  because the model is skewed towards term frequency and may not pick up on subtle sub themes from the book description. With that said, I think the model still gives an overall generally good representation of the content by comparing the results with [PEN America's findings]. I ran several models and finally settled on the ones I thought gave the most accurate and coherent descriptions. While the topics banned in schools were somewhat predictable, some of the prison topics were quite surprising!

Anyway, here is a link to the interactive [Banned Book Directory]. 

[Banned Book Directory]: https://roderick.shinyapps.io/Banned_Book_Directory/

[PEN America's findings]: https://pen.org/report/banned-usa-growing-movement-to-censor-books-in-schools/
[BERTopic]: https://github.com/MaartenGr/BERTopic
# Data Sources
School Book Bans: 

https://pen.org/banned-book-list-2021-2022/

https://pen.org/index-of-school-book-bans-2022/

Prison Book Bans: 

https://observablehq.com/@themarshallproject/prison-banned-books?collection=@themarshallproject/data-releases
