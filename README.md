# STA 523 :: Exam 2

## Introduction

[News API](https://newsapi.org/) gives you access to breaking news headlines
and historical articles from over 30,000 news sources. The purpose of this exam
is to use their API to create an R Shiny news and sentiment analysis dashboard.
A free account with News API will grant you 500 requests per day and allow you
to query articles up to one month old. Breaking news headlines do have a 60
minute time delay.

[SENTIM-API](https://sentim-api.herokuapp.com) is a free API for sentiment
analysis. To use it, follow the instructions and submit a POST request. An
example of doing this with R code is provided further below in Task 0.

## Tasks

You may use any R package.

#### Task 0

1. After you create a free account and receive an API key, read the News API
   [documentation](https://newsapi.org/docs). Focus on the endpoints
   "Top headlines" and "Sources" as these will be the two you will work with in
   your app. Try a few calls to make sure everything works as expected.

2. The below R code will allow you to make POST requests to the SENTIM-API.
   Understand the code and its output. All you have to change is the text
   in the body of the POST request to have the sentiment analysis conducted
   on new text.

   ```r
   library(httr)

   # make POST request and save response
   r <- POST(
     url    = "https://sentim-api.herokuapp.com/api/v1/",
     config = add_headers("Accept"       = "application/json",
                          "Content-Type" = "application/json"),
     body   = list(text = "Let's evaluate this text. Is this positive? Probably negative!"),
     encode = "json")

   # extract content from POST response
   content(r, "text", encoding = "UTF-8")
   ```

#### Task 1

Create three functions in `news/api_wrappers.R`. These will ultimately be loaded
into `news/app.R` with `source(file = "api_wrappers.R")`. The three functions
should be defined and operate as described below.

1. `get_top_headlines()`: should serve as a wrapper to the "Top headlines"
   endpoint for News API. The function should contain arguments equivalent to
   all request parameters specified in the documentation. Furthermore, checks
   should be implemented in your function so requests outside the parameters'
   scope are not made. For example, a `pageSize` request of 400 should not be
   allowed since 100 is the maximum. In all requests have `country=us` or
   `country=""`. Your function does not need to account for all the country
   codes, only U.S. and worldwide.

2. `get_sources()`: should serve as a wrapper to the "Sources" endpoint for News
   API. The function should contain arguments equivalent to all request
   parameters specified in the documentation except for the language request
   parameter. Write your function so is makes every API call with `language=en`
   in the query portion of the URL. Furthermore, checks should be
   implemented in your function so requests outside the parameters' scope
   are not made. In all requests have `country=us` or `country=""`. Your
   function does not need to account for all the country codes, only U.S. and
   worldwide.

3. `get_sentim(x)`: should serve as a wrapper to the R code above that allows
   you to make POST requests to the SENTIM-API. The single argument `x` should
   take an atomic character vector of length 1. This will be the text you want
   analyzed.

All three functions should output the response of the API request in a workable
format that is either a tibble or list. You do not need to load any packages
in `news/api_wrappers.R`. The necessary packages for the functions to run can
be loaded in `news/app.R`. Of course, if you want to test your function as you
develop it, you will need to have the necessary packages loaded in your
R environment.

#### Task 2

Create a Shiny app or dashboard that serves as a central news hub in
`news/app.R`. You may want to work with the
[Navigation Bar Page](https://shiny.rstudio.com/gallery/navbar-example.html)
layout or [Shiny Dashboard](https://rstudio.github.io/shinydashboard/).
Both options will neatly structure your UI; however, you are not limited in
working with only these two.

Required app features:

1. The app must display news articles and results from a sentiment analysis
   about the articles' title or description.

2. Your app must incorporate all three functions you created in Task 1.

3. The app should allow the user to enter and use their own API key.

4. The user should be able to specify **at least four**
   (not including the API key widget) query parameters through
   input widgets that will ultimately get passed to `get_top_headlines()` or
   `get_sources()`.

5. You should make use of an action button(s) (or another technique) that only
   fetches the News API data when the button is clicked.

6. The app should be well organized and aesthetically pleasing. You may choose
   which information from the query to present to the user. This can include the
   article's title, link, author, image, etc. Consider incorporating a
   [modal dialog box](https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html).

The rest of the app's design and functionality is at your discretion - the
number of articles you want to display, an instruction page, an API call
counter, other user feedback. Feel free to add colors, alerts, a theme, or other
stylistic features. A portion of your grade (see below) is allocated towards
your creativity and the app's user friendliness (intuitive layout, no glitches
or uninformative error messages, etc.).

#### Task 3

Publish your app on https://www.shinyapps.io/.

#### Task 4

Modify this README.

1. Change the title from `STA 523 :: Exam 2` to something
   more meaningful as it relates to your app.

2. Remove all other text but a References section
   (if you used outside resources).

3. Add a link to your app that you published in Task 3.

4. Add any other relevant information to this README that someone looking at
   your repo may be interested in knowing. This can be text or images as they
   relate to your app.

## Essential details

### Deadline and submission

**The deadline to submit Exam 2 is Monday, Oct 26 at 11:59pm EST** Only the
code in your master branch will be graded.

### Rules

- This is an individual assignment.

- Everything in your repository is for your eyes only except for the
  instructor and TAs.

- You may not communicate anything about this exam to anyone. This includes
  posting or interacting in any online forums.

- You may read/reference any online, book, and note resources. As always, you
  must cite any code you use as inspiration that is beyond what we introduced
  thus far in the course. Add a References section to this `README.md` file.

- For any questions, send a direct message on Slack (or email) to the
  instructor. Questions should only be about understanding the data or the
  exam's instructions.

### Academic integrity

  To uphold the Duke Community Standard:

  - I will not lie, cheat, or steal in my academic endeavors;
  - I will conduct myself honorably in all my endeavors; and
  - I will act if the Standard is compromised.

  Duke University is a community dedicated to scholarship, leadership, and
  service and to the principles of honesty, fairness, respect, and accountability.
  Citizens of this community commit to reflect upon and uphold these principles in
  all academic and non-academic endeavors, and to protect and promote a culture of
  integrity. Cheating on exams and quizzes, plagiarism on homework assignments and
  projects, lying about an illness or absence and other forms of academic
  dishonesty are a breach of trust with classmates and faculty, violate the Duke
  Community Standard, and will not be tolerated. Such incidences will result in a
  0 grade for all parties involved as well as being reported to the University
  Judicial Board. Additionally, there may be penalties to your final class grade.
  Please review Duke’s Standards of Conduct.

### Grading

| Topic                               | Points |
|-------------------------------------|:------:|
| Task 1:                             |   20   |
|               `get_top_headlines()` |    8   |
|                     `get_sources()` |    8   |
|                      `get_sentim()` |    4   |
|                                     |        |
| Task 2:                             |   35   |
|      Display articles and sentiment |    4   |
|   Use all three functions correctly |    3   |
|        Working API Key input widget |    2   |
| At least four other working widgets |    8   |
| Action button or delayed data fetch |    4   |
|           Organization / appearance |    3   |
|             Creativity / complexity |    3   |
|                       User-friendly |    4   |
|       Neat and organized code style |    4   |
|                                     |        |
| Task 3:                             |    3   |
| Task 4:                             |    2   |
| Total                               |   60   |

*A portion of the points will be allocated to efficiency and code style in
Task 1.*
