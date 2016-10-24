# resume-matching
### A package and app for resume to job matching (geared towards data science / technical jobs)<br> 
#### resmatch helps a job seeker find the perfect job

##### Description:
resmatch hooks into indeed.com's api and returns jobs requested by the user, capped at 25 jobs per request.
Once the api request has gone through, resmatch follows the url to grab text from the job posting. resmatch
then breaks down the text to simpler forms for comparison. After this process ( convert_href_to_text_indeed() ),
resmatch breaks down the users inputted resume to a comparable form and then calculates the cosine similarity score
between the resume and each job posting.

In addition to looking at newly pulled jobs, resmatch stores each indeed.com api pull in a postgres database. When a user searches a job in the app, a postgres database is queried in addition to the api pull and the new api pulled jobs are added to the database. This allows more jobs to be compared in a much shorter amount of time. The database will become a source for future research to see how the data science jobs are transforming over time.

resmatch is not intended for general use, but is the backbone of an R Shiny App. 
