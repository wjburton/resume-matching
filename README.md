# resume-matching
### Package for resume to job matching <br> 
#### resmatch helps a job seeker find the perfect job

##### Description:
resmatch hooks into indeed.com's api and returns jobs requested by the user, capped at 25 jobs per request.
Once the api request has gone through, resmatch follows the url to grab text from the job posting. resmatch
then breaks down the text to simpler forms for comparison. After this process ( convert_href_to_text_indeed() ),
resmatch breaks down the users inputted resume to a comparable form and then calculates the cosine similarity score
between the resume and each job posting.

resmatch is not intended for general use, but is the backbone of an R Shiny App. 
The shiny app can be found here:
