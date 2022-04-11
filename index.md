As the number of people seeking home loans in America has shot up in recent years during a period of historic demand, an analysis of data from the Consumer Financial Protection Bureau shows even as the likelihood of denial has gone down, certain factors leave the door to the housing market more open to some than others.

Our data analysis — using information that only became available in 2018 — suggests non-white borrowers are at a disadvantage when seeking a home loan. Even when controlling for income and debt-to-income ratio, non-Hispanic Black and Latinx applicants are consistently denied a mortgage at higher rates than whites. 

For example, in South Carolina, white mortgage applicants who make between $100,000 and $150,000 in income and have a 20%-29% debt-to-income ratio were denied a loan just 5% of the time between 2018 and 2020. But Black applicants with the same financial characteristics failed to secure a loan 21% of the time.
The trend is true even for applicants with relatively high income.

The same takeaway can be found in Florida.

<iframe src="graphics/p1.html" width = "900" height = "500"></iframe>

Where:
- <mark style="background-color: #1f77b4"> Medium debt, income and loan: Debt-to-income ratio = 30%-39% & income = $100k to $150k & loan size = $200k to $250k </mark>
- <mark style="background-color: #ff7f0e">Higher debt, lower income, medium loan: Debt-to-income ratio = 40%-49% & income = $50k to $100k & loan size = $200k to $250k </mark>
- <mark style="background-color: #2ca02c">Higher debt, medium income, larger loan: Debt-to-income ratio = 40%-49% & income = $100k to $150k & loan size = $350k to $400k </mark>
- <mark style="background-color: #d62728">Higher debt, lower income, smaller loan: Debt-to-income ratio = 40%-49% & income = $50k to $100k & loan size = $150k to $200k </mark>

Drilling down to individual lenders, it's clear each of the top five mortgage underwriters deny Black applicants at higher rates than whites, begging the question of what other data points lenders may be relying on when making decisions, and to what extent human biases could be in play.

<iframe src="graphics/lenderplot.html" width = "700" height = "350"></iframe>
(Where applicants' debt-to-income ratio = 40%-49% and income = $100k to $150k)

Some critical information likely to influence a lender's decision, including the applicant's credit score and the size of the down payment they planned to make, is missing. However, we were able to predict whether a loan would fail or not with above 90% accuracy using the variables available in the data using a random forest model. 

<h3><a href="https://mkwildeman.shinyapps.io/mortgagebias/">Explore the Shiny app I built here to see how these rates differ for yourself.</a></h3>

And stay tuned, as the full data from 2021 is expected to be released in June.
