As the number of people seeking home loans in America has shot up in recent years during a period of historic demand, an analysis of data from the Consumer Financial Protection Bureau shows even as the likelihood of denial has gone down, certain factors leave the door to the housing market more open to some than others.

<h3><a href="https://mkwildeman.shinyapps.io/mortgagebias/">Explore the Shiny app I built here to see how these rates differ for yourself.</a></h3>

Our data analysis — using information that only became available in 2018 — suggests non-white borrowers are at a disadvantage when seeking a home loan. Even when controlling for income and debt-to-income ratio, non-Hispanic Black and Latinx applicants are consistently denied a mortgage at higher rates than whites. 

For example, in South Carolina, white mortgage applicants who make between $100,000 and $150,000 in income and have a 20%-29% debt-to-income ratio were denied a loan just 5% of the time between 2018 and 2020. But Black applicants with the same financial characteristics failed to secure a loan 21% of the time.
The trend is true even for applicants with relatively high income.

The same takeaway can be found in Florida.

<iframe title="Even in the same income, debt and loan brackets, white applicants win a loan more often in FL" aria-label="Grouped Bars" id="datawrapper-chart-1PqYq" src="https://datawrapper.dwcdn.net/1PqYq/1/" scrolling="no" frameborder="0" style="width: 0; min-width: 100% !important; border: none;" height="676"></iframe><script type="text/javascript">!function(){"use strict";window.addEventListener("message",(function(e){if(void 0!==e.data["datawrapper-height"]){var t=document.querySelectorAll("iframe");for(var a in e.data["datawrapper-height"])for(var r=0;r<t.length;r++){if(t[r].contentWindow===e.source)t[r].style.height=e.data["datawrapper-height"][a]+"px"}}}))}();
</script>
<hr>

Drilling down to individual lenders, it's clear each of the top five mortgage underwriters deny Black applicants at higher rates than whites, begging the question of what other data points lenders may be relying on when making decisions, and to what extent human biases could be in play.

<iframe title="Major lenders deny mortgage applicants at different rates, even when they have similar debt and income levels" aria-label="Grouped Bars" id="datawrapper-chart-WJLx9" src="https://datawrapper.dwcdn.net/WJLx9/1/" scrolling="no" frameborder="0" style="width: 0; min-width: 100% !important; border: none;" height="645"></iframe><script type="text/javascript">!function(){"use strict";window.addEventListener("message",(function(e){if(void 0!==e.data["datawrapper-height"]){var t=document.querySelectorAll("iframe");for(var a in e.data["datawrapper-height"])for(var r=0;r<t.length;r++){if(t[r].contentWindow===e.source)t[r].style.height=e.data["datawrapper-height"][a]+"px"}}}))}();
</script>

Some critical information likely to influence a lender's decision, including the applicant's credit score and the size of the down payment they planned to make, is missing. However, we were able to predict whether a loan would fail or not with above 90% accuracy using the variables available in the data using a random forest model. 

And stay tuned, as the full data from 2021 is expected to be released in June.
