# Decision-Making and Anxiety
Research project submitted for the Computational Psychiatry Conference (2024) during my time at the Yale Rutledge Lab. Visit https://ellenmartin11.github.io/anxiety_decision_making/ for all code and results. 

## Dataset Description
The dataset consists of over 5000 participants' worth of data collected remotely, via smartphone app called the Happiness Project. These results focus mainly on play 2 of data to avoid capturing the effects of participants learning how to play this complicated game. 

## Key Results
### Risk-Taking Behaviour
- Participants take more risks when in a loss vs gain domain.

### Happiness Model Parameters and Anxiety Groups
- People with higher anxiety scores (GAD 6+) have a greater EV parameter compared to people with lower anxiety scores (GAD < 6).
- The correlation between GAD and EV was significant (rho = 0.05, p = 0.002).
- The correlation between GAD and RPE was not significant (rho = 0.003, p = 0.841).
- When looking at Depression level, there were no significant difference between people with high depression scores (PHQ 7+) and people with low scores (PHQ < 7) in terms of RPE and EV weights.
- Non-symmetric happiness responses based on EV in gain and loss domain (people get unhappier quicker in the loss domain with increasing absolute EV compared to in the gain domain).

### Happiness and Risk-Taking Outcomes (Winning vs Losing)
- As expected, people were happier when they won risky gambles in the gain domain, compared to losing in the gain domain.
- The happiness model was better at predicting happiness when winning risky gambles in the gain domain, but underestimated people's unhappiness when losing in the gain domain.
- That is, people were less happy than the model predicted when they lost in the gain domain. 

- Again, as expected, in the loss domain, people were happier winning gambles vs losing.
- The happiness model was better at predicting happiness when losing gambles in the loss domain, vs winning gambles in the loss domain.
- That is, people were happier when they won in the loss domain than the model predicted they would be.

### Happiness and Risk-Taking Choices (Gamble vs Safe)
- People were happiest playing it safe in gain domains, and least happy playing it safe in loss domains.


### Future Information and Risk-Taking
- People were more likely to take a risk when the future was a gain domain vs loss domain.
- When including current trial information, people were most likely to take a risk when they were currently in a loss domain, and the future is positive.
- People were least likely to take a risk when they were currently in a gain domain, and the future is negative.
  
### Future Information and Happiness
- People were happier when the future island was a loss domain vs a gain domain.
- When including current trial information, people were happiest when currently in a gain domain, with a loss domain upcoming.
- People were least happy when currently in a loss domain, with a positive future upcoming.
- There may be an interaction with depression level, where people with PHQ 7+ were significantly more unhappy (p < 0.001) when currently in a loss domain, with a positive future upcoming.

### Gender Differences
- In the gain domain, women gamble more than men.
- In the loss domain, men gamble more than women.

