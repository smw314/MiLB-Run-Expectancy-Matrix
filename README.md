# MiLB-Run-Expectancy-Matrix

In this project, I converted Minor League play-by-play data into a run-expectancy matrix to determine the run values used in weighted on-base average (wOBA), weighted runs above average (wRAA) and weighted runs created plus (wRC+).

I came into this summer with little experience coding in R but was determined to hone my skills in the language by completing an independent, hands-on baseball analytics project. At the same time, I interned in the front office of the Tacoma Rainiers, the Seattle Mariners’ Triple-A affiliate. Every day I combed through players’ pages on MiLB.com, looking for noteworthy metrics or trends in game-logs to include in the game notes and stat packs that I distributed to coaches and media members. Unfortunately, I was underwhelmed by the resources at my disposal. At the Major League level, there exists a seemingly infinite amount of data readily accessible for public consumption. On the other hand, advanced statistics at the minor-league level are scarce. This makes it far harder for analysts to track player development, estimate a prospect’s trade value, project Major League performance and more.

Motivated by the lack of information, I decided to create and publish MiLB wOBA, wRAA and wRC+ statistics, along with a run-expectancy (RE24) matrix. Please note: this was a personal project and is not Tacoma Rainiers intellectual property done with the knowledge of Rainiers staff.
Also note: this code takes a **very long time to run**. I am working on a code that process much quicker, but in the meantime, please consider running this overnight. 

A full write-up can be found on Medium (https://medium.com/@samwirth/milb-run-expectancy-matrix-65e01c52dc) or as an R Markdown file (https://rpubs.com/samwirth/milb_run_expectancy).
