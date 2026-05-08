# Nathan Hellwege's Data Analysis Portfolio

These are several projects that I have completed using a wide variety of hockey data.

### 1. Interactive Hockey Analytics Dashboard
This dashboard allows the user to select a team and any number of players, including all of the players in a recent line or pairing. The application will then display the combined shotmap for those players (with an opportunity to filter for game state) as well as the statistics from when all selected players were on the ice together.\
There is also a player tab where you can view the shotmap for a single player, as well as the most common linemates for that player and three similar players (as determined by a knn model).
### 2. NHL Clustering and RAPM
In this project, heirarchical cluster analysis was performed on NHL player statistics to determine a variety of player types. Shift and play-by-play data was then used to calculate teh regularized adjusted plus-minus for each player group to determine the expected value of players in each group.
### 3. NHL Pre-draft Player Stats Analysis
In this project, a classification tree was built using pre-draft player data from players recently drafted to the NHL to try and predict whether each player would actually make the NHL. This project included data from many different professional leagues from around the world, including the CHL, USHL, SHL, KHL, and DEL. The objective of this analysis was to determine what stats are often indicative of a player being able to make the NHL, when taken in the context of the league they played in (since stats from young players in leagues like the SHL are often much lower than in leagues like the OHL).
### 4. NHL Team Salary Analysis
In this project, team salary data from 2022-2025 was analyzed to determine the impact that team spending has on their success in both the regular season and the playoffs. Analysis was performed on the impact of total spending, spending on each position, spending on each line, and the number of highly paid players a team had (defined as a player making >10% of the salary cap).
### Data Sources:
1. NHL api
2. moneypuck.com
3. eliteprospects.com
4. hockey-reference.com
5. spotrac.com
6. capwages.com
