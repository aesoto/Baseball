# Baseball
Analyzes baseball pitch data

Purpose: 
Given that a fastball is thrown, develop a predictive model to estimate that a SwingStrike will occur

Data: 
Trackman pitch-level data for ~500k pitches representing 30 teams playing each other over 2,394 games.  Approximately 390K of these pitches are fastballs given by 736 pitchers to 938 batters. 6.84% of these fastballs result in a swingstrike

Methodology: 
The data are examined in two ways:
I. Probit Regression
II. Random Forest

Results:
		Methodology		Accuracy	Error		Bias
		Probit Regression
		Random Forest


Comments:
Probit, while an orthodox approach, is constrained by the linearity of the model.  I’m surprised random forest is so accurate given the size of the data set – I expected it to be too small.  A concern to be aware of is that the swingstrike rate is relatively small: 6.84%  This implies that a suitable model’s error rate must be <7%.
