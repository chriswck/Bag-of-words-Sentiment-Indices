### Bag-of-words-Sentiment-Indices

This folder contains some of the files used in constructing a bag-of-words sentiment index from 20-years of WSJ front page articles and includes the following files:

1. Python scripts that does the word count
2. R scripts that processes the word counts and plots the sentiment for data exploration purposes
3. The plots folder contain the R script plots

A problem that surfaced with the plots is that since the crisis dummies are constructed from daily data, the geom that graphed out the black and yellow bands presented the plot in a way that made crises visually seem more prevalent than one would expect. This is likely due to the relatively small number of pixels in relation to the number days through that period, and the geom "optimized" in a way that was not particularly conducive for visual representation.
