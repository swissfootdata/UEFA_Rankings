uefa_ranking_ts <- ts(data=uefa_ranking, start=28)
library(ggplot2)
library(ggfortify)
autoplot(uefa_ranking_ts, facets = FALSE, size=2)+
  geom_hline(yintercept=27.833, linetype="dashed", color="blue", size=1.5)+
  geom_hline(yintercept=25.3, linetype="dashed", color="red", size=1.5)+
  labs(titel="Uefa 5 Year Ranking 2018/2019", x="Week of 2018", y="Points")+
  theme(legend.title=element_blank())