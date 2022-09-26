library(ggplot2)
top20 <- Uefa_5_Jahreswertung[1:20,]
View(top20)
ggplot(top20, aes(reorder(Country_short, -Total), Total))+geom_bar(stat="identity", aes(fill=reorder(Country, -Total)))+
  geom_text(aes(label=round(Total, digits=2)), vjust=-1, fontface="bold")+
  labs(title="UEFA 5-year Club Ranking 2018", x="Country", y="Score")+
  geom_hline(yintercept=25.05, linetype="dashed", color="red")+
  geom_hline(yintercept=59, linetype="dashed", color="blue")
sui <- Uefa_5_Jahreswertung[13,]  
nine_seventeen <- Uefa_5_Jahreswertung[9:17,]
View(nine_seventeen)

## Teams 9-17
ggplot(nine_seventeen, aes(reorder(Country_short, -Total),Total))+geom_bar(stat="identity", aes(fill=reorder(Country, -Total)))+
  geom_text(aes(label=round(Total, digits=2)), vjust=-1, fontface="bold")+
  labs(title="UEFA 5-year Club Ranking 2018", x="Country", y="Score")

## Reihen als Spalten
test2 <- as.data.frame(t(nine_seventeen))
View(test2)
test2 <- test2[6:12,]
test2[] <- lapply(test2, as.character)
colnames(test2) <- test2[1, ]
test2 <- test2[-1 ,]
View(test2)
str(test2)
as.numeric(test2$Schweiz)

## Vergleich 9-17
test2[ "Season" ] <- rownames(test2)
test2 <- test2[1:5,]
View(test2)
Ustr(test2)
test2 <- as.numeric(test2$Belgien)
str(test2)
ggplot(test2, aes(Season, as.numeric(Schweiz)) + geom_line()
View(Uefa_5_Jahreswertung)
str(Uefa_5_Jahreswertung)
schweiz <- Uefa_5_Jahreswertung[Uefa_5_Jahreswertung$Country == "Schweiz",]
View(schweiz)

#Teams in der CH
library(ggplot2)
ggplot(UEFA5_Data_Teams, aes(reorder(Team,-Points), Points)) + geom_bar(stat="identity", aes(fill=Season)) +
  labs(title="Points for UEFA 5-year Club Ranking 2014-2018", x="Team", y="Points per Season")
