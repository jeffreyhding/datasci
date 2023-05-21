# Load the CourseKata library
suppressPackageStartupMessages({
    library(coursekata)
})

draft <- subset(nba_draft_2015, draft_year == 2015)
draft$proj_spm <- round(draft$projected_spm, 5)
draft$superstar <- round(draft$superstar, 5)
draft$starter <- round(draft$starter, 5)
draft$role_player <- round(draft$role_player, 5)
draft$bust <- round(draft$bust, 5)

head(select(draft, player, position, proj_spm, superstar, starter, role_player, bust), 10)

gf_boxplot(~starter, data = draft, fill = "coral")%>%
gf_labs(title = "Probability of a top projected player in the 2015 NBA draft\nbecoming a NBA starter", 
        x = "Probability of becoming a starter")

summary(subset(draft, position == "C")$starter)
summary(subset(draft, position == "PF")$starter)
summary(subset(draft, position == "PG")$starter)
summary(subset(draft, position == "SF")$starter)
summary(subset(draft, position == "SG")$starter)

gf_boxplot(~starter, data = draft, fill = "coral")%>%
gf_facet_grid(position ~ .)%>%
gf_labs(title = "Probability of a top projected player in the 2015 NBA draft\nbecoming a NBA starter by position",
        x = "Probability of becoming a starter")

position.model <- lm(starter ~ position, data = draft)
position.model

dat <- draft%>%
group_by(position)%>%
summarize(mean = mean(starter))

gf_boxplot(~starter, data = draft, fill = "coral")%>%
gf_facet_grid(position ~ .)%>%
gf_vline(xintercept = ~mean, data = dat, color = "red")%>%
gf_labs(title = "Probability of a top projected player in the 2015 NBA draft\nbecoming a NBA starter by position",
        subtitle = "Red line: mean probability of becoming a starter",
        x = "Probability of becoming a starter")

supernova(position.model)

position.model
confint(position.model)

pwc <- pairwise(position.model)
pwc
plot(pwc)

table(draft$position)
table(draft$position)/77
