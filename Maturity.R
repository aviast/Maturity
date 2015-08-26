options(repos = c(CRAN = "https://cran.revolutionanalytics.com"))

library(cowplot)

set.seed(1234)

DATA <- data.frame(System = rep(paste("System", 1:6), each = 3), Level = rep(c("Low","Medium","High"), times = 6), Maturity = rep(rbinom(6, 6, 0.5), each = 3))

DATA$Maturity <- factor(DATA$Maturity, levels = 1:6)

LOW <- rbinom(6, 25, 0.6) * 4
MED <- rbinom(6, 25, 0.2) * 4
HI <- rbinom(6, 25, 0.1) * 4

DATA[DATA$Level == "Low", "Score"] <- LOW
DATA[DATA$Level == "Medium", "Score"] <- MED
DATA[DATA$Level == "High", "Score"] <- HI

DATA$System <- factor(DATA$System)
DATA$Level <- factor(DATA$Level, levels = c("Low","Medium","High"))

DATA$ymin <- integer(length(nrow(DATA)))
DATA$ymax <- integer(length(nrow(DATA)))

for (sys in levels(DATA$System)) {
  DATA[DATA$System == sys & DATA$Level == "Low", "ymin"] <- 0L
  DATA[DATA$System == sys & DATA$Level == "Low", "ymax"] <- DATA[DATA$System == sys & DATA$Level == "Low", "ymin"] + DATA[DATA$System == sys & DATA$Level == "Low", "Score"]
  DATA[DATA$System == sys & DATA$Level == "Medium", "ymin"] <- DATA[DATA$System == sys & DATA$Level == "Low", "ymax"]
  DATA[DATA$System == sys & DATA$Level == "Medium", "ymax"] <- DATA[DATA$System == sys & DATA$Level == "Medium", "ymin"] + DATA[DATA$System == sys & DATA$Level == "Medium", "Score"]
  DATA[DATA$System == sys & DATA$Level == "High", "ymin"] <- DATA[DATA$System == sys & DATA$Level == "Medium", "ymax"]
  DATA[DATA$System == sys & DATA$Level == "High", "ymax"] <- DATA[DATA$System == sys & DATA$Level == "High", "ymin"] + DATA[DATA$System == sys & DATA$Level == "High", "Score"]
}

DATA$xmin <- 0
DATA$xmax <- DATA$Maturity

ggplot(DATA, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Level)) +
  geom_rect() +
  scale_x_discrete(name = "Maturity", drop = FALSE) +
  scale_y_continuous(name = "Number of Risks", breaks = c(0, 50, 100)) +
  scale_fill_manual(name = "Risk\nLevel", values = c("darkgreen","yellow2","red3")) +
  facet_wrap(~ System, ncol = 1)

ggsave(filename = "Maturity.png", width = 10, height = 15, units = "cm")
