library(dplyr)
library(ggplot2)

time <- paste0("ZT_", seq(0,20, by=4))
gene1 <- c("PER2", "BMAL1")

Samples_PER2 <- paste0("S", 1:6)
Samples_BMAL1 <- paste0("S", 7:12)

exp_PER2 <- c(1.8, 3.2, 6.5, 9.4, 7.1, 3.6)
exp_BMAL1 <- c(8.6, 5.9, 2.7, 1.5, 3.4, 6.8)



circ_df <- data.frame(
  gene = c(
    rep(gene1[1], length(Samples_PER2)),
    rep(gene1[2], length(Samples_BMAL1))
  ), 
  samples = c(Samples_PER2, Samples_BMAL1),
  time,
  expr = c(exp_PER2, exp_BMAL1)
)

View(circ_df)


str(circ_df)


#2. (1 mark) Using base R indexing only (no dplyr), 
#store the six PER2 rows in a new data frame called per2_rows. 
#Then report the mean, median and standard deviation of PER2 expression.

per2_rows <- circ_df[circ_df$gene=="PER2",]

cat("Mean of expression for per2 gene: ", mean(per2_rows$expr))
cat("Median of expression for per2 gene: ", median(per2_rows$expr))
cat("Std deviation of expression for per2 gene: ", sd(per2_rows$expr))



#3. (1 mark) Save circ_df as circadian_expression.csv without row numbers, 
#read it back into circ_reloaded, and use identical() to confirm that the Expression column survived the round trip.
#In one sentence, say what would change in the file if you left out row.names = FALSE.

#writing it
write.csv(circ_df, "circadian_expression.csv", row.names = FALSE)

#reading it 
circ_reloaded <- read.csv("circadian_expression.csv")

identical(circ_df, circ_reloaded)



#4. (1 mark) Write two dplyr pipelines, one per gene, that keep only that gene's rows 
#and sort them from highest to lowest expression. 
#From the top row of each, report the ZT at which PER2 and BMAL1 peak, 
#and how many hours apart the two peaks are.

per2 <- filter(circ_df, gene=="PER2") %>% arrange(desc(expr))
peak_per2 <- per2$time[1]


bmal1 <- filter(circ_df, gene=="BMAL1")
peak_bmal1 <- bmal1$time[1]

cat(peak_per2)
cat(peak_bmal1)


#Q5. (2 marks) Add a TRUE/FALSE column called dark that is TRUE for samples taken at ZT12 or later (the lights-off half of the day), 
#and save the result as circ_df2. 
#Then write one pipeline that reports, for the dark samples only, 
#the mean expression of each gene and how many samples it is based on. 
#Repeat it for the light samples. 
#In one sentence, say which gene is higher in the dark. 
#Hint: Practice Exercise 2 built a TRUE/FALSE column with mutate(). filter() can keep the rows you want before you group them.



circ_df2 <- circ_df %>%
  mutate(dark = time %in% time[4:6])

circ_df2 %>% 
  filter(dark==TRUE) %>% 
  group_by(gene) %>% 
  summarize(mean_expr = mean(expr),
            sample_size = n())


circ_df2 %>% 
  filter(dark==FALSE) %>% 
  group_by(gene) %>% 
  summarize(mean_expr = mean(expr),
            sample_size = n())


#Q6 (2 marks) 
#Make a scatter plot of Expression against ZT with one colour per gene. 
#Choose the two colours yourself, add a title and axis labels (the x-axis should read Zeitgeber time (h)), 
#and use theme_minimal(). 
#Save the plot to a variable called rhythm_plot and write it to rhythm_plot.png with ggsave(). 
#In one or two sentences, describe what each gene does across the day.



circ_df$time <- factor(
  circ_df$time,
  levels = c("ZT_0", "ZT_4", "ZT_8", "ZT_12", "ZT_16", "ZT_20")
)

rhythm_plot <- ggplot(circ_df, aes(x = time, y = expr, colour = gene)) +
  geom_point() +
  theme_minimal() + 
  labs(
    x = "Zeitgeber time (h)", 
    y = "Expression of gene",
    title = "Expression of gene against time (ZT)"
  )

ggsave("rhythm_plot.png")



#Q7. (2 marks) Make a box plot of Expression for each gene, 
#filled by gene with the same two colours as Question 6, and save it as expression_boxplot.png. 
#Then compare it with rhythm_plot in two or three sentences: 
#what does the box plot suggest about the two genes, and what has it hidden that Question 6 showed?

ggplot(circ_df, aes(x = gene, y = expr, fill = gene))+
  geom_boxplot() +
  theme_minimal()

ggsave("expression_boxplot.png")
