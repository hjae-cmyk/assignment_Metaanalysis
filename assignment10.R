
library(meta)
library(dplyr)

df <- read.csv("metaanalysis_data.xlsx - Arkusz2.csv", stringsAsFactors = FALSE)


colnames(df)


m_male_toys <- metacont(
  n.e = N_boys,                  
  mean.e = Mean_boys_play_male,  
  sd.e = SD_boys_play_male,      
  n.c = N_girls,                
  mean.c = Mean_girls_play_male, 
  sd.c = SD_girls_play_male,     
  data = df,
  studlab = Study,               
  comb.fixed = FALSE,            
  comb.random = TRUE,            
  sm = "SMD"                     


summary(m_male_toys)


forest(m_male_toys, sortvar = TE)

funnel(m_male_toys)


contour_levels <- c(0.90, 0.95, 0.99)
contour_colors <- c("darkblue", "blue", "lightblue")
funnel(m_male_toys, contour = contour_levels, col.contour = contour_colors)
legend("topright", c("p < 0.10", "p < 0.05", "p < 0.01"), bty = "n", fill = contour_colors)

reg_methods <- metareg(m_male_toys, ~ NOS.score + Setting + Parent.present + Neutral.toys)

summary(reg_methods)


reg_gender <- metareg(m_male_toys, ~ Female.authors + Male.authors)


summary(reg_gender)
