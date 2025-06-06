# Packages

p_needed <- c("tidyverse", 
              
              # data management        
              "readr", 
              "readxl",
              "writexl", 
              "haven",
              "janitor",
              "htmlwidgets",
              "maps",

              # visualization
              "ggplot2", 
              "ggpol",
              "ggmap",
              "ggrepel", 
              "webshot",
              "wordcloud2",
              "jcolors", 
              "corrplot", 
              "ggcorrplot",
              "gridExtra",
              "ggridges",
              "dotwhisker",
              
              # table formatting
              "knitr",
              "xtable", 
              "kableExtra", 
              
              # network
              "igraph", 
              
              # analyses
              "sjPlot",
              "psych",
              "sjlabelled",
              "GPArotation",
              "lavaan",
              "Hmisc",  
              "peRspective",
              "broom",
              "glmnet",
              "WRS2",
              "rstatix",
              "emmeans",
              "ggpubr", 
              "estimatr",
              "multcomp",
              "forcats",
              "stargazer",
              "modelsummary",
              "texreg",
              "gdata",
              "lme4",
              "merTools",
              "MASS"
              
            
              
       
)


packages <- rownames(installed.packages())

p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
lapply(p_needed, require, character.only = TRUE)
