#render report
source("code/metaturf.R")
source("code/vikesland/cleaning_vikesland.R")
source("code/vikesland/Flux_Plots_vikesland.R")
source("code/data_dic/data_dictionnary.R")


rmarkdown::render(input = "group_report.Rmd")
