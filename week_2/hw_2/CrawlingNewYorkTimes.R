install.packages("rvest")
library(rvest)

title=read_html("https://www.nytimes.com/")   
title=html_nodes(title,".story-heading")   
title=html_text(title)   
title=iconv(title,"UTF-8")  
View(title)
