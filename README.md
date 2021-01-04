# BachelorsProjectCOVID19NLP

Assuming R is installed, open an R sesions typing R in the terminal. Then ren the command below:


```
$ R

>install.packages("tidyverse",
+                    "text2vec",
+                    "tidytext",
+                    "shiny",
+                    "shinyjs",
+                    "UsingR",
+                    "shinythemes",
+                    "DT")
```
Shiny Server needs to be installed. The following links has some information on that...

https://rstudio.com/products/shiny/download-server/

https://docs.rstudio.com/shiny-server/


To start the server follow this tutorial: https://linuxize.com/post/how-to-use-linux-screen/



Start the app by running: 

```
$ Rscript -e "shiny::runApp('BachelorsProjectCOVID19NLP/HOPE_Topic_Model_Dashboard_Bachelor', port = 8119)"
```
