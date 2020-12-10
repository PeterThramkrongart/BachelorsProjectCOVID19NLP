# BachelorsProjectCOVID19NLP

Assuming R is installed, it should be possible to run the command below in the shell.


```
$ Rscript -e 'install.packages(c("pacman","tidyverse","text2vec","tidytext","shiny","shinyjs","UsingR", "DT", "shinythemes"),repos = "http://cran.us.r-project.org")'
```
Shiny Server needs to be installed. The following links has some information on that...

https://rstudio.com/products/shiny/download-server/

https://docs.rstudio.com/shiny-server/


To start the server follow this tutorial: https://linuxize.com/post/how-to-use-linux-screen/

You can start a session with screen pressing ctrl+a To restart screen run:

```
$ screen -r 
```


To stop screen press ctrl+c


Start the app by running: 

```
$ Rscript -e "shiny::runApp('BachelorsProjectCOVID19NLP/HOPE_SemTrackR_Shiny', port = 8119)"
```
