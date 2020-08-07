library(tidyverse)

csv = "C:\\Users\\Aubur\\github\\auburngrads\\afmc_we_need\\data\\AWNComment_Freels.csv"
#csv = "E:/AFIT/Thesis/RawAWN_Data/AWNComment_Freels.csv"
DATA = Text.Replace:::extract_text(csv)

DAT = subset(DATA, Source == "Field Survey" & Question == 1)

comments = DAT$Comments

# subset as needed

text_tb <- tibble::tibble(id = base::seq_along(comments),
                          text = comments)

text_tb


btm_test <- text_tb %>%
        tidytext::unnest_tokens(word, text, token = 'words')

btm = BTM::BTM(btm_test,
  k = 5,
  alpha = 10,
  beta = 0.1,
  iter = 1000,
  window = 15,
  background = !FALSE,
  trace = !FALSE,
  detailed = !FALSE
)

library(LDAvis)
docsize <- table(btm_test$id)
scores  <- predict(btm, btm_test)
scores  <- scores[names(docsize), ]
json <- createJSON(
  phi = t(btm$phi), 
  theta = scores, 
  doc.length = as.integer(docsize),
  vocab = btm$vocabulary$token, 
  term.frequency = btm$vocabulary$freq)
serVis(json)


library(textplot)
library(ggraph)
library(concaveman)
plot(btm, top_n = 4)
