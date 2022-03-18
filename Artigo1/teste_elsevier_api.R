library(httr)

article = GET('https://api.elsevier.com/content/article/pii/S0957417413000961?httpAccept=application/json&?apiKey=9a51cbb38a3ec83788718d289c91d868')

content(article)
