######################################################
######################################################
# classificar arquivos

library(dplyr)
arqs <- 'data-raw/bd' %>% dir(full.names = TRUE)
arqs_treino <- 'data-raw/treino' %>% dir(full.names = FALSE)
arqs <- arqs[!tools::file_path_sans_ext(basename(arqs)) %in%
               stringr::str_match(tools::file_path_sans_ext(basename(arqs_treino)), '[0-9]{4}_([0-9]+)')[,2]]

classificar <- function(arq, path) {
  suppressWarnings(dir.create(path))
  arq %>% ler() %>% desenhar() %>% print()
  letras <- readline(prompt="Letras: ")
  file.copy(arq, sprintf('%s/%s_%s.jpeg', path, letras,
                         tools::file_path_sans_ext(basename(arq))))
}

arqs %>%
  sapply(function(x) {
    classificar(x, 'data-raw/treino')
  })

length(dir('data-raw/treino'))

######################################################
######################################################
# modelagem

library(caret)
set.seed(123)
d_train <- d %>%
  sample_n(3300) %>%
  mutate(y = factor(letra)) %>%
  select(-arq, -group, -letra)
m <- train(y~., d_train)
saveRDS(m, 'data-raw/m.rds')

# Acerto do meu modelo
# ainda bem que dei set.seed
aff <- d %>% sample_n(3300)
acerto <- d %>%
  anti_join(aff, c('arq', 'group')) %>%
  mutate(y = factor(letra)) %>%
  mutate(yest = predict(m, newdata = .)) %>%
  with(table(y, yest))


# Forma de uso...
a <- download()
a %>% ler %>% desenhar
a %>% predizer(m)





