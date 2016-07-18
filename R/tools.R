#' Download captcha
#'
#' @param dest destino do arquivo que vc deseja salvar o captcha
#' @export
#'
download <- function(dest = NULL) {
  base <- 'http://www.tjrs.jus.br/site_php/consulta'
  u <- paste0(base, '/human_check/humancheck_showcode.php')
  if(is.null(dest)) dest <- tempfile(pattern = 'captcha', fileext = '.jpeg')
  # download.file(url = paste0(base, captcha), destfile = dest, mode = "wb")
  httr::GET(u, httr::write_disk(dest, overwrite = TRUE))
  return(dest)
}

#' Ler Captcha
#'
#' copiada do captchasaj/ só mudei que o arquivo é JPEG
#' agora é PNG denovo :P
#'
#' @export
#'
ler <- function(a) {
  img <- jpeg::readJPEG(a)
  img_dim <- dim(img)
  img_df <- data.frame(
    x = rep(1:img_dim[2], each = img_dim[1]),
    y = rep(img_dim[1]:1, img_dim[2]),
    r = as.vector(img[,,1]),
    g = as.vector(img[,,2]),
    b = as.vector(img[,,3])
  )
  d <- dplyr::mutate(img_df, cor = rgb(r, g, b), id = 1:n())
  d <- dplyr::filter(d, cor != '#FFFFFF')
  tibble::as_data_frame(d)
}

#' Desenhar Capctha
#'
#' copiada do captchasaj
#'
#' @export
#'
desenhar <- function(d){
  d <- dplyr::mutate(d, cor = rgb(r, g, b), id = 1:n())
  p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y))
  p <- p +
    ggplot2::coord_equal() +
    ggplot2::theme_bw()
  if(tibble::has_name(d, 'posicao')) {
    p <- p + ggplot2::geom_point(shape = 15)
    p <- p + ggplot2::facet_wrap(~posicao, scales = 'free_x', ncol = 6)
  } else {
    p <- p + ggplot2::geom_point(colour = d$cor, shape = 15, size = 3)
  }
  p +
    ggplot2::scale_x_continuous(breaks = 0:1000 * 3) +
    ggplot2::scale_y_continuous(breaks = 0:100 * 5)
}

#' Converter
#'
#' Converter de data.frame p/ matriz e de matriz p/ data.frame.
#'
#' @rdname converter
#' @param img imagem em formato de data.frame com as coluans x, y, r,g, e b
#'
#' @export
converter_em_matriz <- function(img){
  img %>%
    dplyr::select(x, y, r) %>%
    tidyr::spread(x, r, fill = 1) %>%
    dplyr::select(-y) %>%
    as.matrix()
}

#' Converter em data.frame
#'
#' @rdname converter
#' @export
converter_em_df <- function(m) {
  as.data.frame(m) %>%
    dplyr::mutate(y = as.numeric(1:nrow(.))) %>%
    tidyr::gather(x, r, -y) %>%
    dplyr::mutate(x = tidyr::extract_numeric(x))
}

#' Preencher
#'
#' @rdname limpar
#'
#' @export
preencher <- function(img, x, y, lim, x_min = 0, y_min = 0){
  img_completa = expand.grid(x = x_min:x, y = y_min:y)
  dplyr::left_join(img_completa, img, c('x', 'y')) %>%
    dplyr::mutate(
      r = ifelse(is.na(r), 1, r),
      r = ifelse(r > lim, 1, 0)
    )
}

#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Limpar imagem
#'
#' @rdname limpar
#'
#' @param img imagem em formato de data.frame com as coluans x, y, r,g, e b
#' @param n raio da vizinhança utilizado. 1 utiliza apenas as células adjacentes.
#' @param k número de pontos escuros necessários no entorno, para que este seja mantido.
#' @param x x máximo
#' @param y y máximo
#' @param lim tom limite de cor [0,1]. quanto mais proximo de 0, mais escuro. Ou seja
#' quanto menor o valor dessa variável, mais fácil de desconsiderar aquele ponto.
#' Como o default é 0, significa que apenas são considerados os pontos que são totalmente pretos.
#'
#' @note se você estiver usando a função \code{limpar_por_posicao}, é importante
#' que o data.frame contenha uma coluna \code{posicao}
#'
#' @export
limpar <- function(img, n = 1, k = 6, x = 170, y = 30, lim = 0){

  fk <- function(x) {sum(x == 0)}
  mk <- matrix(1, 1 + 2 * n, 1 + 2 * n)

  arrumado <- preencher(img, x, y, lim)

  for(j in k) {
    m_inicial <- arrumado %>% converter_em_matriz()
    m <- m_inicial %>%
      raster::raster() %>%
      raster::focal(mk, fk, pad = TRUE, padValue = 1) %>%
      raster::as.matrix()
    m <- ifelse(m >= j & m_inicial == 0, 0, 1)
    arrumado <- converter_em_df(m)
  }

  arrumado %>% dplyr::filter(r == 0)
}

#' @export
classificar <- function(arq, letras = NULL, path) {
  suppressWarnings(dir.create(path))
  arq %>% ler() %>% desenhar() %>% print()
  letras <- readline(prompt="Letras: ")
  file.copy(arq, sprintf('%s/%s.jpeg', path, letras))
}

#' Arruma o arquivo num BD pronto para predição.
#'
#' Deve ser usada dentro da função predizer.
#'
#' @param arqs_treino arquivo com a imagem em jpeg.
#' @param nm nomes do modelo para completar a base de predição.
#'
#' @export
arrumar <- function(arqs_treino, nm) {
  bd <- tibble::data_frame(arq = arqs_treino) %>%
    dplyr::arrange(arq) %>%
    dplyr::mutate(.id = 1:n()) %>%
    dplyr::group_by(arq) %>%
    dplyr::do({
      ler(.$arq) %>%
        dplyr::mutate(r = g, b = g) %>%
        dplyr::mutate(r = 1 - r) %>%
        dplyr::filter(r <= .2) %>%
        dplyr::mutate(r = ifelse(r < 1, 0, 1)) %>%
        dplyr::mutate(g = r, b = r) %>%
        limpar(y = 40, k = 6) %>%
        dplyr::mutate(g = r, b = r) %>%
        dplyr::filter(y >= 5, y <= 40, x >= 10, x <= 106) %>%
        dplyr::mutate(group = cut(x, c(10, 34, 58, 82, 106), labels = c(1, 2, 3, 4)),
               group = as.character(group),
               group = ifelse(is.na(group), '4', group))
    }) %>%
    ungroup()
  d <- bd %>%
    dplyr::mutate(letras = stringr::str_match(basename(arq), '([0-9]{4})_')[, 2]) %>%
    dplyr::group_by(arq) %>%
    dplyr::mutate(letra = stringr::str_sub(letras, as.numeric(group), as.numeric(group))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(x = x - 24 * (as.numeric(group) - 1)) %>%
    dplyr::select(arq, letra, group, x, y, r) %>%
    dplyr::mutate(x = sprintf('%03d', x), y = sprintf('%03d', y)) %>%
    tidyr::unite(xy, x, y, sep = '_') %>%
    tidyr::spread(xy, r, fill = 1)
  nm <- nm[nm != '.outcome']
  novos <- nm[!nm %in% names(d)]
  d[, novos] <- 1
  d
}

#' Predizer os numeros do arquivo
#'
#' Recebe um arquivo jpeg e retorna o texto do resultado predito.
#'
#' @param a arquivo com a imagem em jpeg.
#'
#' @return character com o número predito.
#'
#' @export
predizer <- function(a) {
  a %>%
    arrumar(names(m$trainingData)) %>%
    {caret::predict.train(m, newdata = .)} %>%
    paste(collapse = '')
}

