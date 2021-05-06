

`%>%` <- magrittr::`%>%`



respostas <- readr::read_csv2('./dados/respostas_2021.csv') %>%

  janitor::clean_names()



colnames(respostas) <- c('carimbo', 'participante', 'idade', 'naturalidade', 'genero', 'origem_estudo',

                         'religiosidade', 'escolaridade_pai', 'idade_pai', 'escolaridade_mae', 'idade_mae',

                         'religiosidade_pai', 'religiosidade_mae', 'renda_familiar', 'hierarquia', 'jeitinho',

                         'violencia_institucional', 'desenvolvimentismo', 'cotas', 'aborto', 'liberalismo',

                         'homo_masculina', 'homo_feminina', 'censura', 'bolsa_familia', 'primeira_escolha',

                         'segunda_escolha', 'utilitarismo')



respostas %>%

  ggplot2::ggplot() +

  ggplot2::geom_bar(mapping = ggplot2::aes(y = naturalidade), fill = '#126244') +

  ggplot2::labs(y = 'Naturalidade dos Estudantes',

                x = 'Total de Estudantes') +

  ggplot2::scale_x_continuous(breaks = seq(0,20,1))


respostas %>%

  ggplot2::ggplot() +

  ggplot2::geom_bar(mapping = ggplot2::aes(y = genero), fill = '#126244') +

  ggplot2::labs(y = 'Identificação de Gênero',

                x = 'Total de Estudantes') +

  ggplot2::scale_x_continuous(breaks = seq(0,20,1))


respostas %>%

  ggplot2::ggplot() +

  ggplot2::geom_bar(mapping = ggplot2::aes(y = origem_estudo), fill = '#126244') +

  ggplot2::labs(y = 'Origem da Escolaridade',

                x = 'Total de Estudantes') +

  ggplot2::scale_x_continuous(breaks = seq(0,20,1))


respostas %>%

  ggplot2::ggplot() +

  ggplot2::geom_bar(mapping = ggplot2::aes(y = religiosidade), fill = '#126244') +

  ggplot2::labs(y = 'Religiosidade',

                x = 'Total de Estudantes') +

  ggplot2::scale_x_continuous(breaks = seq(0,21,1))





respostas %>%

  dplyr::select(participante, escolaridade_pai, escolaridade_mae) %>%

  tidyr::pivot_longer(-participante, names_to = 'ref', values_to = 'escolaridade') %>%

  ggplot2::ggplot() +

  ggplot2::geom_bar(mapping = ggplot2::aes(y = escolaridade, fill = ref), position = 'dodge') +

  ggplot2::labs(y = '',

                x = 'Quantidade',

                fill = '') +

  ggplot2::scale_x_continuous(breaks = seq(0,21,1)) +

  ggplot2::scale_fill_manual(labels = c('Mãe', 'Pai'),

                             values = c('#7D030E', '#1C3C52')) +

  ggplot2::theme(legend.position = 'bottom')




respostas %>%

  ggplot2::ggplot() +

  ggplot2::geom_boxplot(mapping = ggplot2::aes(y = renda_familiar), fill = '#126244', alpha = 0.5) +

  ggplot2::labs(y = 'Renda',

                x = '') +

  ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = 'R$', big.mark = '.')) +

  ggplot2::theme(axis.text.x = ggplot2::element_blank())




respostas %>%

  dplyr::select(participante, idade, idade_pai, idade_mae) %>%

  tidyr::pivot_longer(-participante, names_to = 'ref', values_to = 'idades') %>%

  ggplot2::ggplot() +

  ggplot2::geom_boxplot(mapping = ggplot2::aes(fill = ref, y = idades), alpha = 0.5) +

  ggplot2::labs(y = 'Idades',

                x = '',

                fill = '') +

  ggplot2::scale_x_continuous(breaks = seq(0,21,1)) +

  ggplot2::scale_fill_manual(labels = c('Estudante','Mãe', 'Pai'),

                             values = c('#126244', '#7D030E', '#1C3C52')) +

  ggplot2::theme(legend.position = 'bottom',

                 axis.text.x = ggplot2::element_blank())



respostas %>%

  dplyr::select(participante, hierarquia, jeitinho,

                violencia_institucional, desenvolvimentismo, cotas, aborto, liberalismo,

                homo_masculina, homo_feminina, censura, bolsa_familia, utilitarismo) %>%

  tidyr::pivot_longer(-participante, names_to = 'ref', values_to = 'social') %>%

  ggplot2::ggplot(mapping = ggplot2::aes(y = ref, x = social)) +

  ggplot2::geom_boxplot(mapping = ggplot2::aes(x = social), fill = '#126244', alpha = 0.5) +

  ggplot2::labs(y = 'Item analisado',

                x = '',

                fill = '') +

  ggplot2::scale_x_continuous(breaks = seq(1, 6, 1)) +

  ggplot2::theme(legend.position = 'none') +

  ggplot2::scale_y_discrete(labels = c('Aborto',

                                       'Bolsa Família',

                                       'Censura',

                                       'Cotas',

                                       'Desenvolvimentismo',

                                       'Hierarquia',

                                       'Homoafetividade Feminina',

                                       'Homoafetividade Masculina',

                                       'Jeitinho Brasileiro',

                                       'Liberalismo',

                                       'Utilitarismo',

                                       'Violência Institucional'))















correlacao <- respostas %>%

  dplyr::mutate(genero = ifelse(genero == 'Masculino', 1, 0),

                origem_estudo = dplyr::case_when(origem_estudo == 'Apenas em escola pública' ~ 1,

                                                 origem_estudo == 'Majoritariamente em escola pública' ~ 2,

                                                 origem_estudo == 'Majoritariamente em escola privada' ~ 3,

                                                 origem_estudo == 'Apenas em escola privada' ~ 4),

                religiosidade = ifelse(religiosidade == 'Sim', 1, 0),

                religiosidade_pai = ifelse(religiosidade_pai == 'Sim', 1, 0),

                religiosidade_mae = ifelse(religiosidade_mae == 'Sim', 1, 0),

                escolaridade_pai = dplyr::case_when(escolaridade_pai == 'Sem escolaridade' ~ 1,
                                                    escolaridade_pai == 'Ensino básico (até 4º ano - primeiro grau)' ~ 2,
                                                    escolaridade_pai == 'Ensino fundamental (até 9º ano - primeiro grau)' ~ 3,
                                                    escolaridade_pai == 'Ensino Médio (até 3º ano - segundo grau)' ~ 4,
                                                    escolaridade_pai == 'Graduação' ~ 5,
                                                    escolaridade_pai == 'Mestrado' ~ 6,
                                                    escolaridade_pai == 'Doutorado' ~ 7,
                                                    escolaridade_pai == 'Pós-Doutorado' ~ 8),

                escolaridade_mae = dplyr::case_when(escolaridade_mae == 'Sem escolaridade' ~ 1,
                                                    escolaridade_mae == 'Ensino básico (até 4º ano - primeiro grau)' ~ 2,
                                                    escolaridade_mae == 'Ensino fundamental (até 9º ano - primeiro grau)' ~ 3,
                                                    escolaridade_mae == 'Ensino Médio (até 3º ano - segundo grau)' ~ 4,
                                                    escolaridade_mae == 'Graduação' ~ 5,
                                                    escolaridade_mae == 'Mestrado' ~ 6,
                                                    escolaridade_mae == 'Doutorado' ~ 7,
                                                    escolaridade_mae == 'Pós-Doutorado' ~ 8)) %>%

  dplyr::select(- carimbo, - participante, - naturalidade, - primeira_escolha, - segunda_escolha)


correlacao <- Hmisc::rcorr(as.matrix(correlacao))


corrplot::corrplot(correlacao$r, p.mat = correlacao$P, sig.level = 0.005, type = 'lower')

library(Hmisc)

library(corrplot)

corrplot(cabeca1$r, p.mat = cabeca1$P, sig.level = 0.005 ,type = 'lower')



corrplot::corrplot()



