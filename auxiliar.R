
##### Fazendo manualmente ####
i=2

##### primeira parte #####
total_transf.19 <- svytotal(x=~V4013>=10010 & V4013<=33002 , design = a$pnad_design[[i]], na.rm = TRUE) ## sem bootstrap


total_transf[[i]] <- as_tibble(total_transf.19, rownames = "total_transf") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

# ver o total de pessoas para saber se estou pegando toda população

total.19 <- svytotal(x=~V2007d, design = a$pnad_design[[i]], na.rm = TRUE)


total[[i]] <- as_tibble(total.19, rownames = "total_") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


# ver o total de pessoas formais - empregado no setor privado com carteira assinada + empregador com cnpj

formais_transf.1 <- svytotal(x=~V4013>=10010 & V4013<=33002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                             "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_transf.2 <- svytotal(x=~V4013>=10010 & V4013<=33002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                             "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_transf <- formais_transf.1[[2]]+formais_transf.2[[2]] ## esse vai


total_formal_transformacao[[i]] <- as_tibble(total_formal_transf, rownames = "total_formal_transf") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))



# total de informais 

total_informais_scarteira_transf <- svytotal(x=~V4013>=10010 & V4013<=33002, design=subset(a$pnad_design[[i]], 
                                                                                           VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                             na.rm = TRUE) ### esse vai

total_informais_sc_transf[[i]] <- as_tibble(total_informais_scarteira_transf, rownames = "total_informais_scarteiral_transf") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))



informais_autonomos_transf.1 <- svytotal(x=~V4013>=10010 & V4013<=33002, design=subset(a$pnad_design[[i]], 
                                                                                       VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), na.rm = TRUE)


informais_autonomos_transf.2 <- svytotal(x=~V4013>=10010 & V4013<=33002, design=subset(a$pnad_design[[i]], V4019d == 
                                                                                         "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos <- informais_autonomos_transf.1[[2]] + informais_autonomos_transf.2[[2]] ## esse vai



total_informais_auto[[i]] <- as_tibble(total_informais_autonomos, rownames = "total_informais_autonomos") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

####### separando por divisão

############ Fabricação de produtos alimentícios 10010 a 10099 - 10 ##################

formais_10_.1 <- svytotal(x=~V4013>=10010 & V4013<=10099, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_10_.2 <- svytotal(x=~V4013>=10010 & V4013<=10099, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_10 <- formais_10_.1[[2]]+formais_10_.2[[2]] ## vai

total_formal_transf_10[[i]] <- as_tibble(total_formal_10, rownames = "total_formal_10") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


# total de informais 

total_informais_scarteira_10 <- svytotal(x=~V4013>=10010 & V4013<=10099, design=subset(a$pnad_design[[i]], 
                                                                                       VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                         na.rm = TRUE) ## vai

total_informais_sc_transf_10[[i]] <- as_tibble(total_informais_scarteira_10, rownames = "total_informais_scarteiral_transf_10") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


informais_autonomos_10.1 <- svytotal(x=~V4013>=10010 & V4013<=10099, design=subset(a$pnad_design[[i]], 
                                                                                   VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                     na.rm = TRUE)


informais_autonomos_10.2 <- svytotal(x=~V4013>=10010 & V4013<=10099, design=subset(a$pnad_design[[i]],
                                                                                   V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos_10 <- informais_autonomos_10.1[[2]] + informais_autonomos_10.2[[2]] ## vai

total_informais_auto_10[[i]] <- as_tibble(total_informais_autonomos_10, rownames = "total_informais_autonomos_10") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

# rendimento de todos os trabalhos efetivo dos autonomos

renda_autonomos_transf_10.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=10010 & V4013<=10099 & VD4009d %in% c("Conta-própria", 
                                                                                                             "Trabalhador familiar auxiliar")), na.rm = TRUE)


renda_autonomos_transf_10.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=10010 & V4013<=10099 & V4019d == "Não" 
                                                                & VD4009d == "Empregador"), na.rm = TRUE)

media_renda_autonomos_10 <- (renda_autonomos_transf_10.1[[1]]*informais_autonomos_10.1[[2]] + 
                               renda_autonomos_transf_10.2[[1]]*informais_autonomos_10.2[[2]])/(
                                 informais_autonomos_10.1[[2]]+informais_autonomos_10.2[[2]]) ### vai

media_renda_auto_10[[i]] <- as_tibble(media_renda_autonomos_10, rownames = "media_renda_auto_10") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


############ Fabricação de bebidas - 11 - 11000 ############

formais_11_.1 <- svytotal(x=~V4013== 11000, design=subset(a$pnad_design[[i]], VD4009d == 
                                                            "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_11_.2 <- svytotal(x=~V4013== 11000, design=subset(a$pnad_design[[i]], VD4009d == 
                                                            "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_11 <- formais_11_.1[[2]]+formais_11_.2[[2]] ##vai

total_formal_transf_11[[i]] <- as_tibble(total_formal_11, rownames = "total_formal_11") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


# total de informais 

total_informais_scarteira_11 <- svytotal(x=~V4013== 11000, design=subset(a$pnad_design[[i]], 
                                                                         VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                         na.rm = TRUE) ## vai 

total_informais_sc_transf_11[[i]] <- as_tibble(total_informais_scarteira_11, rownames = "total_informais_scarteiral_transf_11") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

informais_autonomos_11.1 <- svytotal(x=~V4013== 11000, design=subset(a$pnad_design[[i]], 
                                                                     VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                     na.rm = TRUE)


informais_autonomos_11.2 <- svytotal(x=~V4013== 11000, design=subset(a$pnad_design[[i]],
                                                                     V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos_11 <- informais_autonomos_11.1[[2]] + informais_autonomos_11.2[[2]]

total_informais_auto_11[[i]] <- as_tibble(total_informais_autonomos_11, rownames = "total_informais_autonomos_11") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


# rendimento de todos os trabalhos efetivo dos autonomos

renda_autonomos_transf_11.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013== 11000 & VD4009d %in% c("Conta-própria", 
                                                                                               "Trabalhador familiar auxiliar")), na.rm = TRUE)


renda_autonomos_transf_11.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013== 11000 & V4019d == "Não" 
                                                                & VD4009d == "Empregador"), na.rm = TRUE)

media_renda_autonomos_11 <- (renda_autonomos_transf_11.1[[1]]*informais_autonomos_11.1[[2]] + 
                               renda_autonomos_transf_11.2[[1]]*informais_autonomos_11.2[[2]])/(
                                 informais_autonomos_11.1[[2]]+informais_autonomos_11.2[[2]])

media_renda_auto_11[[i]] <- as_tibble(media_renda_autonomos_11, rownames = "media_renda_auto_11") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

############ Fabricação de produtos do fumo - 12 - 12000 ############

formais_12_.1 <- svytotal(x=~V4013== 12000, design=subset(a$pnad_design[[i]], VD4009d == 
                                                            "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_12_.2 <- svytotal(x=~V4013== 12000, design=subset(a$pnad_design[[i]], VD4009d == 
                                                            "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_12 <- formais_12_.1[[2]]+formais_12_.2[[2]]

total_formal_transf_12[[i]] <- as_tibble(total_formal_12, rownames = "total_formal_12") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


# total de informais 

total_informais_scarteira_12 <- svytotal(x=~V4013== 12000, design=subset(a$pnad_design[[i]], 
                                                                         VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                         na.rm = TRUE)


total_informais_sc_transf_12[[i]] <- as_tibble(total_informais_scarteira_12, rownames = "total_informais_scarteiral_transf_12") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))



informais_autonomos_12.1 <- svytotal(x=~V4013== 12000, design=subset(a$pnad_design[[i]], 
                                                                     VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                     na.rm = TRUE)


informais_autonomos_12.2 <- svytotal(x=~V4013== 12000, design=subset(a$pnad_design[[i]],
                                                                     V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos_12 <- informais_autonomos_12.1[[2]] + informais_autonomos_12.2[[2]]


total_informais_auto_12[[i]] <- as_tibble(total_informais_autonomos_12, rownames = "total_informais_autonomos_12") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

# rendimento de todos os trabalhos efetivo dos autonomos

renda_autonomos_transf_12.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013== 12000 & VD4009d %in% c("Conta-própria", 
                                                                                               "Trabalhador familiar auxiliar")), na.rm = TRUE)


renda_autonomos_transf_12.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013== 12000 & V4019d == "Não" 
                                                                & VD4009d == "Empregador"), na.rm = TRUE)

media_renda_autonomos_12 <- (renda_autonomos_transf_12.1[[1]]*informais_autonomos_12.1[[2]] + 
                               renda_autonomos_transf_12.2[[1]]*informais_autonomos_12.2[[2]])/(
                                 informais_autonomos_12.1[[2]]+informais_autonomos_12.2[[2]])

media_renda_auto_12[[i]] <- as_tibble(media_renda_autonomos_12, rownames = "media_renda_auto_12") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))




############ Fabricação de produtos têxteis - 13 - 13001 e 13002 ##################

formais_13_.1 <- svytotal(x=~V4013>=13001 & V4013<=13002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_13_.2 <- svytotal(x=~V4013>=13001 & V4013<=13002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_13 <- formais_13_.1[[2]]+formais_13_.2[[2]]

total_formal_transf_13[[i]] <- as_tibble(total_formal_13, rownames = "total_formal_13") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))



# total de informais 

total_informais_scarteira_13 <- svytotal(x=~V4013>=13001 & V4013<=13002, design=subset(a$pnad_design[[i]], 
                                                                                       VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                         na.rm = TRUE)

total_informais_sc_transf_13[[i]] <- as_tibble(total_informais_scarteira_13, rownames = "total_informais_scarteiral_transf_13") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))



informais_autonomos_13.1 <- svytotal(x=~V4013>=13001 & V4013<=13002, design=subset(a$pnad_design[[i]], 
                                                                                   VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                     na.rm = TRUE)


informais_autonomos_13.2 <- svytotal(x=~V4013>=13001 & V4013<=13002, design=subset(a$pnad_design[[i]],
                                                                                   V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos_13 <- informais_autonomos_13.1[[2]] + informais_autonomos_13.2[[2]]


total_informais_auto_13[[i]] <- as_tibble(total_informais_autonomos_13, rownames = "total_informais_autonomos_13") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

# rendimento de todos os trabalhos efetivo dos autonomos

renda_autonomos_transf_13.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=13001 & V4013<=13002 & VD4009d %in% c("Conta-própria", 
                                                                                                             "Trabalhador familiar auxiliar")), na.rm = TRUE)


renda_autonomos_transf_13.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=13001 & V4013<=13002 & V4019d == "Não" 
                                                                & VD4009d == "Empregador"), na.rm = TRUE)

media_renda_autonomos_13 <- (renda_autonomos_transf_13.1[[1]]*informais_autonomos_13.1[[2]] + 
                               renda_autonomos_transf_13.2[[1]]*informais_autonomos_13.2[[2]])/(
                                 informais_autonomos_13.1[[2]]+informais_autonomos_13.2[[2]])

media_renda_auto_13[[i]] <- as_tibble(media_renda_autonomos_13, rownames = "media_renda_auto_13") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))



############ Confeccão de artigos do vestuário e acessórios - 14 - 14001 e 14002 ##################

formais_14_.1 <- svytotal(x=~V4013>=14001 & V4013<=14002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_14_.2 <- svytotal(x=~V4013>=14001 & V4013<=14002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_14 <- formais_14_.1[[2]]+formais_14_.2[[2]]


# total de informais 

total_informais_scarteira_14 <- svytotal(x=~V4013>=14001 & V4013<=14002, design=subset(a$pnad_design[[i]], 
                                                                                       VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                         na.rm = TRUE)


informais_autonomos_14.1 <- svytotal(x=~V4013>=14001 & V4013<=14002, design=subset(a$pnad_design[[i]], 
                                                                                   VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                     na.rm = TRUE)


informais_autonomos_14.2 <- svytotal(x=~V4013>=14001 & V4013<=14002, design=subset(a$pnad_design[[i]],
                                                                                   V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos_14 <- informais_autonomos_14.1[[2]] + informais_autonomos_14.2[[2]]

# rendimento de todos os trabalhos efetivo dos autonomos

renda_autonomos_transf_14.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=14001 & V4013<=14002 & VD4009d %in% c("Conta-própria", 
                                                                                                             "Trabalhador familiar auxiliar")), na.rm = TRUE)


renda_autonomos_transf_14.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=14001 & V4013<=14002 & V4019d == "Não" 
                                                                & VD4009d == "Empregador"), na.rm = TRUE)

media_renda_autonomos_14 <- (renda_autonomos_transf_14.1[[1]]*informais_autonomos_14.1[[2]] + 
                               renda_autonomos_transf_14.2[[1]]*informais_autonomos_14.2[[2]])/(
                                 informais_autonomos_14.1[[2]]+informais_autonomos_14.2[[2]])



############ PREPARAÇÃO DE COUROS E FABRICAÇÃO DE ARTEFATOS DE COURO, ARTIGOS DE VIAGEM E CALÇADOS
#############- 15 - 15011 a 15020 ##################

formais_15_.1 <- svytotal(x=~V4013>=15011 & V4013<=15020, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_15_.2 <- svytotal(x=~V4013>=15011 & V4013<=15020, design=subset(a$pnad_design[[i]], VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"), na.rm = TRUE)


total_formal_15 <- formais_15_.1[[2]]+formais_15_.2[[2]]


# total de informais 

total_informais_scarteira_15 <- svytotal(x=~V4013>=15011 & V4013<=15020, design=subset(a$pnad_design[[i]], 
                                                                                       VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                         na.rm = TRUE)


informais_autonomos_15.1 <- svytotal(x=~V4013>=15011 & V4013<=15020, design=subset(a$pnad_design[[i]], 
                                                                                   VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                     na.rm = TRUE)


informais_autonomos_15.2 <- svytotal(x=~V4013>=15011 & V4013<=15020, design=subset(a$pnad_design[[i]],
                                                                                   V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos_15 <- informais_autonomos_15.1[[2]] + informais_autonomos_15.2[[2]]

# rendimento de todos os trabalhos efetivo dos autonomos

renda_autonomos_transf_15.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=15011 & V4013<=15020 & VD4009d %in% c("Conta-própria", 
                                                                                                             "Trabalhador familiar auxiliar")), na.rm = TRUE)


renda_autonomos_transf_15.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=15011 & V4013<=15020 & V4019d == "Não" 
                                                                & VD4009d == "Empregador"), na.rm = TRUE)

media_renda_autonomos_15 <- (renda_autonomos_transf_15.1[[1]]*informais_autonomos_15.1[[2]] + 
                               renda_autonomos_transf_15.2[[1]]*informais_autonomos_15.2[[2]])/(
                                 informais_autonomos_15.1[[2]]+informais_autonomos_15.2[[2]])



############ FABRICAÇÃO DE PRODUTOS DE MADEIRA - 16 - 16001 a 16002 ##################

formais_16_.1 <- svytotal(x=~V4013>=16001 & V4013<=16002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_16_.2 <- svytotal(x=~V4013>=16001 & V4013<=16002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_16 <- formais_16_.1[[2]]+formais_16_.2[[2]]


# total de informais 

total_informais_scarteira_16 <- svytotal(x=~V4013>=16001 & V4013<=16002, design=subset(a$pnad_design[[i]], 
                                                                                       VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                         na.rm = TRUE)


informais_autonomos_16.1 <- svytotal(x=~V4013>=16001 & V4013<=16002, design=subset(a$pnad_design[[i]], 
                                                                                   VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                     na.rm = TRUE)


informais_autonomos_16.2 <- svytotal(x=~V4013>=16001 & V4013<=16002, design=subset(a$pnad_design[[i]],
                                                                                   V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos_16 <- informais_autonomos_16.1[[2]] + informais_autonomos_16.2[[2]]

# rendimento de todos os trabalhos efetivo dos autonomos

renda_autonomos_transf_16.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=16001 & V4013<=16002  & VD4009d %in% c("Conta-própria", 
                                                                                                              "Trabalhador familiar auxiliar")), na.rm = TRUE)


renda_autonomos_transf_16.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=16001 & V4013<=16002 & V4019d == "Não" 
                                                                & VD4009d == "Empregador"), na.rm = TRUE)

media_renda_autonomos_16 <- (renda_autonomos_transf_16.1[[1]]*informais_autonomos_16.1[[2]] + 
                               renda_autonomos_transf_16.2[[1]]*informais_autonomos_16.2[[2]])/(
                                 informais_autonomos_16.1[[2]]+informais_autonomos_16.2[[2]])


############ FABRICAÇÃO DE CELULOSE, PAPEL E PRODUTOS DE PAPEL - 17 - 17001 a 17002 ##################

formais_17_.1 <- svytotal(x=~V4013>=17001 & V4013<=17002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_17_.2 <- svytotal(x=~V4013>=17001 & V4013<=17002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_17 <- formais_17_.1[[2]]+formais_17_.2[[2]]


# total de informais 

total_informais_scarteira_17 <- svytotal(x=~V4013>=17001 & V4013<=17002, design=subset(a$pnad_design[[i]], 
                                                                                       VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                         na.rm = TRUE)


informais_autonomos_17.1 <- svytotal(x=~V4013>=17001 & V4013<=17002, design=subset(a$pnad_design[[i]], 
                                                                                   VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                     na.rm = TRUE)


informais_autonomos_17.2 <- svytotal(x=~V4013>=17001 & V4013<=17002, design=subset(a$pnad_design[[i]],
                                                                                   V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos_17 <- informais_autonomos_17.1[[2]] + informais_autonomos_17.2[[2]]

# rendimento de todos os trabalhos efetivo dos autonomos

renda_autonomos_transf_17.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=17001 & V4013<=17002  & VD4009d %in% c("Conta-própria", 
                                                                                                              "Trabalhador familiar auxiliar")), na.rm = TRUE)


renda_autonomos_transf_17.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=17001 & V4013<=17002 & V4019d == "Não" 
                                                                & VD4009d == "Empregador"), na.rm = TRUE)

media_renda_autonomos_17 <- (renda_autonomos_transf_17.1[[1]]*informais_autonomos_17.1[[2]] + 
                               renda_autonomos_transf_17.2[[1]]*informais_autonomos_17.2[[2]])/(
                                 informais_autonomos_17.1[[2]]+informais_autonomos_17.2[[2]])


############ IMPRESSÃO E REPRODUÇÃO DE GRAVAÇÕES - 18 - 18000 ############

formais_18_.1 <- svytotal(x=~V4013== 18000, design=subset(a$pnad_design[[i]], VD4009d == 
                                                            "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_18_.2 <- svytotal(x=~V4013== 18000, design=subset(a$pnad_design[[i]], VD4009d == 
                                                            "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_18 <- formais_18_.1[[2]]+formais_18_.2[[2]]


# total de informais 

total_informais_scarteira_18 <- svytotal(x=~V4013== 18000, design=subset(a$pnad_design[[i]], 
                                                                         VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                         na.rm = TRUE)


informais_autonomos_18.1 <- svytotal(x=~V4013== 18000, design=subset(a$pnad_design[[i]], 
                                                                     VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                     na.rm = TRUE)


informais_autonomos_18.2 <- svytotal(x=~V4013== 18000, design=subset(a$pnad_design[[i]],
                                                                     V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos_18 <- informais_autonomos_18.1[[2]] + informais_autonomos_18.2[[2]]


# rendimento de todos os trabalhos efetivo dos autonomos

renda_autonomos_transf_18.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013== 18000 & VD4009d %in% c("Conta-própria", 
                                                                                               "Trabalhador familiar auxiliar")), na.rm = TRUE)


renda_autonomos_transf_18.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013== 18000 & V4019d == "Não" 
                                                                & VD4009d == "Empregador"), na.rm = TRUE)

media_renda_autonomos_18 <- (renda_autonomos_transf_18.1[[1]]*informais_autonomos_18.1[[2]] + 
                               renda_autonomos_transf_18.2[[1]]*informais_autonomos_18.2[[2]])/(
                                 informais_autonomos_18.1[[2]]+informais_autonomos_18.2[[2]])


############ FABRICAÇÃO DE COQUE; PRODUTOS DERIVADOS DE PETRÓLEO E DE BIOCOMBUSTÍVEIS - 19 - 19010 a 19030 ##################

formais_19_.1 <- svytotal(x=~V4013>=19010 & V4013<=19030, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_19_.2 <- svytotal(x=~V4013>=19010 & V4013<=19030, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_19 <- formais_19_.1[[2]]+formais_19_.2[[2]]


# total de informais 

total_informais_scarteira_19 <- svytotal(x=~V4013>=19010 & V4013<=19030, design=subset(a$pnad_design[[i]], 
                                                                                       VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                         na.rm = TRUE)


informais_autonomos_19.1 <- svytotal(x=~V4013>=19010 & V4013<=19030, design=subset(a$pnad_design[[i]], 
                                                                                   VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                     na.rm = TRUE)


informais_autonomos_19.2 <- svytotal(x=~V4013>=19010 & V4013<=19030, design=subset(a$pnad_design[[i]],
                                                                                   V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos_19 <- informais_autonomos_19.1[[2]] + informais_autonomos_19.2[[2]]

# rendimento de todos os trabalhos efetivo dos autonomos

renda_autonomos_transf_19.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=19010 & V4013<=19030  & VD4009d %in% c("Conta-própria", 
                                                                                                              "Trabalhador familiar auxiliar")), na.rm = TRUE)


renda_autonomos_transf_19.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=19010 & V4013<=19030 & V4019d == "Não" 
                                                                & VD4009d == "Empregador"), na.rm = TRUE)

media_renda_autonomos_19 <- (renda_autonomos_transf_19.1[[1]]*informais_autonomos_19.1[[2]] + 
                               renda_autonomos_transf_19.2[[1]]*informais_autonomos_19.2[[2]])/(
                                 informais_autonomos_19.1[[2]]+informais_autonomos_19.2[[2]])



############ FABRICAÇÃO DE PRODUTOS QUÍMICOS- 20 - 20010 a 20090 ##################

formais_20_.1 <- svytotal(x=~V4013>=20010 & V4013<=20090, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_20_.2 <- svytotal(x=~V4013>=20010 & V4013<=20090, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_20 <- formais_20_.1[[2]]+formais_20_.2[[2]]


# total de informais 

total_informais_scarteira_20 <- svytotal(x=~V4013>=20010 & V4013<=20090, design=subset(a$pnad_design[[i]], 
                                                                                       VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                         na.rm = TRUE)


informais_autonomos_20.1 <- svytotal(x=~V4013>=20010 & V4013<=20090, design=subset(a$pnad_design[[i]], 
                                                                                   VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                     na.rm = TRUE)


informais_autonomos_20.2 <- svytotal(x=~V4013>=20010 & V4013<=20090, design=subset(a$pnad_design[[i]],
                                                                                   V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos_20 <- informais_autonomos_20.1[[2]] + informais_autonomos_20.2[[2]]

# rendimento de todos os trabalhos efetivo dos autonomos

renda_autonomos_transf_20.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=20010 & V4013<=20090  & VD4009d %in% c("Conta-própria", 
                                                                                                              "Trabalhador familiar auxiliar")), na.rm = TRUE)


renda_autonomos_transf_20.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=20010 & V4013<=20090 & V4019d == "Não" 
                                                                & VD4009d == "Empregador"), na.rm = TRUE)

media_renda_autonomos_20 <- (renda_autonomos_transf_20.1[[1]]*informais_autonomos_20.1[[2]] + 
                               renda_autonomos_transf_20.2[[1]]*informais_autonomos_20.2[[2]])/(
                                 informais_autonomos_20.1[[2]]+informais_autonomos_20.2[[2]])



############ FABRICAÇÃO DE PRODUTOS FARMOQUÍMICOS E FARMACÊUTICOS- 21 - 21000 ############

formais_21_.1 <- svytotal(x=~V4013== 21000, design=subset(a$pnad_design[[i]], VD4009d == 
                                                            "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_21_.2 <- svytotal(x=~V4013== 21000, design=subset(a$pnad_design[[i]], VD4009d == 
                                                            "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_21 <- formais_21_.1[[2]]+formais_21_.2[[2]]


# total de informais 

total_informais_scarteira_21 <- svytotal(x=~V4013== 21000, design=subset(a$pnad_design[[i]], 
                                                                         VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                         na.rm = TRUE)


informais_autonomos_21.1 <- svytotal(x=~V4013== 21000, design=subset(a$pnad_design[[i]], 
                                                                     VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                     na.rm = TRUE)


informais_autonomos_21.2 <- svytotal(x=~V4013== 21000, design=subset(a$pnad_design[[i]],
                                                                     V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos_21 <- informais_autonomos_21.1[[2]] + informais_autonomos_21.2[[2]]

# rendimento de todos os trabalhos efetivo dos autonomos

renda_autonomos_transf_21.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013== 21000 & VD4009d %in% c("Conta-própria", 
                                                                                               "Trabalhador familiar auxiliar")), na.rm = TRUE)


renda_autonomos_transf_21.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013== 21000 & V4019d == "Não" 
                                                                & VD4009d == "Empregador"), na.rm = TRUE)

media_renda_autonomos_21 <- (renda_autonomos_transf_21.1[[1]]*informais_autonomos_21.1[[2]] + 
                               renda_autonomos_transf_21.2[[1]]*informais_autonomos_21.2[[2]])/(
                                 informais_autonomos_21.1[[2]]+informais_autonomos_21.2[[2]])



############ FABRICAÇÃO DE PRODUTOS DE BORRACHA E DE MATERIAL PLÁSTICO - 22 - 22010 e 22020 ##################

formais_22_.1 <- svytotal(x=~V4013>=22010 & V4013<=22020, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_22_.2 <- svytotal(x=~V4013>=22010 & V4013<=22020, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_22 <- formais_22_.1[[2]]+formais_22_.2[[2]]


# total de informais 

total_informais_scarteira_22 <- svytotal(x=~V4013>=22010 & V4013<=22020, design=subset(a$pnad_design[[i]], 
                                                                                       VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                         na.rm = TRUE)


informais_autonomos_22.1 <- svytotal(x=~V4013>=22010 & V4013<=22020, design=subset(a$pnad_design[[i]], 
                                                                                   VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                     na.rm = TRUE)


informais_autonomos_22.2 <- svytotal(x=~V4013>=22010 & V4013<=22020, design=subset(a$pnad_design[[i]],
                                                                                   V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos_22 <- informais_autonomos_22.1[[2]] + informais_autonomos_22.2[[2]]

# rendimento de todos os trabalhos efetivo dos autonomos

renda_autonomos_transf_22.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=22010 & V4013<=2202  & VD4009d %in% c("Conta-própria", 
                                                                                                             "Trabalhador familiar auxiliar")), na.rm = TRUE)


renda_autonomos_transf_22.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=22010 & V4013<=2202 & V4019d == "Não" 
                                                                & VD4009d == "Empregador"), na.rm = TRUE)

media_renda_autonomos_22 <- (renda_autonomos_transf_22.1[[1]]*informais_autonomos_22.1[[2]] + 
                               renda_autonomos_transf_22.2[[1]]*informais_autonomos_22.2[[2]])/(
                                 informais_autonomos_22.1[[2]]+informais_autonomos_22.2[[2]])


############ FABRICAÇÃO DE PRODUTOS DE MINERAIS NÃO-METÁLICOS - 23 - 23010 e 23099 ##################

formais_23_.1 <- svytotal(x=~V4013>=23010 & V4013<=23099, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_23_.2 <- svytotal(x=~V4013>=23010 & V4013<=23099, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_23 <- formais_23_.1[[2]]+formais_23_.2[[2]]


# total de informais 

total_informais_scarteira_23 <- svytotal(x=~V4013>=23010 & V4013<=23099, design=subset(a$pnad_design[[i]], 
                                                                                       VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                         na.rm = TRUE)


informais_autonomos_23.1 <- svytotal(x=~V4013>=23010 & V4013<=23099, design=subset(a$pnad_design[[i]], 
                                                                                   VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                     na.rm = TRUE)


informais_autonomos_23.2 <- svytotal(x=~V4013>=23010 & V4013<=23099, design=subset(a$pnad_design[[i]],
                                                                                   V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos_23 <- informais_autonomos_23.1[[2]] + informais_autonomos_23.2[[2]]

# rendimento de todos os trabalhos efetivo dos autonomos

renda_autonomos_transf_23.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=23010 & V4013<=23099  & VD4009d %in% c("Conta-própria", 
                                                                                                              "Trabalhador familiar auxiliar")), na.rm = TRUE)


renda_autonomos_transf_23.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=23010 & V4013<=23099 & V4019d == "Não" 
                                                                & VD4009d == "Empregador"), na.rm = TRUE)

media_renda_autonomos_23 <- (renda_autonomos_transf_23.1[[1]]*informais_autonomos_23.1[[2]] + 
                               renda_autonomos_transf_23.2[[1]]*informais_autonomos_23.2[[2]])/(
                                 informais_autonomos_23.1[[2]]+informais_autonomos_23.2[[2]])


############ METALURGIA - 24 - 24001 e 24003 ##################

formais_24_.1 <- svytotal(x=~V4013>=24001 & V4013<=24003, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_24_.2 <- svytotal(x=~V4013>=24001 & V4013<=24003, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_24 <- formais_24_.1[[2]]+formais_24_.2[[2]]


# total de informais 

total_informais_scarteira_24 <- svytotal(x=~V4013>=24001 & V4013<=24003, design=subset(a$pnad_design[[i]], 
                                                                                       VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                         na.rm = TRUE)


informais_autonomos_24.1 <- svytotal(x=~V4013>=24001 & V4013<=24003, design=subset(a$pnad_design[[i]], 
                                                                                   VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                     na.rm = TRUE)


informais_autonomos_24.2 <- svytotal(x=~V4013>=24001 & V4013<=24003, design=subset(a$pnad_design[[i]],
                                                                                   V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos_24 <- informais_autonomos_24.1[[2]] + informais_autonomos_24.2[[2]]

# rendimento de todos os trabalhos efetivo dos autonomos

renda_autonomos_transf_24.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=24001 & V4013<=24003  & VD4009d %in% c("Conta-própria", 
                                                                                                              "Trabalhador familiar auxiliar")), na.rm = TRUE)


renda_autonomos_transf_24.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=24001 & V4013<=24003 & V4019d == "Não" 
                                                                & VD4009d == "Empregador"), na.rm = TRUE)

media_renda_autonomos_24 <- (renda_autonomos_transf_24.1[[1]]*informais_autonomos_24.1[[2]] + 
                               renda_autonomos_transf_24.2[[1]]*informais_autonomos_24.2[[2]])/(
                                 informais_autonomos_24.1[[2]]+informais_autonomos_24.2[[2]])



############ FABRICAÇÃO DE PRODUTOS DE METAL, EXCETO MÁQUINAS E EQUIPAMENTOS - 25 - 25001 e 25002 ##################

formais_25_.1 <- svytotal(x=~V4013>=25001 & V4013<=25002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_25_.2 <- svytotal(x=~V4013>=25001 & V4013<=25002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_25 <- formais_25_.1[[2]]+formais_25_.2[[2]]


# total de informais 

total_informais_scarteira_25 <- svytotal(x=~V4013>=25001 & V4013<=25002, design=subset(a$pnad_design[[i]], 
                                                                                       VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                         na.rm = TRUE)


informais_autonomos_25.1 <- svytotal(x=~V4013>=25001 & V4013<=25002, design=subset(a$pnad_design[[i]], 
                                                                                   VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                     na.rm = TRUE)


informais_autonomos_25.2 <- svytotal(x=~V4013>=25001 & V4013<=25002, design=subset(a$pnad_design[[i]],
                                                                                   V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos_25 <- informais_autonomos_25.1[[2]] + informais_autonomos_25.2[[2]]

# rendimento de todos os trabalhos efetivo dos autonomos

renda_autonomos_transf_25.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=25001 & V4013<=25002  & VD4009d %in% c("Conta-própria", 
                                                                                                              "Trabalhador familiar auxiliar")), na.rm = TRUE)


renda_autonomos_transf_25.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=25001 & V4013<=25002 & V4019d == "Não" 
                                                                & VD4009d == "Empregador"), na.rm = TRUE)

media_renda_autonomos_25 <- (renda_autonomos_transf_25.1[[1]]*informais_autonomos_25.1[[2]] + 
                               renda_autonomos_transf_25.2[[1]]*informais_autonomos_25.2[[2]])/(
                                 informais_autonomos_25.1[[2]]+informais_autonomos_25.2[[2]])


############ FABRICAÇÃO DE EQUIPAMENTOS DE INFORMÁTICA, PRODUTOS ELETRÔNICOS E ÓPTICOS - 26 - 26010 a 26042 ##################

formais_26_.1 <- svytotal(x=~V4013>=26010 & V4013<=26042, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_26_.2 <- svytotal(x=~V4013>=26010 & V4013<=26042, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_26 <- formais_26_.1[[2]]+formais_26_.2[[2]]


# total de informais 

total_informais_scarteira_26 <- svytotal(x=~V4013>=26010 & V4013<=26042, design=subset(a$pnad_design[[i]], 
                                                                                       VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                         na.rm = TRUE)


informais_autonomos_26.1 <- svytotal(x=~V4013>=26010 & V4013<=26042, design=subset(a$pnad_design[[i]], 
                                                                                   VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                     na.rm = TRUE)


informais_autonomos_26.2 <- svytotal(x=~V4013>=26010 & V4013<=26042, design=subset(a$pnad_design[[i]],
                                                                                   V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos_26 <- informais_autonomos_26.1[[2]] + informais_autonomos_26.2[[2]]

# rendimento de todos os trabalhos efetivo dos autonomos

renda_autonomos_transf_26.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=26010 & V4013<=26042  & VD4009d %in% c("Conta-própria", 
                                                                                                              "Trabalhador familiar auxiliar")), na.rm = TRUE)


renda_autonomos_transf_26.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=26010 & V4013<=26042 & V4019d == "Não" 
                                                                & VD4009d == "Empregador"), na.rm = TRUE)

media_renda_autonomos_26 <- (renda_autonomos_transf_26.1[[1]]*informais_autonomos_26.1[[2]] + 
                               renda_autonomos_transf_26.2[[1]]*informais_autonomos_26.2[[2]])/(
                                 informais_autonomos_26.1[[2]]+informais_autonomos_26.2[[2]])



############ FABRICAÇÃO DE MÁQUINAS, APARELHOS E MATERIAIS ELÉTRICOS - 27 - 27010 a 27090 ##################

formais_27_.1 <- svytotal(x=~V4013>=27010 & V4013<=27090, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_27_.2 <- svytotal(x=~V4013>=27010 & V4013<=27090, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_27 <- formais_27_.1[[2]]+formais_27_.2[[2]]


# total de informais 

total_informais_scarteira_27 <- svytotal(x=~V4013>=27010 & V4013<=27090, design=subset(a$pnad_design[[i]], 
                                                                                       VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                         na.rm = TRUE)


informais_autonomos_27.1 <- svytotal(x=~V4013>=27010 & V4013<=27090, design=subset(a$pnad_design[[i]], 
                                                                                   VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                     na.rm = TRUE)


informais_autonomos_27.2 <- svytotal(x=~V4013>=27010 & V4013<=27090, design=subset(a$pnad_design[[i]],
                                                                                   V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos_27 <- informais_autonomos_27.1[[2]] + informais_autonomos_27.2[[2]]

# rendimento de todos os trabalhos efetivo dos autonomos

renda_autonomos_transf_27.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=27010 & V4013<=27090  & VD4009d %in% c("Conta-própria", 
                                                                                                              "Trabalhador familiar auxiliar")), na.rm = TRUE)


renda_autonomos_transf_27.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=27010 & V4013<=27090 & V4019d == "Não" 
                                                                & VD4009d == "Empregador"), na.rm = TRUE)

media_renda_autonomos_27 <- (renda_autonomos_transf_27.1[[1]]*informais_autonomos_27.1[[2]] + 
                               renda_autonomos_transf_27.2[[1]]*informais_autonomos_27.2[[2]])/(
                                 informais_autonomos_27.1[[2]]+informais_autonomos_27.2[[2]])



############ FABRICAÇÃO DE MÁQUINAS E EQUIPAMENTOS- 28 - 28000 ############


formais_28_.1 <- svytotal(x=~V4013== 28000, design=subset(a$pnad_design[[i]], VD4009d == 
                                                            "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_28_.2 <- svytotal(x=~V4013== 28000, design=subset(a$pnad_design[[i]], VD4009d == 
                                                            "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_28 <- formais_28_.1[[2]]+formais_28_.2[[2]]


# total de informais 

total_informais_scarteira_28 <- svytotal(x=~V4013== 28000, design=subset(a$pnad_design[[i]], 
                                                                         VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                         na.rm = TRUE)


informais_autonomos_28.1 <- svytotal(x=~V4013== 28000, design=subset(a$pnad_design[[i]], 
                                                                     VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                     na.rm = TRUE)


informais_autonomos_28.2 <- svytotal(x=~V4013== 28000, design=subset(a$pnad_design[[i]],
                                                                     V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos_28 <- informais_autonomos_28.1[[2]] + informais_autonomos_28.2[[2]]

# rendimento de todos os trabalhos efetivo dos autonomos

renda_autonomos_transf_28.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013== 28000 & VD4009d %in% c("Conta-própria", 
                                                                                               "Trabalhador familiar auxiliar")), na.rm = TRUE)


renda_autonomos_transf_28.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013== 28000 & V4019d == "Não" 
                                                                & VD4009d == "Empregador"), na.rm = TRUE)

media_renda_autonomos_28 <- (renda_autonomos_transf_28.1[[1]]*informais_autonomos_28.1[[2]] + 
                               renda_autonomos_transf_28.2[[1]]*informais_autonomos_28.2[[2]])/(
                                 informais_autonomos_28.1[[2]]+informais_autonomos_28.2[[2]])




############ FABRICAÇÃO DE VEÍCULOS AUTOMOTORES, REBOQUES E CARROCERIAS - 29 - 29001 a 29003 ##################

formais_29_.1 <- svytotal(x=~V4013>=29001 & V4013<=29003, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_29_.2 <- svytotal(x=~V4013>=29001 & V4013<=29003, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_29 <- formais_29_.1[[2]]+formais_29_.2[[2]]


# total de informais 

total_informais_scarteira_29 <- svytotal(x=~V4013>=29001 & V4013<=29003, design=subset(a$pnad_design[[i]], 
                                                                                       VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                         na.rm = TRUE)


informais_autonomos_29.1 <- svytotal(x=~V4013>=29001 & V4013<=29003, design=subset(a$pnad_design[[i]], 
                                                                                   VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                     na.rm = TRUE)


informais_autonomos_29.2 <- svytotal(x=~V4013>=29001 & V4013<=29003, design=subset(a$pnad_design[[i]],
                                                                                   V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos_29 <- informais_autonomos_29.1[[2]] + informais_autonomos_29.2[[2]]


# rendimento de todos os trabalhos efetivo dos autonomos

renda_autonomos_transf_29.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=29001 & V4013<=29003  & VD4009d %in% c("Conta-própria", 
                                                                                                              "Trabalhador familiar auxiliar")), na.rm = TRUE)


renda_autonomos_transf_29.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=29001 & V4013<=29003 & V4019d == "Não" 
                                                                & VD4009d == "Empregador"), na.rm = TRUE)

media_renda_autonomos_29 <- (renda_autonomos_transf_29.1[[1]]*informais_autonomos_29.1[[2]] + 
                               renda_autonomos_transf_29.2[[1]]*informais_autonomos_29.2[[2]])/(
                                 informais_autonomos_29.1[[2]]+informais_autonomos_29.2[[2]])


############ FABRICAÇÃO DE OUTROS EQUIPAMENTOS DE TRANSPORTE, EXCETO VEÍCULOS AUTOMOTORES - 30 - 30010 a 30090 ##################

formais_30_.1 <- svytotal(x=~V4013>=30010 & V4013<=30090, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_30_.2 <- svytotal(x=~V4013>=30010 & V4013<=30090, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_30 <- formais_30_.1[[2]]+formais_30_.2[[2]]


# total de informais 

total_informais_scarteira_30 <- svytotal(x=~V4013>=30010 & V4013<=30090, design=subset(a$pnad_design[[i]], 
                                                                                       VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                         na.rm = TRUE)


informais_autonomos_30.1 <- svytotal(x=~V4013>=30010 & V4013<=30090, design=subset(a$pnad_design[[i]], 
                                                                                   VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                     na.rm = TRUE)


informais_autonomos_30.2 <- svytotal(x=~V4013>=30010 & V4013<=30090, design=subset(a$pnad_design[[i]],
                                                                                   V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos_30 <- informais_autonomos_30.1[[2]] + informais_autonomos_30.2[[2]]

# rendimento de todos os trabalhos efetivo dos autonomos

renda_autonomos_transf_30.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=30010 & V4013<=30090  & VD4009d %in% c("Conta-própria", 
                                                                                                              "Trabalhador familiar auxiliar")), na.rm = TRUE)


renda_autonomos_transf_30.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=30010 & V4013<=30090 & V4019d == "Não" 
                                                                & VD4009d == "Empregador"), na.rm = TRUE)

media_renda_autonomos_30 <- (renda_autonomos_transf_30.1[[1]]*informais_autonomos_30.1[[2]] + 
                               renda_autonomos_transf_30.2[[1]]*informais_autonomos_30.2[[2]])/(
                                 informais_autonomos_30.1[[2]]+informais_autonomos_30.2[[2]])



############ FABRICAÇÃO DE MÓVEIS- 31 - 31000 ############


formais_31_.1 <- svytotal(x=~V4013== 31000, design=subset(a$pnad_design[[i]], VD4009d == 
                                                            "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_31_.2 <- svytotal(x=~V4013== 31000, design=subset(a$pnad_design[[i]], VD4009d == 
                                                            "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_31 <- formais_31_.1[[2]]+formais_31_.2[[2]]


# total de informais 

total_informais_scarteira_31 <- svytotal(x=~V4013== 31000, design=subset(a$pnad_design[[i]], 
                                                                         VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                         na.rm = TRUE)


informais_autonomos_31.1 <- svytotal(x=~V4013== 31000, design=subset(a$pnad_design[[i]], 
                                                                     VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                     na.rm = TRUE)


informais_autonomos_31.2 <- svytotal(x=~V4013== 31000, design=subset(a$pnad_design[[i]],
                                                                     V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos_31 <- informais_autonomos_31.1[[2]] + informais_autonomos_31.2[[2]]

# rendimento de todos os trabalhos efetivo dos autonomos

renda_autonomos_transf_31.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013== 31000 & VD4009d %in% c("Conta-própria", 
                                                                                               "Trabalhador familiar auxiliar")), na.rm = TRUE)


renda_autonomos_transf_31.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013== 31000 & V4019d == "Não" 
                                                                & VD4009d == "Empregador"), na.rm = TRUE)

media_renda_autonomos_31 <- (renda_autonomos_transf_31.1[[1]]*informais_autonomos_31.1[[2]] + 
                               renda_autonomos_transf_31.2[[1]]*informais_autonomos_31.2[[2]])/(
                                 informais_autonomos_31.1[[2]]+informais_autonomos_31.2[[2]])




############ FABRICAÇÃO DE PRODUTOS DIVERSOS - 32 - 32001 a 32009 ##################

formais_32_.1 <- svytotal(x=~V4013>=32001 & V4013<=32009, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_32_.2 <- svytotal(x=~V4013>=32001 & V4013<=32009, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_32 <- formais_32_.1[[2]]+formais_32_.2[[2]]


# total de informais 

total_informais_scarteira_32 <- svytotal(x=~V4013>=32001 & V4013<=32009, design=subset(a$pnad_design[[i]], 
                                                                                       VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                         na.rm = TRUE)


informais_autonomos_32.1 <- svytotal(x=~V4013>=32001 & V4013<=32009, design=subset(a$pnad_design[[i]], 
                                                                                   VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                     na.rm = TRUE)


informais_autonomos_32.2 <- svytotal(x=~V4013>=32001 & V4013<=32009, design=subset(a$pnad_design[[i]],
                                                                                   V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos_32 <- informais_autonomos_32.1[[2]] + informais_autonomos_32.2[[2]]



# rendimento de todos os trabalhos efetivo dos autonomos

renda_autonomos_transf_32.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=32001 & V4013<=32009  & VD4009d %in% c("Conta-própria", 
                                                                                                              "Trabalhador familiar auxiliar")), na.rm = TRUE)


renda_autonomos_transf_32.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=32001 & V4013<=32009 & V4019d == "Não" 
                                                                & VD4009d == "Empregador"), na.rm = TRUE)

media_renda_autonomos_32 <- (renda_autonomos_transf_32.1[[1]]*informais_autonomos_32.1[[2]] + 
                               renda_autonomos_transf_32.2[[1]]*informais_autonomos_32.2[[2]])/(
                                 informais_autonomos_32.1[[2]]+informais_autonomos_32.2[[2]])



############ MANUTENÇÃO, REPARAÇÃO E INSTALAÇÃO DE MÁQUINAS E EQUIPAMENTOS - 33 - 33001 a 33002 ##################

formais_33_.1 <- svytotal(x=~V4013>=33001 & V4013<=33002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregador" & V4019d == "Sim"), na.rm = TRUE) 

formais_33_.2 <- svytotal(x=~V4013>=33001 & V4013<=33002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                          "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)


total_formal_33 <- formais_33_.1[[2]]+formais_33_.2[[2]]


# total de informais 

total_informais_scarteira_33 <- svytotal(x=~V4013>=33001 & V4013<=33002, design=subset(a$pnad_design[[i]], 
                                                                                       VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                         na.rm = TRUE)


informais_autonomos_33.1 <- svytotal(x=~V4013>=33001 & V4013<=33002, design=subset(a$pnad_design[[i]], 
                                                                                   VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                     na.rm = TRUE)


informais_autonomos_33.2 <- svytotal(x=~V4013>=33001 & V4013<=33002, design=subset(a$pnad_design[[i]],
                                                                                   V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)


total_informais_autonomos_33 <- informais_autonomos_33.1[[2]] + informais_autonomos_33.2[[2]]


# rendimento de todos os trabalhos efetivo dos autonomos

renda_autonomos_transf_33.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=33001 & V4013<=33002  & VD4009d %in% c("Conta-própria", 
                                                                                                              "Trabalhador familiar auxiliar")), na.rm = TRUE)


renda_autonomos_transf_33.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                V4013>=33001 & V4013<=33002 & V4019d == "Não" 
                                                                & VD4009d == "Empregador"), na.rm = TRUE)

media_renda_autonomos_33 <- (renda_autonomos_transf_33.1[[1]]*informais_autonomos_33.1[[2]] + 
                               renda_autonomos_transf_33.2[[1]]*informais_autonomos_33.2[[2]])/(
                                 informais_autonomos_33.1[[2]]+informais_autonomos_33.2[[2]])









#####

total_formal_transf_14[[i]] <- as_tibble(total_formal_14, rownames = "total_formal_14") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_informais_sc_transf_14[[i]] <- as_tibble(total_informais_scarteira_14, rownames = "total_informais_scarteiral_transf_14") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


total_informais_auto_14[[i]] <- as_tibble(total_informais_autonomos_14, rownames = "total_informais_autonomos_14") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

media_renda_auto_14[[i]] <- as_tibble(media_renda_autonomos_14, rownames = "media_renda_auto_14") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))



total_formal_transf_15[[i]] <- as_tibble(total_formal_15, rownames = "total_formal_15") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_informais_sc_transf_15[[i]] <- as_tibble(total_informais_scarteira_15, rownames = "total_informais_scarteiral_transf_15") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


total_informais_auto_15[[i]] <- as_tibble(total_informais_autonomos_15, rownames = "total_informais_autonomos_15") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

media_renda_auto_15[[i]] <- as_tibble(media_renda_autonomos_15, rownames = "media_renda_auto_15") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_formal_transf_16[[i]] <- as_tibble(total_formal_16, rownames = "total_formal_16") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_informais_sc_transf_16[[i]] <- as_tibble(total_informais_scarteira_16, rownames = "total_informais_scarteiral_transf_16") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


total_informais_auto_16[[i]] <- as_tibble(total_informais_autonomos_16, rownames = "total_informais_autonomos_16") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

media_renda_auto_16[[i]] <- as_tibble(media_renda_autonomos_16, rownames = "media_renda_auto_16") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


total_formal_transf_17[[i]] <- as_tibble(total_formal_17, rownames = "total_formal_17") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_informais_sc_transf_17[[i]] <- as_tibble(total_informais_scarteira_17, rownames = "total_informais_scarteiral_transf_17") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


total_informais_auto_17[[i]] <- as_tibble(total_informais_autonomos_17, rownames = "total_informais_autonomos_17") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

media_renda_auto_17[[i]] <- as_tibble(media_renda_autonomos_17, rownames = "media_renda_auto_17") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_formal_transf_18[[i]] <- as_tibble(total_formal_18, rownames = "total_formal_18") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_informais_sc_transf_18[[i]] <- as_tibble(total_informais_scarteira_18, rownames = "total_informais_scarteiral_transf_18") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


total_informais_auto_18[[i]] <- as_tibble(total_informais_autonomos_18, rownames = "total_informais_autonomos_18") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

media_renda_auto_18[[i]] <- as_tibble(media_renda_autonomos_18, rownames = "media_renda_auto_18") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_formal_transf_19[[i]] <- as_tibble(total_formal_19, rownames = "total_formal_19") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_informais_sc_transf_19[[i]] <- as_tibble(total_informais_scarteira_19, rownames = "total_informais_scarteiral_transf_19") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


total_informais_auto_19[[i]] <- as_tibble(total_informais_autonomos_19, rownames = "total_informais_autonomos_19") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

media_renda_auto_19[[i]] <- as_tibble(media_renda_autonomos_19, rownames = "media_renda_auto_19") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_formal_transf_20[[i]] <- as_tibble(total_formal_20, rownames = "total_formal_20") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_informais_sc_transf_20[[i]] <- as_tibble(total_informais_scarteira_20, rownames = "total_informais_scarteiral_transf_20") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


total_informais_auto_20[[i]] <- as_tibble(total_informais_autonomos_20, rownames = "total_informais_autonomos_20") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

media_renda_auto_20[[i]] <- as_tibble(media_renda_autonomos_20, rownames = "media_renda_auto_20") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_formal_transf_21[[i]] <- as_tibble(total_formal_21, rownames = "total_formal_21") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_informais_sc_transf_21[[i]] <- as_tibble(total_informais_scarteira_21, rownames = "total_informais_scarteiral_transf_21") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


total_informais_auto_21[[i]] <- as_tibble(total_informais_autonomos_21, rownames = "total_informais_autonomos_21") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

media_renda_auto_21[[i]] <- as_tibble(media_renda_autonomos_21, rownames = "media_renda_auto_21") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_formal_transf_22[[i]] <- as_tibble(total_formal_22, rownames = "total_formal_22") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_informais_sc_transf_22[[i]] <- as_tibble(total_informais_scarteira_22, rownames = "total_informais_scarteiral_transf_22") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


total_informais_auto_22[[i]] <- as_tibble(total_informais_autonomos_22, rownames = "total_informais_autonomos_22") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

media_renda_auto_22[[i]] <- as_tibble(media_renda_autonomos_22, rownames = "media_renda_auto_22") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_formal_transf_23[[i]] <- as_tibble(total_formal_23, rownames = "total_formal_23") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_informais_sc_transf_23[[i]] <- as_tibble(total_informais_scarteira_23, rownames = "total_informais_scarteiral_transf_23") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


total_informais_auto_23[[i]] <- as_tibble(total_informais_autonomos_23, rownames = "total_informais_autonomos_23") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

media_renda_auto_23[[i]] <- as_tibble(media_renda_autonomos_23, rownames = "media_renda_auto_23") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_formal_transf_24[[i]] <- as_tibble(total_formal_24, rownames = "total_formal_24") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_informais_sc_transf_24[[i]] <- as_tibble(total_informais_scarteira_24, rownames = "total_informais_scarteiral_transf_24") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


total_informais_auto_24[[i]] <- as_tibble(total_informais_autonomos_24, rownames = "total_informais_autonomos_24") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

media_renda_auto_24[[i]] <- as_tibble(media_renda_autonomos_24, rownames = "media_renda_auto_24") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


total_formal_transf_25[[i]] <- as_tibble(total_formal_25, rownames = "total_formal_25") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_informais_sc_transf_25[[i]] <- as_tibble(total_informais_scarteira_25, rownames = "total_informais_scarteiral_transf_25") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


total_informais_auto_25[[i]] <- as_tibble(total_informais_autonomos_25, rownames = "total_informais_autonomos_25") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

media_renda_auto_25[[i]] <- as_tibble(media_renda_autonomos_25, rownames = "media_renda_auto_25") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))



total_formal_transf_26[[i]] <- as_tibble(total_formal_26, rownames = "total_formal_26") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_informais_sc_transf_26[[i]] <- as_tibble(total_informais_scarteira_26, rownames = "total_informais_scarteiral_transf_26") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


total_informais_auto_26[[i]] <- as_tibble(total_informais_autonomos_26, rownames = "total_informais_autonomos_26") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

media_renda_auto_26[[i]] <- as_tibble(media_renda_autonomos_26, rownames = "media_renda_auto_26") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_formal_transf_27[[i]] <- as_tibble(total_formal_27, rownames = "total_formal_27") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_informais_sc_transf_27[[i]] <- as_tibble(total_informais_scarteira_27, rownames = "total_informais_scarteiral_transf_27") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


total_informais_auto_27[[i]] <- as_tibble(total_informais_autonomos_27, rownames = "total_informais_autonomos_27") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

media_renda_auto_27[[i]] <- as_tibble(media_renda_autonomos_27, rownames = "media_renda_auto_27") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_formal_transf_28[[i]] <- as_tibble(total_formal_28, rownames = "total_formal_28") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_informais_sc_transf_28[[i]] <- as_tibble(total_informais_scarteira_28, rownames = "total_informais_scarteiral_transf_28") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


total_informais_auto_28[[i]] <- as_tibble(total_informais_autonomos_28, rownames = "total_informais_autonomos_28") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

media_renda_auto_28[[i]] <- as_tibble(media_renda_autonomos_28, rownames = "media_renda_auto_28") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_formal_transf_29[[i]] <- as_tibble(total_formal_29, rownames = "total_formal_29") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_informais_sc_transf_29[[i]] <- as_tibble(total_informais_scarteira_29, rownames = "total_informais_scarteiral_transf_29") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


total_informais_auto_29[[i]] <- as_tibble(total_informais_autonomos_29, rownames = "total_informais_autonomos_29") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

media_renda_auto_29[[i]] <- as_tibble(media_renda_autonomos_29, rownames = "media_renda_auto_29") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_formal_transf_30[[i]] <- as_tibble(total_formal_30, rownames = "total_formal_30") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_informais_sc_transf_30[[i]] <- as_tibble(total_informais_scarteira_30, rownames = "total_informais_scarteiral_transf_30") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


total_informais_auto_30[[i]] <- as_tibble(total_informais_autonomos_30, rownames = "total_informais_autonomos_30") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

media_renda_auto_30[[i]] <- as_tibble(media_renda_autonomos_30, rownames = "media_renda_auto_30") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_formal_transf_31[[i]] <- as_tibble(total_formal_31, rownames = "total_formal_31") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_informais_sc_transf_31[[i]] <- as_tibble(total_informais_scarteira_31, rownames = "total_informais_scarteiral_transf_31") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


total_informais_auto_31[[i]] <- as_tibble(total_informais_autonomos_31, rownames = "total_informais_autonomos_31") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

media_renda_auto_31[[i]] <- as_tibble(media_renda_autonomos_31, rownames = "media_renda_auto_31") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_formal_transf_32[[i]] <- as_tibble(total_formal_32, rownames = "total_formal_32") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_informais_sc_transf_32[[i]] <- as_tibble(total_informais_scarteira_32, rownames = "total_informais_scarteiral_transf_32") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


total_informais_auto_32[[i]] <- as_tibble(total_informais_autonomos_32, rownames = "total_informais_autonomos_32") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

media_renda_auto_32[[i]] <- as_tibble(media_renda_autonomos_32, rownames = "media_renda_auto_32") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_formal_transf_33[[i]] <- as_tibble(total_formal_33, rownames = "total_formal_33") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

total_informais_sc_transf_33[[i]] <- as_tibble(total_informais_scarteira_33, rownames = "total_informais_scarteiral_transf_33") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))


total_informais_auto_33[[i]] <- as_tibble(total_informais_autonomos_33, rownames = "total_informais_autonomos_33") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))

media_renda_auto_33[[i]] <- as_tibble(media_renda_autonomos_33, rownames = "media_renda_auto_33") %>%
  mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
         ano = max(a$pnad_design[[i]]$variables$ano))














##### Criando tibble com todas as tabelas ####

#lembrar das primeiras, antes das divisoes por setor

a <-  map_chr(10:33, ~print(paste("total_formal_transf_",as.character(.), sep = "")))
b <- map_chr(10:33, ~print(paste("total_informais_sc_transf_",as.character(.), sep = "")))
c <- map_chr(10:33, ~print(paste("total_informais_auto_",as.character(.), sep = "")))
d <- map_chr(10:33, ~print(paste("media_renda_auto_",as.character(.), sep = "")))
e <- "total"
f <- "total_formal_transformacao"
g <- "total_informais_sc_transf"
h<- "total_informais_auto"
i <- "total_transf"


final <- left_join(vetor2, vetor3, by = "value")
final <- left_join(final, vetor4, by= "value")


final %>%
  select(-value)%>%
  mutate(final = pmap(.,rbind))%>%
  select(final)-> a

final <- cbind(final$value, a)


vetor2 <- as_tibble(c(a,b,c,d,e,f,g,h,i)) %>% 
  mutate(listas = map(.x = value , function(.x){ 
    return(reduce(get(.x), rbind))
    
  }))
  
#### Exportando para xlsx ####

wb <- createWorkbook()

walk(vetor2$value, ~addWorksheet(wb, sheetName = .x))

walk2(.x = vetor2$value, .y = vetor2$listas, ~writeData(wb, sheet = .x, x = .y))

saveWorkbook(wb, "pnad_resultados_2021.xlsx", overwrite = TRUE)


#### Exportando para xlsx ####

wb <- createWorkbook()

walk(final$`final$value`, ~addWorksheet(wb, sheetName = .x))

walk2(.x = final$`final$value`, .y = final$final, ~writeData(wb, sheet = .x, x = .y))

saveWorkbook(wb, "pnad_resultados_final.xlsx", overwrite = TRUE)


for (i in 1:7) {
  
  ##### primeira parte #####
  total_transf.19 <- svytotal(x=~V4013>=10010 & V4013<=33002 , design = a$pnad_design[[i]], na.rm = TRUE) ## sem bootstrap
  
  
  total_transf[[i]] <- as_tibble(total_transf.19, rownames = "total_transf") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  # ver o total de pessoas para saber se estou pegando toda população
  
  total.19 <- svytotal(x=~V2007d, design = a$pnad_design[[i]], na.rm = TRUE)
  
  
  total[[i]] <- as_tibble(total.19, rownames = "total_") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  # ver o total de pessoas formais - empregado no setor privado com carteira assinada + empregador com cnpj
  
  formais_transf.1 <- svytotal(x=~V4013>=10010 & V4013<=33002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                               "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_transf.2 <- svytotal(x=~V4013>=10010 & V4013<=33002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                               "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_transf <- formais_transf.1[[2]]+formais_transf.2[[2]] ## esse vai
  
  
  total_formal_transformacao[[i]] <- as_tibble(total_formal_transf, rownames = "total_formal_transf") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  
  # total de informais 
  
  total_informais_scarteira_transf <- svytotal(x=~V4013>=10010 & V4013<=33002, design=subset(a$pnad_design[[i]], 
                                                                                             VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                               na.rm = TRUE) ### esse vai
  
  total_informais_sc_transf[[i]] <- as_tibble(total_informais_scarteira_transf, rownames = "total_informais_scarteiral_transf") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  
  informais_autonomos_transf.1 <- svytotal(x=~V4013>=10010 & V4013<=33002, design=subset(a$pnad_design[[i]], 
                                                                                         VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  informais_autonomos_transf.2 <- svytotal(x=~V4013>=10010 & V4013<=33002, design=subset(a$pnad_design[[i]], V4019d == 
                                                                                           "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos <- informais_autonomos_transf.1[[2]] + informais_autonomos_transf.2[[2]] ## esse vai
  
  
  
  total_informais_auto[[i]] <- as_tibble(total_informais_autonomos, rownames = "total_informais_autonomos") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  ####### separando por divisão
  
  ############ Fabricação de produtos alimentícios 10010 a 10099 - 10 ##################
  
  formais_10_.1 <- svytotal(x=~V4013>=10010 & V4013<=10099, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_10_.2 <- svytotal(x=~V4013>=10010 & V4013<=10099, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_10 <- formais_10_.1[[2]]+formais_10_.2[[2]] ## vai
  
  total_formal_transf_10[[i]] <- as_tibble(total_formal_10, rownames = "total_formal_10") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  # total de informais 
  
  total_informais_scarteira_10 <- svytotal(x=~V4013>=10010 & V4013<=10099, design=subset(a$pnad_design[[i]], 
                                                                                         VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                           na.rm = TRUE) ## vai
  
  total_informais_sc_transf_10[[i]] <- as_tibble(total_informais_scarteira_10, rownames = "total_informais_scarteiral_transf_10") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  informais_autonomos_10.1 <- svytotal(x=~V4013>=10010 & V4013<=10099, design=subset(a$pnad_design[[i]], 
                                                                                     VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                       na.rm = TRUE)
  
  
  informais_autonomos_10.2 <- svytotal(x=~V4013>=10010 & V4013<=10099, design=subset(a$pnad_design[[i]],
                                                                                     V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos_10 <- informais_autonomos_10.1[[2]] + informais_autonomos_10.2[[2]] ## vai
  
  total_informais_auto_10[[i]] <- as_tibble(total_informais_autonomos_10, rownames = "total_informais_autonomos_10") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  # rendimento de todos os trabalhos efetivo dos autonomos
  
  renda_autonomos_transf_10.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=10010 & V4013<=10099 & VD4009d %in% c("Conta-própria", 
                                                                                                               "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  renda_autonomos_transf_10.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=10010 & V4013<=10099 & V4019d == "Não" 
                                                                  & VD4009d == "Empregador"), na.rm = TRUE)
  
  media_renda_autonomos_10 <- (renda_autonomos_transf_10.1[[1]]*informais_autonomos_10.1[[2]] + 
                                 renda_autonomos_transf_10.2[[1]]*informais_autonomos_10.2[[2]])/(
                                   informais_autonomos_10.1[[2]]+informais_autonomos_10.2[[2]]) ### vai
  
  media_renda_auto_10[[i]] <- as_tibble(media_renda_autonomos_10, rownames = "media_renda_auto_10") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  ############ Fabricação de bebidas - 11 - 11000 ############
  
  formais_11_.1 <- svytotal(x=~V4013== 11000, design=subset(a$pnad_design[[i]], VD4009d == 
                                                              "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_11_.2 <- svytotal(x=~V4013== 11000, design=subset(a$pnad_design[[i]], VD4009d == 
                                                              "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_11 <- formais_11_.1[[2]]+formais_11_.2[[2]] ##vai
  
  total_formal_transf_11[[i]] <- as_tibble(total_formal_11, rownames = "total_formal_11") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  # total de informais 
  
  total_informais_scarteira_11 <- svytotal(x=~V4013== 11000, design=subset(a$pnad_design[[i]], 
                                                                           VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                           na.rm = TRUE) ## vai 
  
  total_informais_sc_transf_11[[i]] <- as_tibble(total_informais_scarteira_11, rownames = "total_informais_scarteiral_transf_11") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  informais_autonomos_11.1 <- svytotal(x=~V4013== 11000, design=subset(a$pnad_design[[i]], 
                                                                       VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                       na.rm = TRUE)
  
  
  informais_autonomos_11.2 <- svytotal(x=~V4013== 11000, design=subset(a$pnad_design[[i]],
                                                                       V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos_11 <- informais_autonomos_11.1[[2]] + informais_autonomos_11.2[[2]]
  
  total_informais_auto_11[[i]] <- as_tibble(total_informais_autonomos_11, rownames = "total_informais_autonomos_11") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  # rendimento de todos os trabalhos efetivo dos autonomos
  
  renda_autonomos_transf_11.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013== 11000 & VD4009d %in% c("Conta-própria", 
                                                                                                 "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  renda_autonomos_transf_11.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013== 11000 & V4019d == "Não" 
                                                                  & VD4009d == "Empregador"), na.rm = TRUE)
  
  media_renda_autonomos_11 <- (renda_autonomos_transf_11.1[[1]]*informais_autonomos_11.1[[2]] + 
                                 renda_autonomos_transf_11.2[[1]]*informais_autonomos_11.2[[2]])/(
                                   informais_autonomos_11.1[[2]]+informais_autonomos_11.2[[2]])
  
  media_renda_auto_11[[i]] <- as_tibble(media_renda_autonomos_11, rownames = "media_renda_auto_11") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  ############ Fabricação de produtos do fumo - 12 - 12000 ############
  
  formais_12_.1 <- svytotal(x=~V4013== 12000, design=subset(a$pnad_design[[i]], VD4009d == 
                                                              "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_12_.2 <- svytotal(x=~V4013== 12000, design=subset(a$pnad_design[[i]], VD4009d == 
                                                              "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_12 <- formais_12_.1[[2]]+formais_12_.2[[2]]
  
  total_formal_transf_12[[i]] <- as_tibble(total_formal_12, rownames = "total_formal_12") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  # total de informais 
  
  total_informais_scarteira_12 <- svytotal(x=~V4013== 12000, design=subset(a$pnad_design[[i]], 
                                                                           VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                           na.rm = TRUE)
  
  
  total_informais_sc_transf_12[[i]] <- as_tibble(total_informais_scarteira_12, rownames = "total_informais_scarteiral_transf_12") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  
  informais_autonomos_12.1 <- svytotal(x=~V4013== 12000, design=subset(a$pnad_design[[i]], 
                                                                       VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                       na.rm = TRUE)
  
  
  informais_autonomos_12.2 <- svytotal(x=~V4013== 12000, design=subset(a$pnad_design[[i]],
                                                                       V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos_12 <- informais_autonomos_12.1[[2]] + informais_autonomos_12.2[[2]]
  
  
  total_informais_auto_12[[i]] <- as_tibble(total_informais_autonomos_12, rownames = "total_informais_autonomos_12") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  # rendimento de todos os trabalhos efetivo dos autonomos
  
  renda_autonomos_transf_12.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013== 12000 & VD4009d %in% c("Conta-própria", 
                                                                                                 "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  renda_autonomos_transf_12.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013== 12000 & V4019d == "Não" 
                                                                  & VD4009d == "Empregador"), na.rm = TRUE)
  
  media_renda_autonomos_12 <- (renda_autonomos_transf_12.1[[1]]*informais_autonomos_12.1[[2]] + 
                                 renda_autonomos_transf_12.2[[1]]*informais_autonomos_12.2[[2]])/(
                                   informais_autonomos_12.1[[2]]+informais_autonomos_12.2[[2]])
  
  media_renda_auto_12[[i]] <- as_tibble(media_renda_autonomos_12, rownames = "media_renda_auto_12") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  
  
  ############ Fabricação de produtos têxteis - 13 - 13001 e 13002 ##################
  
  formais_13_.1 <- svytotal(x=~V4013>=13001 & V4013<=13002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_13_.2 <- svytotal(x=~V4013>=13001 & V4013<=13002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_13 <- formais_13_.1[[2]]+formais_13_.2[[2]]
  
  total_formal_transf_13[[i]] <- as_tibble(total_formal_13, rownames = "total_formal_13") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  
  # total de informais 
  
  total_informais_scarteira_13 <- svytotal(x=~V4013>=13001 & V4013<=13002, design=subset(a$pnad_design[[i]], 
                                                                                         VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                           na.rm = TRUE)
  
  total_informais_sc_transf_13[[i]] <- as_tibble(total_informais_scarteira_13, rownames = "total_informais_scarteiral_transf_13") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  
  informais_autonomos_13.1 <- svytotal(x=~V4013>=13001 & V4013<=13002, design=subset(a$pnad_design[[i]], 
                                                                                     VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                       na.rm = TRUE)
  
  
  informais_autonomos_13.2 <- svytotal(x=~V4013>=13001 & V4013<=13002, design=subset(a$pnad_design[[i]],
                                                                                     V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos_13 <- informais_autonomos_13.1[[2]] + informais_autonomos_13.2[[2]]
  
  
  total_informais_auto_13[[i]] <- as_tibble(total_informais_autonomos_13, rownames = "total_informais_autonomos_13") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  # rendimento de todos os trabalhos efetivo dos autonomos
  
  renda_autonomos_transf_13.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=13001 & V4013<=13002 & VD4009d %in% c("Conta-própria", 
                                                                                                               "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  renda_autonomos_transf_13.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=13001 & V4013<=13002 & V4019d == "Não" 
                                                                  & VD4009d == "Empregador"), na.rm = TRUE)
  
  media_renda_autonomos_13 <- (renda_autonomos_transf_13.1[[1]]*informais_autonomos_13.1[[2]] + 
                                 renda_autonomos_transf_13.2[[1]]*informais_autonomos_13.2[[2]])/(
                                   informais_autonomos_13.1[[2]]+informais_autonomos_13.2[[2]])
  
  media_renda_auto_13[[i]] <- as_tibble(media_renda_autonomos_13, rownames = "media_renda_auto_13") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  
  ############ Confeccão de artigos do vestuário e acessórios - 14 - 14001 e 14002 ##################
  
  formais_14_.1 <- svytotal(x=~V4013>=14001 & V4013<=14002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_14_.2 <- svytotal(x=~V4013>=14001 & V4013<=14002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_14 <- formais_14_.1[[2]]+formais_14_.2[[2]]
  
  
  # total de informais 
  
  total_informais_scarteira_14 <- svytotal(x=~V4013>=14001 & V4013<=14002, design=subset(a$pnad_design[[i]], 
                                                                                         VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                           na.rm = TRUE)
  
  
  informais_autonomos_14.1 <- svytotal(x=~V4013>=14001 & V4013<=14002, design=subset(a$pnad_design[[i]], 
                                                                                     VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                       na.rm = TRUE)
  
  
  informais_autonomos_14.2 <- svytotal(x=~V4013>=14001 & V4013<=14002, design=subset(a$pnad_design[[i]],
                                                                                     V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos_14 <- informais_autonomos_14.1[[2]] + informais_autonomos_14.2[[2]]
  
  # rendimento de todos os trabalhos efetivo dos autonomos
  
  renda_autonomos_transf_14.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=14001 & V4013<=14002 & VD4009d %in% c("Conta-própria", 
                                                                                                               "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  renda_autonomos_transf_14.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=14001 & V4013<=14002 & V4019d == "Não" 
                                                                  & VD4009d == "Empregador"), na.rm = TRUE)
  
  media_renda_autonomos_14 <- (renda_autonomos_transf_14.1[[1]]*informais_autonomos_14.1[[2]] + 
                                 renda_autonomos_transf_14.2[[1]]*informais_autonomos_14.2[[2]])/(
                                   informais_autonomos_14.1[[2]]+informais_autonomos_14.2[[2]])
  
  
  
  ############ PREPARAÇÃO DE COUROS E FABRICAÇÃO DE ARTEFATOS DE COURO, ARTIGOS DE VIAGEM E CALÇADOS
  #############- 15 - 15011 a 15020 ##################
  
  formais_15_.1 <- svytotal(x=~V4013>=15011 & V4013<=15020, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_15_.2 <- svytotal(x=~V4013>=15011 & V4013<=15020, design=subset(a$pnad_design[[i]], VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_15 <- formais_15_.1[[2]]+formais_15_.2[[2]]
  
  
  # total de informais 
  
  total_informais_scarteira_15 <- svytotal(x=~V4013>=15011 & V4013<=15020, design=subset(a$pnad_design[[i]], 
                                                                                         VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                           na.rm = TRUE)
  
  
  informais_autonomos_15.1 <- svytotal(x=~V4013>=15011 & V4013<=15020, design=subset(a$pnad_design[[i]], 
                                                                                     VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                       na.rm = TRUE)
  
  
  informais_autonomos_15.2 <- svytotal(x=~V4013>=15011 & V4013<=15020, design=subset(a$pnad_design[[i]],
                                                                                     V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos_15 <- informais_autonomos_15.1[[2]] + informais_autonomos_15.2[[2]]
  
  # rendimento de todos os trabalhos efetivo dos autonomos
  
  renda_autonomos_transf_15.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=15011 & V4013<=15020 & VD4009d %in% c("Conta-própria", 
                                                                                                               "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  renda_autonomos_transf_15.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=15011 & V4013<=15020 & V4019d == "Não" 
                                                                  & VD4009d == "Empregador"), na.rm = TRUE)
  
  media_renda_autonomos_15 <- (renda_autonomos_transf_15.1[[1]]*informais_autonomos_15.1[[2]] + 
                                 renda_autonomos_transf_15.2[[1]]*informais_autonomos_15.2[[2]])/(
                                   informais_autonomos_15.1[[2]]+informais_autonomos_15.2[[2]])
  
  
  
  ############ FABRICAÇÃO DE PRODUTOS DE MADEIRA - 16 - 16001 a 16002 ##################
  
  formais_16_.1 <- svytotal(x=~V4013>=16001 & V4013<=16002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_16_.2 <- svytotal(x=~V4013>=16001 & V4013<=16002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_16 <- formais_16_.1[[2]]+formais_16_.2[[2]]
  
  
  # total de informais 
  
  total_informais_scarteira_16 <- svytotal(x=~V4013>=16001 & V4013<=16002, design=subset(a$pnad_design[[i]], 
                                                                                         VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                           na.rm = TRUE)
  
  
  informais_autonomos_16.1 <- svytotal(x=~V4013>=16001 & V4013<=16002, design=subset(a$pnad_design[[i]], 
                                                                                     VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                       na.rm = TRUE)
  
  
  informais_autonomos_16.2 <- svytotal(x=~V4013>=16001 & V4013<=16002, design=subset(a$pnad_design[[i]],
                                                                                     V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos_16 <- informais_autonomos_16.1[[2]] + informais_autonomos_16.2[[2]]
  
  # rendimento de todos os trabalhos efetivo dos autonomos
  
  renda_autonomos_transf_16.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=16001 & V4013<=16002  & VD4009d %in% c("Conta-própria", 
                                                                                                                "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  renda_autonomos_transf_16.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=16001 & V4013<=16002 & V4019d == "Não" 
                                                                  & VD4009d == "Empregador"), na.rm = TRUE)
  
  media_renda_autonomos_16 <- (renda_autonomos_transf_16.1[[1]]*informais_autonomos_16.1[[2]] + 
                                 renda_autonomos_transf_16.2[[1]]*informais_autonomos_16.2[[2]])/(
                                   informais_autonomos_16.1[[2]]+informais_autonomos_16.2[[2]])
  
  
  ############ FABRICAÇÃO DE CELULOSE, PAPEL E PRODUTOS DE PAPEL - 17 - 17001 a 17002 ##################
  
  formais_17_.1 <- svytotal(x=~V4013>=17001 & V4013<=17002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_17_.2 <- svytotal(x=~V4013>=17001 & V4013<=17002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_17 <- formais_17_.1[[2]]+formais_17_.2[[2]]
  
  
  # total de informais 
  
  total_informais_scarteira_17 <- svytotal(x=~V4013>=17001 & V4013<=17002, design=subset(a$pnad_design[[i]], 
                                                                                         VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                           na.rm = TRUE)
  
  
  informais_autonomos_17.1 <- svytotal(x=~V4013>=17001 & V4013<=17002, design=subset(a$pnad_design[[i]], 
                                                                                     VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                       na.rm = TRUE)
  
  
  informais_autonomos_17.2 <- svytotal(x=~V4013>=17001 & V4013<=17002, design=subset(a$pnad_design[[i]],
                                                                                     V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos_17 <- informais_autonomos_17.1[[2]] + informais_autonomos_17.2[[2]]
  
  # rendimento de todos os trabalhos efetivo dos autonomos
  
  renda_autonomos_transf_17.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=17001 & V4013<=17002  & VD4009d %in% c("Conta-própria", 
                                                                                                                "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  renda_autonomos_transf_17.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=17001 & V4013<=17002 & V4019d == "Não" 
                                                                  & VD4009d == "Empregador"), na.rm = TRUE)
  
  media_renda_autonomos_17 <- (renda_autonomos_transf_17.1[[1]]*informais_autonomos_17.1[[2]] + 
                                 renda_autonomos_transf_17.2[[1]]*informais_autonomos_17.2[[2]])/(
                                   informais_autonomos_17.1[[2]]+informais_autonomos_17.2[[2]])
  
  
  ############ IMPRESSÃO E REPRODUÇÃO DE GRAVAÇÕES - 18 - 18000 ############
  
  formais_18_.1 <- svytotal(x=~V4013== 18000, design=subset(a$pnad_design[[i]], VD4009d == 
                                                              "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_18_.2 <- svytotal(x=~V4013== 18000, design=subset(a$pnad_design[[i]], VD4009d == 
                                                              "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_18 <- formais_18_.1[[2]]+formais_18_.2[[2]]
  
  
  # total de informais 
  
  total_informais_scarteira_18 <- svytotal(x=~V4013== 18000, design=subset(a$pnad_design[[i]], 
                                                                           VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                           na.rm = TRUE)
  
  
  informais_autonomos_18.1 <- svytotal(x=~V4013== 18000, design=subset(a$pnad_design[[i]], 
                                                                       VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                       na.rm = TRUE)
  
  
  informais_autonomos_18.2 <- svytotal(x=~V4013== 18000, design=subset(a$pnad_design[[i]],
                                                                       V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos_18 <- informais_autonomos_18.1[[2]] + informais_autonomos_18.2[[2]]
  
  
  # rendimento de todos os trabalhos efetivo dos autonomos
  
  renda_autonomos_transf_18.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013== 18000 & VD4009d %in% c("Conta-própria", 
                                                                                                 "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  renda_autonomos_transf_18.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013== 18000 & V4019d == "Não" 
                                                                  & VD4009d == "Empregador"), na.rm = TRUE)
  
  media_renda_autonomos_18 <- (renda_autonomos_transf_18.1[[1]]*informais_autonomos_18.1[[2]] + 
                                 renda_autonomos_transf_18.2[[1]]*informais_autonomos_18.2[[2]])/(
                                   informais_autonomos_18.1[[2]]+informais_autonomos_18.2[[2]])
  
  
  ############ FABRICAÇÃO DE COQUE; PRODUTOS DERIVADOS DE PETRÓLEO E DE BIOCOMBUSTÍVEIS - 19 - 19010 a 19030 ##################
  
  formais_19_.1 <- svytotal(x=~V4013>=19010 & V4013<=19030, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_19_.2 <- svytotal(x=~V4013>=19010 & V4013<=19030, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_19 <- formais_19_.1[[2]]+formais_19_.2[[2]]
  
  
  # total de informais 
  
  total_informais_scarteira_19 <- svytotal(x=~V4013>=19010 & V4013<=19030, design=subset(a$pnad_design[[i]], 
                                                                                         VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                           na.rm = TRUE)
  
  
  informais_autonomos_19.1 <- svytotal(x=~V4013>=19010 & V4013<=19030, design=subset(a$pnad_design[[i]], 
                                                                                     VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                       na.rm = TRUE)
  
  
  informais_autonomos_19.2 <- svytotal(x=~V4013>=19010 & V4013<=19030, design=subset(a$pnad_design[[i]],
                                                                                     V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos_19 <- informais_autonomos_19.1[[2]] + informais_autonomos_19.2[[2]]
  
  # rendimento de todos os trabalhos efetivo dos autonomos
  
  renda_autonomos_transf_19.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=19010 & V4013<=19030  & VD4009d %in% c("Conta-própria", 
                                                                                                                "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  renda_autonomos_transf_19.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=19010 & V4013<=19030 & V4019d == "Não" 
                                                                  & VD4009d == "Empregador"), na.rm = TRUE)
  
  media_renda_autonomos_19 <- (renda_autonomos_transf_19.1[[1]]*informais_autonomos_19.1[[2]] + 
                                 renda_autonomos_transf_19.2[[1]]*informais_autonomos_19.2[[2]])/(
                                   informais_autonomos_19.1[[2]]+informais_autonomos_19.2[[2]])
  
  
  
  ############ FABRICAÇÃO DE PRODUTOS QUÍMICOS- 20 - 20010 a 20090 ##################
  
  formais_20_.1 <- svytotal(x=~V4013>=20010 & V4013<=20090, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_20_.2 <- svytotal(x=~V4013>=20010 & V4013<=20090, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_20 <- formais_20_.1[[2]]+formais_20_.2[[2]]
  
  
  # total de informais 
  
  total_informais_scarteira_20 <- svytotal(x=~V4013>=20010 & V4013<=20090, design=subset(a$pnad_design[[i]], 
                                                                                         VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                           na.rm = TRUE)
  
  
  informais_autonomos_20.1 <- svytotal(x=~V4013>=20010 & V4013<=20090, design=subset(a$pnad_design[[i]], 
                                                                                     VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                       na.rm = TRUE)
  
  
  informais_autonomos_20.2 <- svytotal(x=~V4013>=20010 & V4013<=20090, design=subset(a$pnad_design[[i]],
                                                                                     V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos_20 <- informais_autonomos_20.1[[2]] + informais_autonomos_20.2[[2]]
  
  # rendimento de todos os trabalhos efetivo dos autonomos
  
  renda_autonomos_transf_20.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=20010 & V4013<=20090  & VD4009d %in% c("Conta-própria", 
                                                                                                                "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  renda_autonomos_transf_20.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=20010 & V4013<=20090 & V4019d == "Não" 
                                                                  & VD4009d == "Empregador"), na.rm = TRUE)
  
  media_renda_autonomos_20 <- (renda_autonomos_transf_20.1[[1]]*informais_autonomos_20.1[[2]] + 
                                 renda_autonomos_transf_20.2[[1]]*informais_autonomos_20.2[[2]])/(
                                   informais_autonomos_20.1[[2]]+informais_autonomos_20.2[[2]])
  
  
  
  ############ FABRICAÇÃO DE PRODUTOS FARMOQUÍMICOS E FARMACÊUTICOS- 21 - 21000 ############
  
  formais_21_.1 <- svytotal(x=~V4013== 21000, design=subset(a$pnad_design[[i]], VD4009d == 
                                                              "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_21_.2 <- svytotal(x=~V4013== 21000, design=subset(a$pnad_design[[i]], VD4009d == 
                                                              "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_21 <- formais_21_.1[[2]]+formais_21_.2[[2]]
  
  
  # total de informais 
  
  total_informais_scarteira_21 <- svytotal(x=~V4013== 21000, design=subset(a$pnad_design[[i]], 
                                                                           VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                           na.rm = TRUE)
  
  
  informais_autonomos_21.1 <- svytotal(x=~V4013== 21000, design=subset(a$pnad_design[[i]], 
                                                                       VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                       na.rm = TRUE)
  
  
  informais_autonomos_21.2 <- svytotal(x=~V4013== 21000, design=subset(a$pnad_design[[i]],
                                                                       V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos_21 <- informais_autonomos_21.1[[2]] + informais_autonomos_21.2[[2]]
  
  # rendimento de todos os trabalhos efetivo dos autonomos
  
  renda_autonomos_transf_21.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013== 21000 & VD4009d %in% c("Conta-própria", 
                                                                                                 "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  renda_autonomos_transf_21.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013== 21000 & V4019d == "Não" 
                                                                  & VD4009d == "Empregador"), na.rm = TRUE)
  
  media_renda_autonomos_21 <- (renda_autonomos_transf_21.1[[1]]*informais_autonomos_21.1[[2]] + 
                                 renda_autonomos_transf_21.2[[1]]*informais_autonomos_21.2[[2]])/(
                                   informais_autonomos_21.1[[2]]+informais_autonomos_21.2[[2]])
  
  
  
  ############ FABRICAÇÃO DE PRODUTOS DE BORRACHA E DE MATERIAL PLÁSTICO - 22 - 22010 e 22020 ##################
  
  formais_22_.1 <- svytotal(x=~V4013>=22010 & V4013<=22020, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_22_.2 <- svytotal(x=~V4013>=22010 & V4013<=22020, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_22 <- formais_22_.1[[2]]+formais_22_.2[[2]]
  
  
  # total de informais 
  
  total_informais_scarteira_22 <- svytotal(x=~V4013>=22010 & V4013<=22020, design=subset(a$pnad_design[[i]], 
                                                                                         VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                           na.rm = TRUE)
  
  
  informais_autonomos_22.1 <- svytotal(x=~V4013>=22010 & V4013<=22020, design=subset(a$pnad_design[[i]], 
                                                                                     VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                       na.rm = TRUE)
  
  
  informais_autonomos_22.2 <- svytotal(x=~V4013>=22010 & V4013<=22020, design=subset(a$pnad_design[[i]],
                                                                                     V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos_22 <- informais_autonomos_22.1[[2]] + informais_autonomos_22.2[[2]]
  
  # rendimento de todos os trabalhos efetivo dos autonomos
  
  renda_autonomos_transf_22.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=22010 & V4013<=2202  & VD4009d %in% c("Conta-própria", 
                                                                                                               "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  renda_autonomos_transf_22.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=22010 & V4013<=2202 & V4019d == "Não" 
                                                                  & VD4009d == "Empregador"), na.rm = TRUE)
  
  media_renda_autonomos_22 <- (renda_autonomos_transf_22.1[[1]]*informais_autonomos_22.1[[2]] + 
                                 renda_autonomos_transf_22.2[[1]]*informais_autonomos_22.2[[2]])/(
                                   informais_autonomos_22.1[[2]]+informais_autonomos_22.2[[2]])
  
  
  ############ FABRICAÇÃO DE PRODUTOS DE MINERAIS NÃO-METÁLICOS - 23 - 23010 e 23099 ##################
  
  formais_23_.1 <- svytotal(x=~V4013>=23010 & V4013<=23099, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_23_.2 <- svytotal(x=~V4013>=23010 & V4013<=23099, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_23 <- formais_23_.1[[2]]+formais_23_.2[[2]]
  
  
  # total de informais 
  
  total_informais_scarteira_23 <- svytotal(x=~V4013>=23010 & V4013<=23099, design=subset(a$pnad_design[[i]], 
                                                                                         VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                           na.rm = TRUE)
  
  
  informais_autonomos_23.1 <- svytotal(x=~V4013>=23010 & V4013<=23099, design=subset(a$pnad_design[[i]], 
                                                                                     VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                       na.rm = TRUE)
  
  
  informais_autonomos_23.2 <- svytotal(x=~V4013>=23010 & V4013<=23099, design=subset(a$pnad_design[[i]],
                                                                                     V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos_23 <- informais_autonomos_23.1[[2]] + informais_autonomos_23.2[[2]]
  
  # rendimento de todos os trabalhos efetivo dos autonomos
  
  renda_autonomos_transf_23.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=23010 & V4013<=23099  & VD4009d %in% c("Conta-própria", 
                                                                                                                "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  renda_autonomos_transf_23.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=23010 & V4013<=23099 & V4019d == "Não" 
                                                                  & VD4009d == "Empregador"), na.rm = TRUE)
  
  media_renda_autonomos_23 <- (renda_autonomos_transf_23.1[[1]]*informais_autonomos_23.1[[2]] + 
                                 renda_autonomos_transf_23.2[[1]]*informais_autonomos_23.2[[2]])/(
                                   informais_autonomos_23.1[[2]]+informais_autonomos_23.2[[2]])
  
  
  ############ METALURGIA - 24 - 24001 e 24003 ##################
  
  formais_24_.1 <- svytotal(x=~V4013>=24001 & V4013<=24003, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_24_.2 <- svytotal(x=~V4013>=24001 & V4013<=24003, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_24 <- formais_24_.1[[2]]+formais_24_.2[[2]]
  
  
  # total de informais 
  
  total_informais_scarteira_24 <- svytotal(x=~V4013>=24001 & V4013<=24003, design=subset(a$pnad_design[[i]], 
                                                                                         VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                           na.rm = TRUE)
  
  
  informais_autonomos_24.1 <- svytotal(x=~V4013>=24001 & V4013<=24003, design=subset(a$pnad_design[[i]], 
                                                                                     VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                       na.rm = TRUE)
  
  
  informais_autonomos_24.2 <- svytotal(x=~V4013>=24001 & V4013<=24003, design=subset(a$pnad_design[[i]],
                                                                                     V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos_24 <- informais_autonomos_24.1[[2]] + informais_autonomos_24.2[[2]]
  
  # rendimento de todos os trabalhos efetivo dos autonomos
  
  renda_autonomos_transf_24.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=24001 & V4013<=24003  & VD4009d %in% c("Conta-própria", 
                                                                                                                "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  renda_autonomos_transf_24.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=24001 & V4013<=24003 & V4019d == "Não" 
                                                                  & VD4009d == "Empregador"), na.rm = TRUE)
  
  media_renda_autonomos_24 <- (renda_autonomos_transf_24.1[[1]]*informais_autonomos_24.1[[2]] + 
                                 renda_autonomos_transf_24.2[[1]]*informais_autonomos_24.2[[2]])/(
                                   informais_autonomos_24.1[[2]]+informais_autonomos_24.2[[2]])
  
  
  
  ############ FABRICAÇÃO DE PRODUTOS DE METAL, EXCETO MÁQUINAS E EQUIPAMENTOS - 25 - 25001 e 25002 ##################
  
  formais_25_.1 <- svytotal(x=~V4013>=25001 & V4013<=25002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_25_.2 <- svytotal(x=~V4013>=25001 & V4013<=25002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_25 <- formais_25_.1[[2]]+formais_25_.2[[2]]
  
  
  # total de informais 
  
  total_informais_scarteira_25 <- svytotal(x=~V4013>=25001 & V4013<=25002, design=subset(a$pnad_design[[i]], 
                                                                                         VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                           na.rm = TRUE)
  
  
  informais_autonomos_25.1 <- svytotal(x=~V4013>=25001 & V4013<=25002, design=subset(a$pnad_design[[i]], 
                                                                                     VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                       na.rm = TRUE)
  
  
  informais_autonomos_25.2 <- svytotal(x=~V4013>=25001 & V4013<=25002, design=subset(a$pnad_design[[i]],
                                                                                     V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos_25 <- informais_autonomos_25.1[[2]] + informais_autonomos_25.2[[2]]
  
  # rendimento de todos os trabalhos efetivo dos autonomos
  
  renda_autonomos_transf_25.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=25001 & V4013<=25002  & VD4009d %in% c("Conta-própria", 
                                                                                                                "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  renda_autonomos_transf_25.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=25001 & V4013<=25002 & V4019d == "Não" 
                                                                  & VD4009d == "Empregador"), na.rm = TRUE)
  
  media_renda_autonomos_25 <- (renda_autonomos_transf_25.1[[1]]*informais_autonomos_25.1[[2]] + 
                                 renda_autonomos_transf_25.2[[1]]*informais_autonomos_25.2[[2]])/(
                                   informais_autonomos_25.1[[2]]+informais_autonomos_25.2[[2]])
  
  
  ############ FABRICAÇÃO DE EQUIPAMENTOS DE INFORMÁTICA, PRODUTOS ELETRÔNICOS E ÓPTICOS - 26 - 26010 a 26042 ##################
  
  formais_26_.1 <- svytotal(x=~V4013>=26010 & V4013<=26042, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_26_.2 <- svytotal(x=~V4013>=26010 & V4013<=26042, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_26 <- formais_26_.1[[2]]+formais_26_.2[[2]]
  
  
  # total de informais 
  
  total_informais_scarteira_26 <- svytotal(x=~V4013>=26010 & V4013<=26042, design=subset(a$pnad_design[[i]], 
                                                                                         VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                           na.rm = TRUE)
  
  
  informais_autonomos_26.1 <- svytotal(x=~V4013>=26010 & V4013<=26042, design=subset(a$pnad_design[[i]], 
                                                                                     VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                       na.rm = TRUE)
  
  
  informais_autonomos_26.2 <- svytotal(x=~V4013>=26010 & V4013<=26042, design=subset(a$pnad_design[[i]],
                                                                                     V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos_26 <- informais_autonomos_26.1[[2]] + informais_autonomos_26.2[[2]]
  
  # rendimento de todos os trabalhos efetivo dos autonomos
  
  renda_autonomos_transf_26.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=26010 & V4013<=26042  & VD4009d %in% c("Conta-própria", 
                                                                                                                "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  renda_autonomos_transf_26.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=26010 & V4013<=26042 & V4019d == "Não" 
                                                                  & VD4009d == "Empregador"), na.rm = TRUE)
  
  media_renda_autonomos_26 <- (renda_autonomos_transf_26.1[[1]]*informais_autonomos_26.1[[2]] + 
                                 renda_autonomos_transf_26.2[[1]]*informais_autonomos_26.2[[2]])/(
                                   informais_autonomos_26.1[[2]]+informais_autonomos_26.2[[2]])
  
  
  
  ############ FABRICAÇÃO DE MÁQUINAS, APARELHOS E MATERIAIS ELÉTRICOS - 27 - 27010 a 27090 ##################
  
  formais_27_.1 <- svytotal(x=~V4013>=27010 & V4013<=27090, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_27_.2 <- svytotal(x=~V4013>=27010 & V4013<=27090, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_27 <- formais_27_.1[[2]]+formais_27_.2[[2]]
  
  
  # total de informais 
  
  total_informais_scarteira_27 <- svytotal(x=~V4013>=27010 & V4013<=27090, design=subset(a$pnad_design[[i]], 
                                                                                         VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                           na.rm = TRUE)
  
  
  informais_autonomos_27.1 <- svytotal(x=~V4013>=27010 & V4013<=27090, design=subset(a$pnad_design[[i]], 
                                                                                     VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                       na.rm = TRUE)
  
  
  informais_autonomos_27.2 <- svytotal(x=~V4013>=27010 & V4013<=27090, design=subset(a$pnad_design[[i]],
                                                                                     V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos_27 <- informais_autonomos_27.1[[2]] + informais_autonomos_27.2[[2]]
  
  # rendimento de todos os trabalhos efetivo dos autonomos
  
  renda_autonomos_transf_27.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=27010 & V4013<=27090  & VD4009d %in% c("Conta-própria", 
                                                                                                                "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  renda_autonomos_transf_27.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=27010 & V4013<=27090 & V4019d == "Não" 
                                                                  & VD4009d == "Empregador"), na.rm = TRUE)
  
  media_renda_autonomos_27 <- (renda_autonomos_transf_27.1[[1]]*informais_autonomos_27.1[[2]] + 
                                 renda_autonomos_transf_27.2[[1]]*informais_autonomos_27.2[[2]])/(
                                   informais_autonomos_27.1[[2]]+informais_autonomos_27.2[[2]])
  
  
  
  ############ FABRICAÇÃO DE MÁQUINAS E EQUIPAMENTOS- 28 - 28000 ############
  
  
  formais_28_.1 <- svytotal(x=~V4013== 28000, design=subset(a$pnad_design[[i]], VD4009d == 
                                                              "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_28_.2 <- svytotal(x=~V4013== 28000, design=subset(a$pnad_design[[i]], VD4009d == 
                                                              "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_28 <- formais_28_.1[[2]]+formais_28_.2[[2]]
  
  
  # total de informais 
  
  total_informais_scarteira_28 <- svytotal(x=~V4013== 28000, design=subset(a$pnad_design[[i]], 
                                                                           VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                           na.rm = TRUE)
  
  
  informais_autonomos_28.1 <- svytotal(x=~V4013== 28000, design=subset(a$pnad_design[[i]], 
                                                                       VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                       na.rm = TRUE)
  
  
  informais_autonomos_28.2 <- svytotal(x=~V4013== 28000, design=subset(a$pnad_design[[i]],
                                                                       V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos_28 <- informais_autonomos_28.1[[2]] + informais_autonomos_28.2[[2]]
  
  # rendimento de todos os trabalhos efetivo dos autonomos
  
  renda_autonomos_transf_28.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013== 28000 & VD4009d %in% c("Conta-própria", 
                                                                                                 "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  renda_autonomos_transf_28.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013== 28000 & V4019d == "Não" 
                                                                  & VD4009d == "Empregador"), na.rm = TRUE)
  
  media_renda_autonomos_28 <- (renda_autonomos_transf_28.1[[1]]*informais_autonomos_28.1[[2]] + 
                                 renda_autonomos_transf_28.2[[1]]*informais_autonomos_28.2[[2]])/(
                                   informais_autonomos_28.1[[2]]+informais_autonomos_28.2[[2]])
  
  
  
  
  ############ FABRICAÇÃO DE VEÍCULOS AUTOMOTORES, REBOQUES E CARROCERIAS - 29 - 29001 a 29003 ##################
  
  formais_29_.1 <- svytotal(x=~V4013>=29001 & V4013<=29003, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_29_.2 <- svytotal(x=~V4013>=29001 & V4013<=29003, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_29 <- formais_29_.1[[2]]+formais_29_.2[[2]]
  
  
  # total de informais 
  
  total_informais_scarteira_29 <- svytotal(x=~V4013>=29001 & V4013<=29003, design=subset(a$pnad_design[[i]], 
                                                                                         VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                           na.rm = TRUE)
  
  
  informais_autonomos_29.1 <- svytotal(x=~V4013>=29001 & V4013<=29003, design=subset(a$pnad_design[[i]], 
                                                                                     VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                       na.rm = TRUE)
  
  
  informais_autonomos_29.2 <- svytotal(x=~V4013>=29001 & V4013<=29003, design=subset(a$pnad_design[[i]],
                                                                                     V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos_29 <- informais_autonomos_29.1[[2]] + informais_autonomos_29.2[[2]]
  
  
  # rendimento de todos os trabalhos efetivo dos autonomos
  
  renda_autonomos_transf_29.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=29001 & V4013<=29003  & VD4009d %in% c("Conta-própria", 
                                                                                                                "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  renda_autonomos_transf_29.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=29001 & V4013<=29003 & V4019d == "Não" 
                                                                  & VD4009d == "Empregador"), na.rm = TRUE)
  
  media_renda_autonomos_29 <- (renda_autonomos_transf_29.1[[1]]*informais_autonomos_29.1[[2]] + 
                                 renda_autonomos_transf_29.2[[1]]*informais_autonomos_29.2[[2]])/(
                                   informais_autonomos_29.1[[2]]+informais_autonomos_29.2[[2]])
  
  
  ############ FABRICAÇÃO DE OUTROS EQUIPAMENTOS DE TRANSPORTE, EXCETO VEÍCULOS AUTOMOTORES - 30 - 30010 a 30090 ##################
  
  formais_30_.1 <- svytotal(x=~V4013>=30010 & V4013<=30090, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_30_.2 <- svytotal(x=~V4013>=30010 & V4013<=30090, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_30 <- formais_30_.1[[2]]+formais_30_.2[[2]]
  
  
  # total de informais 
  
  total_informais_scarteira_30 <- svytotal(x=~V4013>=30010 & V4013<=30090, design=subset(a$pnad_design[[i]], 
                                                                                         VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                           na.rm = TRUE)
  
  
  informais_autonomos_30.1 <- svytotal(x=~V4013>=30010 & V4013<=30090, design=subset(a$pnad_design[[i]], 
                                                                                     VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                       na.rm = TRUE)
  
  
  informais_autonomos_30.2 <- svytotal(x=~V4013>=30010 & V4013<=30090, design=subset(a$pnad_design[[i]],
                                                                                     V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos_30 <- informais_autonomos_30.1[[2]] + informais_autonomos_30.2[[2]]
  
  # rendimento de todos os trabalhos efetivo dos autonomos
  
  renda_autonomos_transf_30.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=30010 & V4013<=30090  & VD4009d %in% c("Conta-própria", 
                                                                                                                "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  renda_autonomos_transf_30.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=30010 & V4013<=30090 & V4019d == "Não" 
                                                                  & VD4009d == "Empregador"), na.rm = TRUE)
  
  media_renda_autonomos_30 <- (renda_autonomos_transf_30.1[[1]]*informais_autonomos_30.1[[2]] + 
                                 renda_autonomos_transf_30.2[[1]]*informais_autonomos_30.2[[2]])/(
                                   informais_autonomos_30.1[[2]]+informais_autonomos_30.2[[2]])
  
  
  
  ############ FABRICAÇÃO DE MÓVEIS- 31 - 31000 ############
  
  
  formais_31_.1 <- svytotal(x=~V4013== 31000, design=subset(a$pnad_design[[i]], VD4009d == 
                                                              "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_31_.2 <- svytotal(x=~V4013== 31000, design=subset(a$pnad_design[[i]], VD4009d == 
                                                              "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_31 <- formais_31_.1[[2]]+formais_31_.2[[2]]
  
  
  # total de informais 
  
  total_informais_scarteira_31 <- svytotal(x=~V4013== 31000, design=subset(a$pnad_design[[i]], 
                                                                           VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                           na.rm = TRUE)
  
  
  informais_autonomos_31.1 <- svytotal(x=~V4013== 31000, design=subset(a$pnad_design[[i]], 
                                                                       VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                       na.rm = TRUE)
  
  
  informais_autonomos_31.2 <- svytotal(x=~V4013== 31000, design=subset(a$pnad_design[[i]],
                                                                       V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos_31 <- informais_autonomos_31.1[[2]] + informais_autonomos_31.2[[2]]
  
  # rendimento de todos os trabalhos efetivo dos autonomos
  
  renda_autonomos_transf_31.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013== 31000 & VD4009d %in% c("Conta-própria", 
                                                                                                 "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  renda_autonomos_transf_31.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013== 31000 & V4019d == "Não" 
                                                                  & VD4009d == "Empregador"), na.rm = TRUE)
  
  media_renda_autonomos_31 <- (renda_autonomos_transf_31.1[[1]]*informais_autonomos_31.1[[2]] + 
                                 renda_autonomos_transf_31.2[[1]]*informais_autonomos_31.2[[2]])/(
                                   informais_autonomos_31.1[[2]]+informais_autonomos_31.2[[2]])
  
  
  
  
  ############ FABRICAÇÃO DE PRODUTOS DIVERSOS - 32 - 32001 a 32009 ##################
  
  formais_32_.1 <- svytotal(x=~V4013>=32001 & V4013<=32009, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_32_.2 <- svytotal(x=~V4013>=32001 & V4013<=32009, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_32 <- formais_32_.1[[2]]+formais_32_.2[[2]]
  
  
  # total de informais 
  
  total_informais_scarteira_32 <- svytotal(x=~V4013>=32001 & V4013<=32009, design=subset(a$pnad_design[[i]], 
                                                                                         VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                           na.rm = TRUE)
  
  
  informais_autonomos_32.1 <- svytotal(x=~V4013>=32001 & V4013<=32009, design=subset(a$pnad_design[[i]], 
                                                                                     VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                       na.rm = TRUE)
  
  
  informais_autonomos_32.2 <- svytotal(x=~V4013>=32001 & V4013<=32009, design=subset(a$pnad_design[[i]],
                                                                                     V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos_32 <- informais_autonomos_32.1[[2]] + informais_autonomos_32.2[[2]]
  
  
  
  # rendimento de todos os trabalhos efetivo dos autonomos
  
  renda_autonomos_transf_32.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=32001 & V4013<=32009  & VD4009d %in% c("Conta-própria", 
                                                                                                                "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  renda_autonomos_transf_32.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=32001 & V4013<=32009 & V4019d == "Não" 
                                                                  & VD4009d == "Empregador"), na.rm = TRUE)
  
  media_renda_autonomos_32 <- (renda_autonomos_transf_32.1[[1]]*informais_autonomos_32.1[[2]] + 
                                 renda_autonomos_transf_32.2[[1]]*informais_autonomos_32.2[[2]])/(
                                   informais_autonomos_32.1[[2]]+informais_autonomos_32.2[[2]])
  
  
  
  ############ MANUTENÇÃO, REPARAÇÃO E INSTALAÇÃO DE MÁQUINAS E EQUIPAMENTOS - 33 - 33001 a 33002 ##################
  
  formais_33_.1 <- svytotal(x=~V4013>=33001 & V4013<=33002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregador" & V4019d == "Sim"), na.rm = TRUE) 
  
  formais_33_.2 <- svytotal(x=~V4013>=33001 & V4013<=33002, design=subset(a$pnad_design[[i]], VD4009d == 
                                                                            "Empregado no setor privado com carteira de trabalho assinada"), na.rm = TRUE)
  
  
  total_formal_33 <- formais_33_.1[[2]]+formais_33_.2[[2]]
  
  
  # total de informais 
  
  total_informais_scarteira_33 <- svytotal(x=~V4013>=33001 & V4013<=33002, design=subset(a$pnad_design[[i]], 
                                                                                         VD4009d == "Empregado no setor privado sem carteira de trabalho assinada"),
                                           na.rm = TRUE)
  
  
  informais_autonomos_33.1 <- svytotal(x=~V4013>=33001 & V4013<=33002, design=subset(a$pnad_design[[i]], 
                                                                                     VD4009d %in% c("Conta-própria", "Trabalhador familiar auxiliar")), 
                                       na.rm = TRUE)
  
  
  informais_autonomos_33.2 <- svytotal(x=~V4013>=33001 & V4013<=33002, design=subset(a$pnad_design[[i]],
                                                                                     V4019d == "Não" & VD4009d == "Empregador"), na.rm = TRUE)
  
  
  total_informais_autonomos_33 <- informais_autonomos_33.1[[2]] + informais_autonomos_33.2[[2]]
  
  
  # rendimento de todos os trabalhos efetivo dos autonomos
  
  renda_autonomos_transf_33.1 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=33001 & V4013<=33002  & VD4009d %in% c("Conta-própria", 
                                                                                                                "Trabalhador familiar auxiliar")), na.rm = TRUE)
  
  
  renda_autonomos_transf_33.2 <- svymean(x=~VD4020, design=subset(a$pnad_design[[i]], 
                                                                  V4013>=33001 & V4013<=33002 & V4019d == "Não" 
                                                                  & VD4009d == "Empregador"), na.rm = TRUE)
  
  media_renda_autonomos_33 <- (renda_autonomos_transf_33.1[[1]]*informais_autonomos_33.1[[2]] + 
                                 renda_autonomos_transf_33.2[[1]]*informais_autonomos_33.2[[2]])/(
                                   informais_autonomos_33.1[[2]]+informais_autonomos_33.2[[2]])
  
  
  
  
  
  
  
  
  
  #####
  
  total_formal_transf_14[[i]] <- as_tibble(total_formal_14, rownames = "total_formal_14") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_informais_sc_transf_14[[i]] <- as_tibble(total_informais_scarteira_14, rownames = "total_informais_scarteiral_transf_14") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  total_informais_auto_14[[i]] <- as_tibble(total_informais_autonomos_14, rownames = "total_informais_autonomos_14") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  media_renda_auto_14[[i]] <- as_tibble(media_renda_autonomos_14, rownames = "media_renda_auto_14") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  
  total_formal_transf_15[[i]] <- as_tibble(total_formal_15, rownames = "total_formal_15") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_informais_sc_transf_15[[i]] <- as_tibble(total_informais_scarteira_15, rownames = "total_informais_scarteiral_transf_15") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  total_informais_auto_15[[i]] <- as_tibble(total_informais_autonomos_15, rownames = "total_informais_autonomos_15") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  media_renda_auto_15[[i]] <- as_tibble(media_renda_autonomos_15, rownames = "media_renda_auto_15") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_formal_transf_16[[i]] <- as_tibble(total_formal_16, rownames = "total_formal_16") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_informais_sc_transf_16[[i]] <- as_tibble(total_informais_scarteira_16, rownames = "total_informais_scarteiral_transf_16") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  total_informais_auto_16[[i]] <- as_tibble(total_informais_autonomos_16, rownames = "total_informais_autonomos_16") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  media_renda_auto_16[[i]] <- as_tibble(media_renda_autonomos_16, rownames = "media_renda_auto_16") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  total_formal_transf_17[[i]] <- as_tibble(total_formal_17, rownames = "total_formal_17") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_informais_sc_transf_17[[i]] <- as_tibble(total_informais_scarteira_17, rownames = "total_informais_scarteiral_transf_17") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  total_informais_auto_17[[i]] <- as_tibble(total_informais_autonomos_17, rownames = "total_informais_autonomos_17") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  media_renda_auto_17[[i]] <- as_tibble(media_renda_autonomos_17, rownames = "media_renda_auto_17") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_formal_transf_18[[i]] <- as_tibble(total_formal_18, rownames = "total_formal_18") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_informais_sc_transf_18[[i]] <- as_tibble(total_informais_scarteira_18, rownames = "total_informais_scarteiral_transf_18") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  total_informais_auto_18[[i]] <- as_tibble(total_informais_autonomos_18, rownames = "total_informais_autonomos_18") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  media_renda_auto_18[[i]] <- as_tibble(media_renda_autonomos_18, rownames = "media_renda_auto_18") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_formal_transf_19[[i]] <- as_tibble(total_formal_19, rownames = "total_formal_19") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_informais_sc_transf_19[[i]] <- as_tibble(total_informais_scarteira_19, rownames = "total_informais_scarteiral_transf_19") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  total_informais_auto_19[[i]] <- as_tibble(total_informais_autonomos_19, rownames = "total_informais_autonomos_19") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  media_renda_auto_19[[i]] <- as_tibble(media_renda_autonomos_19, rownames = "media_renda_auto_19") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_formal_transf_20[[i]] <- as_tibble(total_formal_20, rownames = "total_formal_20") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_informais_sc_transf_20[[i]] <- as_tibble(total_informais_scarteira_20, rownames = "total_informais_scarteiral_transf_20") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  total_informais_auto_20[[i]] <- as_tibble(total_informais_autonomos_20, rownames = "total_informais_autonomos_20") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  media_renda_auto_20[[i]] <- as_tibble(media_renda_autonomos_20, rownames = "media_renda_auto_20") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_formal_transf_21[[i]] <- as_tibble(total_formal_21, rownames = "total_formal_21") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_informais_sc_transf_21[[i]] <- as_tibble(total_informais_scarteira_21, rownames = "total_informais_scarteiral_transf_21") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  total_informais_auto_21[[i]] <- as_tibble(total_informais_autonomos_21, rownames = "total_informais_autonomos_21") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  media_renda_auto_21[[i]] <- as_tibble(media_renda_autonomos_21, rownames = "media_renda_auto_21") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_formal_transf_22[[i]] <- as_tibble(total_formal_22, rownames = "total_formal_22") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_informais_sc_transf_22[[i]] <- as_tibble(total_informais_scarteira_22, rownames = "total_informais_scarteiral_transf_22") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  total_informais_auto_22[[i]] <- as_tibble(total_informais_autonomos_22, rownames = "total_informais_autonomos_22") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  media_renda_auto_22[[i]] <- as_tibble(media_renda_autonomos_22, rownames = "media_renda_auto_22") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_formal_transf_23[[i]] <- as_tibble(total_formal_23, rownames = "total_formal_23") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_informais_sc_transf_23[[i]] <- as_tibble(total_informais_scarteira_23, rownames = "total_informais_scarteiral_transf_23") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  total_informais_auto_23[[i]] <- as_tibble(total_informais_autonomos_23, rownames = "total_informais_autonomos_23") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  media_renda_auto_23[[i]] <- as_tibble(media_renda_autonomos_23, rownames = "media_renda_auto_23") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_formal_transf_24[[i]] <- as_tibble(total_formal_24, rownames = "total_formal_24") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_informais_sc_transf_24[[i]] <- as_tibble(total_informais_scarteira_24, rownames = "total_informais_scarteiral_transf_24") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  total_informais_auto_24[[i]] <- as_tibble(total_informais_autonomos_24, rownames = "total_informais_autonomos_24") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  media_renda_auto_24[[i]] <- as_tibble(media_renda_autonomos_24, rownames = "media_renda_auto_24") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  total_formal_transf_25[[i]] <- as_tibble(total_formal_25, rownames = "total_formal_25") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_informais_sc_transf_25[[i]] <- as_tibble(total_informais_scarteira_25, rownames = "total_informais_scarteiral_transf_25") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  total_informais_auto_25[[i]] <- as_tibble(total_informais_autonomos_25, rownames = "total_informais_autonomos_25") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  media_renda_auto_25[[i]] <- as_tibble(media_renda_autonomos_25, rownames = "media_renda_auto_25") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  
  total_formal_transf_26[[i]] <- as_tibble(total_formal_26, rownames = "total_formal_26") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_informais_sc_transf_26[[i]] <- as_tibble(total_informais_scarteira_26, rownames = "total_informais_scarteiral_transf_26") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  total_informais_auto_26[[i]] <- as_tibble(total_informais_autonomos_26, rownames = "total_informais_autonomos_26") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  media_renda_auto_26[[i]] <- as_tibble(media_renda_autonomos_26, rownames = "media_renda_auto_26") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_formal_transf_27[[i]] <- as_tibble(total_formal_27, rownames = "total_formal_27") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_informais_sc_transf_27[[i]] <- as_tibble(total_informais_scarteira_27, rownames = "total_informais_scarteiral_transf_27") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  total_informais_auto_27[[i]] <- as_tibble(total_informais_autonomos_27, rownames = "total_informais_autonomos_27") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  media_renda_auto_27[[i]] <- as_tibble(media_renda_autonomos_27, rownames = "media_renda_auto_27") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_formal_transf_28[[i]] <- as_tibble(total_formal_28, rownames = "total_formal_28") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_informais_sc_transf_28[[i]] <- as_tibble(total_informais_scarteira_28, rownames = "total_informais_scarteiral_transf_28") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  total_informais_auto_28[[i]] <- as_tibble(total_informais_autonomos_28, rownames = "total_informais_autonomos_28") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  media_renda_auto_28[[i]] <- as_tibble(media_renda_autonomos_28, rownames = "media_renda_auto_28") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_formal_transf_29[[i]] <- as_tibble(total_formal_29, rownames = "total_formal_29") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_informais_sc_transf_29[[i]] <- as_tibble(total_informais_scarteira_29, rownames = "total_informais_scarteiral_transf_29") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  total_informais_auto_29[[i]] <- as_tibble(total_informais_autonomos_29, rownames = "total_informais_autonomos_29") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  media_renda_auto_29[[i]] <- as_tibble(media_renda_autonomos_29, rownames = "media_renda_auto_29") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_formal_transf_30[[i]] <- as_tibble(total_formal_30, rownames = "total_formal_30") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_informais_sc_transf_30[[i]] <- as_tibble(total_informais_scarteira_30, rownames = "total_informais_scarteiral_transf_30") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  total_informais_auto_30[[i]] <- as_tibble(total_informais_autonomos_30, rownames = "total_informais_autonomos_30") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  media_renda_auto_30[[i]] <- as_tibble(media_renda_autonomos_30, rownames = "media_renda_auto_30") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_formal_transf_31[[i]] <- as_tibble(total_formal_31, rownames = "total_formal_31") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_informais_sc_transf_31[[i]] <- as_tibble(total_informais_scarteira_31, rownames = "total_informais_scarteiral_transf_31") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  total_informais_auto_31[[i]] <- as_tibble(total_informais_autonomos_31, rownames = "total_informais_autonomos_31") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  media_renda_auto_31[[i]] <- as_tibble(media_renda_autonomos_31, rownames = "media_renda_auto_31") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_formal_transf_32[[i]] <- as_tibble(total_formal_32, rownames = "total_formal_32") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_informais_sc_transf_32[[i]] <- as_tibble(total_informais_scarteira_32, rownames = "total_informais_scarteiral_transf_32") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  total_informais_auto_32[[i]] <- as_tibble(total_informais_autonomos_32, rownames = "total_informais_autonomos_32") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  media_renda_auto_32[[i]] <- as_tibble(media_renda_autonomos_32, rownames = "media_renda_auto_32") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_formal_transf_33[[i]] <- as_tibble(total_formal_33, rownames = "total_formal_33") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  total_informais_sc_transf_33[[i]] <- as_tibble(total_informais_scarteira_33, rownames = "total_informais_scarteiral_transf_33") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  total_informais_auto_33[[i]] <- as_tibble(total_informais_autonomos_33, rownames = "total_informais_autonomos_33") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  media_renda_auto_33[[i]] <- as_tibble(media_renda_autonomos_33, rownames = "media_renda_auto_33") %>%
    mutate(trimestre =  max(a$pnad_design[[i]]$variables$trimestre),
           ano = max(a$pnad_design[[i]]$variables$ano))
  
  
  
  
  
  
  
  
  
  
  
  
}
