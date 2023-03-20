library(tidyverse)

# Importacao da tabela
data=data.frame('1235'=c('A','B','F','E','D','C'),
                '1536'=c('B','C','D','E','A','F'),
                '2134'=c('E','F','D','C','B','A'),
                '1428'=c('C','D','F','E','B','A'),
                '1665'=c('A','B','D','F','E','C'),
                '986'=c('D','B','E','C','F','A'),
                '1016'=c('F','C','A','E','B','D')) |>
  as_tibble() |>
  rename('1235'=X1235,
         '1536'=X1536,
         '2134'=X2134,
         '1428'=X1428,
         '1665'=X1665,
         '986'=X986,
         '1016'=X1016)


#1
# Via metodo plural eh aquele que recebeu mais votos


plural_data=data |>
            filter(row_number()==1)

matriz_data=data.frame(votes=c(as.numeric(colnames(plural_data))),candidate=c(as.character(plural_data[1,]))) |>
            group_by(candidate) |>
            summarise(sum(votes)) |>
            arrange(desc(`sum(votes)`))

# Assim o candidado A foi aquele a receber mais votos

#2

#Para saber a proporcao temos 

matriz_data|>
  select(`sum(votes)`) |>
  sum()

prop=matriz_data |>
  select(`sum(votes)`) |>
  sapply(function(x)return(x/10000)) |>
  as_tibble()|>
  rename(proportion='sum(votes)')

cbind(matriz_data,prop)

#  é possivel nenehum candiadto atingiu 50% +1
# o segundo turno tera participacao dos candiadtos A e E
# assim a nova tabela eh dada por

plural_second=sapply(1:7,function(i){
  ret=which(data[,i]=='A' |data[,i]=='E')
  r=data[c(ret),i]
  return(r)
})|>
  as_tibble()

matriz_data_second=data.frame(votes=c(as.numeric(colnames(plural_second))),candidate=c(as.character(plural_second[1,]))) |>
  group_by(candidate) |>
  summarise(sum(votes)) |>
  arrange(desc(`sum(votes)`))

prop_second=matriz_data_second |>
  select(`sum(votes)`) |>
  sapply(function(x)return(x/10000)) |>
  as_tibble()|>
  rename(proportion='sum(votes)')

cbind(matriz_data_second,prop_second)

# Portanto o candito E é aquele com o maior numero de votos

#3
data_eliminated=data
steps=list()
for(i in 1:5){
  c=data_eliminated|>
    filter(row_number()==1)
  to_be_remove=data.frame(cand=c(as.character(c)),votes=c(as.numeric(colnames(data_eliminated))))|>
    group_by(cand)|>
    summarise(sum(votes)) |>
    rename(votes='sum(votes)')|>
    slice(which.min(votes))
  new=data_eliminated|>
    mutate_if(~ all(nchar(.) <= 1), ~ str_replace_all(., to_be_remove$cand, '')) |>
    mutate_all(~ ifelse(nchar(.) == 0, NA, .))
  w=vector()
    for(k in 1:7){
      w[k]=new[!is.na(new[,k]),k]
    }
  data_eliminated=unlist(w) |>
                  matrix(byrow=F,nrow = 6-i) |>
                  as_tibble()|>
                  rename(`1235`=V1,
                         `1536`=V2,
                         `2134`=V3,
                         `1428`=V4,
                         `1665`=V5,
                         `986`=V6,
                         `1016`=V7
)
  
  steps[i]=data_eliminated
    
  
}

# pportanto C sera o canditato eleito


# 4

data_eliminated=data
steps=list()
for(i in 1:5){
  c=data_eliminated|>
    filter(row_number()==nrow(data_eliminated))
  to_be_remove=data.frame(cand=c(as.character(c)),votes=c(as.numeric(colnames(data_eliminated))))|>
    group_by(cand)|>
    summarise(sum(votes)) |>
    rename(votes='sum(votes)')|>
    slice(which.max(votes))
  new=data_eliminated|>
    mutate_if(~ all(nchar(.) <= 1), ~ str_replace_all(., to_be_remove$cand, '')) |>
    mutate_all(~ ifelse(nchar(.) == 0, NA, .))
  w=vector()
  for(k in 1:7){
    w[k]=new[!is.na(new[,k]),k]
  }
  data_eliminated=unlist(w) |>
    matrix(byrow=F,nrow = 6-i) |>
    as_tibble()|>
    rename(`1235`=V1,
           `1536`=V2,
           `2134`=V3,
           `1428`=V4,
           `1665`=V5,
           `986`=V6,
           `1016`=V7
    )
  
  steps[i]=data_eliminated
  
  
}


#5 

cand=data$`1235`

total=vector()            
index=vector()

for (j in 1:6) {
  for(i in 1:7){
    index[i]=which(cand[j]==data[,i])
    }
  index=7-index
  total[j]=sum(index*as.numeric(colnames(data)))
}


#6

cand=data$`1235`|>
  as.character()
for(j in 1:6){
  home=cand[j]
  away=cand[j+1]
    for(i in 1:7){
      data[,i]|>
      group_by(votacao, comb) |>
      arrange(Candidato1) |>
      summarise_all(first) |>
      group_by(Candidato1, Candidato2) |>
      summarise(
      `Votação 1` = sum(ifelse(ganhador1, votacao, 0)),
      `Votação 2` = sum(ifelse(!ganhador1, votacao, 0))
      )
          }
  
}
