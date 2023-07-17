library(dplyr)
library(httr)
library(ggplot2)
library(getPass)
library(repr)

token = getPass()  #Token de acesso à API da PCDaS

url_base = "https://bigdata-api.fiocruz.br"

convertRequestToDF <- function(request){
  variables = unlist(content(request)$columns)
  variables = variables[names(variables) == "name"]
  column_names <- unname(variables)
  values = content(request)$rows
  df <- as.data.frame(do.call(rbind,lapply(values,function(r) {
    row <- r
    row[sapply(row, is.null)] <- NA
    rbind(unlist(row))
  } )))
  names(df) <- column_names
  return(df)
}

#numero de nascimentos
estados <- c('RO','AC','AM','RR','PA','AP','TO','MA','PI','CE','RN','PB','PE','AL','SE','BA','MG','ES','RJ','SP','PR','SC','RS','MS','MT','GO','DF')
dataframe <- data.frame()

endpoint <- paste0(url_base,"/","sql_query")

for (estado in estados){
  # Nascimentos total por município
  
  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query":"SELECT res_SIGLA_UF, ano_nasc, COUNT(1) ',
        'FROM \\"datasus-sinasc_final_1996-2020_preliminar_2021_2022\\" ',
        'WHERE res_SIGLA_UF = \'',estado,'\'  ',
        'GROUP BY res_SIGLA_UF, ano_nasc", "fetch_size": 65000}
      }
    }')
  
  request <- POST(url = endpoint, body = params, encode = "form")
  df_mun <- convertRequestToDF(request)
  names(df_mun) <- c('UF', 'Ano', 'Nascidos')
  dataframe <- rbind(dataframe, df_mun)
  
  repeat {
    
    cursor <- content(request)$cursor
    
    params = paste0('{
          "token": {
            "token": "',token,'"
          },
          "sql": {
            "sql": {"query":"SELECT res_SIGLA_UF, ano_nasc, COUNT(1)',
            ' FROM \\"datasus-sinasc_final_1996-2020_preliminar_2021_2022\\" ',
            'WHERE res_SIGLA_UF = \'',estado,'\' ',
            'GROUP BY res_SIGLA_UF, ano_nasc", "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')
    
    
    request <- POST(url = endpoint, body = params, encode = "form")
    
    if (length(content(request)$rows) == 0)
      break
    else print("oi")
    
    df_mun <- convertRequestToDF(request)
    names(df_mun) <- c('UF', 'Ano', 'Nascidos')
    dataframe <- rbind(dataframe, df_mun)
  }
}

dataframe <- dataframe[dataframe$Ano <= 2020,]
write.csv(dataframe,'nascidos_muni.csv')
