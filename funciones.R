


#Funcion que crea un data frame con todos los elementos de 
#la prueba de hipótesis de la igualdad de dos varianzas poblacionales
ph_igual_varianzas <- function(var_cuant, var_cual, nivel_de_significancia,tipo_de_prueba){
        grados_de_libertad_num <- tapply(var_cuant, var_cual, length)[1]-1
        grados_de_libertad_den <- tapply(var_cuant, var_cual, length)[2]-1
        nivel_de_significancia <- 1 - nivel_de_significancia
        
        if(tipo_de_prueba == "two.sided"){
                
                #Lado izquierdo y lado de recho donde inician las zonas de rechazo de la hipotesis nula
                lado_der <- qf(1-(nivel_de_significancia/2), grados_de_libertad_num, grados_de_libertad_den)
                lado_izq <- qf(nivel_de_significancia/2, grados_de_libertad_num, grados_de_libertad_den)
                
                #Estadistico de prueba
                ep <- tapply(var_cuant, var_cual, var)[1]/tapply(var_cuant, var_cual, var)[2]
                
                #Data frame que se imprime en el aplicativo
                bd <- data.frame(Resultados=c("Estadistico de prueba (EP)",
                                              "Nivel de significancia",
                                              "Regiones criticas de rechazo de hipotesis nula",
                                              "Conclusion")
                                 ,
                                 Valores=c(as.character(round(ep,8)),
                                           as.character(nivel_de_significancia),
                                           paste("EP <= ",as.character(round(lado_izq,4)),
                                                 "o",
                                                 "EP >= ", as.character(round(lado_der,4)),
                                                 sep = "  "),
                                           if(ep <= lado_izq |
                                              ep >= lado_der){
                                                   paste("El estadistico de prueba es igual a", round(ep,8),
                                                         ". Como el estadistico de prueba se encuentra
                              dentro de la region critica de rechazo entonces se rechaza
                              la hipotesis nula en favor de la hipotesis alternativa.")
                                           }else{
                                                   paste("El estadistico de prueba es igual a", round(ep,8),
                                                         ". Como el estadistico de prueba se encuentra fuera
                              de la region critica de rechazo entonces se concluye que
                              los datos no son suficientes para rechazar la hipotesis nula.")
                                           }
                                 ))
                bd
                
        }else{
                if(tipo_de_prueba == "less"){
                        
                        #Lado izquierdo  donde inicia la zona de rechazo de la hipotesis nula
                        lado_izq <- qf(nivel_de_significancia, grados_de_libertad_num, grados_de_libertad_den)
                        
                        #Estadistico de prueba
                        ep <- tapply(var_cuant, var_cual, var)[1]/tapply(var_cuant, var_cual, var)[2]
                        
                        #Data frame que se imprime en el aplicativo
                        bd <- data.frame(Resultados=c("Estadistico de prueba (EP)",
                                                      "Nivel de significancia",
                                                      "Region critica de rechazo de hipotesis nula",
                                                      "Conclusion")
                                         ,
                                         Valores=c(as.character(round(ep,8)),
                                                   as.character(nivel_de_significancia),
                                                   paste("EP <= ",as.character(round(lado_izq,4))),
                                                   if(ep <= lado_izq){
                                                           paste("El estadistico de prueba es igual a", round(ep,8),
                                                                 ". Como el estadistico de prueba se encuentra
                                                                 dentro de la region critica de rechazo entonces se rechaza
                                                                 la hipotesis nula en favor de la hipotesis alternativa.")
                                                   }else{
                                                           paste("El estadistico de prueba es igual a", round(ep,8),
                                                                 ". Como el estadistico de prueba se encuentra fuera
                                                                 de la region critica de rechazo entonces se concluye que
                                                                 los datos no son suficientes para rechazar la hipotesis nula.")
                                                   }
                                         ))
                        bd
                        
                }else{
                        if(tipo_de_prueba == "greater"){
                                
                                #Lado derecho donde inician las zonas de rechazo de la hipotesis nula
                                lado_der <- qf(1-nivel_de_significancia, grados_de_libertad_num, grados_de_libertad_den)
                                
                                #Estadistico de prueba
                                ep <- tapply(var_cuant, var_cual, var)[1]/tapply(var_cuant, var_cual, var)[2]
                                
                                #Data frame que se imprime en el aplicativo
                                bd <- data.frame(Resultados=c("Estadistico de prueba (EP)",
                                                              "Nivel de significancia",
                                                              "Region critica de rechazo de hipotesis nula",
                                                              "Conclusion")
                                                 ,
                                                 Valores=c(as.character(round(ep,8)),
                                                           as.character(nivel_de_significancia),
                                                           paste("EP >= ", as.character(round(lado_der,4)),
                                                                 sep = "  "),
                                                           if(ep >= lado_der){
                                                                   paste("El estadistico de prueba es igual a", round(ep,8),
                                                                         ". Como el estadistico de prueba se encuentra
                                                                         dentro de la region critica de rechazo entonces se rechaza
                                                                         la hipotesis nula en favor de la hipotesis alternativa.")
                                                           }else{
                                                                   paste("El estadistico de prueba es igual a", round(ep,8),
                                                                         ". Como el estadistico de prueba se encuentra fuera
                                                                         de la region critica de rechazo entonces se concluye que
                                                                         los datos no son suficientes para rechazar la hipotesis nula.")
                                                           }
                                                 ))
                                bd
                }
        }
        }
}