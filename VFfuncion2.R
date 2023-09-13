
VFfuncion2 <- function(umbral, nIter) {
  
  VA <- 1 
  i <- 1 
  VF0 <- 1 
  DeltaVF <- Inf 
  n <- 0 
  
  while (n < nIter) {
    
    n <- n + 1
    
    VF <- VA * (1 + (1 / n))^n
    
    DeltaVF <- VF - VF0
    
    VF0 <- VF
    
    if (abs(DeltaVF) < umbral) {
      break 
    }
  }
  
  respuesta <- list(
    VF = VF,
    DeltaVF = DeltaVF,
    N = n
  )
  
  if (n == nIter) {
    mensaje <- paste0("El algoritmo se detuvo después de ", n, " iteraciones ",
                      "al cumplir el número de iteraciones en nIter. VF =", VF)
  } else {
    mensaje <- paste0("El algoritmo se detuvo después de ", n, " iteraciones ",
                      "al llegar al umbral de diferencia de valores futuros. VF =", VF)
  }
  
  print(mensaje)
  
  return(respuesta)
}

VFfuncion=function(umbral){
  
  VA=1 
  i=1 # Tasa de interés anualizada de 100% (i=1)
  #paro=100000 # Número de iteraciones (cálculos) que deseamos realizar
  VF0=1 # El valor inicial del VF en la iteración
  #umbral=0.000000000001 # Asigno un valor de umbral
  DeltaVF=Inf 
  n=0 
  

  while (DeltaVF>umbral){
  
    n=n+1

    VF=VA*(1+(1/n))^n
    
    DeltaVF=VF-VF0
   
    VF0=VF
  }
  
  
  respuesta=list(
    VF=VF,
    DeltaVF=DeltaVF,
    N=n  
  )
 
  print(
    paste0("Después de ",n," iteraciones, ",
           "se llegó a un VF de ",VF,", dado el umbral de",
           umbral," que llevó a un valor de DeltaVF de ",DeltaVF)
  )

  return(respuesta)
}

switchPersonal <- function(accion) {
  
  mensaje <- switch(accion,
                    saludo = "¡Hola! ¿Qué tal vas?",
                    despedida = "¡Hasta luego!",
                    pregunta = "¿Tienes alguna duda?",
                    afirmacion = "¡Estoy de acuerdo contigo!",
                    negacion = "No creo que eso sea correcto.",
                    "Lo siento, no reconozco esa acción."
  )
  
  return(mensaje)
}