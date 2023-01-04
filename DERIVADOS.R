library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)



CUMPLISDER <- read_xlsx("C:/Users/npvazquez/Desktop/AGC_DATOS/Exportaciones/Asignaciones03012023.xlsx", sheet = "Cumplimientos")
CARGASDER <- read_xlsx("C:/Users/npvazquez/Desktop/AGC_DATOS/Exportaciones/Asignaciones03012023.xlsx", sheet = "Cargas")
IPDER <- read_xlsx("C:/Users/npvazquez/Desktop/AGC_DATOS/Exportaciones/Asignaciones03012023.xlsx", sheet = "Inspecciones pendientes")
HABDER <- read_xlsx("C:/Users/npvazquez/Desktop/AGC_DATOS/Exportaciones/Asignaciones03012023.xlsx", sheet = "Habilitaciones")



##########selección de columnas

CARGAS <- CARGASDER %>%
  select ("Fecha iniciado AGC", "N Denuncia", "DEN",  "Estado", "Tkt hijo", "Propuesta cierre por antecedentes","Tkt hijo para cumplir","Coincide criterio", "Estado de cierre efectivo", "Tkt hijo de cierre efectivo" )%>%
  rename ('Fecha' = "Fecha iniciado AGC")%>%
  rename ('NSUACI'= "N Denuncia")

CUMPLIS <- CUMPLISDER %>%
  select ("Fecha tkt", "N Denuncia", "DEN",  "Tkt Área","Estado propuesto en Gestión Colaborativa", "Tkt propuesta para cierre", "Estado en GC tras revisión (si no coincide con propuesta)", "TKT de cierre (si no coincide con propuesta)")%>%
  rename ('Fecha' = "Fecha tkt")%>%
  rename ('NSUACI'= "N Denuncia")

IP <- IPDER %>%
  select ("Fecha", "N Denuncia", "DEN",  "Tkt área", "Propuesta Estado GC", "Estado GC (si no coincide con propuesta)", "Tkt de cierre efectivo")%>%
  rename ('NSUACI'= "N Denuncia")

HAB <- HABDER %>%
  select ("Fecha de consulta", "N Denuncia", "DEN", "Estado SUACI posterior a respuesta", "Nuevo Tkt")%>%
  rename ('Fecha' = "Fecha de consulta")%>%
  rename ('NSUACI'= "N Denuncia")

##########cargas

CARGAS <- CARGAS %>%
  filter (Fecha > "2022-12-17")

CARGAS <- CARGAS %>%
  rename ('CIERRE_ANTECEDENTES' = "Propuesta cierre por antecedentes")
CARGAS <- CARGAS %>%
  rename ('CIERRE_EFECTIVO' = "Estado de cierre efectivo")

CARGAS$CIERRE_ANTECEDENTES[is.na(CARGAS$CIERRE_ANTECEDENTES)] <- 0
CARGAS$CIERRE_EFECTIVO[is.na(CARGAS$CIERRE_EFECTIVO)] <- 0

CARGAS_Caso1 <- CARGAS  %>% filter ((CARGAS ['Estado'] == "Derivada") & (CARGAS ['CIERRE_ANTECEDENTES'] == "0")) 

CARGAS_Caso2 <- CARGAS %>%  filter ((CARGAS ['CIERRE_EFECTIVO'] == "0") & (CARGAS ['CIERRE_ANTECEDENTES'] == "DERIVADO"|
                                                                         CARGAS ['CIERRE_ANTECEDENTES'] == "ACTAS- DERIVACIÓN" |
                                                                         CARGAS ['CIERRE_ANTECEDENTES'] == "SIN ACTAS- DERIVACIÓN" |
                                                                         CARGAS ['CIERRE_ANTECEDENTES'] == "OBRA PERMITIDA- DERIVACIÓN" |
                                                                         CARGAS ['CIERRE_ANTECEDENTES'] == "ESTABLECIMIENTO HABILITADO- DERIVACIÓN"|
                                                                         CARGAS ['CIERRE_ANTECEDENTES'] == "INSTALACIÓN APTA PARA USO- DERIVACIÓN"  ))

CARGAS_Caso3 <- CARGAS %>% filter (CARGAS ['CIERRE_EFECTIVO'] == "DERIVADO" | 
                CARGAS ['CIERRE_EFECTIVO'] == "ACTAS- DERIVACIÓN" |
                CARGAS ['CIERRE_EFECTIVO'] == "SIN ACTAS- DERIVACIÓN" |
                CARGAS ['CIERRE_EFECTIVO'] == "OBRA PERMITIDA- DERIVACIÓN" |
                CARGAS ['CIERRE_EFECTIVO'] == "ESTABLECIMIENTO HABILITADO- DERIVACIÓN" |
                CARGAS ['CIERRE_EFECTIVO'] == "INSTALACIÓN APTA PARA USO- DERIVACIÓN" )



CARGASDERIVADAS <- rbind (CARGAS_Caso1, CARGAS_Caso2, CARGAS_Caso3 )



###########cumplis

CUMPLIS <- CUMPLIS %>%
  filter (Fecha > "2022-12-17")

CUMPLIS <- CUMPLIS %>%
  rename ('ESTADO_REVISOR'="Estado en GC tras revisión (si no coincide con propuesta)")

CUMPLIS$ESTADO_REVISOR[is.na(CUMPLIS$ESTADO_REVISOR)] <- 0

CUMPLIS_Caso1 <- CUMPLIS  %>%
  filter ( ( CUMPLIS$ESTADO_REVISOR == "0") & (CUMPLIS ['Estado propuesto en Gestión Colaborativa'] == "DERIVADO" | 
          CUMPLIS ['Estado propuesto en Gestión Colaborativa'] == "ACTAS- DERIVACIÓN" |
          CUMPLIS ['Estado propuesto en Gestión Colaborativa'] == "SIN ACTAS- DERIVACIÓN" |
          CUMPLIS ['Estado propuesto en Gestión Colaborativa'] == "OBRA PERMITIDA- DERIVACIÓN" |
          CUMPLIS ['Estado propuesto en Gestión Colaborativa'] == "ESTABLECIMIENTO HABILITADO- DERIVACIÓN" |
          CUMPLIS ['Estado propuesto en Gestión Colaborativa'] == "INSTALACIÓN APTA PARA USO- DERIVACIÓN" ) )

CUMPLIS_Caso2 <- CUMPLIS %>%
  filter (CUMPLIS ['ESTADO_REVISOR'] == "DERIVADA" | 
          CUMPLIS ['ESTADO_REVISOR'] == "ACTAS- DERIVACIÓN" |
          CUMPLIS ['ESTADO_REVISOR'] == "SIN ACTAS- DERIVACIÓN" |
          CUMPLIS ['ESTADO_REVISOR'] == "OBRA PERMITIDA- DERIVACIÓN" |
          CUMPLIS ['ESTADO_REVISOR'] == "ESTABLECIMIENTO HABILITADO- DERIVACIÓN" |
          CUMPLIS ['ESTADO_REVISOR'] == "INSTALACIÓN APTA PARA USO- DERIVACIÓN"  )


CUMPLISDERIVADAS <- rbind (CUMPLIS_Caso1, CUMPLIS_Caso2)



###########IP

IP <- IP %>%
  filter (Fecha > "2022-12-18")

IP <- IP %>%
  rename ('ESTADO_GC'="Propuesta Estado GC") %>%
  rename ('ESTADO_REVISOR' = "Estado GC (si no coincide con propuesta)")


IP_Caso1 <- IP %>%
  filter (IP ['ESTADO_GC'] == "DERIVADA" | 
            IP ['ESTADO_GC'] == "ACTAS- DERIVACIÓN" |
            IP ['ESTADO_GC'] == "SIN ACTAS- DERIVACIÓN" |
            IP ['ESTADO_GC'] == "OBRA PERMITIDA- DERIVACIÓN" |
            IP ['ESTADO_GC'] == "ESTABLECIMIENTO HABILITADO- DERIVACIÓN" |
            IP ['ESTADO_GC'] == "INSTALACIÓN APTA PARA USO- DERIVACIÓN"  )


#########HAB
             
HAB <- HAB %>%
  filter (Fecha > "2022-12-18")

HAB <- HAB %>%
  filter (HAB ['Estado SUACI posterior a respuesta'] == "Derivada") 





write_xlsx(x = CARGASDERIVADAS ,path = 'C:/Users/npvazquez/Desktop/AGC_DATOS/RESULTADOS_CONTROLES/CARGASDERIVADAS.xlsx')
write_xlsx(x = CUMPLISDERIVADAS ,path = 'C:/Users/npvazquez/Desktop/AGC_DATOS/RESULTADOS_CONTROLES/CUMPLISDERIVADAS.xlsx')
write_xlsx(x = HAB ,path = 'C:/Users/npvazquez/Desktop/AGC_DATOS/RESULTADOS_CONTROLES/HABILITACIONESDERIVADAS.xlsx')
write_xlsx(x = CARGASDERIVADAS ,path = 'C:/Users/npvazquez/Desktop/AGC_DATOS/RESULTADOS_CONTROLES/CARGASDERIVADAS.xlsx')


