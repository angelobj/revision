#' Función de revisión o autocorrección
#'
#' Una función para corregir automáticamente un examen
#' @keywords revision, autocorrección
#' @export
#' @examples
#' revision("ejemplo.csv",pautas=2,exigencia=60,dcto=-0.25,ID=3,e=0,nombres="lista.xls",Formas=Formas)
#' Alumnos: write.table(x$Resumen.Alum,file="Notas.csv",col.names=T,sep=";")
#' Profesor: write.table(x$Resumen.prof,file="Prof.csv",col.names=T,sep=";")
#' Stats: write.table(round(Notas$Stat[[4]],1),sep=";",dec=",",file="Stat.csv")
#' Formato de lista debe tener: Rut, Apellidos y Nombres
#' Formas es una lista de Formas, donde cada forma contiene la secuencia de preguntas.
#' Formas=list(
#'            Forma0=list(l=7,a=13,c=8),
#'            Forma1=list(a=13,c=8,l=7))

revision<-function(respuestas,pautas,exigencia=60,dcto=0,ID=3,e=0,nombres=NULL,Formas=NULL,...)
{
  if (missing(respuestas))
    stop("Ingresar nombre de archivo con respuestas")
  if (missing(pautas))
    stop("Debe ingresar el número de pautas de corrección")
  if(missing(exigencia))
    warning("Se ha utilizado un nivel de exigencia del 60%")
  if(dcto<(-1))
    stop("El dcto debe ser un número entre -1 y 0, con un punto como separador decimal")
  if(missing(Formas))
    stop("Debe ingresar una lista con las formas")
  if(dcto>(0))
    stop("El dcto debe ser un número entre -1 y 0, con un punto como separador decimal")
  if(missing(dcto))
    warning("No hubo descuento o penalización por preguntas erradas")
  if(!is.null(ID))
    warning("Las ID=3 primeras columnas tienen la identificación de los alumnos")
  if(!is.null(e))
    warning("La tolerancia o error permitido es 0. Utilizar 0.01,0.02,0.05")
calculo_nota <- as.matrix(read.table(respuestas,header=F,sep=";"))

P=pautas
alum<-dim(calculo_nota)[1]
num_preg<-seq(from=1,to=(dim(calculo_nota)[2]-ID),by=1) #ID es el número de columnas que no tienen preguntas (identificación de Nombre, sección, forma, etc)
corte<-((length(num_preg))*exigencia)/100  #Para la escala
Rs<-paste("R",num_preg, sep = "") # Para enumerar las preguntas
if(!is.null(nombres)) {
  resultados<-matrix(NA,nrow=dim(calculo_nota)[1]+1,ncol=dim(calculo_nota)[2]+7) # 7=4 columnas para Puntaje, Nota, nombre y apellidos y 3 para conteo
  colnames(resultados)<-c("Rut","Sección","Forma",c(Rs),"Puntaje","Nota","Nombre","Apellidos","Buenas","Malas","Omitidas")

  require("gdata") #Cargar librería, si aparece error, usar comando install.packages("")
  #REVISAR ARCHIVO CON RESPUESTA,
  # Agregar Nombre, Apellidos y Otros
  lista <- as.matrix(read.xls(nombres,header=T)) #Lectura de archivo, verificar nombre
  pos_lista<-match(intersect((lista[,1]),(calculo_nota[,1])),(lista[,1]))
  pos_resultado<-match(intersect((lista[,1]),(calculo_nota[,1])),(calculo_nota[,1]))
  for (k in 1:length(pos_lista)){
    resultados[pos_resultado[k],dim(calculo_nota)[2]+3]<-lista[pos_lista[k],3]
    resultados[pos_resultado[k],dim(calculo_nota)[2]+4]<-lista[pos_lista[k],2]
  }
} else {
  resultados<-matrix(NA,nrow=dim(calculo_nota)[1]+1,ncol=dim(calculo_nota)[2]+5) #ELIMINAR EL 1!! 5= 2 columnas para puntaje y nota y 3 para conteo
  colnames(resultados)<-c("Rut","Sección","Forma",c(Rs),"Puntaje","Nota","Buenas","Malas","Omitidas")
}
resultados[1:alum,1:ID]<-matrix(calculo_nota[1:alum,1:ID],ncol=ID) #En primeras "ID" columnas van datos de Identificación

# Hasta el momento llevo la matriz y la comparación. Revisar que compara con pauta...
#resultados<-matrix(NA,nrow=dim(calculo_nota1)[1]+1,ncol=dim(calculo_nota1)[2]+2)
#resultados[1:alum,1:ID]<-matrix(calculo_nota1[1:alum,1:ID],ncol=ID)
#colnames(resultados)<-c("Rut","Sección","Forma",c(Rs),"Puntaje","Nota")

# Esta forma es más rápida, pero no permite nivel de error

# for (i in 1:alum){
#  for (j in 1:P){
#    resultados[i,(ID+1):(ID+R)]<-(if(calculo_nota[i,ID]==calculo_nota[j,ID]){
#                                (ifelse(calculo_nota[i,(ID+1):(ID+R)]==calculo_nota[j,(ID+1):(ID+R)],
#                                yes=1,no=(ifelse(calculo_nota[i,(ID+1):(ID+R)]=="",yes=0,no=dcto))
#))
#} else {next})
#}}
R<-length(num_preg)
for (i in 1:alum){  	# desde i=1º Alumno hasta último, si hay P pautas, las primeras P pautas no se evalúan
  for (j in 1:P){			# desde j=1º Pauta hasta la última, para compararlas hasta encontrar una igual
    if(calculo_nota[i,ID]==calculo_nota[j,ID]){
    for (k in 1:R){		# desde k=1º pregunta hasta preg=prguntas
        resultados[i,(k+ID)]<-
#ID es la posición de la pauta, P es el número de pautas
          ifelse(!is.na(as.numeric(calculo_nota[i,(k+ID)]))==TRUE,
            yes=ifelse(abs((as.numeric(calculo_nota[i,(k+ID)]))-(as.numeric(calculo_nota[j,(k+ID)])))
                <=((as.numeric(calculo_nota[j,(k+ID)]))*e),yes=1,
                no=(ifelse(calculo_nota[i,(k+ID)]=="",yes=0,no=dcto))),
            no=ifelse(calculo_nota[i,(k+ID)]==calculo_nota[j,(k+ID)],yes=1,
                no=(ifelse(calculo_nota[i,(k+ID)]=="",yes=0,no=dcto))))
    }
    }else{next}
  }
}

resultados[,dim(calculo_nota)[2]+1]<-as.matrix(apply(resultados[,(ID+1):(ID+R)], function(x){sum(na.omit(as.numeric(x)))},MARGIN=1),ncol=1)
resultados[,dim(calculo_nota)[2]+2]<-round(ifelse(as.numeric(resultados[,dim(calculo_nota)[2]+1])<=corte,
                                    yes=(3/(corte)*(as.numeric(resultados[,dim(calculo_nota)[2]+1])))+1,
                                    no=(3/(length(num_preg)-corte)*(as.numeric(resultados[,dim(calculo_nota)[2]+1])-corte))+4),2)

#resultados[,dim(calculo_nota)[2]+6]<-sum(as.numeric(na.omit((x$Revision[i,(ID+1):(ID+R)])=="-0.25"))) #Contar respuestas buenas de cada alumno
#resultados[,dim(calculo_nota)[2]+7]<-sum(as.numeric(is.na(x$Revision[i,(ID+1):(ID+R)]))) # Contar omitidas


if(!is.null(nombres)) {
  resultados[,dim(calculo_nota)[2]+5]<-as.matrix(apply(resultados[,(ID+1):(ID+R)], function(x){sum(na.omit((x=="1")))},MARGIN=1),ncol=1) #Contar respuestas buenas de cada alumno
  resultados[,dim(calculo_nota)[2]+6]<-as.matrix(apply(resultados[,(ID+1):(ID+R)], function(x){sum(na.omit((x==paste("",dcto,"",sep=""))))},MARGIN=1),ncol=1) #Contar respuestas buenas de cada alumno
  resultados[,dim(calculo_nota)[2]+7]<-as.matrix(apply(resultados[,(ID+1):(ID+R)], function(x){sum(na.omit((x=="0")))},MARGIN=1),ncol=1) #Contar respuestas buenas de cada alumno
} else {
  resultados[,dim(calculo_nota)[2]+3]<-as.matrix(apply(resultados[,(ID+1):(ID+R)], function(x){sum(na.omit((x=="1")))},MARGIN=1),ncol=1) #Contar respuestas buenas de cada alumno
  resultados[,dim(calculo_nota)[2]+4]<-as.matrix(apply(resultados[,(ID+1):(ID+R)], function(x){sum(na.omit((x==paste("",dcto,"",sep=""))))},MARGIN=1),ncol=1) #Contar respuestas buenas de cada alumno
  resultados[,dim(calculo_nota)[2]+5]<-as.matrix(apply(resultados[,(ID+1):(ID+R)], function(x){sum(na.omit((x=="0")))},MARGIN=1),ncol=1) #Contar respuestas buenas de cada alumno

}

#Estadística, hay que revisar
n_p<-Formas # Nº de preguntas en las secciones que se evalúan
names(n_p)<-calculo_nota[1:P,ID]
Formas<-sapply(names(n_p), USE.NAMES = TRUE,simplify=FALSE, function(i)(
sapply(names(n_p[[i]]), USE.NAMES = TRUE,simplify=FALSE, function(d){
{paste(d,seq(1:n_p[[i]][[d]]))}})))
Formas<-matrix(unlist(Formas,use.names=T),nrow=P,byrow=T) # No extrajo todo
row.names(Formas)<-calculo_nota[1:P,ID]


Pos<-matrix(NA,ncol=R,nrow=P)
R<-length(num_preg)
P=pautas
for (i in 1:P){
Pos[i,]<-match(intersect(Formas[1,],Formas[i,]),Formas[i,])} # Orden de preguntas en Forma 0
row.names(Pos)<-calculo_nota[1:P,ID]


# subset(resultados[,(ID+1):(ID+R)], resultados[,ID] == resultados[P,(ID)]) # Extrae respuestas de pauta == [P,..]

 stat.b<-matrix(NA,ncol=P+1,nrow=(R))
 stat.m<-matrix(NA,ncol=P+1,nrow=(R))
 stat.o<-matrix(NA,ncol=P+1,nrow=(R))
colnames(stat.b)<-c(paste("Buenas",0:(P-1),sep=" "),"TB")
colnames(stat.m)<-c(paste("Malas",0:(P-1),sep=" "),"TM")
colnames(stat.o)<-c(paste("Omitidas",0:(P-1),sep=" "),"TO")
for (i in 1:P){
  stat.b[,i]<-as.matrix(apply(subset(resultados[,(ID+1):(ID+R)], resultados[,ID] == resultados[i,(ID)]),
                              function(x){sum(na.omit((x=="1")))},MARGIN=2),ncol=1)
  stat.m[,i]<-as.matrix(apply(subset(resultados[,(ID+1):(ID+R)], resultados[,ID] == resultados[i,(ID)]),
                              function(x){sum(na.omit((x==paste("",dcto,"",sep=""))))},MARGIN=2),ncol=1)
  stat.o[,i]<-as.matrix(apply(subset(resultados[,(ID+1):(ID+R)], resultados[,ID] == resultados[i,(ID)]),
                              function(x){sum(na.omit((x=="0")))},MARGIN=2),ncol=1)
}
# Revisar estructura para obtener estadística en más de dos formas
for (i in 1:R)
{
  stat.b[i,3]<-sum(stat.b[Pos[1,i],1],stat.b[Pos[2,i],2])
  stat.m[i,3]<-sum(stat.m[Pos[1,i],1],stat.m[Pos[2,i],2])
  stat.o[i,3]<-sum(stat.o[Pos[1,i],1],stat.o[Pos[2,i],2])
}
stat.t<-data.frame((stat.b[,3]*100)/alum,(stat.m[,3])*100/alum,(stat.o[,3]*100/alum))


if(!is.null(nombres)) {
  return(list(Revision=resultados,Respuestas=calculo_nota,
        Resumen.Prof=resultados[,c("Rut","Nombre","Apellidos","Nota")],
        Resumen.alum=resultados[,c("Rut","Buenas","Malas","Omitidas","Puntaje","Nota")],
        Stat=list(stat.b,stat.m,stat.o,stat.t)))
} else {
  return(list(Revision=resultados,Respuestas=calculo_nota,
        Resumen.Prof=resultados[,c("Rut","Nota")],
        Resumen.Alum=resultados[,c("Rut","Buenas","Malas","Omitidas","Puntaje","Nota")],
        Stat=list(stat.b,stat.m,stat.o,stat.t)))}


}
