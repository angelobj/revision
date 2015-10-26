\name{revision}
\alias{revision}

\title{
Una función para corregir automáticamente un examen
}
\description{
Una función para corregir automáticamente un examen
}
\usage{
revision(respuestas,pautas,exigencia=60,dcto=0,ID=3,e=0,nombres=NULL,Formas=NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{respuestas}{Archivo ".csv" con respuestas de evaluación}
  \item{pautas}{Número de pautas (ubicadas en las primeras líneas)}
  \item{exigencia}{Exigencia para cálculo de nota, por defecto al 60}
  \item{dcto}{Descuento de preguntas correctas por número de malas, por defecto 0}
  \item{ID}{Número de columnas que contienen identificación, por defecto 3}
  \item{nombres}{Planilla Excel con nombres y Rut de alumnos}
  \item{Formas}{Lista con la secuencia de preguntas para calcular estadística}
}
\author{
Angelo Bartsch
}
\examples{
revision("ejemplo.csv",pautas=2,exigencia=60,dcto=-0.25,ID=3,e=0,nombres="lista.xlsx"
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ revision }
\keyword{ evaluacion }% __ONLY ONE__ keyword per line
