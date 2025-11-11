---
title: ' '
params:
  mes: NA
  anio: NA
  producto: NA
  subtitulo: NA
  maximo: NA
  plot: NA
  mensaje1: NA
output:
  pdf_document:
    latex_engine: xelatex
  html_document:
    df_print: paged
header-includes:
- \usepackage{float}
- \usepackage{arydshln}
- \usepackage{tabu}
- \usepackage{xcolor}
- \usepackage{fontspec}
- \usepackage{booktabs}  
- \usepackage{fancyhdr}
- \usepackage{graphicx}
- \definecolor{mygreen}{RGB}{26,73,34}
- \definecolor{gray}{RGB}{128,128,128}
- \definecolor{green2}{RGB}{13,141,56}
- "\\setmainfont{Prompt-Regular.ttf}"
- \pagestyle{fancy}
- \fancyfoot{} 
- \usepackage{colortbl}
- \usepackage{adjustbox}
- \setlength{\headheight}{2cm}
- \fancyhead[C]{\includegraphics[width=\textwidth]{www/logo_3.png}}
- \renewcommand{\headrule}{\color{mygreen}\hrule width\headwidth height\headrulewidth} 
- \usepackage{eso-pic}
---



<!-- Fecha -->
\begin{flushright}
\textcolor{gray}{Informe en línea generado el: 10 noviembre, 2025}
\end{flushright}
\section*{Parámetros con los cuales se generó el informe:}

\begin{tabu} to \linewidth {>{\raggedright}X>{\raggedright}X}
\toprule
Año: & Todos los años\\
Mes: & Todos los meses\\
Producto: & Todos los productos\\
\bottomrule
\end{tabu}
<!-- Titulo del tablaro -->
\fontsize{14}{14} \selectfont \textcolor{mygreen}{Cundinamarca y su importancia en la recepción de alimentos}\
<!-- Subtitulo -->
\fontsize{12}{12} \selectfont \textcolor{green2}{Descubre la importancia de Cundinamarca como receptor de alimentos desde otros departamentos del país.}\
<!-- Subtitulo dinamico -->
\fontsize{10}{10} \selectfont Cundinamarca es uno de los principales receptores de Guainía con un procentaje de 88%.\

\begin{center}\includegraphics[width=0.9\linewidth]{C:/Users/danie/AppData/Local/Temp/RtmpOG0je1/file1b350116b6f21_files/figure-latex/unnamed-chunk-2-1} \end{center}
\fontsize{10}{10} \selectfont El 87% del volumen total de alimentos que reportan como origen los territorios de Cundinamarca llega a las principales centrales de abasto de Bogotá.\

\fontsize{8}{8} \selectfont \textcolor{mygreen}{Cálculos propios a partir de datos del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA).}\newline
\fontsize{8}{8} \selectfont \textcolor{mygreen}{Esta visualización muestra el porcentaje de alimentos enviados a Cundinamarca desde cada departamento, incluyendo productos de origen local. Permite apreciar la importancia de Cundinamarca como receptor de alimentos provenientes de otros territorios del país}\newline 
\fontsize{8}{8} \selectfont \textcolor{mygreen}{Los departamentos en color gris indican la ausencia de reportes de ingresos de productos provenientes de esas áreas en las principales centrales de abasto.}\newline

\AddToShipoutPictureBG*{\includegraphics[width=\paperwidth,height=3cm,keepaspectratio]{www/logo_2.png}}

