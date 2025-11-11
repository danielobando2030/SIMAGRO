# ===============================================================
# Test: Renderizar SOLO el RMarkdown del informe usando una IMAGEN EXISTENTE
# - No recalcula el gráfico
# - Usa la imagen PNG que ya generó tu Shiny
# ===============================================================

suppressPackageStartupMessages({
  library(rmarkdown)
  library(fs)
})

# --- CONFIGURA AQUÍ ---
# Carpeta del proyecto
setwd("C:/Users/Usuario/Universidad EAFIT/VP - 2025_FAO_Cundinamarca/02_AnalisisEmpirico/03_Dash/3_7")

# Parámetros del informe
producto_sel <- "Aguacate"
anio_sel     <- "2014"

# 1) INTENTO 1: usar la imagen temporal que genera el server (si existe)
tmp_png <- file.path(tempdir(), "grafico_tmp.png")

# 2) INTENTO 2: si no existe, buscar la imagen PNG más reciente en la carpeta del proyecto
if (!file_exists(tmp_png)) {
  pngs <- dir_ls(path = getwd(), regexp = "\\.png$", type = "file", fail = FALSE)
  if (length(pngs) > 0) {
    # Ordenar por fecha de modificación (desc) y tomar la más reciente
    info <- file_info(pngs)
    tmp_png <- pngs[order(info$modification_time, decreasing = TRUE)][1]
    message("🔎 No se encontró 'grafico_tmp.png' en tempdir(). Usando PNG más reciente en el proyecto: ", tmp_png)
  }
}

# Validación final
if (!file_exists(tmp_png)) {
  stop("⚠️ No se encontró ninguna imagen PNG para insertar en el informe.\n",
       "  • Ejecuta la app y haz clic en el botón de descarga (para que se genere 'grafico_tmp.png')\n",
       "  • O especifica manualmente la ruta de una imagen PNG en la variable 'tmp_png'.")
}

message("📄 Usando imagen: ", tmp_png)

# Normalizar ruta para knitr/include_graphics
img_path <- normalizePath(tmp_png, winslash = "/")

# Comprobar que el Rmd existe
rmd_path <- file.path(getwd(), "cambio_precios.Rmd")
if (!file_exists(rmd_path)) {
  stop("⚠️ No se encuentra el archivo Rmd en: ", rmd_path)
}

# Salida
out_pdf <- file.path(getwd(), paste0("informe_test_", producto_sel, "_", anio_sel, ".pdf"))

# Render directo a PDF
message("▶️ Renderizando PDF a: ", out_pdf)
res <- rmarkdown::render(
  input        = rmd_path,
  output_format= "pdf_document",
  output_file  = out_pdf,
  params = list(
    producto = producto_sel,
    anio     = anio_sel,
    grafico  = img_path
  ),
  envir = new.env(parent = globalenv())
)

message("✅ Listo. Revisa el PDF en: ", out_pdf)
