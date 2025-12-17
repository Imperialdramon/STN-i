
# nolint start

# TODO: Debe tener la capacidad de tomar los datos de General-Data, que contendrá todas las ejecuciones independientes de testing + tomar el .Rdata de cada semilla del escenario, obteniendo el best_elite en cada caso
# Se debe iterativamente sacar el promedio de calidad del best_elite para cada semilla y luego graficarlo en un boxplot para los escenarios

# ---------- Clean up ----------
rm(list = ls())
gc()
quit(save = "no")

# nolint end