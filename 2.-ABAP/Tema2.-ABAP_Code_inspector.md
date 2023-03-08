


# 1. Code Inspector

La calidad del código es esencial en la programación de todos los objetos, tanto estándar de SAP como especificados por el cliente.

**El Code Inspector es una herramienta que proporciona SAP para la verificación del código ABAP implementado.**

Puede ser utilizado para comprobar la calidad de un programa de forma individual, o de un conjunto de objetos, por ejemplo todos aquellos que estén incluidos en una orden/tarea.

Genera tres tipos de mensajes: Información, Warnings y Error, en función de la importancia del error encontrado.

Es recomendable que el desarrollador chequee los objetos creados de forma proactiva, de manera que se garantice una calidad del código.


# Declaracion en linea en nueva version

```abap
*&---------------------------------------------------------------------*
*& Report ZMM_R_OPTIMIZACION_02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_r_optimizacion_02.

"Declaracion de una tabla en linea
SELECT cod_libro, titulo, tipo
  FROM zmm_t_libros_02
  INTO TABLE @DATA(lt_libros). "Declaramos la tabla aqui mismo

IF sy-subrc NE 0.
  MESSAGE 'No hay datos' TYPE 'E'.
ENDIF.
```