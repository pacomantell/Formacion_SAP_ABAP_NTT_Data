# Tema 2.- Desarrollo de aplicaciones ABAP/4. Trabajo con ficheros

## Índice

1. [Servidor](#1-servidor)
    * [Ejercicio](#ejercicio-zmm_r_fich_coche_loc_02)
2. [Local](#2-local)
3. [Transacciones y funciones.](#3-transacciones-y-funciones)

# 1. Servidor

ABAP dispone de una serie de instrucciones para manejar ficheros binarios de textos.

Estas instrucciones son:

* **OPEN**: Abre un fichero.

    ```abap
    OPEN DATASET [ruta]
        FOR APPENDING/OUTPUT/INPUT
        IN BINARY MODE/TEXT MODE [codificacion].

    IF sy-subrc EQ 0.
        MESSAGE 'Fichero abierto correctamente' TYPE 'E'.
    ELSEIF sy-subrc EQ 8.
        MESSAGE 'Error al abrir fichero' TYPE 'E'.
    ENDIF.
    ```

* **CLOSE**: Cierra un fichero.

    ```abap
    CLOSE DATASET [ruta].
    ```

* **READ**: Lee un fichero. Los datos deben ser de tipo char, sin importar que únicamente sean números.

    ```abap
    READ DATASET [ruta].
    ```

* **TRANSFER**: Escribe sobre un fichero.

    ```abap
    TRANSFER [registro] TO [ruta].
    ```

Se debe especificar siempre la ruta completa del archivo.

Existe una función que accede a los directorios de SAP: /SAPDMC/LSM_F4_SERVER_FILE.

## Ejemplo

```abap
*&---------------------------------------------------------------------*
*& Report ZMM_R_FICHEROS_02
*&---------------------------------------------------------------------*
*& Transferimos datos de tabla libro a un fichero
*&---------------------------------------------------------------------*
REPORT zmm_r_ficheros_02.

TABLES zmm_t_libros_02.

DATA: BEGIN OF ls_libros,
        cod_libro TYPE zmm_t_libros_02-cod_libro,
        titulo    TYPE zmm_t_libros_02-titulo,
        tipo      TYPE zmm_t_libros_02-tipo,
      END OF ls_libros.

DATA: lt_libros LIKE TABLE OF ls_libros,
      lv_path   TYPE string.

* Recupero informacion de la tabla
SELECT cod_libro titulo tipo
  FROM zmm_t_libros_02
  INTO TABLE lt_libros.

IF sy-subrc NE 0.
  MESSAGE 'No existen datos' TYPE 'E'.
ENDIF.

CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
* EXPORTING
*   DIRECTORY              = ' '
*   FILEMASK               = ' '
 IMPORTING
   SERVERFILE             = lv_path
 EXCEPTIONS
   CANCELED_BY_USER       = 1
   OTHERS                 = 2
          .
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.


* Especificamos la ruta y el nombre del fichero
CONCATENATE lv_path '\libros_02.txt' INTO lv_path.

* Abrimos el fichero
OPEN DATASET lv_path
  FOR OUTPUT
  IN TEXT MODE ENCODING DEFAULT.

* Chequeamos que se haya abierto el fichero
IF sy-subrc NE 0.
  MESSAGE 'Error al abrir el fichero' TYPE 'E'.
ENDIF.

* Transferimos datos
LOOP AT lt_libros ASSIGNING FIELD-SYMBOL(<ls_libros>).
  TRANSFER <ls_libros> TO lv_path.
ENDLOOP.

* Cerramos el fichero
CLOSE DATASET lv_path.
```

## Ejercicio: ZMM_R_FICH_COCHE_LOC_02

* **Report**

    ```abap
    *&---------------------------------------------------------------------*
    *& Report ZMM_R_FICH_COCH_LOC_02
    *&---------------------------------------------------------------------*
    *&
    *&---------------------------------------------------------------------*
    REPORT zmm_r_fich_coche_loc_02.

    INCLUDE zmm_i_fich_coche_loc_02_top. "Declaracion de datos
    INCLUDE zmm_i_fich_coche_loc_02_s01. "Pantalla seleccion
    INCLUDE zmm_i_fich_coche_loc_02_e01. "Eventos
    INCLUDE zmm_i_fich_coche_loc_02_f01. "Subrutinas
    ```

* **Declaración de datos: zmm_i_fich_coche_loc_02-top**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_FICH_COCH_LOC_02_TOP
    *&---------------------------------------------------------------------*
    TABLES zmm_t_coches_02.

    DATA: BEGIN OF ls_coches,
            cod_coche         TYPE zmm_t_coches_02-cod_coche,
            marca             TYPE zmm_t_coches_02-marca,
            matricula         TYPE zmm_t_coches_02-matricula,
            modelo            TYPE zmm_t_coches_02-modelo,
            pais_fab          TYPE zmm_t_coches_02-pais_fab,
            tipo_coche        TYPE zmm_t_coches_02-tipo_coche,
            fecha_fabricacion TYPE zmm_t_coches_02-fecha_fabricacion,
            precio            TYPE zmm_t_coches_02-precio,
            moneda            TYPE zmm_t_coches_02-moneda,
        END OF ls_coches.

    DATA: BEGIN OF ls_file,
            cod_coche         TYPE char10,
            marca             TYPE zmm_t_coches_02-marca,
            matricula         TYPE zmm_t_coches_02-matricula,
            modelo            TYPE zmm_t_coches_02-modelo,
            pais_fab          TYPE zmm_t_coches_02-pais_fab,
            tipo_coche        TYPE zmm_t_coches_02-tipo_coche,
            fecha_fabricacion TYPE char20,
            precio            TYPE char10,
            moneda            TYPE char10,
        END OF ls_file.

    DATA: lt_coches LIKE TABLE OF ls_coches,
        lt_file   LIKE TABLE OF ls_file.
    ```

* **Pantalla selección: zmm_i_fich_coche_loc_02_s01**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_FICH_COCH_LOC_02_S01
    *&---------------------------------------------------------------------*

    SELECTION-SCREEN BEGIN OF BLOCK bl WITH FRAME TITLE TEXT-t01.
    SELECT-OPTIONS: so_codco FOR zmm_t_coches_02-cod_coche,
            so_model FOR zmm_t_coches_02-modelo,
            so_matr FOR zmm_t_coches_02-matricula,
            so_prec FOR zmm_t_coches_02-precio,
            so_mon FOR zmm_t_coches_02-moneda.

    PARAMETERS pa_ruta TYPE string OBLIGATORY.

    PARAMETERS: rd_lee RADIOBUTTON GROUP rd1 TYPE xfeld,
                rd_carga RADIOBUTTON GROUP rd1 TYPE xfeld.

    SELECTION-SCREEN END OF BLOCK bl.
    ```

* **Eventos: zmm_i_fich_coche_loc_02_e01**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_FICH_COCHE_LOC_02_E01
    *&---------------------------------------------------------------------*

    START-OF-SELECTION.

    PERFORM f_select.

    AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_ruta.

    PERFORM f_help.

    END-OF-SELECTION.

    PERFORM f_show.
    ```

* **Subrutinas: zmm_i_fich_coches_loc_02_f01**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_FICH_COCHE_LOC_02_F01
    *&---------------------------------------------------------------------*
    *&---------------------------------------------------------------------*
    *&      Form  F_SELECT
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *  -->  p1        text
    *  <--  p2        text
    *----------------------------------------------------------------------*
    FORM f_select .
    SELECT cod_coche marca matricula modelo pais_fab tipo_coche fecha_fabricacion precio moneda
        FROM zmm_t_coches_02
        INTO TABLE lt_coches
        WHERE cod_coche IN so_codco
        AND matricula IN so_matr
        AND modelo IN so_model
        AND precio IN so_prec
        AND moneda IN so_mon.

    IF sy-subrc NE 0.
        MESSAGE 'No existen datos' TYPE 'E'.
    ENDIF.

    CONCATENATE pa_ruta '\coches_02.txt' INTO pa_ruta.

    ENDFORM.
    *&---------------------------------------------------------------------*
    *&      Form  F_HELP
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *  -->  p1        text
    *  <--  p2        text
    *----------------------------------------------------------------------*
    FORM f_help .
    CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
    *   EXPORTING
    *     DIRECTORY              = ' '
    *     FILEMASK               = ' '
        IMPORTING
        serverfile       = pa_ruta
        EXCEPTIONS
        canceled_by_user = 1
        OTHERS           = 2.
    IF sy-subrc NE 0.
        MESSAGE 'Error al cargar ayuda' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

    ENDFORM.
    *&---------------------------------------------------------------------*
    *&      Form  F_SHOW
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *  -->  p1        text
    *  <--  p2        text
    *----------------------------------------------------------------------*
    FORM f_show .
    IF rd_lee EQ abap_true.

        PERFORM f_lee_fichero.

    ELSEIF rd_carga EQ abap_true.

        PERFORM f_carga_fichero.

    ENDIF.
    ENDFORM.
    *&---------------------------------------------------------------------*
    *&      Form  F_LEE_FICHERO
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *  -->  p1        text
    *  <--  p2        text
    *----------------------------------------------------------------------*
    FORM f_lee_fichero .
    OPEN DATASET pa_ruta FOR INPUT IN TEXT MODE ENCODING DEFAULT.

    IF sy-subrc NE 0.
        MESSAGE 'Error al abrir el archivo' TYPE 'E'.
    ENDIF.

    DO.
        READ DATASET pa_ruta INTO ls_file.

        IF sy-subrc NE 0. "Me salgo cuando leo el fichero
        EXIT.
        ENDIF.

        WRITE:/ ls_file-cod_coche, ls_file-marca, ls_file-modelo,
            ls_file-matricula, ls_file-pais_fab, ls_file-tipo_coche,
            ls_file-fecha_fabricacion, ls_file-precio, ls_file-moneda.
    ENDDO.

    CLOSE DATASET pa_ruta.
    ENDFORM.
    *&---------------------------------------------------------------------*
    *&      Form  F_CARGA_FICHERO
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *  -->  p1        text
    *  <--  p2        text
    *----------------------------------------------------------------------*
    FORM f_carga_fichero .
    OPEN DATASET pa_ruta
            FOR OUTPUT
            IN TEXT MODE ENCODING DEFAULT.

    IF sy-subrc NE 0.
        MESSAGE 'Error al abrir el fichero' TYPE 'E'.
    ENDIF.

    LOOP AT lt_coches ASSIGNING FIELD-SYMBOL(<ls_coche>).
        ls_file-cod_coche = <ls_coche>-cod_coche.
        ls_file-marca = <ls_coche>-marca.
        ls_file-modelo = <ls_coche>-modelo.
        ls_file-matricula = <ls_coche>-matricula.
        ls_file-pais_fab = <ls_coche>-pais_fab.
        ls_file-tipo_coche = <ls_coche>-tipo_coche.
        ls_file-fecha_fabricacion = <ls_coche>-fecha_fabricacion.
        ls_file-precio = <ls_coche>-precio.
        ls_file-moneda = <ls_coche>-moneda.
        TRANSFER ls_file TO pa_ruta.
    ENDLOOP.

    MESSAGE 'Fichero cargado con éxito' TYPE 'S'.

    CLOSE DATASET pa_ruta.
    ENDFORM.
    ```

# 2. Local

Para trabajar con ficheros almacenados en el directorio local, SAP provee un conjunto de módulos de funciones o clases para tal gestión. 

Actualmente se está generalizando más el uso de clases para estas tareas y el uso de módulos está quedando obsoleto.

Las **funciones mas utilizadas**, aunque ya **obsoletas, debido al mayor uso de las clases para este fin**, son las siguientes:

* **TB_LIMIT_WS_FILENAME_GET**: Se utiliza para la apertura de ficheros, generalmente esta función se incluye en el evento “AT SELECTION SCREEN ON VALUE REQUEST FOR ‘fichero’”.

    ```abap
    CALL FUNCTION 'TB_LIMIT_WS_FILENAME_GET'
    * EXPORTING
    *   DEF_FILENAME           = ' '
    *   DEF_PATH               = ' '
    *   MASK                   = ' '
    *   MODE                   = ' '
    *   TITLE                  = ' '
    * IMPORTING
    *   FILENAME               =
    *   PATH                   =
    *   FILE                   =
    * EXCEPTIONS
    *   SELECTION_CANCEL       = 1
    *   SELECTION_ERROR        = 2
    *   OTHERS                 = 3
            .
    IF sy-subrc <> 0.
    * Implement suitable error handling here
    ENDIF.
    ```

* **GUI_UPLOAD**: Subida de contenido del fichero a una tabla interna.

    ```abap
    CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
        filename                = lv_filename
    *   FILETYPE                = 'ASC'
    *   HAS_FIELD_SEPARATOR     = ' '
    *   HEADER_LENGTH           = 0
    *   READ_BY_LINE            = 'X'
    *   DAT_MODE                = ' '
    *   CODEPAGE                = ' '
    *   IGNORE_CERR             = ABAP_TRUE
    *   REPLACEMENT             = '#'
    *   CHECK_BOM               = ' '
    *   VIRUS_SCAN_PROFILE      =
    *   NO_AUTH_CHECK           = ' '
    * IMPORTING
    *   FILELENGTH              =
    *   HEADER                  =
    TABLES
        data_tab                = lt_mat
    * CHANGING
    *   ISSCANPERFORMED         = ' '
    EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.
    IF sy-subrc EQ 0.
    MESSAGE 'Fichero cargado con éxito' TYPE 'S'.
    ENDIF.
    ```

* **GUI_DOWNLOAD**: Descarga del contenido de una tabla interna a fichero.

    ```abap
    CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
    *   BIN_FILESIZE            =
        filename                = lv_filename
    *   FILETYPE                = 'ASC'
    *   APPEND                  = ' '
    *   WRITE_FIELD_SEPARATOR   = ' '
    *   HEADER                  = '00'
    *   TRUNC_TRAILING_BLANKS   = ' '
    *   WRITE_LF                = 'X'
    *   COL_SELECT              = ' '
    *   COL_SELECT_MASK         = ' '
    *   DAT_MODE                = ' '
    *   CONFIRM_OVERWRITE       = ' '
    *   NO_AUTH_CHECK           = ' '
    *   CODEPAGE                = ' '
    *   IGNORE_CERR             = ABAP_TRUE
    *   REPLACEMENT             = '#'
    *   WRITE_BOM               = ' '
    *   TRUNC_TRAILING_BLANKS_EOL       = 'X'
    *   WK1_N_FORMAT            = ' '
    *   WK1_N_SIZE              = ' '
    *   WK1_T_FORMAT            = ' '
    *   WK1_T_SIZE              = ' '
    *   WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
    *   SHOW_TRANSFER_STATUS    = ABAP_TRUE
    *   VIRUS_SCAN_PROFILE      = '/SCET/GUI_DOWNLOAD'
    * IMPORTING
    *   FILELENGTH              =
    TABLES
        data_tab                = lt_libros
    *   FIELDNAMES              =
    EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.
    IF sy-subrc <> 0.
    * Implement suitable error handling here
    ENDIF.
    ```

En la actualidad, es más común el **uso de la clase CL_GUI_FRONTEND_SERVICES para la gestión de dicho fichero**. Esta clase se compone de diferentes **métodos**, aunque los **más utilizados** son:

* **FILE_OPEN_DIALOG**: Ayuda de búsqueda para mostrar los ficheros del equipo local, generalmente esta llamada se incluye en el evento “AT SELECTION SCREEN ON VALUE REQUEST FOR P_PATH. Donde P_PATH es el parámetro donde se indica el fichero a leer.

* **DIRECTORY_BROWSE**: Ayuda de búsqueda para mostrar un directorio local 

* **DIRECTORY_EXIST**: Comprueba si el directorio pasado como parámetro existe.

* **FILE_EXIST**: Se comprueba si el fichero pasado como parámetro existe.

* **GUI_UPLOAD**: Para cargar el contenido del fichero en una tabla interna.

* **GUI_DOWNLOAD**: Descarga del contenido de una tabla interna en un fichero local. Se indicará el nombre del fichero (con la extensión incluida) y se informará el parámetro DATA_TAB con los datos que se desean descargar en el fichero.

## Ejercicio: Modificamos el [ejercicio anterior](#ejercicio-zmm_r_fich_coche_loc_02) para hacer la carga/descarga local

* **Report:zmm_r_fich_coche_loc2_02**

    ```abap
    *&---------------------------------------------------------------------*
    *& Report ZMM_R_FICH_COCH_LOC_02
    *&---------------------------------------------------------------------*
    *&
    *&---------------------------------------------------------------------*
    REPORT zmm_r_fich_coche_loc2_02.

    INCLUDE zmm_i_fich_coche_loc2_02_top. "Declaracion de datos
    INCLUDE zmm_i_fich_coche_loc2_02_s01. "Pantalla seleccion
    INCLUDE zmm_i_fich_coche_loc2_02_e01. "Eventos
    INCLUDE zmm_i_fich_coche_loc2_02_f01. "Subrutinas
    ```

* **Declaración de variables:zmm_i_fich_coche_loc2_02_top**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_FICH_COCH_LOC_02_TOP
    *&---------------------------------------------------------------------*
    TABLES zmm_t_coches_02.

    DATA: BEGIN OF ls_coches,
            cod_coche         TYPE zmm_t_coches_02-cod_coche,
            marca             TYPE zmm_t_coches_02-marca,
            matricula         TYPE zmm_t_coches_02-matricula,
            modelo            TYPE zmm_t_coches_02-modelo,
            pais_fab          TYPE zmm_t_coches_02-pais_fab,
            tipo_coche        TYPE zmm_t_coches_02-tipo_coche,
            fecha_fabricacion TYPE zmm_t_coches_02-fecha_fabricacion,
            precio            TYPE zmm_t_coches_02-precio,
            moneda            TYPE zmm_t_coches_02-moneda,
        END OF ls_coches.

    DATA: lt_coches LIKE TABLE OF ls_coches,
        lt_file   TYPE TABLE OF string.
    ```

* **Pantalla de selección: zmm_i_fich_coche_loc2_02_s01**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_FICH_COCH_LOC_02_S01
    *&---------------------------------------------------------------------*

    SELECTION-SCREEN BEGIN OF BLOCK bl WITH FRAME TITLE TEXT-t01.
    SELECT-OPTIONS: so_codco FOR zmm_t_coches_02-cod_coche,
            so_model FOR zmm_t_coches_02-modelo,
            so_matr FOR zmm_t_coches_02-matricula,
            so_prec FOR zmm_t_coches_02-precio,
            so_mon FOR zmm_t_coches_02-moneda.

    PARAMETERS pa_ruta TYPE string OBLIGATORY.

    PARAMETERS: rd_lee RADIOBUTTON GROUP rd1 TYPE xfeld,
                rd_carga RADIOBUTTON GROUP rd1 TYPE xfeld.

    SELECTION-SCREEN END OF BLOCK bl.
    ```

* **Eventos: zmm_i_fich_coche_loc2_02_e01**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_FICH_COCHE_LOC_02_E01
    *&---------------------------------------------------------------------*

    START-OF-SELECTION.

    PERFORM f_select.

    AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_ruta.

    PERFORM f_help.

    END-OF-SELECTION.

    PERFORM f_show.
    ```

* **Subrutinas: zmm_i_fich_coche_loc2_02_f01**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_FICH_COCHE_LOC_02_F01
    *&---------------------------------------------------------------------*
    *&---------------------------------------------------------------------*
    *&      Form  F_SELECT
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *  -->  p1        text
    *  <--  p2        text
    *----------------------------------------------------------------------*
    FORM f_select .
    SELECT cod_coche marca matricula modelo pais_fab tipo_coche fecha_fabricacion precio moneda
        FROM zmm_t_coches_02
        INTO TABLE lt_coches
        WHERE cod_coche IN so_codco
        AND matricula IN so_matr
        AND modelo IN so_model
        AND precio IN so_prec
        AND moneda IN so_mon.

    IF sy-subrc NE 0.
        MESSAGE 'No existen datos' TYPE 'E'.
    ENDIF.

    ENDFORM.
    *&---------------------------------------------------------------------*
    *&      Form  F_HELP
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *  -->  p1        text
    *  <--  p2        text
    *----------------------------------------------------------------------*
    FORM f_help .
    CALL FUNCTION 'TB_LIMIT_WS_FILENAME_GET'
    *   EXPORTING
    *     DEF_FILENAME           = ' '
    *     DEF_PATH               = ' '
    *     MASK                   = ' '
    *     MODE                   = ' '
    *     TITLE                  = ' '
        IMPORTING
        filename         = pa_ruta
    *     PATH             =
    *     FILE             =
        EXCEPTIONS
        selection_cancel = 1
        selection_error  = 2
        OTHERS           = 3.
    IF sy-subrc NE 0.
        MESSAGE 'Error al buscar el archivo' TYPE 'E'.
    ENDIF.


    ENDFORM.
    *&---------------------------------------------------------------------*
    *&      Form  F_SHOW
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *  -->  p1        text
    *  <--  p2        text
    *----------------------------------------------------------------------*
    FORM f_show .
    IF rd_lee EQ abap_true.

        PERFORM f_lee_fichero.

    ELSEIF rd_carga EQ abap_true.

        PERFORM f_carga_fichero.

    ENDIF.
    ENDFORM.
    *&---------------------------------------------------------------------*
    *&      Form  F_LEE_FICHERO
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *  -->  p1        text
    *  <--  p2        text
    *----------------------------------------------------------------------*
    FORM f_lee_fichero .

    CALL FUNCTION 'GUI_UPLOAD'
        EXPORTING
        filename                = pa_ruta
    *     FILETYPE                = 'ASC'
    *     HAS_FIELD_SEPARATOR     = ' '
    *     HEADER_LENGTH           = 0
    *     READ_BY_LINE            = 'X'
    *     DAT_MODE                = ' '
    *     CODEPAGE                = ' '
    *     IGNORE_CERR             = ABAP_TRUE
    *     REPLACEMENT             = '#'
    *     CHECK_BOM               = ' '
    *     VIRUS_SCAN_PROFILE      =
    *     NO_AUTH_CHECK           = ' '
    *   IMPORTING
    *     FILELENGTH              =
    *     HEADER                  =
        TABLES
        data_tab                = lt_file
    *   CHANGING
    *     ISSCANPERFORMED         = ' '
        EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.
    IF sy-subrc NE 0.
        MESSAGE 'Error al cargar archivo' TYPE 'E'.
    ENDIF.

    LOOP AT lt_file ASSIGNING FIELD-SYMBOL(<ls_file>).
        WRITE:/ <ls_file>.
    ENDLOOP.

    CLOSE DATASET pa_ruta.
    ENDFORM.
    *&---------------------------------------------------------------------*
    *&      Form  F_CARGA_FICHERO
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *  -->  p1        text
    *  <--  p2        text
    *----------------------------------------------------------------------*
    FORM f_carga_fichero .
    CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
    *     BIN_FILESIZE            =
        filename                = pa_ruta
    *     FILETYPE                = 'ASC'
    *     APPEND                  = ' '
        write_field_separator   = cl_abap_char_utilities=>horizontal_tab
    *     HEADER                  = '00'
    *     TRUNC_TRAILING_BLANKS   = ' '
    *     WRITE_LF                = 'X'
    *     COL_SELECT              = ' '
    *     COL_SELECT_MASK         = ' '
    *     DAT_MODE                = ' '
    *     CONFIRM_OVERWRITE       = ' '
    *     NO_AUTH_CHECK           = ' '
    *     CODEPAGE                = ' '
    *     IGNORE_CERR             = ABAP_TRUE
    *     REPLACEMENT             = '#'
    *     WRITE_BOM               = ' '
    *     TRUNC_TRAILING_BLANKS_EOL       = 'X'
    *     WK1_N_FORMAT            = ' '
    *     WK1_N_SIZE              = ' '
    *     WK1_T_FORMAT            = ' '
    *     WK1_T_SIZE              = ' '
    *     WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
    *     SHOW_TRANSFER_STATUS    = ABAP_TRUE
    *     VIRUS_SCAN_PROFILE      = '/SCET/GUI_DOWNLOAD'
    *   IMPORTING
    *     FILELENGTH              =
        TABLES
        data_tab                = lt_coches
    *     FIELDNAMES              =
        EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.
    IF sy-subrc NE 0.
        MESSAGE 'Error al descargar archivo' TYPE 'E'.
    ENDIF.

    ENDFORM.
    ```

# 3. Transacciones y funciones

Algunas **transacciones útiles para el manejo de ficheros** de servidor son:

* **AL11**: Listado de ficheros disponibles en el servidor.

* **CG3Z**: Subir fichero de directorio local a servidor.

* **CG3Y**: Descargar fichero de servidor a local.

Otras funciones útiles en la gestión de ficheros:

* **EPS_DELETE_FILE**: Borra ficheros del servidor, es equivalente a la sentencia DELETE DATASET.

* **TEXT_CONVERT_XLS_TO_SAP**: Carga ficheros Excel a un tabla interna.

* **ALSM_EXCEL_TO_INTERNAL_TABLE**: Carga de de ficheros Excel en una tabla interna
