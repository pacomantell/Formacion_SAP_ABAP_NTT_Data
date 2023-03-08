# Tema2.- Desarrollo de aplicaciones ABAP/4. Formularios

## Índice

# 1. SAPScripts

**SAPScript están ya obsoletos.**

# 2. Smartforms

Smartforms es una herramienta para la creación y el mantenimiento de los formularios.

Permite ejecutar modificaciones simples en el formulario y en la lógica mediante herramientas gráficas simples.

Para imprimir un formulario, se necesita un programa de tratamiento de datos y un Smartform. El programa de aplicación pasa los datos a través de un módulo de funciones al Smartform.

Tras activar el Smartform, **el sistema genera automáticamente un módulo de funciones**. En tiempo de ejecución, el sistema procesa este módulo de funciones.  

Con la herramienta **SAP Smartform se puede trabajar con tablas estáticas y dinámicas**. Esto incluye, por ejemplo, los saltos de línea en celdas individuales de la tabla, eventos de activación para títulos de la tabla y subtotales, y clasificación de datos antes de la salida. 

**SAP Smartform permite validar nodos de manera individual**, o validar todo el formulario y así poder encontrar posibles errores en la estructura del árbol, por ejemplo, uno de los controles que se realizan es validar que todos los campos (variables) tienen un valor definido en el momento de su visualización, ya sea mediante cálculo o por paso desde parámetros. 


## 2.1. ARQUITECTURA


## Creacion y modificación

Transacción Smartforms

la ventana main se suele usar para tablas (generacion de facturas por ejemplo).

Se recomienda crear un estilo por cada Smartform.

Para ver gráficos: SE78

Para activar editor word en la form: SE38->RSCPSETEDITOR

# Ejemplo llamada a smartform desde un report

```abap
*&---------------------------------------------------------------------*
*& Report ZMM_R_IMPRIMIR_SMART_02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_r_imprimir_smart_02.

DATA: lt_libros        TYPE TABLE OF zmm_t_libros_02,
      lv_function_name TYPE rs38l_fnam.

SELECT *
  FROM zmm_t_libros_02
  INTO TABLE lt_libros.

IF sy-subrc NE 0.
  MESSAGE 'No existen libros.' TYPE 'E'.
ENDIF.

" Obtenemos el código interno del modulo de funcion del smartform
CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
  EXPORTING
    formname           = 'ZMM_LIBROS_02'
  IMPORTING
    fm_name            = lv_function_name
  EXCEPTIONS
    no_form            = 1
    no_function_module = 2
    OTHERS             = 3.

IF sy-subrc EQ 1.
  MESSAGE 'No existe el form' TYPE 'E'.
ELSEIF sy-subrc EQ 2.
  MESSAGE 'No existen funciones' TYPE 'E'.
ELSEIF sy-subrc NE 0.
  MESSAGE 'no se encontro el smartform' TYPE 'E'.
ENDIF.

"Llamada al smartform
CALL FUNCTION lv_function_name
  EXPORTING
    it_libros = lt_libros.
```

## Ejercicio: Mostrar alv y form de vuelos

* **Report: zmm_r_imprimir_vuelos_02**

    ```abap
    *&---------------------------------------------------------------------*
    *& Report ZMM_R_IMPRIMIR_VUELOS_02
    *&---------------------------------------------------------------------*
    *&
    *&---------------------------------------------------------------------*
    REPORT ZMM_R_IMPRIMIR_VUELOS_02.

    INCLUDE zmm_i_imprimir_vuelos_02_top. "Declaraciones globales
    INCLUDE zmm_i_imprimir_vuelos_02_s01. "Pantalla selección
    INCLUDE zmm_i_imprimir_vuelos_02_e01. "Eventos
    INCLUDE zmm_i_imprimir_vuelos_02_f01. "Subrutinas

    INCLUDE zmm_i_imprimir_vuelos_02_o01. "Modulos PBO
    INCLUDE zmm_i_imprimir_vuelos_02_i01. "Modulos PAI
    ```

* **Declaraciones globales: zmm_i_imprimir_vuelos_02_top**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_IMPRIMIR_VUELOS_02_TOP
    *&---------------------------------------------------------------------*

    TABLES: spfli, t005t.

    "Estructura
    DATA: BEGIN OF gs_vuelos,
            mandt     TYPE spfli-mandt,
            countryfr TYPE spfli-countryfr,
            land1fr   TYPE t005t-landx,
            cityfrom  TYPE spfli-cityfrom,
            countryto TYPE spfli-countryto,
            land1to   TYPE t005t-landx,
            cityto    TYPE spfli-cityto,
            deptime   TYPE spfli-deptime,
        END OF gs_vuelos.

    DATA gt_vuelos LIKE TABLE OF gs_vuelos.

    "ALV
    DATA: go_alv       TYPE REF TO cl_gui_alv_grid,
        go_container TYPE REF TO cl_gui_custom_container,
        gv_ok_code   TYPE sy-ucomm.

    "Smartform
    DATA: gv_fm_name  TYPE rs38l_fnam,
        gv_sys_date TYPE string.
    ```

* **Pantalla selección: zmm_i_imprimir_vuelos_02_s01**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_IMPRIMIR_VUELOS_02_S01
    *&---------------------------------------------------------------------*

    SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.

    SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t02.

    SELECT-OPTIONS: so_paisf FOR spfli-countryfr,
                    so_cityf FOR spfli-cityfrom,
                    so_paist FOR spfli-countryto,
                    so_cityt FOR spfli-cityto.

    SELECTION-SCREEN END OF BLOCK b2.

    SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-t03.

    PARAMETERS pa_met TYPE xfeld AS CHECKBOX.

    SELECTION-SCREEN END OF BLOCK b3.

    SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-t04.

    PARAMETERS: rd_alv  RADIOBUTTON GROUP rd1 TYPE xfeld,
                rd_form RADIOBUTTON GROUP rd1 TYPE xfeld.

    SELECTION-SCREEN END OF BLOCK b4.

    SELECTION-SCREEN END OF BLOCK b1.
    ```

* **Eventos: zmm_i_imprimir_vuelos_02_e01**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_IMPRIMIR_VUELOS_02_E01
    *&---------------------------------------------------------------------*

    ************************************************************************
    START-OF-SELECTION.
    ************************************************************************

    PERFORM f_select.

    ************************************************************************
    END-OF-SELECTION.
    ************************************************************************

    PERFORM f_validation.
    ```

* **Subrutinas: zmm_i_imprimir_vuelos_02_f01**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_IMPRIMIR_VUELOS_02_F01
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

    "Selección de datos
    SELECT spfli~mandt countryfr t1~landx cityfrom countryto t2~landx cityto deptime
        FROM spfli
        INNER JOIN t005t AS t1
        ON spfli~countryfr EQ t1~land1
        INNER JOIN t005t AS t2
        ON spfli~countryto EQ t2~land1
        INTO TABLE gt_vuelos
        WHERE countryfr IN so_paisf
        AND cityfrom IN so_cityf
        AND countryto IN so_paist
        AND cityto IN so_cityt
        AND t1~spras EQ sy-langu
        AND t2~spras EQ sy-langu..

    IF sy-subrc NE 0.
        MESSAGE 'No existen vuelos' TYPE 'E'.
    ENDIF.

    ENDFORM.
    *&---------------------------------------------------------------------*
    *&      Form  F_VALIDATION
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *  -->  p1        text
    *  <--  p2        text
    *----------------------------------------------------------------------*
    FORM f_validation .

    "Validaciones
    IF rd_alv EQ abap_true.

        "Genero la dynpro
        IF gt_vuelos IS NOT INITIAL.
        CALL SCREEN 9000.
        ENDIF.
    ELSEIF rd_form EQ abap_true.

        PERFORM f_smartform.


    ELSE.

        MESSAGE 'Seleccione una visualización' TYPE 'E'.

    ENDIF.

    ENDFORM.
    *&---------------------------------------------------------------------*
    *&      Form  F_STATUS_TITLE
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *  -->  p1        text
    *  <--  p2        text
    *----------------------------------------------------------------------*
    FORM f_status_title .

    "Botones
    SET PF-STATUS 'ZSTATUS02'.

    "Título
    SET TITLEBAR 'ZTITLE02'.

    ENDFORM.
    *&---------------------------------------------------------------------*
    *&      Module  SET_ALV  OUTPUT
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    MODULE set_alv OUTPUT.

    PERFORM f_set_alv.

    ENDMODULE.
    *&---------------------------------------------------------------------*
    *&      Form  F_SET_ALV
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *  -->  p1        text
    *  <--  p2        text
    *----------------------------------------------------------------------*
    FORM f_set_alv .

    "Catalogo de campos
    "Catalogo de campos
    DATA: lt_fieldcat TYPE lvc_t_fcat,
            ls_fieldcat TYPE lvc_s_fcat,
            ls_layout   TYPE lvc_s_layo,
            ls_variant  TYPE disvariant.

    "Creamos el catalogo

    ls_fieldcat-fieldname = 'COUNTRYFR'.
    ls_fieldcat-scrtext_l = 'País Origen'.
    APPEND ls_fieldcat TO lt_fieldcat.
    CLEAR ls_fieldcat.

    ls_fieldcat-fieldname = 'LAND1FR'.
    ls_fieldcat-scrtext_l = 'Pais O. Desc.'.
    APPEND ls_fieldcat TO lt_fieldcat.
    CLEAR ls_fieldcat.

    ls_fieldcat-fieldname = 'CITYFROM'.
    ls_fieldcat-scrtext_l = 'Ciudad Origen'.
    APPEND ls_fieldcat TO lt_fieldcat.
    CLEAR ls_fieldcat.

    ls_fieldcat-fieldname = 'COUNTRYTO'.
    ls_fieldcat-scrtext_l = 'País Destino'.
    APPEND ls_fieldcat TO lt_fieldcat.
    CLEAR ls_fieldcat.

    ls_fieldcat-fieldname = 'LAND1TO'.
    ls_fieldcat-scrtext_l = 'País D. Desc.'.
    APPEND ls_fieldcat TO lt_fieldcat.
    CLEAR ls_fieldcat.

    ls_fieldcat-fieldname = 'CITYTO'.
    ls_fieldcat-scrtext_l = 'Ciudad Destino'.
    APPEND ls_fieldcat TO lt_fieldcat.
    CLEAR ls_fieldcat.

    ls_fieldcat-fieldname = 'DEPTIME'.
    ls_fieldcat-scrtext_l = 'Hora Salida'.
    APPEND ls_fieldcat TO lt_fieldcat.
    CLEAR ls_fieldcat.

    "Botones
    ls_variant-report = sy-repid.

    "Zebreado
    ls_layout-zebra = abap_true.

    "Ancho columnas
    ls_layout-cwidth_opt = abap_true.

    "Validacion container
    IF go_container IS INITIAL.

        "Instancia Container
        CREATE OBJECT go_container
        EXPORTING
            container_name = 'CUST_CONTAINER'.

        "Instancia ALV
        CREATE OBJECT go_alv
        EXPORTING
            i_parent = go_container.

    ENDIF.

    "Vista ALV
    CALL METHOD go_alv->set_table_for_first_display
        EXPORTING
        i_save          = 'A'
        is_variant      = ls_variant
        is_layout       = ls_layout
        CHANGING
        it_outtab       = gt_vuelos
        it_fieldcatalog = lt_fieldcat.

    ENDFORM.
    *&---------------------------------------------------------------------*
    *&      Form  F_USER_COMMAND
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *  -->  p1        text
    *  <--  p2        text
    *----------------------------------------------------------------------*
    FORM f_user_command .
    "Funcionalidad de cada boton
    CASE gv_ok_code.
        WHEN 'BACK'.
        SET SCREEN 0.
        LEAVE SCREEN.
        WHEN 'EXIT'.
        SET SCREEN 0.
        LEAVE SCREEN.
        WHEN 'CANCEL'.
        SET SCREEN 0.
        LEAVE SCREEN.
        WHEN 'PRINT'.
        PERFORM f_smartform.
    ENDCASE.
    ENDFORM.
    *&---------------------------------------------------------------------*
    *&      Form  F_SMARTFORM
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *  -->  p1        text
    *  <--  p2        text
    *----------------------------------------------------------------------*
    FORM f_smartform .

    "Codigo smartform
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
        formname           = 'ZMM_VUELOS_02'
        IMPORTING
        fm_name            = gv_fm_name
        EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
    * Implement suitable error handling here
    ENDIF.

    CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum+0(4) INTO gv_sys_date SEPARATED BY '/'.

    CALL FUNCTION gv_fm_name
        EXPORTING
        iv_meteo  = pa_met
        it_vuelos = gt_vuelos
        iv_date   = gv_sys_date.

    ENDFORM.
    ```

* **Dynpro 9000**

    ```abap
    ************************************************************************
    PROCESS BEFORE OUTPUT.
    ************************************************************************

    "Botones y titulo
    MODULE set_status_title.

    "alv
    MODULE set_alv.

    ************************************************************************
    PROCESS AFTER INPUT.
    ************************************************************************

    MODULE user_command.
    ```

* **Módulos PBO: zmm_i_imprimir_vuelos_02_o01**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_IMPRIMIR_VUELOS_02_O01
    *&---------------------------------------------------------------------*
    *&---------------------------------------------------------------------*
    *&      Module  SET_STATUS_TITLE  OUTPUT
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    MODULE set_status_title OUTPUT.

    PERFORM f_status_title.

    ENDMODULE.
    ```

* **Módulos PAI: zmm_i_imprimir_vuelos_02_i01**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_IMPRIMIR_VUELOS_02_I01
    *&---------------------------------------------------------------------*
    *&---------------------------------------------------------------------*
    *&      Module  USER_COMMAND  INPUT
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    MODULE user_command INPUT.

    PERFORM f_user_command.

    ENDMODULE.
    ```

* **Smartform**

    * **Atributos**

        ![Atributas smartform](/2.-ABAP/images/smartform1.PNG)

    * **Interfase**

    ![Interfase smartform](/2.-ABAP/images/smartform3.PNG)

    * **Definiciones globales**

        ![Definiciones smartform](/2.-ABAP/images/smartform4.PNG)

    * **Elementos**

        ![Elementos smartform](/2.-ABAP/images/smartform2.PNG)

    * **Ventana aviso meteo: WD_METEO**

        ![Elementos smartform](/2.-ABAP/images/smartform5.PNG)

    * **Tabla datos de ventana principal: TB_LISTA_VUELOS**

        ![Elementos smartform](/2.-ABAP/images/smartform6.PNG)

    * **Logica proceso zebreado**

        Se crean las variables globales y se especifican las condiciones en cada linea. La logica debe ir en una celda de cada tipo de linea.

        ![Elementos smartform](/2.-ABAP/images/smartform7.PNG)