# Tema 2.- Desarrollo de aplicaciones ABAP/4. Ficheros ALV.

## Índice

# 1. Introducción

Abap List Viewer: Podemos definir un ALV como un listado de datos, sobre los cuales podremos realizar ciertas acciones:

* **Clasificar**: se puede ordenar la lista de salida de forma ascendente/descendente por cualquiera de los campos.

* **Filtrar**: una vez se ha mostrado la información se podrán realizar filtros por cada uno de los campos.

* **Disposición**: se podrá modificar los campos que se desean visualizar así como modificar su orden y disposición.

* **Exportar**: se podrá exporta la información visualizada a ficheros locales.

* **Modificar**: un ALV puede ser editable, de forma que se pueda modificar la información visualizada.

* **Botones**: se podrán añadir botones Z que realicen cierta funcionalidad no aportada por el estándar.


# 2. ALV Grid

Método para que aparezca el ALV por pantalla: set_table_for_first_display

La implementación de un listado mediante ALV Grid supone la preparación de una serie de componentes:

* **Lista de datos**: se deberá disponer de una **tabla interna que contenga los datos a listar**. Ésta puede estar implementada mediante cualquier tipo de estructura plana. Los campos estructurados sólo están permitidos para determinadas funcionalidades del ALV Grid (por ejemplo, los atributos de color de las celdas).

* **Field Catalog**: Se deberá disponer de otra **tabla interna que contendrá las especificaciones de cómo deben visualizarse los campos de la lista de datos**. Esta tabla interna se deberá referenciar sobre el tipo “LVC_T_FCAT”.

* **Layout**: Se deberá rellenar una **estructura para especificar las opciones del layout del ALV Grid**. Mediante esta estructura es posible definir opciones de visualización generales para el componente. Esta estructura se deberá referenciar sobre el tipo “LVC_S_LAYO”.

* **Event Handler**: Se deberá definir e implementar en el programa una **clase para gestión de eventos** si se desean gestionar los eventos que pueda lanzar el ALV Grid. Después de crear la instancia, se deberá registrar también una instancia sobre la clase gestora de eventos que se haya definido en el programa.

* **Datos adicionales**: Para poner en marcha algunas funcionalidades que proporciona el control ALV Grid, se deberán pasar algunos parámetros más al componente. Por ejemplo para definir criterios de ordenación o la desactivación de botones de control.


Nomenclatura:

* go_: Global Object.
* grf_: Global reference to.

## Creación de una vista de fichero ALV: ZMM_R_ALV_LIBROS_02

* **Report**

    ```abap
    *&---------------------------------------------------------------------*
    *& Report ZMM_R_ALV_LIBROS_02
    *&---------------------------------------------------------------------*
    *&
    *&---------------------------------------------------------------------*
    REPORT ZMM_R_ALV_LIBROS_02.

    INCLUDE zmm_i_alv_libros_02_top. "Declaraciones de datos
    INCLUDE zmm_i_alv_libros_02_s01. "Pantalla seleccion
    INCLUDE zmm_i_alv_libros_02_e01. "Eventos
    INCLUDE zmm_i_alv_libros_02_f01. "Subrutinas
    INCLUDE zmm_i_alv_libros_02_i01. "Modulos PAI
    INCLUDE zmm_i_alv_libros_02_o01. "Modulos PBO
    ```

* **Declaraciones globales**: Para crear el ALV, necesitamos definir la clase del mismo, la de un contenedor y la tabla con los datos del siguiente modo:

    ```abap
  *&---------------------------------------------------------------------*
  *&  Include           ZMM_I_ALV_LIBROS_02_TOP
  *&---------------------------------------------------------------------*

  TABLES zmm_t_libros_02.

  * Creamos el ALV
  DATA: go_alv       TYPE REF TO cl_gui_alv_grid,         "Clase para el ALV
        go_container TYPE REF TO cl_gui_custom_container, "Clase para el contenedor
        gt_libros    TYPE TABLE OF zmm_t_libros_02,       "Tabla de datos
        gv_ok_code   TYPE sy-ucomm.                       "Comandos
    ```

* **Pantalla seleccion**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_ALV_LIBROS_02_S01
    *&---------------------------------------------------------------------*

    SELECTION-SCREEN begin of BLOCK b1 WITH FRAME TITLE text-t01.
    select-OPTIONS so_codli for zmm_t_libros_02-cod_libro.
    SELECTION-SCREEN end of BLOCK b1.
    ```

* **Eventos**: Guardamos los datos en nuestra tabla interna y llamamos a la pantalla de la Dynpro en el END-OF-SELECTION:

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_ALV_LIBROS_02_E01
    *&---------------------------------------------------------------------*

    ************************************************************************
    START-OF-SELECTION.
    ************************************************************************
    * Seleccion de datos
    PERFORM f_select_data.

    ************************************************************************
    END-OF-SELECTION.
    ************************************************************************

    IF gt_libros IS NOT INITIAL.
        CALL SCREEN 9000.
    ENDIF.
    ```

* **Subrutinas**

  ```abap
  *&---------------------------------------------------------------------*
  *&  Include           ZMM_I_ALV_LIBROS_02_F01
  *&---------------------------------------------------------------------*
  *&---------------------------------------------------------------------*
  *&      Form  F_SELECT_DATA
  *&---------------------------------------------------------------------*
  *       text
  *----------------------------------------------------------------------*
  *  -->  p1        text
  *  <--  p2        text
  *----------------------------------------------------------------------*
  FORM f_select_data .
    SELECT *
      FROM zmm_t_libros_02
      INTO TABLE gt_libros
      WHERE cod_libro IN so_codli.

    IF sy-subrc NE 0.
      MESSAGE 'No se han encontrado datos' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *&      Form  F_SET_ALV
  *&---------------------------------------------------------------------*
  *       text
  *----------------------------------------------------------------------*
  *  -->  p1        text
  *  <--  p2        text
  *----------------------------------------------------------------------*
  FORM f_set_alv .

  * Validamos si se ha creado el container
    IF go_container IS INITIAL.

      "Instanciamos el container
      CREATE OBJECT go_container
        EXPORTING
          container_name = 'CUST_CONTAINER'.

      "Instanciamos el ALV
      CREATE OBJECT go_alv
        EXPORTING
          i_parent = go_container.
    ENDIF.

    "Generamos la vista del ALV
    CALL METHOD go_alv->set_table_for_first_display
      EXPORTING
        i_structure_name = 'ZMM_T_LIBROS_02'
      CHANGING
        it_outtab        = gt_libros.

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

    "Activamos botones
    SET PF-STATUS 'ZSTATUS_02'.

    " Activamos titulo
    SET TITLEBAR 'ZTITULO_02'.

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
      WHEN 'PDF'.
    ENDCASE.

  ENDFORM.
  ```

* **Modulos PBO**

  ```abap
  *&---------------------------------------------------------------------*
  *&  Include           ZMM_I_ALV_LIBROS_02_O01
  *&---------------------------------------------------------------------*
  *&---------------------------------------------------------------------*
  *&      Module  SET_ALV  OUTPUT
  *&---------------------------------------------------------------------*
  *       text
  *----------------------------------------------------------------------*
  MODULE set_alv OUTPUT.

    " Mostramos ALV
    PERFORM f_set_alv.

  ENDMODULE.
  *&---------------------------------------------------------------------*
  *&      Module  SET_STATUS_TITLE  OUTPUT
  *&---------------------------------------------------------------------*
  *       text
  *----------------------------------------------------------------------*
  MODULE set_status_title OUTPUT.

    "Set status GUI y titulo
    PERFORM f_status_title.

  ENDMODULE.
  ```

* **Modulos PAI**

  ```abap
  *&---------------------------------------------------------------------*
  *&  Include           ZMM_I_ALV_LIBROS_02_I01
  *&---------------------------------------------------------------------*
  *&---------------------------------------------------------------------*
  *&      Module  USER_COMMAND  INPUT
  *&---------------------------------------------------------------------*
  *       text
  *----------------------------------------------------------------------*
  MODULE user_command INPUT.

    " Logica botones
    PERFORM f_user_command.

  ENDMODULE.
  ```

* **Dynpro**

  ```abap
  ************************************************************************
  PROCESS BEFORE OUTPUT.
  ************************************************************************
  * Funcionalidad de botones
  MODULE set_status_title.

  * Cargamos el ALV
  MODULE set_alv.

  ************************************************************************
  PROCESS AFTER INPUT.
  ************************************************************************

  MODULE user_command.
  ```


para añadir funcionalidad a los botones, creamos un status GUI haciendo click con boton derecho sobre el report DENTRO DE LA VISUALIZACION DE OBJETOS => CRear => Status GUI.

En la Dynpro, creamos un elemento llamado gv_ok_code, el cual se debe declarar como variable de tipo sy-ucomm, en el include de declaraciones globales.

Creamos un módulo PBO que muestre los botones activos mediante una subrutina.

Creamos un modulo PAI y una subrutina para darle la funcionalidad a los botones.

## Ejercicio: Fichero ALV para vuelos con Popup en el código de aerolínea para mostrar su descripción.

* **Report: zmm_r_vuelos_alv_02**

    ```abap
    *&---------------------------------------------------------------------*
    *& Report ZMM_R_VUELOS_ALV_02
    *&---------------------------------------------------------------------*
    *&
    *&---------------------------------------------------------------------*
    REPORT ZMM_R_VUELOS_ALV_02.

    INCLUDE zmm_i_vuelos_alv_02_top. "Declaraciones globales
    INCLUDE zmm_i_vuelos_alv_02_c01. "Clases
    INCLUDE zmm_i_vuelos_alv_02_s01. "Pantalla selección
    INCLUDE zmm_i_vuelos_alv_02_e01. "Eventos
    INCLUDE zmm_i_vuelos_alv_02_f01. "Subrutinas
    INCLUDE zmm_i_vuelos_alv_02_o01. "Modulos PBO
    INCLUDE zmm_i_vuelos_alv_02_i01. "Modulos PAI
    ```

* **Declaraciones globales: zmm_i_vuelos_alv_02_top**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_VUELOS_ALV_02_TOP
    *&---------------------------------------------------------------------*

    TABLES: spfli, t005t, scarr.

    "Estructura
    DATA: BEGIN OF gs_vuelos,
            carrid    TYPE spfli-carrid,
            countryfr TYPE spfli-countryfr,
            land1fr   TYPE t005t-landx,
            cityfrom  TYPE spfli-cityfrom,
            countryto TYPE spfli-countryto,
            land1to   TYPE t005t-landx,
            cityto    TYPE spfli-cityto,
            deptime   TYPE spfli-deptime,
        END OF gs_vuelos.


    DATA: go_alv       TYPE REF TO cl_gui_alv_grid,
        go_container TYPE REF TO cl_gui_custom_container,
        gt_vuelos    LIKE TABLE OF gs_vuelos,
        gv_ok_code   TYPE sy-ucomm.

    DATA: BEGIN OF gs_scarr,
            carrid   TYPE scarr-carrid,
            carrname TYPE scarr-carrname,
        END OF gs_scarr.

    DATA: gt_scarr LIKE TABLE OF gs_scarr.
    ```

* **Clases locales: zmm_i_vuelos_alv_02_c01**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_VUELOS_ALV_02_C01
    *&---------------------------------------------------------------------*

    "Definicion del controlador de eventos
    CLASS lcl_event_handler DEFINITION.
    PUBLIC SECTION.


        METHODS:handle_hotspot_click
                    FOR EVENT hotspot_click OF cl_gui_alv_grid "Capturo el evento de la clase
        IMPORTING e_row_id e_column_id.


    ENDCLASS. "lcl_event_handler DEFINITION

    DATA: gr_event_handler TYPE REF TO lcl_event_handler.

    "Implementacion controlador de eventos
    CLASS lcl_event_handler IMPLEMENTATION.

    METHOD handle_hotspot_click.

        "Leo el codigo de aerolinea que he clicado
        READ TABLE gt_vuelos INTO gs_vuelos INDEX e_row_id-index.

        IF sy-subrc NE 0.
          RETURN.
        ENDIF.

        "Selecciono la descripcion correspondiente
        SELECT SINGLE carrid carrname
        FROM scarr
        INTO gs_scarr
        WHERE carrid EQ gs_vuelos-carrid.

        "Pop up
        IF sy-subrc = 0. "Muestro informacion
        CALL FUNCTION 'POPUP_TO_INFORM'
            EXPORTING
            titel = 'Nombre Compañia'
            txt1  = gs_vuelos-carrid
            txt2  = gs_scarr-carrname
            .
        ELSEIF sy-subrc ne 0. "Mensaje si no encuentro nada
        CALL FUNCTION 'POPUP_TO_INFORM'
            EXPORTING
            titel         = 'Error'
            txt1          = 'No se encuentra descripcion para el codigo de Áerolínea:'
            txt2          = gs_vuelos-carrid
            txt3          = 'Compruebe el código'
                    .

        ENDIF.

    ENDMETHOD. "handle_hotspot_click


    ENDCLASS. "lcl_event_handler IMPLEMENTATION
    ```

* **Pantalla selección: zmm_i_vuelos_alv_02_s01**

  ```abap
  *&---------------------------------------------------------------------*
  *&  Include           ZMM_I_VUELOS_ALV_02_S01
  *&---------------------------------------------------------------------*

  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
    select-OPTIONS so_couto FOR spfli-countryto.
  SELECTION-SCREEN END OF BLOCK b1.
  ```

* **Eventos: zmm_i_vuelos_alv_02_e01**

  ```abap
  *&---------------------------------------------------------------------*
  *&  Include           ZMM_I_VUELOS_ALV_02_E01
  *&---------------------------------------------------------------------*

  ************************************************************************
  START-OF-SELECTION.
  ************************************************************************

    PERFORM f_selection.

  ************************************************************************
  END-OF-SELECTION.
  ************************************************************************
    "Genero la dynpro
    IF gt_vuelos IS NOT INITIAL.
      CALL SCREEN 9000.
    ENDIF.
  ```

* **Subrutinas: zmm_i_vuelos_alv_02_f01**

  ```abap
  *&---------------------------------------------------------------------*
  *&  Include           ZMM_I_VUELOS_ALV_02_F01
  *&---------------------------------------------------------------------*
  *&---------------------------------------------------------------------*
  *&      Form  F_SELECTION
  *&---------------------------------------------------------------------*
  *       text
  *----------------------------------------------------------------------*
  *  -->  p1        text
  *  <--  p2        text
  *----------------------------------------------------------------------*
  FORM f_selection .
    
    "Seleccion de datos para el alv
    SELECT carrid countryfr t1~landx cityfrom countryto t2~landx cityto deptime
      FROM spfli
      INNER JOIN t005t AS t1
      ON spfli~countryfr EQ t1~land1
      INNER JOIN t005t AS t2
      ON spfli~countryto EQ t2~land1
      INTO TABLE gt_vuelos
      WHERE countryto IN so_couto
      AND t1~spras EQ sy-langu
      AND t2~spras EQ sy-langu.

    IF sy-subrc NE 0.
      MESSAGE 'No se han encontrado vuelos' TYPE 'E'.
    ENDIF.
  ENDFORM.
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
    DATA: lt_fieldcat TYPE lvc_t_fcat,
          ls_fieldcat TYPE lvc_s_fcat,
          ls_layout   TYPE lvc_s_layo,
          ls_variant  TYPE disvariant,
          lt_sort     TYPE lvc_t_sort,
          ls_sort     TYPE lvc_s_sort.

    "Creamos el catalogo
    ls_fieldcat-fieldname = 'CARRID'.
    ls_fieldcat-scrtext_l = 'Compañia'.
    ls_fieldcat-hotspot = abap_true. "Lo marcamos como Hotspot
    ls_fieldcat-key = abap_true.
    APPEND ls_fieldcat TO lt_fieldcat.
    CLEAR ls_fieldcat.

    ls_fieldcat-fieldname = 'COUNTRYFR'.
    ls_fieldcat-scrtext_l = 'País Origen'.
    APPEND ls_fieldcat TO lt_fieldcat.
    CLEAR ls_fieldcat.

    ls_fieldcat-fieldname = 'LAND1FR'.
    ls_fieldcat-scrtext_l = 'Pais O. Desc.'.
    APPEND ls_fieldcat TO lt_fieldcat.
    CLEAR ls_fieldcat.

    ls_fieldcat-fieldname = 'CITYFROM'.
    ls_fieldcat-scrtext_l = 'Ciudad'.
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

    "Creamos el sort
    ls_sort-fieldname = 'CARRID'.
    ls_sort-up = abap_true.
    APPEND ls_sort TO lt_sort.
    CLEAR ls_sort.

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
        it_fieldcatalog = lt_fieldcat
        it_sort         = lt_sort.

    "Genero el evento popup
    CREATE OBJECT gr_event_handler .
    SET HANDLER gr_event_handler->handle_hotspot_click FOR go_alv.

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

    SET PF-STATUS 'ZSTATUS02'.

    "Activamos titulo
    SET TITLEBAR 'ZTITULO02'.

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
        LEAVE SCREEN.
      WHEN 'EXIT'.
        LEAVE SCREEN.
      WHEN 'CANCEL'.
        LEAVE SCREEN.
    ENDCASE.

  ENDFORM.
  ```

* **Dynpro**

  Para crearla: Click derecho sobre report en lista de elementos -> Crear -> Dynpro. Por defecto se emplea la 9000.

  Se debe añadir ```GV_OK_CODE``` a elementos, para que capte el click en el botón y se pueda lanzar el módulo ```user_command```.

  Hay que crear el estado GUI y el título GUI para poder lanzar el módulo ```set_status_title``` (click derecho sobre report en lista de elementos -> Crear -> Status/Titulo GUI).

  ```abap
  ************************************************************************
  PROCESS BEFORE OUTPUT.
  ************************************************************************

  "Botones y titulo
  MODULE set_status_title.

  "Mostrar ALV
  MODULE set_alv.

  ************************************************************************
  PROCESS AFTER INPUT.
  ************************************************************************

  MODULE user_command.
  ```

* **Modulos PBO: zmm_i_vuelos_alv_02_o01**

  ```abap
  *&---------------------------------------------------------------------*
  *&  Include           ZMM_I_VUELOS_ALV_02_O01
  *&---------------------------------------------------------------------*
  *&---------------------------------------------------------------------*
  *&      Module  SET_ALV  OUTPUT
  *&---------------------------------------------------------------------*
  *       text
  *----------------------------------------------------------------------*
  MODULE set_alv OUTPUT.

    PERFORM f_set_alv.

  ENDMODULE.
  *&---------------------------------------------------------------------*
  *&      Module  SET_STATUS_TITLE  OUTPUT
  *&---------------------------------------------------------------------*
  *       text
  *----------------------------------------------------------------------*
  MODULE set_status_title OUTPUT.

    PERFORM f_status_title.

  ENDMODULE.
  ```

* **Modulos PAI: zmm_i_vuelos_alv_02_i01**

  ```abap
  *&---------------------------------------------------------------------*
  *&  Include           ZMM_I_VUELOS_ALV_02_I01
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

# 3. ALV Factory

Se pueden emplear para obtener listados simples, com menor capacidad de configuración que un ALV Grid. Suelen emplearse para mostrar tablas sencillas en las que no se necesita edición o con poca cantidad de registros.

```abap
*&---------------------------------------------------------------------*
*& Report ZMM_R_ALV_FAC_LIBROS_02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_r_alv_fac_libros_02.

"Declaracion de variables
DATA: lt_libros   TYPE TABLE OF zmm_t_libros_02,
      lo_alv      TYPE REF TO cl_salv_table,
      lo_function TYPE REF TO cl_salv_functions_list.

"Seleccion de datos
SELECT *
  FROM zmm_t_libros_02
  INTO TABLE lt_libros.

IF sy-subrc NE 0.
  MESSAGE 'No existen libros' TYPE 'E'.
ENDIF.

"Instancia del alv
CALL METHOD cl_salv_table=>factory
  IMPORTING
    r_salv_table = lo_alv
  CHANGING
    t_table      = lt_libros.

"Obtener instancia de funciones
lo_function = lo_alv->get_functions( ).

"Mostramos los botones
lo_function->set_all( abap_true ).

"Mostramos ALV
lo_alv->display( ).
```

# 4. ALV Functions


## Ejercicio: ALV para libros mediante funciones.

* **Report: zmm_r_alv_fun_libros_02**

  ```abap
  *&---------------------------------------------------------------------*
  *& Report ZMM_R_ALV_FUN_LIBROS_02
  *&---------------------------------------------------------------------*
  *&
  *&---------------------------------------------------------------------*
  REPORT ZMM_R_ALV_FUN_LIBROS_02.

  INCLUDE zmm_i_alv_fun_libros_02_top. "Declaraciones globales
  INCLUDE zmm_i_alv_fun_libros_02_s01. "Pantalla seleccion
  INCLUDE zmm_i_alv_fun_libros_02_e01. "Eventos
  INCLUDE zmm_i_alv_fun_libros_02_f01. "Subrutinas
  ```

* **Declaraciones globales: zmm_i_alv_fun_libros_02_top**

  ```abap
  *&---------------------------------------------------------------------*
  *&  Include           ZMM_I_ALV_FUN_LIBROS_02_TOP
  *&---------------------------------------------------------------------*

  TABLES zmm_t_libros_02.

  DATA: BEGIN OF ls_libros,
          cod_libro TYPE zmm_e_codigo_libro_02,
          titulo    TYPE zmm_e_titulo_libro_02,
        END OF ls_libros.

  DATA lt_libros LIKE TABLE OF ls_libros.
  ```

* **Pantalla selección: zmm_i_alv_fun_libros_02_s01**

  ```abap
  *&---------------------------------------------------------------------*
  *&  Include           ZMM_I_ALV_FUN_LIBROS_02_S01
  *&---------------------------------------------------------------------*

      SELECTION-SCREEN begin of BLOCK b1 WITH FRAME TITLE text-t01.
      select-OPTIONS so_codli for zmm_t_libros_02-cod_libro.
      SELECTION-SCREEN end of BLOCK b1.
  ```

* **Eventos: zmm_i_alv_fun_libros_02_e01**

  ```abap
  *&---------------------------------------------------------------------*
  *&  Include           ZMM_I_ALV_FUN_LIBROS_02_E01
  *&---------------------------------------------------------------------*

  ************************************************************************
  START-OF-SELECTION.
  ************************************************************************

    PERFORM f_select.

  ************************************************************************
  END-OF-SELECTION.
  ************************************************************************

    PERFORM f_show_alv.
  ```

* **Subrutinas: zmm_i_alv_fun_libros_02_f01**

  ```abap
  *&---------------------------------------------------------------------*
  *&  Include           ZMM_I_ALV_FUN_LIBROS_02_F01
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

    SELECT cod_libro titulo
      FROM zmm_t_libros_02
      INTO TABLE lt_libros
      WHERE cod_libro IN so_codli.

    IF sy-subrc NE 0.
      MESSAGE 'No existen libros' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDFORM.
  *&---------------------------------------------------------------------*
  *&      Form  F_SHOW_ALV
  *&---------------------------------------------------------------------*
  *       text
  *----------------------------------------------------------------------*
  *  -->  p1        text
  *  <--  p2        text
  *----------------------------------------------------------------------*
  FORM f_show_alv .

    "Creamos catalogo de campos
    DATA: lt_fieldcat TYPE TABLE OF slis_fieldcat_alv,
          ls_fieldcat TYPE slis_fieldcat_alv,
          ls_variant  TYPE disvariant,
          ls_layout   TYPE slis_layout_alv.

    ls_fieldcat-fieldname = 'COD_LIBRO'.
    ls_fieldcat-seltext_l = 'Cod libro'.
    "ls_fieldcat-tech = abap_true. "Hago la columna técnica y no se muestra
    APPEND ls_fieldcat TO lt_fieldcat.
    CLEAR ls_fieldcat.

    ls_fieldcat-fieldname = 'TITULO'.
    ls_fieldcat-seltext_l = 'Titulo'.
    ls_fieldcat-outputlen = 25.
    APPEND ls_fieldcat TO lt_fieldcat.
    CLEAR ls_fieldcat.

    "Botones de layout
    ls_variant-report = sy-repid.

    "Color zebreado
    ls_layout-zebra = abap_true.

    "Generamos ALV a partir de catalogo de campos
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        it_fieldcat   = lt_fieldcat
        i_save        = 'A'
        is_variant    = ls_variant
        is_layout     = ls_layout
      TABLES
        t_outtab      = lt_libros
      EXCEPTIONS
        program_error = 1
        OTHERS        = 2.
    "Generamos ALV a partir de una estructura
  *CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  * EXPORTING
  *   I_STRUCTURE_NAME                  = 'ZMM_T_LIBROS_02'
  *  TABLES
  *    t_outtab                          = lt_libros
  * EXCEPTIONS
  *   PROGRAM_ERROR                     = 1
  *   OTHERS                            = 2
  *          .

    IF sy-subrc NE 0.
      MESSAGE 'Error al mostrar ALV' TYPE 'E'.
    ENDIF.


  ENDFORM.
  ```