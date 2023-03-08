# Tema 2.- Desarrollo de aplicaciones ABAP/4. Módulos

## Índice

1. [Introducción](#1-introducción)
2. [Subrutinas](#2-subrutinas)
3. [Grupo de funciones](#3-grupo-de-funciones)
4. [Includes](#4-includes)
5. [Clases](#5-clases)
6. [Métodos](#6-métodos)

# 1. Introducción

ABAP/4 es un lenguaje modular, es decir, permite dividir el programa en varias partes mediante el uso de subrutinas, métodos, funciones, etc. 

Esta modularización hace que el código sea mas fácil de entender, de mantener y que se puedan encapsular los datos.

![Modulos](/2.-ABAP/images/mod1.png)

La modularización puede ser de dos tipos:

* **Local**: Los módulos son accesibles sólo en el programa en el que se crean. Puede llevarse a cabo mediante:
    * Subrutinas.
    * Clases locales.
    * Includes.

    ![Modulos](/2.-ABAP/images/mod2.png)
* **Global**: Los módulos son accesibles desde cualquier programa. Se lleva a cabo mediante:
    * Grupos de función/Módulos de función (SE37).
    * Clases/Métodos (SE24).

    ![Modulos](/2.-ABAP/images/mod3.png)

# 2. Subrutinas

Una subrutina es una sección de código, ejecutable de forma individual, que puede ser llamado desde cualquier punto del programa o desde otros programas externos.

Dentro de una subrutina se pueden declarar variables, ejecutar sentencias, realizar cálculos, mostrar datos por pantalla, lanzar mensajes, etc. 

Para definir una subrutina, se utiliza la sentencia FORM para indicar el inicio de la subrutina (a continuación el nombre) y la sentencia ENDFORM para indicar el final de la misma.

```abap
FORM [nombre].
    ...
ENDFORM.
```

Para hacer la llamada a la subrutina se usa la sentencia PERFORM seguida del nombre de la subrutina.

```abap
PERFORM [nombre].
```

Ejemplo:

```abap
REPORT zmm_r_modular_02.

* Invoco subrutina
PERFORM f_imprimir_nombre.

* Subrutina
FORM f_imprimir_nombre.
  WRITE: / 'Mi nombre es Paco'.
ENDFORM.

START-OF-SELECTION.
* Invoco subrutina
PERFORM f_imprimir_nombre.
```

Las subrutinas permiten el paso de parámetros, especificados en la definición de la misma (FORM), a los que llamamos parámetros formales. Los especificados en el momento en el que se llama a la subrutina se llaman parámetros actuales.

Se pueden diferenciar distintos tipos de parámetros:

* **Entrada**: se usan para pasar datos a la subrutina que solo se usarán dentro de la misma.
* **Salida**: se usan para recibir datos calculados dentro de la subrutina
* **Entrada/salida**: se usan para pasar datos a la subrutina, que estos se modifiquen dentro de la misma y que se devuelvan al programa principal.

Para el paso de parámetros tendremos las opciones TABLES,USING y CHANGING.

Los parámetros que se pasen en las cláusulas USING y CHANGING pueden ser objetos de cualquier tipo (incluso tablas), así como field symbols.

Los parámetros que se pasen en la cláusula TABLES deben de ser tablas internas.

Se recomienda, en los parámetros formales, definir el tipo de cada uno de ellos. Se pueden definir sin tipo pero no es lo recomendable.

Existen dos tipos de subrutinas:

* **Subrutinas Internas**: la subrutina solo es llamada desde el mismo programa donde se define.

    ```abap
    PERFORM [nombre].

    FORM [nombre].
        ...
    ENDFORM.
    ```
* **Subrutinas Externas**: la subrutina también es llamada desde otro programa externo.

    ```ABAP
    PERFORM [nombre]([programa]).
    ```

Paso de parámetros por referencia:

* Los parámetros que se cambian dentro de la subrutina se cambian también en el programa que hace la llamada.

* El parámetro formal y el actual ocupan la misma posición de memoria, con lo que su valor puede ser modificado por la subrutina.

* Ejemplo:

    ```abap
    *Subrutina con parámetros
    FORM f_imprimir_nombre USING uv_nombre TYPE char5
                                uv_apellido TYPE char20.

    WRITE:/ 'Mi nombre es', uv_nombre, uv_apellido.
    ENDFORM.

    START-OF-SELECTION.

    PERFORM f_imprimir_nombre USING 'Paco' 'Mantell'.
    PERFORM f_imprimir_nombre USING 'Pepe' 'Palomino'.
    ```

Paso de parámetros por valor:

* Los parámetros que se cambian dentro de la subrutina no se cambian en el programa que hace la llamada.

* El parámetro formal y el actual no ocupa la misma posición de memoria, con lo que el cambio del parámetro formal no tendrá efecto en el parámetro actual.

* Ejemplo: Subrutina que realiza una operación

    ```abap
    REPORT zmm_r_modular_02.

    FORM f_sumar USING uv_op1 TYPE i
                    uv_op2 TYPE i
                CHANGING cv_result TYPE i.

    cv_result = uv_op1 + uv_op2.

    ENDFORM.

    START-OF-SELECTION.

    DATA lv_res TYPE i.

    PERFORM f_sumar USING 5 2 CHANGING lv_res.

    WRITE:/ 'El resultado es', lv_res.
    ```

**Nomenclatura**

* USING: Parámetros de entrada. No deben modificarse.
    * uv_: Variables.
    * us_: Estructuras.
    * ut_: Tablas internas.
    * uo_: Objetos.
* CHANGIN: Parámetros de entrada/salida que son devueltos al programa.
    * cv_: Variables.
    * cs_: Estructuras.
    * ct_: Tablas internas.
    * co_: Objetos.
* TABLES: Para pasar tablas, aunque no se usa.

# 3. Grupo de funciones

El grupo de funciones es un repositorio que agrupa módulos de funciones.

![Grupo de funciones.](/2.-ABAP/images/mod4.png)

Se utiliza para compartir subrutinas y variables entre los distintos módulos de función que pertenecen al grupo de funciones.

Para visualizar los grupos de funciones se utiliza la **transacción SE37** y se escribirá el nombre de la función en el campo “Módulo funciones”.

![Grupo de funciones.](/2.-ABAP/images/mod5.png)

Las funciones o módulos de funciones son módulos especiales guardados en una librería central y agrupados por la función que realizan.

Las funciones RFC (Remote Function Call) se utiliza para realizar la comunicación entre aplicaciones de diferentes sistemas SAP o sistemas no SAP. El objetivo es acceder a programas externos para obtener datos.

Características:

* Tienen un interface definido que facilita el paso de parámetros de entrada como de salida.
* Realizan un tratamiento de excepciones.

    ```abap
    CALL FUNCTION [nombre].
        EXPORTING [es_val] = [valor]
        IMPORTING [in_value] = [valor]
        TABLES [tab_val] = [tabla]
        EXCEPTIONS [except] = [valor].
    ```

* Parámetro EXPORTING (obligatorio): especifica los parámetros de entrada.

* Parámetro IMPORTING (opcional): indica el resultado o retorno de la función.

* Parámetro TABLES (opcional): indica las tablas que se van a utilizar.

* Parámetro EXCEPTIONS (opcional): indica los valores de excepciones para el retorno de la función, que posteriormente se podrá completar con ```sy-subrc```. Las expresiones se pueden activar mediante las instrucciones:

    ```abap
    MESSAGE [MENSAJE].
    ```
    
    o bien:

    ```abap
    RAISE [excepcion].
    ```

# 4. Includes

La sentencia include permite insertar código fuente dentro del programa especificado.

Normalmente **los includes se utilizan para agrupar subrutinas que van a ser reutilizadas en distintos programas**.

Ventajas de usar includes:

* Los programas son más fáciles de depurar y de mantener a lo largo del tiempo.

* Hace que el cuerpo principal del programa sea reducido para, a simple vista poder identificar cuál es el objetivo de cada subrutina.

Para hacer uso de includes se utiliza la instrucción:

```abap
INCLUDE [subrutina].
```

**Nomenclatura**:

* ZXX_I_YYYY_TOP: Declaraciones globales.
* ZXX_I_YYYY_E01: Eventos.
* ZXX_I_YYYY_F01: Subrutinas.
* ZXX_I_YYYY_C01: Clases locales.
* ZXX_I_YYYY_S01: Pantalla de selección.

## Ejemplo: Modularización del programa de gestión de coches.

El report, con la introducción de los ```INCLUDE``` tendría la siguiente forma:

```abap
*&---------------------------------------------------------------------*
*& Report ZMM_R_GEST_COCHES_02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_r_gest_coches_mod_02.

INCLUDE zmm_i_gest_coches_mod_02_top. "Declaracion de datos
INCLUDE zmm_i_gest_coches_mod_02_s01. "Pantalla de selección
INCLUDE zmm_i_gest_coches_mod_02_e01. "Eventos
INCLUDE zmm_i_gest_coches_mod_02_f01. "Subrutinas
```

Si abrimos la lista de objetos (botón "Visualizar lista de objetos" o Ctrl+Shift+F5) podremos observar los siguientes archivos, tal y como se muestra en la imagen:

![Modulos](/2.-ABAP/images/mod6.PNG)

Cada uno de los include contiene una parte del código. Cada uno de ellos lleva una nomenclatura asociada a la parte de código que albergan:

* **Declaraciones de datos: ZMM_I_GEST_COCHES_MOD_02_TOP**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_GEST_COCHES_MOD_02_TOP
    *&---------------------------------------------------------------------*

    TABLES zmm_t_coches_02.

    * Estructura
    DATA: BEGIN OF ls_listado,
            cod_coche   TYPE zmm_t_coches_02-cod_coche,
            marca       TYPE zmm_t_coches_02-marca,
            modelo      TYPE zmm_t_coches_02-modelo,
            matricula   TYPE zmm_t_coches_02-matricula,
            pais        TYPE zmm_t_coches_02-pais_fab,
            pais_desc   TYPE t005t-landx,
            precio      TYPE zmm_t_coches_02-precio,
            moneda      TYPE zmm_t_coches_02-moneda,
            moneda_desc TYPE tcurt-ltext,
        END OF ls_listado.

    DATA: lt_listado LIKE TABLE OF ls_listado WITH HEADER LINE,
        ls_coches  LIKE zmm_t_coches_02,
        lv_reg     TYPE i.
    ```

* **Pantalla de selección: ZMM_I_GEST_COCHES_MOD_02_S01**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_GEST_COCHES_MOD_02_S01
    *&---------------------------------------------------------------------*

    SELECTION-SCREEN BEGIN OF BLOCK bl WITH FRAME TITLE TEXT-t01.
    * Radiobuttons
    PARAMETERS: rd_vist RADIOBUTTON GROUP rdg1 TYPE xfeld,
                rd_crea RADIOBUTTON GROUP rdg1 TYPE xfeld,
                rd_mod  RADIOBUTTON GROUP rdg1 TYPE xfeld,
                rd_del  RADIOBUTTON GROUP rdg1 TYPE xfeld.

    SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t02.
    * select options
    SELECT-OPTIONS: so_codco FOR zmm_t_coches_02-cod_coche NO-EXTENSION NO INTERVALS,
                so_marca FOR zmm_t_coches_02-marca NO-EXTENSION NO INTERVALS,
                so_model FOR zmm_t_coches_02-modelo NO-EXTENSION NO INTERVALS,
                so_pais FOR zmm_t_coches_02-pais_fab NO-EXTENSION NO INTERVALS,
                so_matr FOR zmm_t_coches_02-matricula NO-EXTENSION NO INTERVALS,
                so_prec FOR zmm_t_coches_02-precio NO-EXTENSION NO INTERVALS,
                so_mon FOR zmm_t_coches_02-moneda NO-EXTENSION NO INTERVALS.
    SELECTION-SCREEN END OF BLOCK b2.
    SELECTION-SCREEN END OF BLOCK bl.
    ```

* **Eventos: ZMM_I_GEST_COCHES_MOD_02_E01**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_GEST_COCHES_MOD_02_E01
    *&---------------------------------------------------------------------*

    ************************************************************************
    START-OF-SELECTION.
    ************************************************************************

    PERFORM f_seleccion.

    **************************************************************************
    AT SELECTION-SCREEN.
    **************************************************************************

    PERFORM f_codigo.

    **************************************************************************
    END-OF-SELECTION.
    **************************************************************************

    PERFORM f_muestra.
    ```

* **Subrutinas: ZMM_I_GEST_COCHES_MOD_02_F01**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_GEST_COCHES_MOD_02_F01
    *&---------------------------------------------------------------------*
    *&---------------------------------------------------------------------*
    *&      Form  F_CREAR
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *  -->  p1        text
    *  <--  p2        text
    *----------------------------------------------------------------------*
    FORM f_crear .
    INSERT zmm_t_coches_02 FROM ls_coches.

    IF sy-subrc NE 0.
        MESSAGE 'Ya se ha creado éste valor' TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
        MESSAGE 'Datos creados' TYPE 'S'.
    ENDIF.
    ENDFORM.
    *&---------------------------------------------------------------------*
    *&      Form  F_VER
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *  -->  p1        text
    *  <--  p2        text
    *----------------------------------------------------------------------*
    FORM f_ver .
    SELECT cod_coche marca modelo matricula pais_fab landx precio moneda ltext
        FROM zmm_t_coches_02
        INNER JOIN t005t
        ON zmm_t_coches_02~pais_fab EQ t005t~land1
        INNER JOIN tcurt
        ON zmm_t_coches_02~moneda EQ tcurt~waers
        INTO TABLE lt_listado
        WHERE cod_coche IN so_codco
        AND marca IN so_marca
        AND modelo IN so_model
        AND matricula IN so_matr
        AND tcurt~spras EQ sy-langu
        AND t005t~spras EQ sy-langu.
    *  verificamos si hay datos
    IF sy-subrc NE 0.
        MESSAGE 'No existen datos' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
    *   Contamos registros
    DESCRIBE TABLE lt_listado LINES lv_reg.
    ENDFORM.
    *&---------------------------------------------------------------------*
    *&      Form  F_MODIF
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *  -->  p1        text
    *  <--  p2        text
    *----------------------------------------------------------------------*
    FORM f_modif .
    UPDATE zmm_t_coches_02 FROM ls_coches.

    IF sy-subrc NE 0.
        MESSAGE 'Error al modificar los datos' TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
        MESSAGE 'Datos modificados' TYPE 'S'.
    ENDIF.
    ENDFORM.
    *&---------------------------------------------------------------------*
    *&      Form  F_DEL
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *  -->  p1        text
    *  <--  p2        text
    *----------------------------------------------------------------------*
    FORM f_del .
    DELETE zmm_t_coches_02 FROM ls_coches.

    IF sy-subrc NE 0.
        MESSAGE 'Error al borrar los datos' TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
        MESSAGE 'Datos borrados con éxito' TYPE 'S'.
    ENDIF.
    ENDFORM.
    *&---------------------------------------------------------------------*
    *&      Form  F_SELECCION
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *  -->  p1        text
    *  <--  p2        text
    *----------------------------------------------------------------------*
    FORM f_seleccion .
    * Asigno datos a la estructura
    ls_coches-cod_coche = so_codco-low.
    ls_coches-marca = so_marca-low.
    ls_coches-modelo = so_model-low.
    ls_coches-matricula = so_matr-low.
    ls_coches-pais_fab = so_pais-low.
    ls_coches-precio = so_prec-low.
    ls_coches-moneda = so_mon-low.

    *Validacion radiobutton
    *Visualizacion
    IF rd_vist EQ abap_true.
        PERFORM f_ver.

    * Creacion
    ELSEIF rd_crea EQ abap_true.
        PERFORM f_crear.

    * Modificacion
    ELSEIF rd_mod EQ abap_true.
        PERFORM f_modif.

    * Eliminacion
    ELSEIF rd_del EQ abap_true.
        PERFORM f_del.
    ENDIF.
    ENDFORM.
    *&---------------------------------------------------------------------*
    *&      Form  F_CODIGO
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *  -->  p1        text
    *  <--  p2        text
    *----------------------------------------------------------------------*
    FORM f_codigo .
    * Codigo obligatorio al borrar coche
    IF rd_del EQ abap_true AND so_codco IS INITIAL.
        MESSAGE 'Debe introducir el código del coche' TYPE 'E'.
    ENDIF.
    ENDFORM.
    *&---------------------------------------------------------------------*
    *&      Form  F_MUESTRA
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *  -->  p1        text
    *  <--  p2        text
    *----------------------------------------------------------------------*
    FORM f_muestra .
    * Muestro por pantalla
    IF rd_vist EQ abap_true.
        LOOP AT lt_listado ASSIGNING FIELD-SYMBOL(<ls_listado>).
        AT FIRST.
            WRITE: 'Código coche', 'Marca', 'Modelo', 'Pais', 'Pais (desc)', 'Matricula', 'Precio',
                'Moneda', 'Moneda(desc)'.
        ENDAT.

        WRITE:/ <ls_listado>-cod_coche, <ls_listado>-marca, <ls_listado>-modelo,
                <ls_listado>-pais, <ls_listado>-pais_desc, <ls_listado>-matricula,
                <ls_listado>-precio, <ls_listado>-moneda, <ls_listado>-moneda_desc.

        AT LAST.
            WRITE:/ 'Numero de registros:', lv_reg.
        ENDAT.
        ENDLOOP.
    ENDIF.
    ENDFORM.
    ```

## Ejemplo: Modularización del programa listado de coches.

Programa principal:

```abap
*&---------------------------------------------------------------------*
*& Report ZMM_R_LIST_COCHE2_02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

REPORT zmm_r_list_coche2_mod_02.

INCLUDE zmm_i_list_coche2_mod_02_top. "Declaracion de variables
INCLUDE zmm_i_list_coche2_mod_02_s01. "Pantalla seleccion
INCLUDE zmm_i_list_coche2_mod_02_e01. "Eventos
INCLUDE zmm_i_list_coche2_mod_02_f01. "Subrutinas
```

Includes:

* **Declaración de variables: zmm_i_list_coche2_mod_02_top**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_LIST_COCHE2_MOD_02_TOP
    *&---------------------------------------------------------------------*

    TABLES zmm_t_coches_02.

    * Estructura
    DATA: BEGIN OF ls_listado,
            cod_coche   TYPE zmm_t_coches_02-cod_coche,
            marca       TYPE zmm_t_coches_02-marca,
            modelo      TYPE zmm_t_coches_02-modelo,
            matricula   TYPE zmm_t_coches_02-matricula,
            pais        TYPE zmm_t_coches_02-pais_fab,
            pais_desc   TYPE t005t-landx,
            precio      TYPE zmm_t_coches_02-precio,
            moneda      TYPE zmm_t_coches_02-moneda,
            moneda_desc TYPE tcurt-ltext,
        END OF ls_listado.

    DATA lt_listado LIKE TABLE OF ls_listado WITH HEADER LINE.
    ```

* **Pantalla de selección: zmm_i_list_coche2_mod_02_s01**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_LIST_COCHE2_MOD_02_S01
    *&---------------------------------------------------------------------*

    SELECTION-SCREEN BEGIN OF BLOCK bl WITH FRAME TITLE text_t01.
    SELECT-OPTIONS: so_codco FOR zmm_t_coches_02-cod_coche,
                    so_marca FOR zmm_t_coches_02-marca,
                    so_model FOR zmm_t_coches_02-modelo,
                    so_matr FOR zmm_t_coches_02-matricula.
    SELECTION-SCREEN END OF BLOCK bl.
    ```

* **Eventos: zmm_i_list_coche2_mod_02_e01**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_LIST_COCHE2_MOD_02_E01
    *&---------------------------------------------------------------------*
    ************************************************************************
    START-OF-SELECTION.
    ************************************************************************

    PERFORM f_seleccion.
    ***********************************************************************
    END-OF-SELECTION.
    ***********************************************************************

    PERFORM f_mostrar.
    ```

* **Subrutinas: zmm_i_list_coche2_mod_02_f01**

    ```abap
    *&---------------------------------------------------------------------*
    *&  Include           ZMM_I_LIST_COCHE2_MOD_02_F01
    *&---------------------------------------------------------------------*
    *&---------------------------------------------------------------------*
    *&      Form  F_SELECCION
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *  -->  p1        text
    *  <--  p2        text
    *----------------------------------------------------------------------*
    FORM f_seleccion .
    * select de datos (Columnas de las tablas origen que quiero seleccionar)
    SELECT cod_coche marca modelo matricula pais_fab landx precio moneda ltext
        FROM zmm_t_coches_02
        INNER JOIN t005t                          "Primer Inner Join
        ON zmm_t_coches_02~pais_fab EQ t005t~land1
        INNER JOIN tcurt                          "Segundo Inner Join
        ON zmm_t_coches_02~moneda EQ tcurt~waers
        INTO TABLE lt_listado
        WHERE cod_coche IN so_codco               "Las condiciones las marcan
        AND marca IN so_marca                     "los Select-options
        AND modelo IN so_model
        AND matricula IN so_matr
        AND tcurt~spras EQ sy-langu                  "Selecciono descripcion.
        AND t005t~spras EQ sy-langu.                 "en español

    * Check
    IF sy-subrc NE 0.
        MESSAGE 'No existen datos' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
    ENDFORM.
    *&---------------------------------------------------------------------*
    *&      Form  F_MOSTRAR
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *  -->  p1        text
    *  <--  p2        text
    *----------------------------------------------------------------------*
    FORM f_mostrar .
    * muestro por pantalla
    LOOP AT lt_listado INTO ls_listado.
        WRITE:/ ls_listado-cod_coche, ls_listado-marca, ls_listado-modelo,
                ls_listado-matricula, ls_listado-pais, ls_listado-pais_desc,
                ls_listado-precio, ls_listado-moneda, ls_listado-moneda_desc.
    ENDLOOP.
    ENDFORM.
    ```
# 5. Clases

En la programación orientada a objetos, la única unidad de estructuración son las clases, es decir, se trabaja con objetos que encapsulan y auto-contienen sus datos.

Las clases son una definición abstracta de objetos. Significa que la declaración o definición de un objeto es la clase.

Los objetos serán “ejemplares” de una clase cualquiera.

Se llama instanciar a la acción de crear un objeto a partir de una clase, por eso se conocen a los objetos como instancias de una clase.

Existen dos tipos de clases, las clase globales y las clases locales:

* **Clases globales**: Todos los programas de ABAP en un sistema SAP pueden acceder a las clases globales.

* **Clases locales**: Se definen dentro de un programa ABAP y pueden ser usadas solamente en el programa donde estén definidas.

Declaración de una clase local:

```abap
CLASS [nombre] DEFINITION.
...
ENDCLASS.
```

Implementación de una clase local:

```abap
CLASS [nombre] IMPLEMENTATION.
...
ENDCLASS.
```

# 6. Métodos

Los métodos son procedimientos que definen el comportamiento de un objeto.

Se utilizan para  implementar una lógica asociada al cometido de la clase, pudiendo acceder y modificar los atributos de la clase.

Una definición completa de una clase constará de una parte declarativa, lo que se conoce como definición de la clase, en la que se definen los componentes, y una parte de implementación, lo que se conoce como implementación de la clase, en la que se implementan estos componentes..

Los métodos pueden ser estáticos o instanciados

Declaración de métodos dependientes de instancia:

```abap
METHODS [nombre].
IMPORTING...
EXPORTING...
CHANGING...
RETURNING VALUE [valor]
```

Declaración de métodos estáticos:

```abap
CLASS-METHODS [nombre]
IMPORTING...
EXPORTING...
CHANGING...
RETURNING VALUE [valor]
EXCEPCIONS...
```

Implementación de métodos:

```abap
METHOD [nombre]
IMPORTING...
EXPORTING...
CHANGING...
RETURNING VALUE [valor]
EXCEPCIONS...
...
ENDMETHOD.
```