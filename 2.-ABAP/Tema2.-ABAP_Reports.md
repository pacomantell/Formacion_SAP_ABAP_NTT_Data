# Tema 2.- Desarrollo de aplicaciones ABAP/4. Reports

## Índice

1. [Introducción](#1-introducción)
2. [Pantalla de selección](#2-pantalla-de-selección)
    1. [Elementos de texto](#21-elementos-de-texto)
    2. [Ejemplos](#22-ejemplos)
3. [Eventos](#3-eventos)
4. [Depurador ABAP](#4-depurador-abap)
5. [Tipos de datos](#5-tipos-de-datos-instalados)
6. [Declaración de variables](#6-declaración-de-variables)
7. [Open SQL](#7-open-sql)
    1. [Sentencia SELECT](#71-sentencia-select)
    2. [Union de tablas: JOINS](#72-unión-de-tablas-joins)
    3. [Sentencias de tratamiento de datos](#73-sentencias-de-tratamiento-de-datos)
    4. [Tablas internas](#74-tablas-internas)
    5. [Field Symbols](#75-field-symbols)

# 1. Introducción

Un report es una generación de una lista de salida (pantalla, impresora u otro medio) basada en datos de BBDD y generada a partir de unos parámetros de entrada indicados por el usuario.

Están basados en un lenguaje estructurado pero orientado a eventos, es decir, la secuencia de instrucciones se ejecuta en función del evento generado por el usuario.

Se generan en la **transacción SE38**.

![Menu report](/2.-ABAP/images/reports1.PNG)

Existe la posibilidad de ejecutar on-line o en fondo.

Existen varios **tipos de reports**. Habrá que especificar de qué tipo será al crearlo:

* **Programa ejecutable**: report ejecutable por si solo, no necesita un report externo para poder ser ejecutado.
* **Programa include**: report no ejecutable. Contiene código que podrá ser ejecutable desde otro report externo (programa de control) pero no desde el mismo programa.
* **Module pool**: programa con control de pantallas, con mas complejidad que un programa ejecutable.

Una vez creado, se nos abre un editor de código ABAP, el cual cuenta con los siguientes botones:

![Editor reports](/2.-ABAP/images/reports2.PNG)
![Editor reports](/2.-ABAP/images/reports3.PNG)

## Pretty printer

En Utilidades-Opciones-Editor ABAP-Editor, es posible cambiar las opciones del **editor de texto integrado en ABAP**, el cual tiene como **objetivo unificar criterios de mayúsculas, minúsculas, sangrías, etc**. 

![Pretty printer](/2.-ABAP/images/reports4.PNG)

Su utilización se considera una buena practica de programación, ayudando a hacer el código más comprensible.

## Modelo

Accediendo a ésta opción, es posible generar el código de llamada de funciones. Posee los siguientes elementos:

* **Call function**: introduciendo el nombre de la función, sap genera el código de la llamada, con sus parámetros y excepciones.
* **ABAP Objets Patterns**: para clases, introduciendo la instancia, clase y método se genera el código de la llamada directamente.
* **Message**: introduciendo el mensaje (id, número y tipo) se genera el código de lanzamiento del mensaje.
* **Select * from**: para generar select a tablas.
* **Perform**: llamadas a subrutinas.
* **Authority check**: genera el código para comprobar el authority check introducido.

![Call Functions](/2.-ABAP/images/reports5.PNG)

# 2. Pantalla de selección

Es la salida generada por los reports. Posee varios elementos:

* **Bloques**: Estructura en la que se integran los elementos de la pantalla de selección. Actúa como un contenedor con título.
  Normalmente, **los bloques delimitan un conjunto de parámetros asociados al mismo concepto**. Se recomienda separar las pantallas de selección en bloques para que aparezcan de una forma estructurada.

  Además, llevan asociados un título para indicar el resumen de los parámentros que se encuentran dentro del mismo.

  Sintáxis de un bloque:

  ```abap
  SELECTION-SCREEN BEGIN OF BLOCK [nombre del bloque] WITH FRAME TITLE [símbolo de texto].
    * Parámetros
  SELECTION SCREEN END OF BLOCK [nombre del bloque].
  ```

* **Select options**: Entradas de datos desde un valor inicial hasta otro valor final. Éstos elementos **deben ir asociados a un campo de una tabla de base de datos**. Además, llevan asociados un texto de selección.

  Sintáxis de un select options:

  ```abap
  SELECT-OPTIONS [nombre] FOR [nombre tabla-nombre campo].
  ```

* **Parámetro**: Entrada de un valor. Los parámetros sólo permiten la entrada de valores individuales. Además, llevan asociados un texto de selección.

  Sintáxis de un parámetro:

  ```abap
  PARAMETERS [nombre del parámetro] TYPE/LIKE [tipo de datos / tabla-nombre de la tabla].
  ```

* **Check Box**: Botón de selección de opción, asociado a dos valores: " " o "X". Pueden llevar sentencias adicionales al final de su declaración.

  Sintáxis:

  ```abap
  PARAMETERS [nombre] TYPE [tipo] AS CHECKBOX
  ```

* **Radiobutton**: Grupo de botones asociado, utilizados en casos en los que una opción excluye al resto; es decir, **sólo se elige uno de ellos**. Pueden llevar sentencias adicionales al final. Habrá que definir tantos elementos del grupo como opciones queramos tener.

  Sintáxis:

  ```abap
  PARAMETERS [nombre] RADIOBUTTON GROUP [nombre grupo] TYPE [tipo].
  ```

![Pantalla seleccion](/2.-ABAP/images/reports6.PNG)

Los elementos anteriores pueden llevar **sentencias adicionales** al final, como:

* **Default**: para introducir un valor por defecto cuando se ejecute el report
* **Obligatory**: hace el parámetro obligatorio
* **Help request**: para añdir una ayuda de búsqueda
* **No display**: mantenerlo oculto

## 2.1. Elementos de texto

Los textos necesarios para nombrar elementos de la pantalla de selección, mensajes, columnas del listado, etc, se definen en los elementos de texto.

Llevan asociada traducción, de modo que, en función del lenguaje con el que acceda a sap el usuario visualizará unos textos y otros.

Para acceder a su contenido, desde cualquier punto del programa se puede hacer referencia a su código.

## 2.2. Ejemplos

### Hola mundo

Veámos cómo escribir un programa que muestre por pantalla la frase 'Hola mundo'. Para ello, ejecutamos la transacción SE38 y creamos un report llamado ZMM_PRUEBA_02, que sea ejecutable, con el siguiente código:

```abap
*&---------------------------------------------------------------------*
*& Report ZMM_R_PRUEBA_02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_r_prueba_02.

WRITE 'Hola Mundo'.
```

### Primera pantalla de seleccion

En ABAP, podemos crear variables asociadas a elementos de datosdel siguiente modo:

```abap
DATA lv_codigo_libro TYPE ZMM_E_CODIGO_LIBRO_02.
```

Veámos cómo sería el report para una pantalla de selección sencilla:

```abap
*&---------------------------------------------------------------------*
*& Report ZMM_R_PRUEBA_02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_r_prueba_02.

*Importamos una tabla
TABLES zmm_t_libros_02.

* Creamos un bloque
SELECTION-SCREEN BEGIN OF BLOCK bl WITH FRAME TITLE TEXT-t01.

    *Parametro
    PARAMETERS pa_codli TYPE zmm_e_codigo_libro_02 OBLIGATORY MATCHCODE OBJECT zmm_h_libros_02. "obligatorio con ayuda de busqueda
    PARAMETERS pa_tit TYPE zmm_e_titulo_libro_02.

    *Select options
    SELECT-OPTIONS so_tipo FOR zmm_t_libros_02-tipo.
    SELECT-OPTIONS so_mon FOR zmm_t_libros_02-moneda.

    * espacio en banco
    SELECTION-SCREEN SKIP.

    *check-box
    PARAMETERS pa_nuevo TYPE zmm_t_libros_02-nuevo.

    * espacio en banco
    SELECTION-SCREEN SKIP.

    *radio-button group
    PARAMETERS pa_rd1 RADIOBUTTON GROUP gr1 TYPE xfeld. "xfeld es como un booleano
    PARAMETERS pa_rd2 RADIOBUTTON GROUP gr1 TYPE xfeld.

SELECTION-SCREEN END OF BLOCK bl.

*Condicional que me selecciona el tipo de tapa a partir de radiobutton
IF pa_rd1 EQ abap_true.

  zmm_t_libros_02-tapa = 1.

ELSEIF pa_rd2 EQ abap_true.

  zmm_t_libros_02-tapa = 2.

ENDIF.
```

Los nombres de variable de parámetros solo pueden tener 8 caracteres de longitud. Las sentencias adicionales se pueden visualizar con Ctrl+Espacio.

### Seleccion de vuelos

Ejemplo de pantalla de seleccion de vuelos a partir de una tabla integrada en SAP:

```abap
*&---------------------------------------------------------------------*
*& Report ZMM_R_SELECT_VUELOS_02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_r_select_vuelos_02.

TABLES spfli.

SELECTION-SCREEN BEGIN OF BLOCK bl WITH FRAME TITLE TEXT-t01.

    SELECTION-SCREEN BEGIN OF BLOCK orig WITH FRAME TITLE TEXT-t02.
        SELECT-OPTIONS so_paiso FOR spfli-countryfr.
        SELECT-OPTIONS so_cityo FOR spfli-cityfrom.
    SELECTION-SCREEN END OF BLOCK orig.

    SELECTION-SCREEN BEGIN OF BLOCK dest WITH FRAME TITLE TEXT-t03.
        SELECT-OPTIONS so_paisd FOR spfli-countryto.
        SELECT-OPTIONS so_cityd FOR spfli-cityto.
    SELECTION-SCREEN END OF BLOCK dest.

    PARAMETERS pa_met TYPE xfeld AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK bl.
```

La salida sería la siguiente:

![Programa vuelos](/2.-ABAP/images/ej_vuelos1.PNG)

donde se han editado los correspondientes elementos de texto.

### Calculadora

Programa calculadora:

```abap
*&---------------------------------------------------------------------*
*& Report ZMM_R_CALCULAR_02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_r_calcular_02.

DATA lv_resultado TYPE i.

*Pantalla seleccion
SELECTION-SCREEN BEGIN OF BLOCK bl WITH FRAME TITLE TEXT-t01.
 SELECTION-SCREEN BEGIN OF BLOCK operadores WITH FRAME TITLE TEXT-t02.
  PARAMETERS pa_op1 OBLIGATORY TYPE i.
  PARAMETERS pa_op2 OBLIGATORY TYPE i.
 SELECTION-SCREEN END OF BLOCK operadores.

 SELECTION-SCREEN BEGIN OF BLOCK operacion WITH FRAME TITLE TEXT-t03.
  PARAMETERS pa_rd1 RADIOBUTTON GROUP gr1 TYPE xfeld.
  PARAMETERS pa_rd2 RADIOBUTTON GROUP gr1 TYPE xfeld.
  PARAMETERS pa_rd3 RADIOBUTTON GROUP gr1 TYPE xfeld.
  PARAMETERS pa_rd4 RADIOBUTTON GROUP gr1 TYPE xfeld.
 SELECTION-SCREEN END OF BLOCK operacion.

SELECTION-SCREEN END OF BLOCK bl.

*Seleccion de operacion
IF pa_rd1 EQ abap_true.
  lv_resultado = pa_op1 + pa_op2.
ELSEIF pa_rd2 EQ abap_true.
  lv_resultado = pa_op1 - pa_op2.
ELSEIF pa_rd3 EQ abap_true.
  lv_resultado = pa_op1 * pa_op2.
ELSEIF pa_rd4 EQ abap_true.
  lv_resultado = pa_op1 / pa_op2.
ENDIF.

*Muestra resultado
WRITE: 'El resultado es: ', lv_resultado.
```

Salida

![Calculadora](/2.-ABAP/images/ej_calculadora1.PNG)

Al ejecutar, se mostrará el resultado de la operación seleccionada, teniendo en cuenta que, por ejemplo, la división es entera en éste caso.

Modificando el bloque IF podemos avisar al dividir por 0:

```abap
*Mensaje de error si dividimos por 0
IF pa_rd4 EQ abap_true AND pa_op2 EQ 0.

  MESSAGE 'No se puede dividir por 0' TYPE 'S' DISPLAY LIKE 'E'.

ELSE.
*seleccion de operacion
 IF pa_rd1 EQ abap_true.
  lv_resultado = pa_op1 + pa_op2.
 ELSEIF pa_rd2 EQ abap_true.
  lv_resultado = pa_op1 - pa_op2.
 ELSEIF pa_rd3 EQ abap_true.
  lv_resultado = pa_op1 * pa_op2.
 ELSEIF pa_rd4 EQ abap_true.
  lv_resultado = pa_op1 / pa_op2.
 ENDIF.
ENDIF.
```

# 3. Eventos

ABAP 4 no es un lenguaje con estructura lineal, sino orientado a eventos, de modo que la secuencia de instrucciones dependen del evento que se haya lanzado en cada momento.

Si no se indica ninguna etiqueta de evento, el que propone sap por defecto es el START-OF-SELECTION.

![Eventos](/2.-ABAP/images/reports7.png)

**El orden de los eventos no tiene por qué ser secuencial**, debido al tipo de programación de ABAP. Un evento comienza con su sentencia de declaración y finaliza cuando comienza otro evento.

**La declaración de variables y la pantalla de selección no se encuentran dentro de ningún evento.**

En un report **no pude haber dos eventos del mismo tipo**.

Eventos más importantes:

* **INITIALIZATION**: Evento lanzado en el momento en el que se ejecuta el programa desde la SE38 o a través de una transacción, se ejecuta antes incluso que el “pintado” de la pantalla de selección. Solo se ejecuta una vez y **se utiliza normalmente para inicializar variables necesarias en la pantalla de selección**.

* **START-OF-SELECTION**: Cuando se ejecuta el report desde la pantalla de selección. Es el lugar indicado para que se **realiza la selección de datos en función de los parámetros indicados en la pantalla de selección.**

* **END-OF-SELECTION**: Se ejecuta justo después del START-OF-SELECTION. Es el **punto indicado para realizar la impresión del listado por pantalla en el caso de que aplique**.

* **TOP-OF-PAGE**: Se ejecuta en el momento previo a imprimir la página actual. **Se utiliza para escribir cabeceras o títulos dentro de una página**.

* **END-OF-PAGE**: Se activa si se alcanza el área de la página reservada para este evento (con la opción LINE-COUNT en PROGRAM o REPORT) al final de la página; si no hay área definida, no se activa el evento.

* **AT SELECTION-SCREEN**: este evento se ejecuta después de que el sistema haya procesado la pantalla de selección. **Si se muestra un mensaje de error dentro de las sentencias asignadas a este evento, no se continúa con el proceso lógico hacia el siguiente evento**, sino que se continúa en la pantalla de selección para que el usuario corrija el error. Todos los campos aparecerán activos. Es muy **empleado para hacer validaciones**. Posee varias **variantes**:

  * **AT SELECTION-SCREEN ON VALUE-REQUEST FOR [parámetro/select-options]**: exactamente igual que el anterior pero **solo se ejecuta cuando el sistema haya procesado el parámetro indicado** y en el momento de mostrar un mensaje de error solo aparecerá activo en la pantalla de selección el parámetro tratado.

  * **AT SELECTION-SCREEN ON END OF [select-options]**: el bloque de proceso asociado al evento se ejecuta cuando se introducen datos en un select-options en su pantalla de selección múltiple.

  * **AT SELECTION-SCREEN ON VALUE-REQUEST FOR [parámetro/select-options]**: este evento permite crear un bloque de proceso asociado en el momento en el que se ejecuta la ayuda de búsqueda de un campo. En este punto se muestra un pop-up con los valores posibles a seleccionar.

  * **AT SELECTION-SCREEN ON RADIOBUTTON GROUP [radiobutton group]**: este evento se lanza cuando se pulse alguno de los radiobutton asociados al radiobutton group indicado. Si se muestra un mensaje de error en el bloque de proceso asignado a este evento, se vuelve a mostrar la pantalla de selección en la que solamente aparecerán activos los radiobutton pertenecientes al radiobutton group indicado.

  * **AT SELECTION-SCREEN ON BLOCK [bloque]**:este evento permite activar un bloque de proceso cuando el sistema termina de procesar un bloque en concreto. Si se lanza un mensaje de error se muestra de nuevo la pantalla de selección y solo los campos del bloque relacionado se pueden modificar.

  * **AT SELECTION-SCREEN OUTPUT**. Sirve para gestionar dinámicamente el estado de los campos.

## Programa ejemplo eventos:

```abap
*&---------------------------------------------------------------------*
*& Report ZPRUEBA_EVENTOS_02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zprueba_eventos_02.

* Defino variables y parametros
DATA: lv_codigo_libro TYPE zmm_e_codigo_libro_02,
      lv_titulo_libro TYPE zmm_e_titulo_libro_02.

PARAMETERS: pa_tit   TYPE zmm_e_tipo_libro_02,
            pa_check AS CHECKBOX USER-COMMAND check,
            pa_tip   TYPE zmm_e_titulo_libro_02.


************************************************************************
START-OF-SELECTION.
************************************************************************
* Realiza una asignacion de variables
  SELECT titulo
    FROM zmm_t_libros_02
  INTO lv_titulo_libro
  WHERE cod_libro EQ lv_codigo_libro.
  ENDSELECT.

************************************************************************
INITIALIZATION.
************************************************************************

* Inicializo variables
  lv_codigo_libro = '00001'.

************************************************************************
END-OF-SELECTION.
************************************************************************

  WRITE lv_codigo_libro.

************************************************************************
TOP-OF-PAGE.
************************************************************************

  WRITE 'Encabezado'.

************************************************************************
END-OF-PAGE.
************************************************************************

  WRITE 'Pie de página'.

************************************************************************
AT SELECTION-SCREEN.
************************************************************************
* Mensaje si no se pone titulo
  IF pa_tit IS INITIAL.
    MESSAGE 'Se debe rellenar el campo título' TYPE 'E'.
  ENDIF.

************************************************************************
AT SELECTION-SCREEN OUTPUT.
************************************************************************
* Opcion de ocultar la entrada titulo
  LOOP AT SCREEN.
    IF screen-name CS 'pa_tip' AND pa_check EQ abap_true.
      screen-input = 0.
      screen-invisible = 1.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.
```

## Programa de la calculadora empleando los eventos

```abap
*&---------------------------------------------------------------------*
*& Report ZMM_R_CALCULAR_EVENTOS_02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_r_calcular_eventos_02.

DATA lv_resultado TYPE i.

*Pantalla seleccion
SELECTION-SCREEN BEGIN OF BLOCK bl WITH FRAME TITLE TEXT-t01.
SELECTION-SCREEN BEGIN OF BLOCK operadores WITH FRAME TITLE TEXT-t02.
PARAMETERS pa_op1 OBLIGATORY TYPE i.
PARAMETERS pa_op2 OBLIGATORY TYPE i.
SELECTION-SCREEN END OF BLOCK operadores.

SELECTION-SCREEN BEGIN OF BLOCK operacion WITH FRAME TITLE TEXT-t03.
PARAMETERS pa_rd1 RADIOBUTTON GROUP gr1 TYPE xfeld.
PARAMETERS pa_rd2 RADIOBUTTON GROUP gr1 TYPE xfeld.
PARAMETERS pa_rd3 RADIOBUTTON GROUP gr1 TYPE xfeld.
PARAMETERS pa_rd4 RADIOBUTTON GROUP gr1 TYPE xfeld.
SELECTION-SCREEN END OF BLOCK operacion.

SELECTION-SCREEN END OF BLOCK bl.

************************************************************************
START-OF-SELECTION.
************************************************************************

*seleccion de operacion
  IF pa_rd1 EQ abap_true.
    lv_resultado = pa_op1 + pa_op2.
  ELSEIF pa_rd2 EQ abap_true.
    lv_resultado = pa_op1 - pa_op2.
  ELSEIF pa_rd3 EQ abap_true.
    lv_resultado = pa_op1 * pa_op2.
  ELSEIF pa_rd4 EQ abap_true.
    lv_resultado = pa_op1 / pa_op2.
  ENDIF.

************************************************************************
END-OF-SELECTION.
************************************************************************

*Muestra resultado
  WRITE: 'El resultado es: ', lv_resultado.

************************************************************************
AT SELECTION-SCREEN.
************************************************************************

* Mensaje si pretendemos dividir por cero
  IF pa_rd4 EQ abap_true AND pa_op2 EQ 0.
    MESSAGE 'No se puede dividir por 0' TYPE 'E'.
  ENDIF.
```

# 4. Depurador ABAP

Herramienta imprenscindible para poder probar y depurar las aplicaciones de SAP.

Hay dos tipos de breakpoints:

* **Breakpoints de sesión**: son puntos solo válidos para la session actual, son independientes del usuario. Son los que más se suelen usar. (Icono de STOP con la pantalla).

* **Breakpoints externos**: son válidos para cualquier aplicación que se ejecute en el sistema, independientemente desde donde se haya lanzado. Ej:en aplicaciones web dynpro que se ejecutan desde un navegador solo funcionarían este tipo de breakpoints. En éste caso hay que configurar el usuario para el que se ejecutará. (Icono de STOP con la persona).

Además de los programas de usuario, es posible depurar transacciones estándar de SAP, como módulos de funciones.

Es posible modificar opciones del debugging en: UtilidadesOpcionesEditor ABAPDebugging

* Se podrá indicar para que usuario se desea que se fijen los breakpoints
* Debugger nuevo/antiguo: se podrá elegir que debugguer se desea utilizar cuando salte el breakpoint.

Una vez comienza la depuración, es posible avanzar en la ejecución de varias formas:

* **Paso a paso (F5)**: Se introducirá en la línea de código que se encuentre el cursor:
  * Si es una función, rutina, método, etc. se accederá dentro de la misma para depurarla.
  * Si se trata de una sentencia simple (IF, READ, MOVE, etc) se pasará a la siguiente línea de código.

* **Ejecutar (F6)**: Avanza hasta la siguiente línea de código, independientemente de lo que sea. Por ejemplo si es una función, esta se ejecuta y el degugguer pasa a la siguiente instrucción.

* **Retornar (F7)**: Si el debugguer se encuentra en una rutina, función, etc, se saldrá de la misma, parando en la línea siguiente a la llamada.

* **Continuar (F8)**: El programa se ejecutará hasta encontrar otro break-point o hasta que finalice.

Además, es posible acceder a la siguiente información a través de las diferentes pestañas:

* Pila de llamadas por las que ha ido pasando el flujo del programa

* Se puede navegar hasta el punto en modo debug haciendo doble click sobre la columna “Event Type”

* También se puede navegar en modo editor pulsando sobre la columna “Navigation”

![Depurador](/2.-ABAP/images/reports8.png)

* Event type: tipo de evento que se ha producido: evento de un report, llamada a subrutina, llamada a función,etc.

* Event: cual es el evento que se ha producido

* Program: programa en el cual se ha producido el evento

* Variables 1 y 2: pestañas editables para introducir el nombre de la variable cuyo contenido se desea visualiza

  ![Depurador](/2.-ABAP/images/reports9.png)

* Locals: pestaña no editable donde se visualizarán las variables locales de la subrutina/método en el que nos encontremos.

  ![Depurador](/2.-ABAP/images/reports10.png)

* Globals: pestaña no editable donde se visualizarán las variables globales del programa/función/etc donde nos encontremos.

  ![Depurador](/2.-ABAP/images/reports11.png)

Se podrá modificar cualquier variable, tanto local como global desde las pestañas “Variables 1” o “Variables 2” pulsando en el símbolo del lápiz.

![Depurador](/2.-ABAP/images/reports12.png)

Se modifica el contenido y pulsando “Enter” o el botón del lápiz la variable toma el contenido introducido.

![Depurador](/2.-ABAP/images/reports13.png)

Muy útil cuando se está depurando un programa y haciendo pruebas del mismo.

En tablas, haciendo doble click sobre la misma se accede a su contenido.

![Depurador](/2.-ABAP/images/reports14.png)

Pulsando sobre el botón de “Herramientas” se accede a todas las opciones de edición que disponemos para las tablas.

![Depurador](/2.-ABAP/images/reports15.png)

Opciones de las herramientas:

* Modificar filas seleccionadas: se harán editables la filas seleccionadas
* Eliminar filas seleccionadas: se eliminarán las filas seleccionadas
* Añadir fila: se añadirá una fila al final de la tabla.
* Insertar fila: se añadirá una fila en el índice indicado.
* Borra tabla: se borrarán las filas entre los índices indicados.

Además, haciendo doble click en una fila determinada también se accederá a su contenido en modo edición

![Depurador](/2.-ABAP/images/reports16.png)

# 5. Tipos de datos instalados.

* **C**: Son los caracteres. Por defecto se inicializan a " " (espacio en blanco), y debe especificarse su longitud mediante paréntesis en el nombre de la variable o por la sentencia adicional LENGTH. Para un carácter de longitud 5 sería:

```abap
DATA lv_nombre(5) TYPE c.

DATA lv_nombre TYPE c LENGTH 5.
```

* **D**: Éste tipo sirve para almacenar fechas. Posee una longitud de 8 caracteres, la cual no se puede modificar, es decir, es un dato predefinido. Las fechas en ABAP tienen el siguiente formato: aaaammdd

* **F**: Punto flotentes, es decir, decimales. Normalmente se emplea para valores muy grandes.

* **I**: Integer.

* **N**: Numeric Text. Se emplea sobretodo para cógidos, por ejemplo, códigos de artículos, números de factura, etc. Se debe especificar su longitud, al igual que en los carácter.

* **P**: Packed Decimal. Se emplea para decimales. Se debe especificar su longitud y el número de decimales mediante LENGTH y DECIMALS, respectivamente. Los decimales en ABAP se escriben con un punto, p.e. 2.8.

* **T**: Time, se emplea para la hora, con formato: hhmmss. Para formatearla, se puede emplear lo siguiente:

```abap
WRITE:/ lv_hora(2),':',lv_hora+2(2),':',lv_hora+4(2),/.

WRITE (8) lv_hora USING EDIT MASK '__:__:__'.
```

Se puden sumar dias a una fecha del siguiente modo:

```abap
lv_fecha = lv_fecha + 5.
```

* **H**: Hexadecimal.

* **String**: Es una variable sin longitud, de modo que resulta útil para guardar textos largos. Por ejemplo, para leer archivos de texto y después analizarlos.

* **xstring**: Como un string pero hexadecimal. Se emplea sobretodo en ficheros con cadenas de caracteres binarios.

![Tipos](/2.-ABAP/images/reports17.png)

# 6. Declaración de variables

Las variables lv se denominan local variables, mientras que las ls son local structures.

Para la declaración de variables se utiliza la sentencia DATA. Con la instrucción TYPE se indica el tipo de la variable.

Se puede dar un valor inicial a una variable mediante la sentencia VALUE:

```abap
DATA [nombre variable] TYPE [tipo variable] VALUE [valor].
```

La sentencia LIKE crea una estructura o tipo de datos como uno que ya existe previamente. Por ejemplo, si definimos la variable lv_nombre, podemos definir otra variable lv_nombre2 que sea como la primera del siguiente modo:

```abap
DATA: lv_nombre TYPE c LENGTH 10.
      lv_nombre2 LIKE lv_nombre.
```

de modo que al cambiar las propiedades de la primera, como longitud, también lo harán las de la segunda.

## Sentencias declarativas

* La sentencia **TABLES [nombre tabla]** importa la tabla nombrada desde el diccionario de la base de datos.

* La sentencia **TYPES** se emplea para definir tipos de datos internos al programa. A diferencia del DATA, no permite almacenar datos en ellos. A partir de ellos se pueden crear estructuras de datos con DATA que tengan la misma estructura.

* La sentencia **INLCUDE** permite importar el código fuente de otro report, lo que se emplea mucho a la hora de importar módulos.

* **RANGES**. Internamente son una tabla (array) con las columnas Signo, Option, valor mínimo y valor máximo. Su funcionamiento es similar al del SELECT-OPTIONS.

  Valores de la columna Signo:

    * **I**: Include. Incluye los valores de los extremos.
    * **E**: Exclude. Excluye los valores de los extremos.

  Valores de la columna Option:

    * **BT** (Between): Selecciona los valores entre dos extremos.
    * **EQ**: Igual a.
    * **LE**: Menor o igual que.
    * **GE**: Mayor o igual que.
    * **LT**: Menor que.
    * **GT**: Mayor que.
    * **NE**: No igual que.
    * **NB**: No entre los valores de dos extremos.
    * **CP**: Contiene el modelo.
    * **NP**: No contiene el modelo.

  **Las tablas en ABAP comienzan en el índice número 1.**

* **FIELD-SYMBOLS** son punteros a la memoria. No ocupan espacio en memoria, sólo apuntan hacia la dirección de memoria de la variables que se les asigna:

  ```abap
  *&---------------------------------------------------------------------*
  *& Report ZMM_R_TIPOS_DATOS_02
  *&---------------------------------------------------------------------*
  *&
  *&---------------------------------------------------------------------*
  REPORT zmm_r_tipos_datos_02.

  DATA lv_nombre TYPE string value 'Paco'.

  *Creo el puntero
  FIELD-SYMBOLS <lv_nombre> TYPE any.

  *Asigno el puntero a una variable
  ASSIGN lv_nombre to <lv_nombre>.

  *Compruebo si está asignada
  IF <lv_nombre> is ASSIGNED.
    write <lv_nombre>.
  ENDIF.

  *Desasigno el puntero de la variable
  UNASSIGN <lv_nombre>.

  BREAK-POINT.
  ```

No se puede ejecutar un puntero sin asignar. El programa nos devolverá un error.

```abap
* EJEMPLO CHECK
DATA lv_cont TYPE i VALUE 0.

DO 100 TIMES.
  ADD 1 TO lv_cont.

  "Condicion para parar el ciclo:
  CHECK lv_cont < 50.

  WRITE: / lv_cont.

ENDDO.
```

* **DESCRIBE FIELD** var [ LENGTH long ] [ TYPE tipo ] [ COMPONENTS comps ] [ OUTPUT-LENGTH long ] [ DECIMALS decimales ] [ EDIT MASK mascara ]: Devuelve los atributos del objeto, si el objeto existe, SY-SUBRC valdrá cero.

* **SELECTION-SCREEN, PARAMETERS, y SELECT-OPTIONS**: Son sentencias declarativas de la pantalla de selección.

## Sentencias para abandonar bloques de procesos

* **STOP** : Incondicional. Solo debe usarse en el evento START-OF-SELECTION. Su efecto es cancelar todas las selecciones de las tablas de la base de datos, abandonando el evento y pasando al END-OF-SELECTION (si no se desea este comportamiento, debe usarse EXIT en lugar de STOP).

* **EXIT** : Incondicional. Abandona totalmente el bloque actual o evento (excepto eventos AT...) si no está dentro de ningún bloque. 

* **CHECK** { condición | tabla_selección | SELECT-OPTIONS }.: Condicional. Si la condición no se cumple, acaba el bloque de proceso o evento (si no está dentro de ningún bloque) actual; si se cumple, continúa la ejecución.

* **REJECT** [ tabla_bd ] : Incondicional. Tras esta sentencia, se procesa el siguiente evento GET para la misma tabla, abandonando la selección actual. 

* **CONTINUE**: Incondicional. Acaba con la vuelta actual de un bucle (DO, WHILE,LOOP), de forma incondicional, pasando a la siguiente vuelta. 


## Operaciones de cálculo:

* **ADD campo2 TO campo1**: Idéntica a la operación: campo1 = campo1 + campo2. 

* **ADD n1 THEN n2 UNTIL ni GIVING | TO m**: Hace la operación: m = m + n1 + n2 + … + ni. Se incluye ‘m’ como sumando o no dependiendo de si se usa GIVING o TO

* **ADD-CORRESPONDING tabla2 TO tabla1**: Suma los campos que se correspondan (por su nombre) de 2 registros o tablas, y deja los nuevos valores en el segundo de ellos (“tabla1”).

* **MOVE campo2 TO campo1**: mueve el contenido del campo2 al campo1: campo 1 = campo2.

* **MOVE-CORRESPONDING estructura2 TO structura1**: mueve el contenido de los campos que se correspondan de la estructura2 a los de la estrucura1.

* **SUBSTRACT campo2 FROM campo1**: campo1 = campo1 – campo2

* **SUBSTRACT-CORRESPONDING tabla2 FROM tabla1**: Resta los campos que se correspondan (por su nombre) de la tabla2 a la tabla1.

* **MULTIPLY campo1 BYcampo2**.

* **MULTIPLY-CORRESPONDING tabla1 BY tabla2**.

* **DIVIDE campo1 BY campo2**

* **DIVIDE-CORRESPONDING tabla1 BY tabla2**.

* **COMPUTEn = expresión**: Permite realizar cualquiera de las operaciones aritméticas permitidas:
  * Operaciones básicas: +, -, *, /, MOD, ** (exponenciación). Cubren las sentencias anteriores.
  * Para todo dato numérico: ABS, CEIL (función techo), FLOOR (función suelo), SIGN (devuelve el “valor” del signo: -1, 0, 1), TRUNC (devuelve la parte entera), FRAC (devuelve la parte fraccionaria).
  * Para coma flotante: COS, SEN, TAN, ACOS, ASEN, ATAN, COSH, SENH, TANH, EXP (en base e), LOG (en base e), LOG10 (en base 10), SQRT.

Ejemplo:

```abap
DATA: lv_var1 TYPE i VALUE 1,
      lv_var2 TYPE i VALUE 5,
      lv_res  TYPE i.

*Sumo ambas variables
lv_res = lv_var1 + lv_var2.

* Operacion equivalente a la suma
ADD lv_var1 TO lv_var2.

* Muevo el contenido de var1 a var2
MOVE lv_var1 TO lv_var2.

* Equivalente a 
lv_var2 = lv_var1.

* Para restar variables
subtract lv_var1 from lv_var2.

* Equivalente a 
lv_res = lv_var1 - lv_var2.
```

## Programa ejemplo tipos de datos y sentencias

```abap
*&---------------------------------------------------------------------*
*& Report ZMM_R_TIPOS_DATOS_02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_r_tipos_datos_02.

DATA: lv_nombre   TYPE c LENGTH 100,
      lv_long     TYPE i,
      lv_long_tec TYPE i,
      lv_date     TYPE d,
      lv_cantidad TYPE f,
      lv_decim    TYPE p LENGTH 8 DECIMALS 3,
      lv_hora     TYPE t,
      lv_texto    TYPE string.

* Creo una estructura de datos
DATA: BEGIN OF ls_libro,
        cod_libro TYPE zmm_e_codigo_libro_02,
        titulo    TYPE zmm_e_titulo_libro_02,
      END OF ls_libro.

* Asigno valores a la estructura
ls_libro-cod_libro = '000004'.
ls_libro-titulo = 'Star Wars'.


* Defino una estructura de datos
DATA ls_libro2 TYPE zmm_s_libro_02.

* CReacion de un rango
RANGES lt_r_cod_libro FOR zmm_t_libros_02-cod_libro.

lv_nombre = 'Diego'.

* Longitud del contenido de la variable
lv_long = strlen( lv_nombre ).

* Longitud tecnica del campo
DESCRIBE FIELD lv_nombre OUTPUT-LENGTH lv_long_tec.

*Checkear si la fecha está bien inicializada
IF lv_date IS INITIAL.
* fecha de hoy
  lv_date = sy-datum.
ENDIF.

* Formateamos la fecha
WRITE lv_date DD/MM/YYYY.

*Establecher la hora actual
lv_hora = sy-uzeit.

* Formatear la hora
WRITE:/ lv_hora(2),':',lv_hora+2(2),':',lv_hora+4(2),/.

WRITE (8) lv_hora USING EDIT MASK '__:__:__'.

BREAK-POINT.
```

## Sentencias de manejo de strings

* **CONCATENATE c1 … cn [ SEPARATED BY separador ] INTO campo**: Concatena las variables c1 … cn (deben ser todas de tipo C). Devuelve SY-SUBRC 0 si la operación se realizado de forma correcta, y 4 si el campo destino es demasiado pequeño para guardar la concatenación de los campos origen.

* **SET LOCALE LANGUAGE lenguaje [ COUNTRY país ][ MODIFIER modificador]**.

* **CONDENSE campo [ NO-GAPS]**: El campo debe ser tipo C (una cadena). Esta instrucción une las palabras del campo dejando entre ellas sólo un blanco. Con NO-GAPS se eliminan todos los blancos.

* **REPLACE string1 WITH string2 INTO campo [ LENGTH long ]**: Reemplaza (sólo la primera ocurrencia) de “string1” por “string2” en el valor de “campo”. Para que reemplace todas las ocurrencias se añade la sentencia ALL OCURRENCES OF.

  ```abap
  DATA: lv_var1 TYPE string VALUE 'HOLA',
        lv_var2 TYPE string VALUE 'MUNDO',
        lv_res  TYPE string.
  * Concateno ambas variables
  CONCATENATE lv_var1 lv_var2 INTO lv_res.

  * Si quiero que aparezcan separadas por un espacio
  CONCATENATE lv_var1 lv_var2 INTO lv_res RESPECTING BLANKS.
  * Equivalente
  CONCATENATE lv_var1 lv_var2 INTO lv_res SEPARATED BY space.

  * Separadas por otra cosa
  CONCATENATE lv_var1 lv_var2 INTO lv_res SEPARATED BY 'X'.

  WRITE lv_res.

  *reemplazar la primera ocurrencia de MUNDO
  REPLACE 'MUNDO' with 'PACO' INTO lv_res.

  * reemplazar todas las ocurrencias
  REPLACE ALL OCCURRENCES OF 'MUNDO' in lv_res WITH 'PACO'.
  ```

* **SPLIT campo AT separador INTO c1 … cn | INTO TABLE table**: divide la variable “campo” por el separador dado “separador” en los campos c1…cn. Si SUBRC = 0, los campos contienen el espacio suficiente, de lo contrario no disponen del espacio suficiente y los trunca. Divide la variable por el separador dado y lo guarda en un array (TABLE):

  ```abap
  DATA: lv_fecha_for TYPE c LENGTH 10 VALUE '18.10.2023',
        lt_fecha_tab TYPE TABLE OF string.

  SPLIT lv_fecha_for AT '.' INTO TABLE lt_fecha_tab.
  ```

* **SHIFT campo [ BY n PLACES ] [ UP TO campo1 ] [ LEFT | RIGHT ] [ CIRCULAR ][ LEFT DELETING LEADING campo1 ] [ RIGHT DELETING TRAILING campo1 ]**: Desplaza “campo” n posiciones (1 por defecto) a la izquierda o derecha, de forma circular o no (según se especifique) Rellena con blancos al desplazar

* **TRANSLATE campo [TO UPPER CASE | TO LOWER CASE ] [ USING campo1 ][FROM CODE PAGE g1 TO CODE PAGE g2 ] [ FROM NUMBER FORMAT n1TONUMBER FORMAT n2 ]**: se modifica el campo a mayúsculas o a minúsculas según se especifique.

  ```abap
  DATA: lv_var1 TYPE string VALUE 'HOLA',
        lv_var2 TYPE string VALUE 'MUNDO',
        lv_res  TYPE string.
  * Concateno ambas variables
  CONCATENATE lv_var1 lv_var2 INTO lv_res.

  WRITE lv_res.

  TRANSLATE lv_res TO LOWER CASE.

  WRITE lv_res.
  ```

* **OVERLAY campo1 WITH campo2 [ ONLY campo3 ]**: El contenido de ‘campo2’ sobreescribe a ‘campo1’ en todas las posiciones donde ‘campo1’ tenga blancos, reemplazándolos por los caracteres de ‘campo2’ de las posiciones respectivas (por defecto se reemplazan blancos). Devuelve 0 si al menos un carácter ha sido sobrescrito, y 4 en otro caso.

* **SEARCH { campo | tabla } FOR criterio [ ABBREVIATED ][ STARTING AT inicio ][ ENDING AT final ] [ AND MARK ]**: Busca en el 'campo' dado el string 'criterio'. Se puede delimitar la búsqueda especificando las posiciones de inicio y fin con STARTING AT y ENDING AT. Con AND MARK se señala en mayúsculas la palabra en la que se encontró el string. Devuelve 0 si encontrado (estará en la posición SY-FDPOS), y 4 en caso contrario.

## Expresiones lógicas

Para campos:

* Condicion1 **AND** condicion2: se cumple la condicion1 y la condicion2.

* Condicion1 **OR** condicion2: se cumple la condicion1 o la condicion2.

* Campo1 **=/EQ** campo2: campo1 es igual a campo2

* Campo1 **<>/NE** campo2: campo1 distinto a campo2

* Campo1 **>/GT** campo2: campo1 es mayor que el campo2

* Campo1 **</LT** campo2: campo1 es menor que el campo2

* Campo1 **>=/GE** campo2: campo1 es mayor o igual que el campo2

* Campo1 **<=/LE** campo2: campo1 es menor o igual que el campo2

Para strings:

* String1 **CO** string2: el string1 solo contiene caracteres del string2

* String1 **CN** string2: el string1 no solo contiene caracteres del string2.

* String1 **CA** string2: el string1 contiene algún carácter del string2.

* String1 **NA** string2:el string1 no contiene ningún carácter del string2.

* String1 **CS** string2: el string1 contiene el string2.

* String1 **NS** string2: el string1 no contiene el string2.

* String1 **CP** string2: el string1 contiene el patrón del string2.

* String1 **NP** string2: el string1 no contiene el patrón del string2.

Otras expresiones:

* Campo **BETWEN** rango:  campo está entre el rango de valores “rango”.

* Variable **IS INITIAL**: variable está vacía.

## Estructuras de control

* **IF**: Sentencia utilizada para chequear condiciones en la lógica de un programa. Existe la sentencia ELSE como alternativa si no se cumple la sentencia IF. La instrucción ELSEIF se utiliza para comprobar condiciones excluyentes. Tienen que finalizar con un ENDIF.

  ```abap
  IF [condicion].
    IF [condicion].
      ...
    ELSEIF [condicion].
      ...
    ELSE.
      ...
    ENDIF.
  ENDIF.
  ```

* **CASE**: Sentencia utilizada para realizar diferentes acciones en función del valor de una variable. Con la instrucción WHEN se añade el valor que podrá tomar la variable y a continuación el bloque de instrucciones a ejecutar en caso de que la variable haya tomado ese valor. Para indicar el final de la sentencia se añade la instrucción ENDCASE. Con OTHERS se tratan todos los casos que no se hayan contemplado en la instrucción WHEN.

  ```abap
  CASE [campo].
    WHEN [condicion/campo].
      ...
    WHEN [condicion/campo].
      ...
    WHEN OTHERS.
  ENDCASE.
  ```

* **DO**: Permite ejecutar un bloque de instrucciones tantas veces como se haya especificado. Con la instrucción TIMES se indica el número de veces que se va a repetir el bloque de instrucciones. Para indicar el final de la sentencia se añade la instrucción ENDDO.

  ```abap
  DO n TIMES.
    ...
  ENDDO.
  ```

* **WHILE**: Permite ejecutar un bloque de instrucciones de manera ininterrumpida mientras que se cumpla la condición especificada. Para indicar el final de la sentencia se añade la instrucción ENDWHILE.

  ```abap
  WHILE [condicion].
    ...
  ENDWHILE.
  ```

### Ejercicio: Datos de un alumno.

Se pide introducir el nombre, apellidos y fecha de alta de un alumno, montar una estructura con su nombre y mostrar por pantalla la frase 'El alumno '+[estructura]+' se ha dado de alta el dia'+[fecha].

Nota: la salida del texto debe ser con las iniciales en mayúscula, y todos los campos deben ser obligatorios.

```abap
*&---------------------------------------------------------------------*
*& Report ZMM_R_DATOS_PERS_02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_r_datos_pers_02.

* defino la variable datos final
DATA: lv_datos  TYPE string.

* defino la estructura
DATA: BEGIN OF ls_datos,
        nombre     TYPE c LENGTH 20,
        apellido1  TYPE c LENGTH 20,
        apellido2  TYPE c LENGTH 20,
        fecha_alta TYPE d,
      END OF ls_datos.

*Pantalla seleccion
SELECTION-SCREEN BEGIN OF BLOCK bl WITH FRAME TITLE TEXT-t01.
SELECTION-SCREEN BEGIN OF BLOCK datos WITH FRAME TITLE TEXT-t02.
PARAMETERS: pa_nom OBLIGATORY TYPE c LENGTH 20,
            pa_ap1 OBLIGATORY TYPE c LENGTH 20,
            pa_ap2 OBLIGATORY TYPE c LENGTH 20.
SELECTION-SCREEN END OF BLOCK datos.

SELECTION-SCREEN BEGIN OF BLOCK fecha WITH FRAME TITLE TEXT-t03.
PARAMETERS pa_fecha OBLIGATORY TYPE datum.
SELECTION-SCREEN END OF BLOCK fecha.

SELECTION-SCREEN END OF BLOCK bl.

***********************************************************************
START-OF-SELECTION.
***********************************************************************

* asigno valores a la estructura
  ls_datos-nombre = pa_nom.
  ls_datos-apellido1 = pa_ap1.
  ls_datos-apellido2 = pa_ap2.
  ls_datos-fecha_alta = pa_fecha.

*Paso a minuscula
  TRANSLATE ls_datos TO LOWER CASE.

* Formateo de nombres y apellidos para tener la inicial en mayuscula
  REPLACE: ls_datos-nombre(1) WITH pa_nom(1) INTO ls_datos-nombre,
           ls_datos-apellido1(1) WITH pa_ap1(1) INTO ls_datos-apellido1,
           ls_datos-apellido2(1) WITH pa_ap2(1) INTO ls_datos-apellido2.

* Concatenamos
  CONCATENATE ls_datos-nombre ls_datos-apellido1 ls_datos-apellido2 INTO lv_datos SEPARATED BY space.

***********************************************************************
END-OF-SELECTION.
***********************************************************************

* muestro por pantalla
  WRITE: 'El alumno/a', lv_datos, 'se ha dado de alta el dia', ls_datos-fecha_alta DD/MM/YYYY.
```

# 7. Open SQL

ABAP traduce Open SQL al idioma de la base de datos a la que esté conectada. Además, si se cambiase de tipo de base de datos, no sería necesario cambiar el código Open SQL.

En ABAP existen las sentencias llamadas OPEN SQL que son el subconjunto del STANDARD SQL integrado en el lenguaje ABAP.

De esta manera se puede acceder a la base de datos de forma uniforme.

Las sentencias OPEN SQL son convertidas en sentencias específicas del standard SQL mediante la interfaz de la base de datos.

![Esquema Open SQL](/2.-ABAP/images/reports18.png)

## 7.1. Sentencia SELECT

Permite obtener datos de la BBDD teniendo en cuenta:
  * Los campos que van a incluirse en el resultado
  * Si el resultado será uno o varios registros
  * Si el resultado podrá tener dos o mas registros idénticos

Posee varias cláusulas:
* **INTO**: especifica la variable interna (variable, estructura ,tabla interna,etc.) donde se almacenarán los datos seleccionados.
* **FROM**: se indica la fuente donde se obtienen los datos, podrán ser una o varias tablas/vistas de la base de datos.
* **WHERE**: a continuación de esta cláusula se especifican las condiciones que se deben de cumplir los resultados a obtener.
* **GROUP BY**: se indican los campos por los se quieren agrupar los resultados de la selección.
* **HAVING**: una vez agrupados los resultados se puede añadir un segundo bloque de condiciones.
* **ORDER BY**: se especifican por qué campos se va a ordenar el resultado.

La sintáxis básica sería:

```sql
SELECT [campo] FROM [tabla_origen] INTO [estructura_destino].
```

Si tenemos varios campos, irán separados por un espacio. Por otro lado, **la tabla de destino deberá tener los mismos campos que los que queremos guardar en ellos**, tanto en cantidad como en dominio. De ése modo, si queremos seleccionar el campo zmm_e_titulo_libro_02, la tabla de destino deberá tener dicho campo. 

Si sólo queremos obtener **un registro, el destino será una estructura. Para obtener varios registros, deberemos guardarlos en una tabla** del siguiente modo:

```sql
SELECT [campo] FROM [tabla_origen] INTO TABLE [tabla_destino].
```

Si queremos especificar un registro en particular, empleamos la sentencia WHERE:

```sql
SELECT [campo] FROM [tabla_origen] INTO TABLE [tabla_destino] WHERE [condicion].
```

Es posible agrupar condiciones mediante operadores lógicos como AND, NOT y OR.

### Selección de un registro

Para la selección de un único registro es posible proceder de dos formas:

* **Cláusula SELECT SINGLE**: se leerá un solo campo/registro  de una tabla/vista de BBDD. Para que la entrada sea única, **el acceso tiene que ser por todos los campos clave de la tabla (Primary Keys)** (cláusula WHERE), en el caso de que no se acceda por todos los campos claves se deberá usar el UP TO 1 ROWS.

  ```sql
  SELECT SINGLE [campo] FROM [tabla_origen] INTO [estructura_destino] WHERE [condicion].
  ```

* **Cláusula UP TO 1 ROWS**:  mediante esta cláusula, en el momento en el que se encuentre un registro se sale del select. En éste caso, no es necesario acceder por todos los campos clave. **Lo que hace es recuperar el primer registro encontrado para las condiciones de selección.**

  ```sql
  SELECT [campo] UP TO 1 ROWS FROM [origen] INTO TABLE [destino] WHERE [condicion].
  ```

Si se usa la cláusula **CORRESPONDING FIELDS OF** se puede usar estructuras no exactamente iguales a los campos de BBDD, ya que se informarán los campos de nuestra estructura que coincidan en nombre con los campos de BBDD. No se debe usar ya que penaliza al rendimiento de la máquina.

Para **comprobar si se han encontrado datos**, **se chequeará el valor de la variable de sistema SY-SUBRC después de cada SELECT**:
* SUBRC = 0:  se ha encontrado el registro
* SUBRC <> 0: nos e ha encontrado el registro para los parámetros indicados.

Para realizar el **checkeo** se hará lo siguiente:

```abap
IF sy-subrc NE 0.
  MESSAGE 'No existen datos' TYPE 'E'.
ENDIF.
```

### Selección de varios registros

Si no se utiliza SINGLE/UP TO 1 ROWS, el número de registros que se leen estará limitado a las condiciones añadidas en la cláusula WHERE.

Existen dos formas distintas:

1. Forma 1:

  ```sql
  SELECT [campos]FROM [tabla]INTO TABLE [tabla interna]WHERE[condiciones].
  ```
  Ejemplo: Se añaden a la tabla interna lt_airpfrom todos los aeropuertos de salida de la table de vuelos de la compañía aérea “001”.

  ```sql
  SELECT airpfrom FROM spfli INTO TABLE lt_airpfrom WHERE carrid =’001’. 
  ```

2. Segunda forma:

  ```sql
  SELECT [campos]FROM [tabla]INTO [estructura interna]WHERE[condiciones] ENDSELECT.
  ```

  Ejemplo: Se van añadiendo a la estructura interna ls_airpfrom cada aeropuerto de salida de la tabla de vuelos de la compañía aérea “001”. Se puede añadir código entre el SELECT-END SELECT.

  ```sql
  SELECT airpfrom FROM spfli INTO ls_airpf where carrid = ‘001’. END SELECT. 
  ```

No se recomienda realizar este tipo de accesos. Se **debe de evitar siempre que se pueda la sentencia END SELECT siempre que no se haya añadido la cláusula UP TO 1 ROWS**.

### Asignación del resultado a la variable

Variables individuales:

* Se definen una serie de variables individuales (tantas como campos tenemos en la cláusula SELECT). En la cláusula INTO indicamos estas variables en el mismo orden en el que aparecen en el SELECT.

  ```sql
  SELECT [campo1,campo2,campo3] FROM [tabla] INTO [variable1,variable2,variable3] FROM [tabla destino].
  ```

Estructura/tabla definida previamente:

* Se define una estructura/tabla en el programa con la misma secuencia de campos que aparecen en la cláusula SELECT. En la cláusula INTO se indica el nombre de esta estructura.

  ```sql
  SELECT [campo1,campo2,campo3] FROM [tabla] INTO (CORRESPONDING FIELDS OF(TABLE)) [tabla/estructura interna] FROM [destino]
  ```

### Agrupación de campos

Sintáxis:

```sql
SELECT [campos] FROM [tabla] INTO TABLE [table local] WHERE [condiciones] GROUP BY [campos].
```

Existe la posibilidad de que cuando se haga una selección se encuentren registros duplicados (cuando los campos seleccionados no forman la clave de la tabla).

En el siguiente ejemplo, el campo airpfrom no es un campo clave en la tabla spfli por lo que es posible que el mismo aeropuerto aparezca varias veces en los distintos registros de la tabla, es decir, es muy probable que para la compañía aérea que hemos seleccionado existan varios vuelos que salgan desde el mismo aeropuerto. Es decir, la tabla local lt_airpfrom tendrá registros duplicados:

```sql
SELECT airpfrom FROM spfli INTO TABLE lt_airpfrom WHERE carrid =’001’. 
```

Para evitar esto, se realizaría la siguiente consulta:

```sql
SELECT airpfrom FROM spfli INTO TABLE lt_airpfrom WHERE carrid =’001’ GROUP BY airpfrom.
```

Para este ejemplo se podría también utilizar también la cláusula DISTINCT:

```sql
SELECT DISTINCT(airpfrom) FROM spfli INTO TABLE lt_airpfrom WHERE carrid =’001’. 
```
### Funciones de agregado

* **MAX** (campo): Devuelve el valor máximo del campo especificado. 

* **MIN** (campo): Devuelve el valor mínimo del campo especificado. 

* **AVG** (campo): Devuelve el valor medio del campo especificado. 

* **SUM** (campo): Devuelve la suma de todos los registros del campo especificado. 

* **COUNT** (campo): Devuelve el número de valores diferentes para el campo especificado. 

### Ejemplo sentencias SQL

Supongamos que queremos obtener la descripción de un material de la tabla MAKT, en la cual aparecen en varios idiomas, en castellano. Para ello podemos hacer el siguiente SELECT SINGLE:

```sql
SELECT SINGLE maktx FROM makt INTO ls_material WHERE matnr EQ '000000000000000199' AND spras EQ 'S'.
```

siendo la estructura:

```abap
DATA: BEGIN OF ls_material,
      matnr TYPE matnr,
      spras TYPE spras,
      maktx TYPE maktx,
  END OF ls_material.
```

El código completo sería:

```abap
*&---------------------------------------------------------------------*
*& Report ZMM_R_PRUEBAS_SQL_02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_r_pruebas_sql_02.

DATA: BEGIN OF ls_material,
        matnr TYPE matnr,
        spras TYPE spras,
        maktx TYPE maktx,
      END OF ls_material.

DATA lt_material LIKE TABLE OF ls_material.

SELECT matnr spras maktx FROM makt INTO TABLE lt_material WHERE matnr BETWEEN '000000000000000073' AND '000000000000000199'.

* verificamos si hay datos
IF sy-subrc NE 0.
  MESSAGE 'No existen datos' TYPE 'E'.
ENDIF.
```

Veámos cómo seleccionar libros en función de su código:

```abap
REPORT zmm_r_pruebas_sql_02.

TABLES zmm_t_libros_02.

* estructura
DATA: BEGIN OF ls_libros,
        cod_libro TYPE zmm_t_libros_02-cod_libro,
        titulo    TYPE zmm_t_libros_02-titulo,
      END OF ls_libros.

*tabla
DATA lt_libros LIKE TABLE OF ls_libros.

* pantalla seleccion
SELECT-OPTIONS so_codli FOR zmm_t_libros_02-cod_libro.

************************************************************************
START-OF-SELECTION.
************************************************************************

  SELECT cod_libro titulo
    FROM zmm_t_libros_02
    INTO TABLE lt_libros
    WHERE cod_libro IN so_codli.

  IF sy-subrc NE 0.
    MESSAGE 'No se han encontrado libros' TYPE 'E'.
  ENDIF.

***********************************************************************
END-OF-SELECTION.
***********************************************************************

* muestro por pantalla
  LOOP AT lt_libros INTO ls_libros.
    WRITE:/ ls_libros-cod_libro, ls_libros-titulo.
  ENDLOOP.
```

### Ejercicio consulta listado coches y suma precio por pais

```abap
REPORT zmm_r_listado_coches_02.

* Tablas
TABLES zmm_t_coches_02.

* Esquema listado
DATA: BEGIN OF ls_coches,
        marca     TYPE zmm_t_coches_02-marca,
        matricula TYPE zmm_t_coches_02-matricula,
        precio    TYPE zmm_t_coches_02-precio,
        moneda    TYPE zmm_t_coches_02-moneda,
      END OF ls_coches.

DATA lt_coches LIKE TABLE OF ls_coches.

* Esquema precios-pais
DATA lv_suma_precios TYPE i.

DATA: BEGIN OF ls_precios,
        suma_precio LIKE lv_suma_precios,
        pais        TYPE zmm_t_coches_02-pais_fab,
      END OF ls_precios.

DATA lt_precios LIKE TABLE OF ls_precios.

************************************************************************
* SELECTION SCREEN
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK bl_codigo WITH FRAME TITLE TEXT-t01.
SELECT-OPTIONS so_codco FOR zmm_t_coches_02-cod_coche OBLIGATORY.
PARAMETERS pa_suma TYPE xfeld AS CHECKBOX USER-COMMAND check.
SELECT-OPTIONS so_pais FOR zmm_t_coches_02-pais_fab.
SELECTION-SCREEN END OF BLOCK bl_codigo.

************************************************************************
START-OF-SELECTION.
************************************************************************

* Selección checkbox con la suma de precios
  IF pa_suma EQ abap_true.
    SELECT SUM( precio ) AS suma_precio pais_fab
      FROM zmm_t_coches_02
      INTO TABLE lt_precios
      WHERE pais_fab IN so_pais
      GROUP BY pais_fab.

    IF sy-subrc NE 0.
      MESSAGE 'No se han encontrado coches' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ELSE.
* seleccion de datos para listado
    SELECT marca matricula precio moneda
        FROM zmm_t_coches_02
        INTO TABLE lt_coches
        WHERE cod_coche IN so_codco.

* Check de la seleccion
    IF sy-subrc NE 0.
      MESSAGE 'No se han encontrado coches' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.
************************************************************************
END-OF-SELECTION.
************************************************************************

  IF pa_suma EQ abap_true.
    WRITE:'Precio total por pais en EUR:'.
    LOOP AT lt_precios INTO ls_precios.
      WRITE:/ ls_precios-pais, ls_precios-suma_precio.
    ENDLOOP.
  ELSE.
* mostramos por pantalla
    WRITE: 'Listado de coches:'.
    LOOP AT lt_coches INTO ls_coches.
      WRITE:/ ls_coches-marca, ls_coches-matricula, ls_coches-precio, ls_coches-moneda.
    ENDLOOP.
  ENDIF.
************************************************************************
AT SELECTION-SCREEN.
************************************************************************

*Validacion pais
  IF pa_suma EQ abap_true AND so_pais IS INITIAL AND SY-UCOMM NE 'CHECK'.
    MESSAGE 'Debe introducir el pais' TYPE 'E'.
  ENDIF.

************************************************************************
AT SELECTION-SCREEN OUTPUT.
************************************************************************

* Ocultamos el select options del pais
  LOOP AT SCREEN.
    IF pa_suma EQ abap_false AND screen-name CS 'SO_PAIS'.
      screen-invisible = 1.
      screen-input = 0.
      MODIFY SCREEN.
    ELSEIF pa_suma EQ abap_true AND screen-name CS 'SO_PAIS'.
      screen-invisible = 0.
      screen-input = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
```

## 7.2. Unión de tablas: JOINS

A la hora de unir dos o más tablas, se debe de saber los campos que se van a utilizer de enlace entre las tablas y el tipo de enlace que se quiere realizar.

Existen dos uniones diferentes:

* **INNER JOIN**: mediante este enlace se seleccionan los registros que tienen correspondencia entre las dos tablas, es decir, para cada registro de la tabla de la izquierda existe uno o más en la tabla de la derecho que cumplan los criterios especificados, en el caso de que no exista ninguno no se recupera el registro correspondiente.

  ```sql
  SELECT [CAMPOS-TABLA1 CAMPOS-TABLA2] FROM TABLA1 INNER JOIN TABLA 2 ON [condiciones de unión] WHERE [condiciones de selección].
  ```

* **LEFT OUTER JOIN**: mediante este enlace se seleccionan los registros de la tabla de la izquierda del join aunque no tengan correspondencia en la tabla de la derecha. Es decir, tomamos como principal la tabla de la izquierda y buscamos las coincidencias de unión en la tabla de la derecha si existen.En el caso de que no existan en la tabla de la derecha se recupera el registro igualmente.

  ```sql
  SELECT [CAMPOS-TABLA1 CAMPOS-TABLA2] FROM [TABLA1] LEFT OUTER JOIN [TABLA2] ON [condiciones de unión] WHERE [condiciones de selección].
  ```

Hay que destacar que **los campos que se deben introducir son aquellos campos de las tablas que queremos mostrar finalmente**.

### Ejemplo de INNER JOIN

```abap
REPORT zmm_r_pruebas_sql_02.

* declaracion de estructura
DATA: BEGIN OF ls_vuelos,
        carrid    TYPE spfli-carrid,
        carrname  TYPE scarr-carrname,
        countryfr TYPE spfli-countryfr,
        cityfrom  TYPE spfli-cityfrom,
      END OF ls_vuelos.

DATA lt_vuelos LIKE TABLE OF ls_vuelos.

* INNER JOIN

SELECT spfli~carrid carrname countryfr cityfrom
  FROM spfli
  INNER JOIN scarr
  ON spfli~carrid EQ scarr~carrid
  INTO TABLE lt_vuelos.

IF sy-subrc NE 0.
  MESSAGE 'No se han encontrado datos' TYPE 'E'.
ENDIF.

LOOP AT lt_vuelos INTO ls_vuelos.
  WRITE:/ ls_vuelos-carrid, ls_vuelos-carrname, ls_vuelos-countryfr, ls_vuelos-cityfrom.
ENDLOOP.
```

### Ejercicio listado de coches empleando joins

En éste ejercicio se muestra una pantalla de selección con varios campos, tras lo que se mostrará por pantalla un listado de la búsqueda realizada. 

```abap
REPORT zmm_r_list_coche2_02.

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

************************************************************************
* PANTALLA SELECCION
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK bl WITH FRAME TITLE text_t01.
SELECT-OPTIONS: so_codco FOR zmm_t_coches_02-cod_coche,
                so_marca FOR zmm_t_coches_02-marca,
                so_model FOR zmm_t_coches_02-modelo,
                so_matr FOR zmm_t_coches_02-matricula.
SELECTION-SCREEN END OF BLOCK bl.

************************************************************************
START-OF-SELECTION.
************************************************************************
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
    AND tcurt~spras EQ sy-langu                  "Selecciono descripcion
    AND t005t~spras EQ sy-langu.                 "en español

* Check
  IF sy-subrc NE 0.
    MESSAGE 'No existen datos' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
***********************************************************************
END-OF-SELECTION.
***********************************************************************
* muestro por pantalla
  LOOP AT lt_listado INTO ls_listado.
    WRITE:/ ls_listado-cod_coche, ls_listado-marca, ls_listado-modelo,
            ls_listado-matricula, ls_listado-pais, ls_listado-pais_desc,
            ls_listado-precio, ls_listado-moneda, ls_listado-moneda_desc.
  ENDLOOP.
```

## 7.3. Sentencias de tratamiento de datos

* Añadir una línea a la base de datos:

  ```sql
  INSERT [tabla BBDD] FROM [estructura interna].
  ```

* Añadir varias líneas de una tabla interna. :

  ```sql
  INSERT [tabla BBDD] FROM [tabla interna].
  ```

* Modificar una línea. Si no existe el registro, en ése caso lo inserta. La modificación debe ser en todos los campos:

  ```sql
  UPDATE [tabla BBDD] FROM [estructura interna].
  ```

* Modificar varias líneas:

  ```sql
  UPDATE [tabla BBDD] SET [campo BBDD] = [variable]  WHERE [condiciones].
  ```

* Modificar varias líneas usando una tabla interna:

  ```sql
  UPDATE [tabla BBDD] FROM TABLE [tabla interna].
  ```

* Añadir o modificar una línea:

  ```sql
  MODIFY [tabla BBDD] FROM [estructura interna].
  ```

* Añadir o modificar varías líneas:

  ```sql
  MODIFY [tabla BBDD] FROM TABLE [tabla interna].
  ```

* Borrar una línea:

  ```sql
  DELETE [tabla BBDD] FROM [estructura interna].
  ```

* Borrar bajo cierta condicion:

  ```sql
  DELETE FROM [tabla BBDD] WHERE [condiciones].
  ```

* Borrar varías líneas usando una tabla interna.

  ```sql
  DELETE [tabla BBDD] FROM TABLE [tabla interna].
  ```

* **No se puede hacer lo siguiente, ya que elimina toda la tabla**

  ```sql
  DELETE FROM [tabla].
  ```

### Ejemplos

* **Insertamos un sólo registro en una tabla**

  ```abap
  REPORT zmm_r_trat_datos_02.

  * Estructura
  DATA ls_libro TYPE zmm_t_libros_02.

  * Asigno valores
  ls_libro-cod_libro = '4'.
  ls_libro-titulo    = 'Star Wars'.
  ls_libro-tipo      = 'R'.
  ls_libro-precio    = '12.75'.
  ls_libro-moneda    = 'EUR'.
  ls_libro-tapa      = '2'.
  ls_libro-nuevo     = abap_true.

  * Inserto valores en la tabla
  INSERT zmm_t_libros_02 FROM ls_libro.

  * Check
  IF sy-subrc NE 0.
    MESSAGE 'Error al guardar los datos' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
  ```

* **Actualización de un registro**

  ```abap
  REPORT zmm_r_trat_datos_02.

  UPDATE zmm_t_libros_02
    SET titulo = 'Pinocho'
        precio = '16.30'
    WHERE cod_libro = '4'.

  * Check
  IF sy-subrc NE 0.
    MESSAGE 'Error al actualizar los datos' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
  ```

* **Modificacion de un registro**

  ```abap
  REPORT zmm_r_trat_datos_02.

  DATA ls_libro TYPE zmm_t_libros_02.

  * Asigno valores
  ls_libro-cod_libro = '6'.
  ls_libro-titulo    = 'Star Wars'.
  ls_libro-tipo      = 'R'.
  ls_libro-precio    = '12.75'.
  ls_libro-moneda    = 'EUR'.
  ls_libro-tapa      = '2'.
  ls_libro-nuevo     = abap_true.

  MODIFY zmm_t_libros_02 FROM ls_libro.

  * Check
  IF sy-subrc NE 0.
    MESSAGE 'Error al modificar los datos' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
  ```

* **Borrar un registro**

  ```abap
  REPORT zmm_r_trat_datos_02.

  * Estructura
  DATA ls_libro TYPE zmm_t_libros_02.

  * Asigno valores
  ls_libro-cod_libro = '6'.
  ls_libro-titulo    = 'El Rapto'.
  ls_libro-tipo      = 'T'.
  ls_libro-precio    = '10.05'.
  ls_libro-moneda    = 'EUR'.
  ls_libro-tapa      = '1'.
  ls_libro-nuevo     = abap_false.

  DELETE zmm_t_libros_02 FROM ls_libro.

  * Check
  IF sy-subrc NE 0.
    MESSAGE 'Error al borrar los datos' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
  ```

## 7.4. Tablas internas

Las tablas internas son aquellas creadas dentro de un report, las cuales no persisten una vez acabada la ejecución del mismo. Existen varias sentencias que permiten manipular éste tipo de tablas para poder persistir los datos tras la ejecución del report:

* **LOOP AT**: Mediante esta instrucción se recorren todos los registros de una tabla interna. 
  * Puede llevar la **cláusula WHERE**, de manera que solo se recorran los registros que cumplan las condiciones especificadas. Ésto es mejor hacerlo en la selección de datos.
  * El registro se vuelca en una estructura local definida con el mismo tipo que la tabla
  * Siempre finaliza con la cláusula ENDLOOP.

  Sintáxis:

  ```sql
  LOOP AT [tabla interna] INTO [estructura local] WHERE [condicion].
      ...
    ENDLOOP.
  ```

  **Ejemplo**: cargamos todos los datos de la tabla libro enuna tabla interna.

    ```abap
    REPORT zmm_r_tablas_inter_02.

    * Creamos tabla interna
    DATA lt_libros TYPE TABLE OF zmm_t_libros_02.

    * Vuelco datos en la tabla interna
    SELECT * FROM zmm_t_libros_02
      INTO TABLE lt_libros.

    IF sy-subrc NE 0.
      MESSAGE 'No existen registros' TYPE 'E'.
    ENDIF.

    * Muestro datos
    LOOP AT lt_libros INTO ls_libros.
      WRITE:/ ls_libros-cod_libro, ls_libros-titulo, ls_libros-tipo, ls_libros-precio, ls_libros-tapa.
    ENDLOOP.
    ```

* **READ TABLE**: Mediante esta instrucción se **accede a un único registro de una tabla local**. 
  * Puede llevar la **cláusula WITH KEY**, si lo que se quiere es acceder a un registro con unas condiciones o bien la **cláusula INDEX**, cuando se quiere acceder a un índice determinado (**no confundir con el campo clave, en éste caso es el número de fila**).
  * El registro se vuelca en una estructura local definida con el mismo tipo que la tabla.

  Sintáxis:

  ```sql
  READ TABLE [tabla interna] INTO [estructura local] WITH KEY [condición].
  ```

  o bien:

  ```sql
  READ TABLE [tabla interna] INTO [estructura local] INDEX [índice].
  ```

  **Ejemplo**: Accedemos a un registro de la tabla:

    ```abap
    * Accedo al registro 2 de la tabla
    READ TABLE lt_libros INTO ls_libros INDEX 2.

    IF sy-subrc NE 0.
      MESSAGE 'No existen el indice buscado' TYPE 'E'.
    ENDIF.

    * muestro el registro
    WRITE:/ ls_libros-cod_libro, ls_libros-titulo, ls_libros-tipo, ls_libros-precio, ls_libros-tapa.
    ```

  Si quisiéramos acceder al registro con valor 'R':

    ```abap
    * Accedo al registro 2 de la tabla
    READ TABLE lt_libros INTO ls_libros WITH KEY tipo='R'.

    IF sy-subrc NE 0.
      MESSAGE 'No existen el indice buscado' TYPE 'E'.
    ENDIF.

    * muestro el registro
    WRITE:/ ls_libros-cod_libro, ls_libros-titulo, ls_libros-tipo, ls_libros-precio, ls_libros-tapa.
    ```

* **APPEND**: Mediante esta instrucción **se añaden registros a una tabla local**. 
  * Se necesita una estructura con los mismos campos que la tabla para poder realizar el append.

  Sintáxis:

  ```sql
  APPEND [estructura interna] TO [tabla interna].
  ```
* *Ejemplo**: Añadimos un registro

    ```abap
    * Añado registro a tabla local
    ls_libros-cod_libro = '7'.
    ls_libros-titulo = 'Blancanieves'.
    ls_libros-precio = '14.00'.

    APPEND ls_libros TO lt_libros.

    IF sy-subrc NE 0.
      MESSAGE 'No se ha podido añadir el registro' TYPE 'E'.
    ENDIF.
    ```

* **DELETE**: Mediante esta instrucción **se borran registros de una tabla local**.
  * Se puede realizar el borrado para los registro que cumplan cierta condición o bien basado en una estructura local.

  Sintáxis:

  ```sql
  DELETE [tabla local] FROM [ estructura local].
  ```

  o bien:

  ```sql
  DELETE [tabla local]  WHERE [condiciones].
  ```

  **Ejemplo**: Borramos los libros de ficcion de la tabla interna

    ```abap
    DELETE lt_libros WHERE tipo EQ 'F'.

    IF sy-subrc NE 0.
      MESSAGE 'No se ha podido borrar el registro' TYPE 'E'.
    ENDIF.
    ```

* **DESCRIBE TABLE LINES**: Mediante esta instrucción se **recupera el número de registros de una tabla interna**. 
  * Se vuelca el número de líneas de la tabla en una variable local.

  Sintáxis:

  ```sql
  DESCRIBE TABLE [tabla interna] LINES [variable local].
  ```

  Otra manera de hacerlo (Nueva manera):

  ```abap
  DATA(lv_linea) = lines( lt_libros ).
  ```

* **AT LAST**: 
  * Solo se puede utilizar **dentro de un LOOP**.
  * Identifica un bloque de proceso.
  * **Se ejecuta cuando se ha llegado al último registro de la tabla**.

  Sintáxis:

  ```sql
  AT LAST.
    ...
  END AT.
  ```

* **AT FIRST**: 
  * Solo se puede utilizar **dentro de un LOOP**.
  * Identifica un bloque de proceso.
  * **Se ejecuta cuando se ha llegado al primer registro de la tabla**.

  Sintáxis:

  ```sql
  AT FIRST.
    ...
  END AT.
  ```

  **Ejemplo**: Mostramos cabezera y numero de registros.

    ```abap
    LOOP AT lt_libros INTO ls_libros.
      AT FIRST.
        WRITE:/ 'Codigo libro', 'Titulo', 'Precio'.
      ENDAT.

      WRITE:/ ls_libros-cod_libro, ls_libros-titulo, ls_libros-tipo, ls_libros-precio, ls_libros-tapa.
      lv_contador = lv_contador + 1.

      AT LAST.
        WRITE:/ 'Número de registros:', lv_contador.
      ENDAT.
    ENDLOOP.
    ```

* **AT NEW**: 
  * Solo se puede utilizar **dentro de un LOOP**.
  * Identifica un bloque de proceso.
  * **Se ejecuta cuando se ha llegado a un registro nuevo para el campo añadido a la derecha de la sentencia**.

  Sintáxis:

  ```sql
  AT NEW [campo tabla interna].
    ...
  END AT.
  ```

* **AT END**: 
  * Solo se puede utilizar **dentro de un LOOP**.
  * Identifica un bloque de proceso.
  * **Se ejecuta cuando se ha llegado al primer registro de la tabla**.

  Sintáxis:

  ```sql
  AT END OF [campo tabla interna].
    ...
  END AT.
  ```

**Las sentencias AT NEW y AT END solo tienen sentido cuando la tabla esta ordenada.**

## 7.5. Field Symbols

Se pueden definir como los **punteros de SAP: variable que hace referencia a una región de memoria, almacena una dirección de memoria.**

Declaración:

```abap
FIELD-SYMBOLS <nombre>.
```

Si se quiere declarar un **field symbol que apunte a cualquier tipo de objeto**:

```abap
FIELD-SYMBOLS <nombre> TYPE ANY.
```

Para declarar un **field symbol que apunte a cualquier tipo de tabla**:

```abap
FIELD-SIMBOLS <f> TYPE ANY TABLE.
```

pero hay que tener en cuenta que no se puede incluir en ninguna instrucción propia de una tabla

También se puede declarer un **field symbol de un tipo determinado**:

```abap
FIELD-SYMBOLS <nombre> TYPE i.
```

Acceso dinámico:

* Los field symbol permiten acceder dinámicamente (en tiempo de ejecución) a los diferentes objetos que existen en SAP.

* Podremos, por lo tanto, **acceder al valor de variables cuyo nombre no se conoce hasta la ejecución del programa**

* Ejemplo:

    ```abap
    DATA lv_saludo TYPE string VALUE 'hola'.
          DATA lv_name_variable (10) TYPE c VALUE ‘lv_saludo'.
          FIELD-SYMBOLS <f> TYPE c.
          ASSIGN (lv_name_variable) TO <f>.
    ```
	En este ejemplo el field symbol apunta a la variable lv_saludo, ya que es el contenido de la variable lv_name_variable.

* Asignación de componentes de una estructura:

  * Se podrá asignar componentes de una estructura a un field symbol

    ```abap
    DATA: BEGIN OF direccion,
      calle(20) TYPE c VALUE 'calle real',
      numero(4) TYPE c VALUE '74',
      END OF direccion.
      FIELD-SYMBOLS <numero> TYPE c.
      ASSIGN COMPONENT ‘NUMERO’ OF STRUCTURE direccion TO <numero>.
    ```

  * Se asigna el valor del componente “numero” de la estructura “direccion” al field symbol.

**Se suele emplear mucho en los loops:**

```abap
LOOP AT lt_libros ASSIGNING FIELD-SYMBOL(<ls_libros>).
  <ls_libros>-titulo = 'Nuevo título'.
ENDLOOP.
```

## Programa de gestion de la tabla coches.

```abap
REPORT zmm_r_gest_coches_02.

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

************************************************************************
* PANTALLA SELECCION
************************************************************************

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

************************************************************************
START-OF-SELECTION.
************************************************************************
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
*verificamos si hay datos
    IF sy-subrc NE 0.
      MESSAGE 'No existen datos' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
* Contamos registros
    DESCRIBE TABLE lt_listado LINES lv_reg.
* Creacion
  ELSEIF rd_crea EQ abap_true.

    INSERT zmm_t_coches_02 FROM ls_coches.

    IF sy-subrc NE 0.
      MESSAGE 'Ya se ha creado éste valor' TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      MESSAGE 'Datos creados' TYPE 'S'.
    ENDIF.

* Modificacion
  ELSEIF rd_mod EQ abap_true.

    UPDATE zmm_t_coches_02 FROM ls_coches.

    IF sy-subrc NE 0.
      MESSAGE 'Error al modificar los datos' TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      MESSAGE 'Datos modificados' TYPE 'S'.
    ENDIF.

* Eliminacion
  ELSEIF rd_del EQ abap_true.

    DELETE zmm_t_coches_02 FROM ls_coches.

    IF sy-subrc NE 0.
      MESSAGE 'Error al borrar los datos' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
    else.
      MESSAGE 'Datos borrados con éxito' TYPE 'S'.
  ENDIF.

**************************************************************************
AT SELECTION-SCREEN.
**************************************************************************

* Codigo obligatorio al borrar coche
  IF rd_del EQ abap_true AND so_codco IS INITIAL.
    MESSAGE 'Debe introducir el código del coche' TYPE 'E'.
  ENDIF.

**************************************************************************
END-OF-SELECTION.
**************************************************************************

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
```