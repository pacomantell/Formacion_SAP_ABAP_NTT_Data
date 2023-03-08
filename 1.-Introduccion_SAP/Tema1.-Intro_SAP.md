# Tema 1.- Introducción a SAP

# 1. Introducción a SAP

**El Sistema SAP o “Systems, Applications, Products in Data Processing”, se trata de un Sistema informático que permite la administración de los recursos humanos, financieros-contables, productivos, logístico, etc… de las empresas**. La mayoría de estas empresas utilizan SAP para gestionar todas las fases de sus modelos de negocios de manera satisfactoria. **Se trata por ello de un ERP (Enterprise Resource Planning).**

## 1.1. Módulos de SAP

SAP es un sistema modular, cada módulo se encarga de aplicar una solución determinada a un área empresarial específica. 

SAP es un producto “genérico”, es decir, se ha creado con la finalidad de satisfacer de forma genérica las necesidades que propone el mercado actual. Propone una funcionalidad estándar, que, si no satisface las necesidades del cliente, permite crear un desarrollo a medida que se adapte a sus requerimientos.

![Módulos de SAP](/1.-Introduccion_SAP/images/modulos_sap.png)

Podemos destacar los siguientes módulos dentro de SAP:

* **Gestión financiera (FI)**. Libro mayor, libros auxiliares, ledgers especiales, etc.

* **Controlling (CO)**. Gastos generales, costes de producto, cuenta de resultados, centros de beneficio, etc.

* **Tesorería (TR)**. Control de fondos, gestión presupuestaria, etc.

* **Sistema de proyectos (PS)**. Grafos, contabilidad de costes de proyecto, etc.

* **Gestión de personal (HR)**. Gestión de personal, cálculo de la nómina, contratación de personal, etc.

* **Mantenimiento (PM)**. Planificación de tareas, planificación de mantenimiento, etc.

* **Gestión de calidad (QM)**. Planificación de calidad, inspección de calidad, certificado de calidad, aviso de calidad, etc.

* **Planificación de producto (PP)**. Fabricación sobre pedido, fabricación en serie, etc.

* **Gestión de material (MM)**. Gestión de stocks, compras, verificación de facturas, etc.

* **Comercial (SD)**. Ventas, expedición, facturación, etc.

* **Workflow (WF), Soluciones sectoriales (IS)**, con funciones que se pueden aplicar en todos los módulos.

## 1.2. Lenguaje de programación de SAP

El lenguaje utilizado en SAP es** ABAP IV (Advanced Business Application Programming)**. 

* Se trata de un **lenguaje de cuarta generación**, utilizado para programar casi la totalidad de sus productos.

* Utiliza sentencias de **OPEN SQL para el acceso a la base de datos**.

* Proporciona ayuda sobre la semántica y sintaxis utilizada en su lenguaje.

* Permite conexiones RFC (Remote Functions Call) para conectarse con sistema SAP o no SAP.

* Permite mostrar con una gran facilidad reportes para mostrar información por pantalla.

* Dispone de la opción de crear formularios rápidamente mediante diferentes alternativas: Sapscripts, Smartforms y Adobeforms.

* Mediante la plataforma NetWeaver, se pueden crear aplicaciones web (BSP, Web Dynpro, SAP UI5 y fiori)

* Dispone de diferentes opciones para extender con funcionalidad a medida la aportada por el paquete estándar de SAP, entre ellas: Field exits, User exits, Badis, Enhancement, etc.

# 2. Navegación en SAP

## SAP GUI

![Interfaz Gráfica de Usuario de SAP](/1.-Introduccion_SAP/images/nav1.PNG)

Veámos cada uno de los componentes:

### Standard Toolbar

Se compone de botones estándar, comunes a todas las transacciones. Se localizan en la parte superior y dado que son comunes, no es posible modificar su comportamiento ni visibilidad dentro de las aplicaciones estándar de SAP.

Los botones que componen ésta barra son los siguientes:

![Botones Standard Toolbar](/1.-Introduccion_SAP/images/nav2.PNG)

### Menu Bar

Todas las opciones del menú estarán disponibles en el Menu Bar.

En función de la tarea o transacción en la que se encuentre el usuario se visualizará un menú u otro.

Para modificar el Menu Bar, SAP permite la creación de status, en los que se puede definir cada punto del menú que se quiere visualizar.

![Menu Bar](/1.-Introduccion_SAP/images/nav3.png)

### Command Bar

Para llamar a las transacciones que nos proporciona SAP (o que hayamos creado a medida) se utiliza la barra de comandos.

**SAP es un sistema basado en transacciones, cualquier acción que desee realizar el usuario la puede hacer mediante la llamada a diferentes transacciones**.

![Command Bar](/1.-Introduccion_SAP/images/nav4.png)

A continuación, se enumeran algunos comandos más utlizados de la command bar:

* **/nxxxx** (xxxx código de la transacción): llamada a una transacción en el mismo modo/ventana.

* **/*xxxx** (xxxx código de la transacción): llamada a una transacción en el mismo modo/ventana saltándose la pantalla inicial.

* **/oxxxx** (xxxx código de la transacción): llamada a una transacción en diferente modo/ventana.

* **/n**: Se finaliza la transacción en la que nos encontremos (los cambios realizados se perderán).

* **/i**: Se finaliza el modo/ventana actual (cerramos sesion en el servidor).

* **/o**: Se genera un modo/ventana nuevo.

* **/ns000**: Se finaliza la transacción actual y se vuelve al menú principal.

* **/nend**: Se realiza el log off del sistema.

* **/nex**: Se realiza el log off del sistema sin la opción de guardar los cambios realizados.

Se pueden tener varios modos/ventanas abiertos a la vez, lo que es algo frecuente.

### Application Toolbar

Se sitúa en la parte inferior del Title Bar

Normalmente cambia en función de la aplicación/pantalla en la que nos encontremos

![Application Toolbar](/1.-Introduccion_SAP/images/nav5.png)

### Status Bar

Se sitúa en la parte inferior de la pantalla

Se muestran los mensajes (success, error y warning) que arroja el sistema.

Muestra información relativa al sistema, cliente, usuario conectado, etc…

![Status Bar](/1.-Introduccion_SAP/images/nav6.png)

# 1.3. Transacciones más utilizadas

* SE10: Órdenes de transporte

* SE11: Mantenimiento de diccionario de datos

* SE16: Visor de datos (Browser)

* SE24: Mantenimiento de clases

* SE36: Mantenimiento de base de datos lógica

* SE37: Mantenimiento de módulos de función

* SE38: Mantenimiento de programas (Editor ABAP/4)

* SE63: Traducción

* SE71-SE78: SAPscript

* SE80: Visor de objetos (Object browser)

* SE91: Gestión de mensajes

* SE93: Gestión de códigos de transacción

* SQ01: Mantenimiento de Querys

* SQ02: Mantenimiento de Infosets

* SM35: Gestión de juegos de datos (Batch-Input)

* SE37: Mantenimiento de Jobs

* SMOD: Gestión de User exits

* SM12: Gestión entradas de bloqueo

* ST22: Gestión de errores en tiempo de ejecución (ABAP Runtime Error-Dump)

* ST05: Gestión trazas SQL

Las transacciones que comienzan por S son aquellas standard del sistema. Aquellas definidas por el usuario suelen empezar por Z o Y.

# 3. Diccionario de datos.

## 3.1. Conceptos Generales

Es la parte central de SAP, donde se guarda toda la información.

![Diccionario de datos](/1.-Introduccion_SAP/images/dic1.jpg)

Fuente central de información, datos almacenados en el sistema.

Su rol es la creación y administración de los datos (metadatos). 

Transacciones mas utilizadas:

* SE11: Diccionario ABAP

* SE14: Herramienta de base de datos

* SE16: Visualizar tabla

* SE16N: Visualizar tabla

* SE16H: Visualizar tabla

* SM30: Vista de actualización/Mantenimiento

* BS22: Status de órdenes

* SNRO: Secuencia de números

## 3.2 Transacción SE11

Transacción para acceder a cualquier objeto de diccionario de datos:

* Tablas
* Vistas
* Tipos de datos: dominios, elementos de datos, estructuras, tipos tabla
* Dominios
* Ayudas de búsqueda
* Objetos de bloqueo


## 3.3. Dominios

Define el tipo de datos de un campo (CHAR, NUMC, I, DEC, etc) y su longitud.

En algunos casos, también delimita los valores posibles del campo mediante Ámbito de valores fijos y Tabla de valores.

Siempre cuelgan de un elemento de datos, es decir un elemento de datos se puede definir mediante un domino.

Pueden tener una rutina de conversión, de modo que tienen un formato interno y un formato externo. Por lo tanto para convertir el contenido de formato interno a externo y viceversa se debe de llamar a la función definida en la rutina de conversión.

DD_DOMVALUES_GET: función para recuperar los valores posibles de un dominio.

Los dominios deben empezar por Z o por Y.

nOMENCLATURA

ZXX_D_YYYYYYYYY

donde:

* XX es el módulo (MM, FI, HR, ...)

* D es el dominio

* YYYYYYYY es el nombre del módulo

![Dominios. Propiedades](/1.-Introduccion_SAP/images/dom1.png)

![Dominios. Definición](/1.-Introduccion_SAP/images/dom2.png)

![Dominios. Rango de Valores fijos](/1.-Introduccion_SAP/images/dom3.png)


## 3.4. Elementos de datos.

Define la representación semántica de un campo (tablas, estructuras, etc), parámetros o variables que lo utilicen.

Se puede definir de varias formas:

* Tipo elemental:
    * Mediante un dominio
    * Mediante un tipo instalado de SAP

* Tipo referencia

DDIF_DTEL_GET: función para recuperar información de un elemento de datos.

Nomenclatura

ZXX_E_YYYYYYYY

![Elementos. Atributos](/1.-Introduccion_SAP/images/elem1.png)

![Elementos. Tipo de datos](/1.-Introduccion_SAP/images/elem2.png)

![Elementos. Descripción](/1.-Introduccion_SAP/images/elem3.png)
## 3.5. Tablas

Son la fuente de información, **objeto donde se almacenan los datos de forma persistente**.

Compuestas por **campos claves**, que **identifican de manera unívoca cada entidad que se almacena en ella**, como los primary keys de SQL.

Tipos:

* **Tablas transparentes**: son las tablas físicas dentro del sistema de base de datos, utilizadas para almacenar los datos empresariales y de aplicación que se usan dentro de SAP. Son las más usadas.

* **Tablas cluster**: se almacenan en un cluster de Base de datos. 

* **Tablas pool**: almacenan la información en una tabla física dentro del gestor de base de datos.

DDIF_FIELDINFO_GET: función para recuperar información de una tabla.

Nomenclatura

ZXX_T_YYYYYYYY

### Tablas Cluster

Tabla de base de datos que contiene a su vez varias tablas

Las “subtablas” tienen un índice común y un campo genérico donde se guardan todos los datos de las mismas

Una Cluster(ed) tabla es una tabla 'virtual' contenida en una Tabla cluster.

Las cluster(ed) tablas se tratan de forma parecida a tablas transparentes, pero en realidad se utiliza la Tabla cluster para crear/visualizar/modificar datos de la tabla.

Una de las ventajas de una tabla cluster es que guarda varios registros de una 'clustered table' en un solo registro de la misma.

El acceso a las tablas cluster no se puede hacer de forma directa, SAP proporciona clases, funciones, etc. para recuperar los datos. Ej: para leer el cluster de nónima de existe la clase CL_HRPAYES_PAYROLL_READER con diversos métodos para recuperar la información.

Desventajas:

* No pueden tener índices secundarios
* No pueden utilizar algunas sentencias ABAP (select distinct / group by)
* No pueden usar SQL nativo
* No se pueden usar sus nombres de campos en una sentencia ORDER salvo la clave primaria.

### Tablas Pool

Varias tablas del diccionario de datos se corresponden con una sola tabla de base de datos (tabla pool).

Se utilizan para almacenar miles de pequeñas tablas de la capa de aplicación en pocas tablas de base de datos

Una Tabla Pool puede contener muchas 'subtablas'. Realmente su efectividad consiste en tener muchas subtablas y así ahorrar accesos a distintas tablas.

### 3.5.1. Elementos de las tablas

### Clase de entrega (Delivery Class)

Define el tipo de acceso que se tendrá a la tabla por parte de los usuarios:

* **A**: Tabla aplicación (datos maestros y de movimiento.
* **C**: Tabla customizing, actualizable sólo por cliente, sin import SAP.
* **L**: Tabla para almacenar datos temporales, entregada vacía.
* **G**: Tabla customizing, protegida contra UPD de SAP, sólo INS.
* **E**: Tabla control, SAP y cliente tienen ámbitos clave propios.
* **S**: Tabla sistema, actualizable sólo por SAP.
* **W**: Tabla sistema, con contenido transportable con objetos TR propios

### Campos

Las **tablas transparentes pueden incluir el campo MANDT (Mandante) como parte de la clave**. LA existencia de este campo hará que el contenido de la tabla sea dependiente del mandante.

![Campos](/1.-Introduccion_SAP/images/tablas2.png)

Cada campo tendrá asignado un elemento de datos o se indicará un tipo básico y longitud para ese tipo de datos.

Se definirán los campos claves.

**Tablas de verificación**: cada campo podrá tener una tabla contra la que verificar el contenido del mismo. En caso de que se intente introducir por pantalla un valor no válido (no incluido en la tabla de verificación de datos) no permitirá la entrada y se emitirá un mensaje de error. **Son como las Foreign keys de SQL**.

**Ayudas de búsqueda**: cada campo podrá tener asociada una ayuda de búsqueda para facilitar la introducción de datos por pantalla, por ejemplo, ID de un cliente, ID de vuelo,…

![Tablas de verificación y Ayuda de busqueda](/1.-Introduccion_SAP/images/tablas3.png)

**Campos moneda / cantidad**: los campos que tengan asignado un tipo moneda o cantidad deberán tener asociado un campo como referencia que indique la moneda o unidad medida de esa cantidad.

![Campos moneda/cantidad](/1.-Introduccion_SAP/images/tablas4.png)

![Campos moneda/cantidad](/1.-Introduccion_SAP/images/tablas5.png)

### Vistas de mantenimiento de datos / Actualización

SAP permite modificar el contenido de las tablas de forma manual, sin necesidad de crear un programa.

Se podrán crear, modificar y borrar registros de las tablas.

Para poder acceder a las vistas de actualización existen dos opciones:

* **Transacción SM30**: desde esta transacción estándar, aunque puede estar restringida para el usuario y que no tenga acceso.

    ![Transaccion SM30](/1.-Introduccion_SAP/images/vistaman1.png)

* **Nueva transacción**: nueva transacción a medida (Z o Y), que a su vez llamará a la SM30 omitiendo la imagen inicial y mostrará directamente el contenido de la tabla para su modificación. Transaccion SE93.

    ![Transaccion a medida](/1.-Introduccion_SAP/images/vista_personaliz.PNG)

    ![Transaccion a medida](/1.-Introduccion_SAP/images/vistaman2.png)

    Para verla, basta con introducir el nombre de la transaccion en la barra de comandos.

Orientadas al usuario, de esta manera podrá modificar el contenido de la tabla de manera rápida y sencilla.

Para generarla, desde la tabla/vista, seleccionamos en el menú Utilidades - Generador actializ.tab. desde donde se generará la siguiente pantalla:

![Generador actializ.tab.](/1.-Introduccion_SAP/images/vistaman3.png)

Elementos importantes:

* **Grupo de autorizaciones**: si se desea restringir el acceso en función de las autorizaciones del usuario, se informará el grupo de autorizaciones, en caso contrario asignamos el **&NC&, sin grupo de autorizaciones**.

* **Grupo de Funciones**: se indicará un nombre del grupo de funciones, en el que SAP creará el código fuente generado de manera automática.

* **Tipo de actualización**: 1 nivel si se quiere actualizar registros de la tabla en masa, 2 niveles si se quiere hacerla registro a registro.

* **Nº de imagen actualiz.**: se numeran las pantallas resumen e imagen individual (en el caso de 2 niveles)

### Índices

Es posible crear un índice para una tabla, aunque no suele ser muy usado.

**Ventajas**: mayor rapidez en consultas sobre campos no clave en tablas con un gran volumen de datos.

**Desventajas**: la inserción de nuevos registros es más lenta, ya que la existencia de un índice implica una re indexación del índice, lo cual empeora el rendimiento.

![Indices](/1.-Introduccion_SAP/images/index1.png)

## 3.6. Estructuras

Conjunto de definiciones de campos almacenados en el diccionario de SAP.

No almacenan datos de forma persistente en la base de datos.

![Estructuras](/1.-Introduccion_SAP/images/struc1.png)

Su uso se limita a la programación ABAP:		

* Definir tipos tabla cuyo tipo de línea será una estructura
* Definir estructuras locales en reports, funciones, etc…

Nomenclatura:

ZXX_E_YYYYYYYY

## 3.7. Tipos tabla

No almacenan datos de forma persistente en la base de datos.

Su uso se limita a la programación ABAP:

* Definir tablas locales en los reports, funciones, etc…
* Definir los tipos de los parámetros en funciones, clases, subrutinas, etc.

**Tipo de línea**: indica la estructura que formará cada línea de la tabla.

**Tipo instalado**: tipo básico de SAP.

![Tipo tabla](/1.-Introduccion_SAP/images/tabletype1.png)

Nomenclatura:

ZXX_TT_YYYYYYYY

## 3.8. Vistas

Visión lógica de una o más tablas, se almacena físicamente en base de datos.

Forma de visualizar el contenido de dos o más tablas al mismo tiempo.

Tipos de vista:

* **Vista de base de datos**: Vista lógica de una o más tablas de BBDD
* **Vista de supresión**: Permite ocultar campos de una tabla para que la inserción en BBDD sea más óptima
* **Vista de actualización**: mantenimiento del contenido de tablas.
* **Vista ayuda**: utilizada para ayudas de búsquedas que necesitan conexión externa.

![Vistas](/1.-Introduccion_SAP/images/view1.png)

Tablas que componen la vista. Solo se podrán mostrar campos procedentes de ellas.

**Condiciones de unión**: se define el conjunto de campos, procedentes de las tablas de la vista, que permitirán identificar los registros a unir y presentar en la vista.

![Vistas. Configuracion](/1.-Introduccion_SAP/images/view2.png)

Nomenclatura:

ZXX_V_YYYYYYYY

## 3.9. Ayudas de búsqueda.

Objeto para definir posibles valores de ayuda de un campo de una tabla de base de datos o de un campo de pantalla.

Es frecuente que cualquier campo de entrada de datos tenga asociado una ayuda de búsqueda de diccionario, sobretodo los que guardan códigos de artículos.

![Ayuda de busqueda.](/1.-Introduccion_SAP/images/searchelp1.png)

Para realizar la selección de datos existen dos opciones:

* **Tabla o Vista para ayuda**: se añade el nombre de la tabla o vista sobre la que se realizará la selección (selecciones mas simples)
* **Función exit de la ayuda de búsqueda**: se añade el nombre de una función donde se podrá hacer la selección de datos por código en función de los parámetros de entrada. (selecciones mas complejas)

**Parámetros**: se añadirán los parámetros (entrada/salida) que tendrá la ayuda de búsqueda.

![Ayuda de busqueda.](/1.-Introduccion_SAP/images/searchelp2.png)

Nomenclatura:

ZXX_H_YYYYYYYY

## 3.10. Objetos de bloqueo

Método para coordinar el acceso de los usuarios a un mismo recurso, de modo que dos usuarios no puedan acceder a la vez al mismo registro.

**Es el primer paso a la hora de crear programas en ABAP**.

Antes de acceder a los datos, se debe de realizar el bloqueo para que ningún otro usuario pueda al objeto bloqueado.

De la misma manera, cuando ya no se necesite el acceso a estos datos, se deberá desbloquear objeto.

![Objeto de bloqueo](/1.-Introduccion_SAP/images/lock1.png)

Nomenclatura:

EZ_XX_YYYYYYYY