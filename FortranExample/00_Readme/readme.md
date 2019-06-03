# Fortran Programmier Beispiel
## Motivation
Diese Anleitung soll dazu dienen, dass allgemein ein leichterer Einstieg in die Programmierung mit Fortran gefunden wird (http://phdcomics.com/comics.php?f=1688). Es wird dabei bewusst auf den Umgang mit den Fortran Bibliotheken der **Getriebeabteilung** verzichtet, allerdings werden �hnliche Sturkturen genutzt.
Wie auch in den "gro�en" Bibliotheken gibt es Module und User defined Datatypes und auch die Namen orientieren sich an Namen in den Bibliotheken. Dabei wird in dem vorliegenden Beispiel konsequent auf ein einheitliches Namensschema und englischsprachige Bezeichnungen gesetzt (z.B. `vector4` statt `Vektor4`). Die Richtlinie zum Programmieren ist auch unter `GT_Fortran_Richtline.f90` in diesem Projekt im Ordner *00_Readme* beigelegt.

## Erste Schritte
Programm auschecken, `FortranExample.vcxproj` starten und auf build in Visual Studio klicken (https://xkcd.com/303/). Alternativ kann man auch die `Erstellen.bat` im Projektordner starten. Sollte es zu einem Fehler kommen, muss in der `set_vars.bat` im Projektordner ein Pfad angepasst werden.  In diesem Fall bitte die Datei `set_vars.bat` im Texteditor �ffnen und `${CMAKE_CURRENT_SOURCE_DIR}` durch den Pfad des Programmierbeispiels ersetzen. Dabei auf den Slasch `/` statt des `\` in der Pfadangabe achten. 

## Tipps
In dem Ordner sind ebenfalls Syntax Highlighting Dateien f�r *Notepad++* f�r die Datenformate markdown *.md* und GT Steuerdateien *.ste* zu finden. Diese K�nnen �ber *Men� > Sprachen > Eigene Sprache definieren ...* in Notepad eingebunden werden.
Ebenfalls sind in dem Readme Ordner Links zu dem Bugtracker Mantis [1], dem Wiki der GT-Berechnungsgruppe [2] bei Mantis kann man sich mit den WZL Login Details anmelden und anschlie�end eine Mail mit dem gew�nschten Projekt, welches man bearbeiten m�chte, an hof schicken. Beim Wiki muss ein neuer Benutzer erstellt werden (VornameNachname). Lesen ist im Wiki ohne Anmeldung m�glich. Eine gute Anlaufquelle sind auch immer die gesammelten Mails von hof. Hier kann man sich mal einlesen [3].
Hilfe zu allen Fortran Befehlen sind unter [5] zu finden.
[1] http://wzl-vcs/tools/wiki/gt/Fortran/
[2] http://wzl-vcs/tools/mantis/gt/main_page.php
[3] http://wzl-vcs/tools/wiki/gt/JensHofschr%C3%B6er/EMails
[4] https://software.intel.com/en-us/node/691996

Die Datei *SVN-Links.txt* beinhaltet die Startpfade f�r den Subversion Server. Von da aus kann man sich durchklicken. Die Pfade stehen aber auch zusammengefasst auf [5]
[5] http://wzl-vcs/tools/wiki/gt/GearToolbox/Quellen

In diesem Beispiel wurden unglaublich viele Kommentare genutzt, da es als Beipiel und gleichzeitig Anleitung dienen soll. Aber denkt immer daran, dass auch jemand anderes euer Programm verstehen soll (http://phdcomics.com/comics.php?f=1689) und nicht zuletzt sollte man es auch selber noch in einem Jahr �berblicken(https://xkcd.com/1421/).

Am besten niemals GOTO verwenden. Das versteht einfach keiner mehr und es gibt eigentlich auch keinen praktischen Grund (https://xkcd.com/292/).

Last but not least eine Suche bei Google mit `Fortran Schl�sselw�rter auf Englisch` hilft bei 90% der Fragen. Gleiches gilt f�r andere Programmiersprachen. Und bei den letzten 10% stehen sowohl Festangestellte als auch andere Assistenten gerne zur Verf�gung.

## Funktion des Programms
Um ein m�glichst anwendungsnahes Beispiel bereitzustellen, wird in dem vorliegenden Beispiel eine einfache Durchdringungsrechnung durchgef�hrt. In der Routine `penetration` wird dazu ein eingelesener Punktezug `rigidContour` auf einem zweiten `softContour` abgew�lzt und das Ergebnis sowie die Differenzen zwischen den Linienz�gen in den jeweiligen Schnitten aka. Spanungsdicken zur�ckgeliefert. Es ist denkbar und einfach m�glich dieses Prgramm zu einer kompletten 3D Durchdringungsrechnung zu erweitern. Um das Beispiel anschaulich zu halten soll dies aber nicht teil der Beispiels sein und kann als Einstieg in die Programmierung gesehen werden. Im Kapitel [Erweiterungen und Ausblick](#Ausblick) werden Ans�tze zur Erweiterung auf eine vollst�ndige 3D Durchdringungsrechnung vorgestellt. Die resultierende Werkst�ckstruktur kann �ber die im `00_Workdir`liegende `plotPoints.m` matlab Datei geplottet werden.

## Struktur
Das Programm liest als erstes die Steuerdatei ein. Diese besitzt die gleiche Syntax wie die Steuerdateien der **WZL Gear Toolbox** und nutzt die gleichen Kenner,allerdings nur einen kleinen Bruchteil des gesamten Umfangs.
Anschlie�end wird die mittels der `simulationControl` der Ablauf im Prgramm festgelegt. Aktuell gibt es nur einen definierten Prozess mit der Prozessnummer `*#### = 2` welcher das Einlesen einer Werkst�ck und einer Werkzeugkontur beinhaltet und anschlie�end diese in einem w�lzenden Prozess zum Schnitt bringt. Dazu werden keine Berechnungen von Werkst�ck oder Werkzeugdaten durchgef�hrt. M�gliche Z-Werte des Werkst�cks wie auch des Werkzeugs werden ignoriert; Die eingelesenen Linienz�ge werden wie im *Stirnschnitt* von Verzahnungen genutzt.
Zus�tzlich ist es m�glich �ber den Prozess `*#### = 1` (equivalent ist kein Kenner) zu w�hlen und eine eigenen Kinematik in Form einer Tabelle, welche mittels `*3000` �bergeben wird, in der Steuerdatei vorzugeben.

### Datenstrukturen
`Datatypes` enth�lt einige Konstanten, welche anschlie�end an verschiedenen Stellen genutzt werden. Hierzu z�hlen beispielweise die Standard Feldl�nge von `CHARACTER` Arrays oder auch der Wert `pi`.

`Vector` beinhaltet die Datenstruktur sowie die Rechenoperationen eines 4 dimensionalen Vektors. Hier kann der Umgang mit Operatoren�berladung gelernt werden.

### Einleseroutinen
In `STE_file` werden s�mtliche Funktionen zusammengefasst, welche ben�tigt werden um die Werte aus der Steuerdatei zu lesen. Dabei greift `STE_file` oft auf Funktionen aus `fIO_file` zur�ck. Dieses Modul stellt grundlegende Funktionen zum Umgang mit Dateien bereit. Sollte man ein Beispiel f�r das Lesen und Schreiben von Textdateien ben�tigen kann man sich hier etwas suchen. Auch gibt es hier ein Beispiel f�r eine rekursive Funktion.

### Kinematik
Die Kinematik wird durch das Modul `KIN_kinematics` zur Verf�gung gestellt. Dabei ist der grundlegende Aufbau identisch zu der Kinematik in der **WZL Gear Toolbox**, wenn auch nicht identisch vom Umfang. Es ist m�glich sowohl konstante Transformationen wie auch linear und quadratisch von der Zeit abh�ngige Transformationen abzubilden. F�r den einfachen Fall des Abw�lzens ist dies implementiert. Dabei wird mithilfe der Subroutine `KIN_addMovement` eine Bewegung der Kinematik angehangen. Dabei kann der Routine zum einen die Art der Bewegung (2. Parameter), als auch der Wert f�r eine konstante (3. Parameter), zeitlich linear (4. Parameter) und quadratisch (5. Parameter) abh�ngige Bewegung �bergeben werden. Zus�tzlich erm�glicht es der letzte, optionale Parameter eine bestimmte Bewegung zu �berschreiben. Zus�tzlich ist es notwendig mit der Routine `KIN_setupKinematics` den Startzeitpunkt einer Bewegung, das Ende und das Simulationsinkrement festzulegen.

### Durchdringungsrechnung
`CC_ContourCalculation` beeinhaltet eine einfache Durchdringungsrechnung. In der **WZL Gear Toolbox** ist eine deutlich umfangreichere Methode hinterlegt.

## <a name="Ausblick"></a> Erweiterungen und Ausblick
Um die Simualtion auf eine 3D Durchdringungsrechnung zu erweitern, m�ssen zum einen H�llk�rper f�r das Werkst�ck und Werkzeug gebildet werden und zum anderen eine Iteration der Durchdringungsrechnung �ber die einzelnen Schnittebenen des Werkst�cks. Hierzu w�rde es sich anbieten die Datenstruktur der `line4` um eine weitere Dimension zu erg�nzen. Beispielsweise einen Datentyp `surface4` welcher ein Feld von `line4` beinhaltet. Zus�tzlich ist es hier erfoderlich, dass der Schnitt des H�llk�rpers in den einzelnen Strinschnittebenen des Werkst�cks berechnet wird. Dazu kann man sich im Toolbox Programm SPARTApro (svn http://wzl-vcs/svn/gt/prox/sparta/pro/spartakus/trunk) die Bibliotheksroutine `CreateWZGSchnitt` angucken.
Anschlie�end k�nnte man Simulationsschritte zum Erstellen oder Einlesen der H�llk�rper in das Hauptprogram hinzuf�gen und in der Simmulationssteuerung verkn�pfen.

## TODO und bekannte Bugs
* Readme erweitern
* Normalen der Schnittpunkte sind noch nicht korrekt
* Whiskey kaufen https://xkcd.com/323/
* ...

