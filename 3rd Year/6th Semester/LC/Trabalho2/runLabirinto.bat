cd Labirinto
set path=..\jre\bin\;..\jre\bin\hotspot;..\sicstusRT\bin
rem set SP_PATH=..\sicstusRT
set pathext=%pathext%;.dll
java -classic -cp "../sicstusRT/bin/jasper.jar;../myJavaclasses;." Labirinto
