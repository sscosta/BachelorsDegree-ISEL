# *********************************************************************
# NOTE:
# You need to change some variables to conform to your installation.
# Definitely SYSTEMTYPE, JAVAVERSION and PLATFORM. Perhaps JAVAPATH,
# and SICSTUSPATH.
# 'make all' will compile the java code and generate HTML-
# documentation in the subdirectory 'docs'. (You have to create the
# directory 'docs' first).
# 'make run' will run the demo.
# *********************************************************************

# 'mswindows' or 'unix'
#SYSTEMTYPE=mswindows
SYSTEMTYPE=unix

# NOTE: The java version must be the same as the one specified when
#       installing SICSTUS.
#JAVAVERSION=1.2.2
JAVAVERSION=1.3.1

# The platform you are building on.
PLATFORM=x86-linux-glibc2.1
#PLATFORM=x86-win32-nt-4
#PLATFORM=sparc-solaris-5.7

ifeq ($(SYSTEMTYPE),unix)
JAVAPATH=/usr/local/jdk$(JAVAVERSION)
SICSTUSPATH=/usr/local/lib/sicstus-3.9.0
CLASSPATH=$(SICSTUSPATH)/bin/jasper.jar:$(CURDIR)
endif

ifeq ($(SYSTEMTYPE),mswindows)
JAVAPATH=c:/jdk$(JAVAVERSION)
SICSTUSPATH=c:/Program Files/SICStus Prolog
CLASSPATH=$(SICSTUSPATH)/bin/jasper.jar;$(CURDIR)
endif

LIBRARYPATH=$(SICSTUSPATH)/..

JAVABINPATH=$(JAVAPATH)/bin
JAVAC=$(JAVABINPATH)/javac
JAVA=$(JAVABINPATH)/java
JAVADOC=$(JAVABINPATH)/javadoc
JDB=$(JAVABINPATH)/jdb
JAVA_ARGS=-classpath '$(CLASSPATH)' -Djava.library.path='$(LIBRARYPATH)' -Dsicstus.path='$(SICSTUSPATH)' Demo

CLASSES=ShortcutMenu ShortcutReceiver PrologPanel PrologFrame

all: Demo.class docs

Demo.class: $(CLASSES:=.class) Demo.java
	$(JAVAC) -classpath '$(CLASSPATH)' Demo.java

docs: Demo.java $(CLASSES:=.java)
	$(JAVADOC) -classpath '$(CLASSPATH)' -d ./docs -windowtitle 'Jasper demo' $^

run: Demo.class
	$(JAVA) $(JAVA_ARGS)

debug: Demo.class
	$(JDB) $(JAVA_ARGS)

%.class: %.java
	$(JAVAC) -classpath '$(CLASSPATH)' $^

clean:
	rm *.class
	(cd ./docs; rm *.html *.css package-list)

.PHONY: all clean docs
