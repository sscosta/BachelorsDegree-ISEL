#
# Copyright (C) 1988-1998 Swedish Institute of Computer Science.
#
# CVS: $Id: Makefile,v 1.3 2001/04/13 14:18:57 perm Exp $
#
# NOTE: This Makefile is *not* used anymore. It probably does not work.
# NOTE: This Makefile requires GNU make.
#

SRCDIR=.

# Include common Makefile parts.
include $(SRCDIR)/../../Common

# LIB_PATH=`pwd`/..
LIBDIR=$(LIB_PATH)

.SUFFIXES:

CFLAGS += -I$(SRCDIR)/../../Emulator			\
	-I$(SRCDIR)/../../Emulator/$(PLATFORM)		\
	-I$(SRCDIR)/../../Utils/$(PLATFORM)/include	\
	$(JAVAINC)

JNI_CLASSES= SICStus SPTerm SPPredicate SPQuery SPException

OBJ= spnative.$(OBJEXT) jasper.$(OBJEXT) flinkage.$(OBJEXT)

HDR= $(patsubst %,se_sics_jasper_%.h,$(JNI_CLASSES))

LOCAL_CLASSPATH= $(SRCDIR)/..
ifneq (.$(JAVA_CLASSPATH).,..)
LOCAL_CLASSPATH= $(SRCDIR)/..$(PATHSEP)$(JAVA_CLASSPATH)
endif

all: $(HDR)

all-recursive: $(OBJ)

$(HDR): se_sics_jasper_%.h: $(SRCDIR)/../../se/sics/jasper/%.class
	$(JAVAH) -classpath $(SRCDIR)/../.. -jni se.sics.jasper.$*
	touch $@


#$(CLASSES): %.class: %.java
#	$(JAVAC) -classpath '$(LOCAL_CLASSPATH)' $^

#install:
#	-(cd examples;  $(MAKE) $(MFLAGS) LIBDIR=$(LIBDIR) SP_INC=$(SP_INC) install )
#	$(MKDIR) $(LIBDIR)/jasper
#	-$(CP) *.class README $(LIBDIR)/jasper

$(OBJ): %.$(OBJEXT): %.c
	$(CC) $(CFLAGS) -DSPDLL $(INCR_CFLAGS) $(NOLINK_OPT) $(NOLINK_OUTPUT_OPT)$@ $<

clean:
	(cd examples; $(MAKE) clean)
	rm -f *~ *.$(OBJEXT) jasper_*.h



