#
# Copyright (C) 1988-1998 Swedish Institute of Computer Science.
#
# CVS: $Id: Makefile,v 1.7 1999/08/25 14:26:53 jojo Exp $
#
# NOTE: This Makefile requires GNU make.
#

SRCDIR=.

# Include common Makefile parts.
include $(SRCDIR)/../../Common

LIB_PATH1=$(TOP_SRCDIR)/library/$(PLATFORM)
LIB_PATH2=$(TOP_SRCDIR)/library/clpfd/$(PLATFORM)

.SUFFIXES:

# 'make all' is done before library/*.so is created
#all:
#	$(MKDIR) `pwd`/$(PLATFORM)
#	$(MAKE) $(NOLINK_OPT) $(SRCDIR)/$(PLATFORM) SRCDIR=`pwd` VPATH=..:../.. -f ../Makefile \
#		all-recursive

## 'make all' is done after library/*.so is created
#all-post:
#	$(MKDIR) `pwd`/$(PLATFORM)
#	$(MAKE) $(NOLINK_OPT) $(SRCDIR)/$(PLATFORM) SRCDIR=`pwd` VPATH=..:../.. -f ../Makefile \
#		all-post-recursive

OBJ= fd_d.$(OBJEXT) fd_s.$(OBJEXT) fd_glue_d.$(OBJEXT) fd_glue_s.$(OBJEXT)

all-recursive: $(OBJ)
all-post-recursive: qls

qls: fcompile.pl ixq.pl enum.pl compiler.pl lib.pl fdsets.pl 
	echo "asserta(library_directory('$(LIB_PATH1)'))," \
	     "prolog_flag(compiling,_,$(PLFLAGS))," \
	     "['$<']." | $(SICSTUS) -f
	mv $(patsubst %.pl,%.ql,$(filter-out $<,$^)) .
	touch qls

fd_glue.c: clpfd.pl
	echo "prolog:prepare_foreign_resource(clpfd, '$^', '$@')." | $(SICSTUS) -f

fd_glue_d.$(OBJEXT): %_d.$(OBJEXT): %.c
fd_glue_s.$(OBJEXT): %_s.$(OBJEXT): %.c

fd_d.$(OBJEXT): %_d.$(OBJEXT): %.c fd_insn.h
fd_s.$(OBJEXT): %_s.$(OBJEXT): %.c fd_insn.h

INCR_CFLAGS += -DSPDLL

CFLAGS += -DSP_INSIDER -I$(SRCDIR)/../../Emulator -I$(SRCDIR)/../../Emulator/$(PLATFORM) \
	-I$(SRCDIR)/../../Utils/$(PLATFORM)/include

$(OBJ):
	$(CC) $(CFLAGS) $(subst _d.$(OBJEXT), $(INCR_CFLAGS),  $(findstring _d.$(OBJEXT),$@)) \
		$(NOLINK_OPT) $(filter %.c,$^) $(NOLINK_OUTPUT_OPT)$@

install:
	$(MKDIR) $(LIBDIR)/clpfd
	-@CP@ *.ql $(LIBDIR)/clpfd
	$(MKDIR) $(LIBDIR)/clpfd/examples
	-@CP@ examples/README examples/*.pl $(LIBDIR)/clpfd/examples

clean:
	rm -rf $(PLATFORM)
	rm -f all fd_glue.c *.ql *.$(OBJEXT) so_locations

sp.gdb:
	$(SP_LIB)/bin/spmkds sp.gdb ../clpfd.$(STSFX)


