#
# Copyright (C) 1988-1998 Swedish Institute of Computer Science.
#
#
# NOTE: This Makefile requires GNU make.
#
# NOTE2: If you get this make file as part of a binary installation
# then it may serve as an example but it is unlikely to work "as is". 
#

# [PM] 3.9 Experiment with making more dependencies explicit to
# decrease the number of "make[4]: `foo' is up to date"
NEW_MAKE_RULES:=yes

SRCDIR=.

.PHONY: default
default: all

# Include common Makefile parts.
include $(SRCDIR)/../Common

.SUFFIXES:
.SUFFIXES: .ql .pl .po .c .$(OBJEXT) $(EXEEXT) .$(SHSFX) .$(STSFX)


# [PM] stuff that is not used from prolog but is still part of
## library, e.g., vbsp.dll (but vbsp.dll is windows specific so added below)
OTHER=

## [PM] 3.8.5 	$(TCLTK_MODULE) temporarily removed from MODULES1, see below
## [MC] 3.9 	db.po removed for good
MODULES1= \
	SU_messages.po \
	resgen.po \
	random.po \
	sockets.po \
	system.po \
	charsio.po \
	arrays.po \
	assoc.po \
	atts.po \
	flinkage.po \
	heaps.po \
	linda/client.po \
	linda/server.po \
	lists.po \
	mkindex.po \
	ordsets.po \
	queues.po \
	terms.po \
	trees.po \
	ugraphs.po \
	wgraphs.po \
	expansion.po \
	nondetdecl.po \
	determinacy.po \
	detcheck.po \
	det.po \
	xref.po \
	pillow.po \
	clpb.po \
	fastrw.po \
	$(BDB_MODULE) \
	$(PERL_MODULE) \
	$(TIMEOUT_MODULE) \
        $(COMCLIENT_MODULE)

# [PM] 3.9b4 now added through TIMEOUT_MODULE
# # Modules not supported under Win32
# ifeq ($(WIN32),no)
# MODULES1 += timeout.po
# endif

# Modules only supported under Win32
ifeq ($(ENABLE_VBSP),yes)
MODULES1 += vbsp/vbsp.po
OTHER +=vbsp
COMCLIENT_MODULE = comclient.po
endif

# [PM] 3.9 it is now possible to turn off clpfd
ifeq ($(ENABLE_CLPFD),yes)
	CLPFD_MODULE=clpfd.po
	MODULES1 += fdbg.po
endif


ifeq ($(TCLTK_MODULE),)
MODULES2=
else
MODULES2= gauge.po
endif

MODULES3= \
	$(CLPFD_MODULE) \
	chr.po \
	clpq.po \
	clpr.po \
	objects.po

# Modules requiring -m (i.e, those that load Java/jasper)
MODULES4= \
	$(JASPER_MODULE)

# Dependencies between .po files
linda/server.po linda/client.po: sockets.po lists.po fastrw.po
clpb.po:		atts.po
assoc.po atts.po: 	lists.po
# db.po: 			fastrw.po
fdbg.po: 		lists.po sockets.po clpfd.po
flinkage.po: 		system.po
gauge.po: 		tcltk.po
mkindex.po:		lists.po charsio.po system.po
random.po: 		assoc.po
terms.po:		assoc.po
ugraphs.po:		ordsets.po lists.po assoc.po random.po
wgraphs.po:		ugraphs.po ordsets.po heaps.po lists.po
xref.po:		assoc.po lists.po system.po 
pillow.po:		lists.po sockets.po system.po 
chr.po:			lists.po terms.po assoc.po atts.po
clpq.po clpr.po: 	ordsets.po lists.po atts.po terms.po assoc.po ugraphs.po
clpfd.po:		atts.po assoc.po ordsets.po trees.po
objects.po:		ordsets.po lists.po \
			objects/expand.pl objects/object.pl objects/runlib.pl
resgen.po:		lists.po system.po
comclient.po:           lists.po


## [PM] 3.8.5 	$(TCLTK_MODULE) temporarily removed from MODULES1, so
## need to be added to MODULES explicitly (otherwise it will not be
## part of install target)
MODULES=$(MODULES1) $(MODULES2) $(MODULES3) $(MODULES4) $(TCLTK_MODULE)

# .po files are not really portable, so make sure that they are
# rebuilt when changing platform.
$(MODULES): $(SRCDIR)/../$(PCACHE)

$(SRCDIR)/../$(PCACHE):
	touch $@

# [PM]
other: $(OTHER)

.PHONY:
test_modules:
	for module in $(MODULES:.po=); do \
	  echo ;\
	  echo "*****************************************" ;\
	  echo "*** Attemting to load module $$module ***" ;\
	  echo "*****************************************" ;\
	  echo "use_module(library('$$module')),halt." | $(SICSTUS) -f ;\
	done


# ------------------------------------------------------------------------------
# Stuff to do before building...
# ------------------------------------------------------------------------------

## [MC] 3.9	db.po removed for good
SUBDIR_MODULES= $(CLPFD_MODULE) chr.po $(JASPER_MODULE) $(TCLTK_MODULE) $(BDB_MODULE) $(COMCLIENT_MODULE)

DIRS=$(patsubst %.po,$(PLATFORM)/%,$(SUBDIR_MODULES))

ifeq ($(NEW_MAKE_RULES),yes)

# does not work. I am not sure why nothing seem to match %.obj
# # e.g., a subdir module foo.po need directory x86-win32-nt-5/foo
# $(SUBDIR_MODULES): %.po : $(PLATFORM)/%/dir_create_stamp
#
# # (mkdir -p is not portable!)
# %/.dir_created:
#	 echo Testing if @D ($(@D)) exists
#	 ( [ -d $(@D) ] && echo "it did not" )|| ( echo "it did" && mkdir -p $(@D) )
#	 touch $@
#
# %.obj: $(@D)/.dir_created

# This does not work, why? (however, it turns out that depending
# directly on a directory is a bad idea anyway since the modification
# of the directory changes as soon as something is put into the
# directory.
# # Anything installed in $(PLATFORM) need that directory created
# $(PLATFORM)/%: $(PLATFORM)
#	 echo "foo"
#
# DIRS += $(PLATFORM)

endif

$(DIRS):
	$(MKDIR) $@

.PHONY: dirs
dirs: $(DIRS)

# ------------------------------------------------------------------------------
# Some library modules want soft-links here and there...
# ------------------------------------------------------------------------------
CLPQR_COMMONS=compenv.pl arith.pl bb.pl bv.pl dump.pl fourmotz.pl \
	   ineq.pl itf3.pl nf.pl ordering.pl project.pl redund.pl store.pl
CLPQ_LINKS= $(patsubst %.pl,clpq/%.pl,$(CLPQR_COMMONS))
CLPR_LINKS= $(patsubst %.pl,clpr/%.pl,$(CLPQR_COMMONS))
CHR_LINKS= chr/sbag.pl  

SOFTLINKS=$(CHR_LINKS) $(CLPQ_LINKS) $(CLPR_LINKS)

ifeq ($(WIN32),yes)
$(CLPQ_LINKS): clpq/%.pl: clpqr/%.pl
$(CLPR_LINKS): clpr/%.pl: clpqr/%.pl
chr/sbag.pl: chr/sbag_l.pl
$(CLPR_LINKS) $(CLPQ_LINKS) chr/sbag.pl:
	($(RM) -f $@; $(CP) -f $< $@)
else
$(CLPQ_LINKS): clpq/%.pl: clpqr/%.pl
	($(RM) -f $@; $(LN) ../$< $@)
$(CLPR_LINKS): clpr/%.pl: clpqr/%.pl
	($(RM) -f $@; $(LN) ../$< $@)
chr/sbag.pl: chr/sbag_l.pl
	($(RM) -f $@; $(LN) "`basename $<`" $@)
endif

chr.po:	chr/sbag.pl
clpq.po: $(CLPQ_LINKS)
clpr.po: $(CLPR_LINKS)

$(SOFTLINKS): $(SRCDIR)/../$(PCACHE)

# ------------------------------------------------------------------------------
# Main rule. Build invididual modules by:
# % make all MODULES='tcltk.po system.po'".
# ------------------------------------------------------------------------------

ifeq ($(NEW_MAKE_RULES),yes)

.PHONY: all
all: dirs
	$(MAKE) all_the_rest

.PHONY: all_the_rest
all_the_rest: $(MODULES) other INDEX.pl


# Duh! Making a real target depend on a phony target will never
# consider the real target up-to-date
# all: $(MODULES) other INDEX.pl
#
# # This is to force dirs before anything else. I did not find a working
# # way to express the dependency on the $(PLATFORM) directory directly.
# # Note that you really do not want to depend on the modification date
# # of a directory, only its existence. One (failed) idea was to use a
# # time-stamp file in each directory.
# $(MODULES): dirs
# other: dirs
# INDEX.pl: dirs


else				# 3.8/early 3.9
.PHONY: all
all:
	$(MAKE) dirs jni_headers other
	$(MAKE) $(MODULES)
	$(MAKE) INDEX.pl
endif

$(MODULES1): %.po: %.pl
	@echo
	@echo "Building $@..."
	@echo =========================================
	@echo 
	echo "make('$*',[]),halt." | $(SICSTUS) -f -l mklibs

ifeq ($(NODE_NAME),mummel)
# [PM] 3.8.5 A temporary hack to enable build on mummel where X11 installation is broken.
#            TCLTK_MODULE should go on MODULES1
$(TCLTK_MODULE): %.po: %.pl
	@echo
	@echo "Building $@..."
	@echo =========================================
	@echo 
	@echo "Warning: build will ignore errors"
	echo "make('$*',[],[on_error(warn)]),halt." | $(SICSTUS) -f -l mklibs
else
$(TCLTK_MODULE): %.po: %.pl
	@echo
	@echo "Building $@..."
	@echo =========================================
	@echo 
	echo "make('$*',[]),halt." | $(SICSTUS) -f -l mklibs
endif

ifeq ($(NODE_NAME),mummel)
# [PM] 3.8.5 The on_error for MODULES2 is just
#            temporary to ignore the TclTk problem on muneca.
$(MODULES2): %.po: %.pl context.pl
	@echo
	@echo "Building $@..."
	@echo =========================================
	@echo 
	@echo "Warning: build will ignore errors"
	echo "make('$*',[context],[on_error(warn)]),halt." | $(SICSTUS) -f -l mklibs
else
$(MODULES2): %.po: %.pl context.pl
	@echo
	@echo "Building $@..."
	@echo =========================================
	@echo 
	echo "make('$*',[context]),halt." | $(SICSTUS) -f -l mklibs
endif


$(MODULES3): %.po: %.pl
	@echo
	@echo "Building $@..."
	@echo =========================================
	@echo 
	echo "make_$*,halt." | $(SICSTUS) -f -l mklibs

$(MODULES4): %.po: %.pl
	@echo
	@echo "Building $@..."
	@echo =========================================
	@echo 
	echo "make('$*',[]),halt." | $(SICSTUS) -m -f -l mklibs

## [PM] Build vbsp dll. Note that this is not a foreign resource. It
## is only loaded from VB.
##

# C flags for foreign resources. Make sure the latest sicstus.h is
# found first by placing prefix/include first.
CFLAGS:= -I'$(shell $(CYGPATH) "$(prefix)/include")' $(CFLAGS)

.PHONY: vbsp
vbsp: $(PLATFORM)/vbsp.$(SHSFX)


ifeq ($(ENABLE_SP_DISPATCH_API),yes)
## We would like to use SPLD but there is currently (3.9 beta/3.8.6)
## no way to avoid linking in charmain.obj etc.
## Want someting like $(SPLD) --main=main /* userwritten main etc */ --dll ...
#$(PLATFORM)/vbsp.$(SHSFX): vbsp/vbsp.c
#	: # do we need to create an import library?
#	$(SPLD) -main=user --cflag=-LD $(SPLDFLAGS) $^ -o $@
#

$(PLATFORM)/vbsp.$(SHSFX): %.$(SHSFX): %.$(OBJEXT)
	$(SHLD) $(SHLDFLAGS) $^ ../Emulator/$(PLATFORM)/spaux.$(OBJEXT) '$(shell $(CYGPATH) "$(SP_BINDIR)/$(IMPLIB)")' $(LINK_OUTPUT_OPT)$@ -IMPLIB:$*.lib
	$(RM) -f $*.exp $(RMDUMMY)
	: # Do we really need a import library?
	$(RM) -f $*.lib $(RMDUMMY)

$(PLATFORM)/vbsp.$(OBJEXT): vbsp/vbsp.c vbsp/sp_fpwrap.h
	$(CC) $(CFLAGS) $(INCR_CFLAGS) $(NOLINK_OPT) $< $(NOLINK_OUTPUT_OPT)$@

else # !ENABLE_SP_DISPATCH_API Old 3.8 way, explicit CC and SHLD

$(PLATFORM)/vbsp.$(SHSFX): %.$(SHSFX): %.$(OBJEXT)
	$(SHLD) $(SHLDFLAGS) $^ '$(shell $(CYGPATH) "$(SP_BINDIR)/$(IMPLIB)")' $(LINK_OUTPUT_OPT)$@ -IMPLIB:$*.lib	
	$(RM) -f $*.exp $(RMDUMMY)


$(PLATFORM)/vbsp.$(OBJEXT): vbsp/vbsp.c
	$(CC) $(CFLAGS) $(INCR_CFLAGS) $(NOLINK_OPT) $< $(NOLINK_OUTPUT_OPT)$@

endif # !ENABLE_SP_DISPATCH_API


# ------------------------------------------------------------------------------
# Definition of foreign resources.
# ------------------------------------------------------------------------------

# TODO: fix dependencies from C source to header files.

# These definitions are used in the makefile fragments generated below by
# genmakefile.pl. Their name is very significant: <module>_src etc. is used
# to construct dependencies for <module>.<shsfx>.
# The .c suffix should not be present
system_src=system systw32
sockets_src=sockets
charsio_src=charsio
timeout_src=timeout
random_src=random
fastrw_src=fastrw
$(patsubst %,$(PLATFORM)/%_d.$(OBJEXT),$(fastrw_src)) : fastrw.h
$(patsubst %,$(PLATFORM)/%_s.$(OBJEXT),$(fastrw_src)) : fastrw.h

perl_src=perl
tcltk_src=tcltk/tcl tcltk/tk tcltk/tkappini tcltk/tkterm tcltk/util
comclient_src=comclient/comclient

clpfd_src= \
clpfd/alldifferent \
clpfd/alldistinct \
clpfd/arith \
clpfd/bool \
clpfd/cumulative \
clpfd/fdsets \
clpfd/indexical \
clpfd/main \
clpfd/profile \
clpfd/reified \
clpfd/relation \
clpfd/saturate \
clpfd/setsingleton \
clpfd/statistics \
clpfd/support \
clpfd/sweep \
clpfd/case \
clpfd/symmcum \
clpfd/gcc

# [PM] 3.9b4 (almost) all CLPFD files depend on fd.h and qsort.h
$(patsubst %,$(PLATFORM)/%_d.$(OBJEXT),$(clpfd_src)) : clpfd/fd.h clpfd/qsort.ic
$(patsubst %,$(PLATFORM)/%_s.$(OBJEXT),$(clpfd_src)) : clpfd/fd.h clpfd/qsort.ic


chr_src=chr/chr
ifeq ($(JASPER_IS_ONLY_FOREIGN),yes)
jasper_src=jasper/jasper
else				# 3.8
jasper_src=jasper/spnative jasper/jasper
endif
bdb_src=bdb/dbaux bdb/hash bdb/bdb bdb/ixkeys
# db_src=db/alloc db/file db/stats db/top db/insert db/print db/cache db/message db/hash db/db_conf

sockets_lib=$(SOCKETLIB)
system_lib=$(SYSTEMLIB) 
tcltk_lib=$(TCLLIB)
bdb_lib=$(BDBLIB)
jasper_lib=$(JAVALIB)

ifneq ($(JASPER_IS_ONLY_FOREIGN),yes)
libjasper_lib=$(jasper_lib)
endif

#################################################################################
######## Tcl/Tk

## [PM] 3.9 Added -I. to CFLAGS for all resources that reside in
##          subdirs in order to find <RESOURCE>_glue.h
tcltk_copt=$(TCLINC) -I.

## [PM] 3.9 On scheutz (sparc-solaris-5.7) The X11 headers contains a
## lot of function declarations without return type.
ifeq "$(strip $(notdir $(CC)))" "gcc"
   tcltk_copt += -Wno-implicit-int
endif

# [PM] 3.9b4 Tcl/Tk headers are broken. they should say that their API uses __cdecl
ifeq ($(WIN32),yes)
   tcltk_copt += -Gd
   tcltk_d_splfr_copt = $(patsubst %,--cflag=%$(space),$(filter-out -Gd, $(tcltk_copt)))
   tcltk_s_splfr_copt = $(patsubst %,--cflag=%$(space),$(filter-out -Gd, $(tcltk_copt)))
endif				# WIN32


ifeq ("$(strip $(notdir $(CC)))","gcc") # gcc

        # [PM] 3.9b4 Older X11 headers are not fully prototyped, even
        # if __STDC__ In particular this happens on scheutz (SunOS
        # 5.7, X11 headers from 1991) We could check the headers, X11
        # on Linux does the right thing.

	tcltk_copt += -Wno-strict-prototypes

endif				# gcc
# [PM] 3.9b5 ignore #pragma ident in Solaris Xlib.h
tcltk_copt += $(NO_UNKNOWN_PRAGMAS)

#################################################################################
######## Berkeley DB

bdb_copt=-DSICSTUS $(BDBINC) -I.

# [PM] 3.9b4 bdb headers are broken. they should say that their API uses__cdecl
ifeq ($(WIN32),yes)
   bdb_copt += -Gd

   bdb_d_splfr_copt = $(patsubst %,--cflag=%$(space),$(filter-out -Gd, $(bdb_copt)))
   bdb_s_splfr_copt = $(patsubst %,--cflag=%$(space),$(filter-out -Gd, $(bdb_copt)))

endif				# WIN32

#################################################################################
######## Sockets

sockets_copt=
ifeq ($(ENABLE_MULTITHREADED_LIBS),yes)
sockets_d_copt = $(sockets_copt)
sockets_d_copt += -DMULTI_SP_AWARE
sockets_d_splfrflags = $(sockets_splfrflags)
sockets_d_splfrflags += --multi-sp-aware
endif				# ENABLE_MULTITHREADED_LIBS

#################################################################################
######## Constraint Handling Rules

chr_copt=-I.
ifeq ($(ENABLE_MULTITHREADED_LIBS),yes)
chr_d_copt = $(chr_copt)
chr_d_copt += -DMULTI_SP_AWARE
chr_d_splfrflags = $(chr_splfrflags)
chr_d_splfrflags += --multi-sp-aware
endif				# ENABLE_MULTITHREADED_LIBS

#################################################################################
######## Jasper

# [PM] 3.9 Q: If PCC_STRUCT_RETURN is needed then it should perhaps
# make it default for all foreign resources (I guess it used to work
# because foreign resources never return structures to SICStus).
jasper_copt=$(PCC_STRUCT_RETURN) $(NO_UNKNOWN_PRAGMAS) $(JAVAINC) -I.


ifneq ($(JASPER_IS_ONLY_FOREIGN),yes)
libjasper_copt=$(jasper_copt)
endif

# <<<<<<< Makefile
# clpfd_copt=-DSP_INSIDER -I$(SRCDIR)/../Emulator -I$(SRCDIR)/../Emulator/$(PLATFORM) -I.
# fastrw_copt=-DSICSTUS
# # [PM] 3.9 this is -DSICSTUS in 3.8.6
# # db_copt=-DSICSTUS -I.
# =======
# >>>>>>> 1.87

# NOTE: (lib)jasper_splfrflags should match what is used by
#        InstallSICStus.in ([PM] 3.8.6)



ifeq ($(JASPER_IS_ONLY_FOREIGN),yes)
jasper_splfrflags=$(JASPER_SPLFR_FLAGS)
else
# [PM] 3.9 beta 1, This is really a separate issue (i.e., if Java
# foreign resources are supported (at all) and if so, if they link
# against an import library.) The plan for 3.9 is somewhere between
# not supprting it (use jasper_call instead) and in either case not
# using an import library (e.g., fake it by generating a foreign
# resource that defines prolog predicates that do use jasper_call).
jasper_splfrflags=--import

# [PM] 3.8.6 --no-rpath prevents the -L<JDK-PATH> from being turned into -rpath
#      . On Linux we now add the JDK rpath to the sicstus executable instead.
#        This solves a 'file not found' problem when JVM dlopens,
#        libzip.so and libverify.so.
#      . On Solaris the rpath of the executable is not used to resolve
#        dependencies of dlopened shared objects so we must continue to
#        hardwire the JDK rpath within libjasper.so. On the other hand
#        $ORIGIN is used to find libzip.so etc.
#
#      The hardwiring of the rpath to libsprt.so is not affected by --no-rpath.
libjasper_splfrflags=$(jasper_splfrflags) $(JASPER_SPLFR_FLAGS)
endif

# NOTE: (lib)jasper_splfrflags should match what is used by
#        InstallSICStus.in ([PM] 3.8.6)

ifeq ($(JASPER_IS_ONLY_FOREIGN),yes)
ifeq ($(ENABLE_SP_DISPATCH_API),yes)
ifeq ($(ENABLE_MULTI_SP),yes)
jasper_d_copt = $(jasper_copt)
jasper_d_copt += -DMULTI_SP_AWARE
jasper_d_splfrflags = $(jasper_splfrflags)
jasper_d_splfrflags += --multi-sp-aware
endif				# ENABLE_MULTI_SP
endif				# ENABLE_SP_DISPATCH_API
endif				# JASPER_IS_ONLY_FOREIGN

#################################################################################
######## charsio
ifeq ($(ENABLE_MULTITHREADED_LIBS),yes)
charsio_d_copt = $(charsio_copt)
charsio_d_copt += -DMULTI_SP_AWARE
charsio_d_splfrflags = $(charsio_splfrflags)
charsio_d_splfrflags += --multi-sp-aware
endif				# ENABLE_MULTITHREADED_LIBS


#################################################################################
######## CLPFD

clpfd_copt=-DSP_INSIDER -I$(SRCDIR)/../Emulator -I$(SRCDIR)/../Emulator/$(PLATFORM) -I.

ifeq ($(ENABLE_CLPFD_MULTI_SP),yes)
ifeq ($(ENABLE_MULTI_SP),yes)
clpfd_d_copt = $(clpfd_copt)
clpfd_d_copt += -DMULTI_SP_AWARE
clpfd_d_splfrflags = $(clpfd_splfrflags)
clpfd_d_splfrflags += --multi-sp-aware
endif				# ENABLE_MULTI_SP
endif				# ENABLE_CLPFD_MULTI_SP


#################################################################################
######## fastrw

fastrw_copt=-DSICSTUS
ifeq ($(ENABLE_MULTITHREADED_LIBS),yes)
fastrw_d_copt = $(fastrw_copt)
fastrw_d_copt += -DMULTI_SP_AWARE
fastrw_d_splfrflags = $(fastrw_splfrflags)
fastrw_d_splfrflags += --multi-sp-aware
endif				# ENABLE_MULTITHREADED_LIBS

#################################################################################
######## db

# [PM] 3.9 this is -DSICSTUS in 3.8.6
# db_copt=-DSICSTUS -I.

#################################################################################
######## random
ifeq ($(ENABLE_MULTITHREADED_LIBS),yes)
random_d_copt = $(random_copt)
random_d_copt += -DMULTI_SP_AWARE
random_d_splfrflags = $(random_splfrflags)
random_d_splfrflags += --multi-sp-aware
endif				# ENABLE_MULTITHREADED_LIBS

#################################################################################
######## system
ifeq ($(ENABLE_SYSTEM_SINGLE_THREADED),yes)
system_d_copt = $(system_copt)
system_d_copt += -DSP_SINGLE_THREADED
system_d_splfrflags = $(system_splfrflags)
system_d_splfrflags += --exclusive-access
endif

#################################################################################
######## timeout
ifeq ($(ENABLE_PTHREAD_TIMEOUT),yes)
# [PM] 3.9b4 the new pthread based timeout. Very experimental
timeout_copt += -DLEGACY_TIMOUT=0
endif




############################################################################################

# ------------------------------------------------------------------------------
# Build rules for foreign resources
# ------------------------------------------------------------------------------

# FOREIGN_MODULES define which modules get a shared/static resource
# built with them. 
## [MC] 3.9	db.po removed for good
FOREIGN_MODULES= \
	random.po system.po timeout.po charsio.po sockets.po \
	fastrw.po $(CLPFD_MODULE) chr.po $(TCLTK_MODULE) \
	$(PERL_MODULE) $(BDB_MODULE) $(COMCLIENT_MODULE)


ifeq ($(JASPER_IS_ONLY_FOREIGN),yes)
# [PM] 3.9 jasper is just another foreign module
FOREIGN_MODULES += $(JASPER_MODULE)
endif

# This rule says: every foreign module has an associated shared resource
# and an optional static resource.
$(FOREIGN_MODULES): %.po: $(PLATFORM)/%.$(SHSFX)
ifeq (.$(ENABLE_STATLIB).,.yes.)
$(FOREIGN_MODULES): %.po: $(PLATFORM)/%.$(STSFX)
endif

SHARED_RESOURCES=$(patsubst %.po,$(PLATFORM)/%.$(SHSFX),$(FOREIGN_MODULES)) 
STATIC_RESOURCES=$(patsubst %.po,$(PLATFORM)/%.$(STSFX),$(FOREIGN_MODULES))

# We generate makefile fragments which describe foreign resource
# dependencies.
# [PM] 3.9 generate in $(PLATFORM)
FR_MAKEFILES=$(patsubst %.po,$(PLATFORM)/Makefile_%,$(FOREIGN_MODULES))

# Generate makefile fragments for foreign resource dependencies.
ifeq (yes,yes)
# [PM] 3.9 no special casing of Makefile_chr
#
# [PM] 3.9 Create $PLATFORM if not already there. (the - is to ignore
# errors in case mkdir complains if the directory is there (which in
# turn will only be a problem with paralell make which we do not ue
# (yet), oh well).
$(FR_MAKEFILES): $(PLATFORM)/Makefile_%: genmakefile.pl
	-test -d $(dir $@) || mkdir $(dir $@)
	$(PERL) genmakefile.pl $* $*.pl $(OBJEXT) $($(*)_src) > $@

else # Pre 3.9 special case Makefile_chr.
$(filter-out Makefile_chr,$(FR_MAKEFILES)): Makefile_%: genmakefile.pl
	$(PERL) genmakefile.pl $* $*.pl $(OBJEXT) $($(*)_src) > $@

# Special rule for Makefile_chr, since it uses a chr_decl.pl instead of
# chr.pl.
Makefile_chr: Makefile_%: genmakefile.pl
	$(PERL) genmakefile.pl $* chr_decl.pl $(OBJEXT) $($(*)_src) > $@

endif				# Makefile_chr special

# [PM] 3.9 Build extern declarations for foreign/2,3 facts
$(PLATFORM)/%_glue.h: %.pl
	$(SPLFR) $(SPLFRFLAGS) --header='$@' --nocompile $^

ifneq ($(JASPER_IS_ONLY_FOREIGN), yes)
# Special case for Jasper
$(PLATFORM)/libjasper_glue.h: jasper.pl
	$(SPLFR) $(SPLFRFLAGS) --header='$@' --nocompile $^
endif

# [PM] 3.9
ifneq ($(JASPER_IS_ONLY_FOREIGN), yes)

# [JE, 2000-04-14] On UNIX, the Jasper-resource must be called
# 'libjasper'. See discussion in library/jasper.pl.

# [JE, 2000-04-14] Originally, the jasper.so-resource was copied as well,
# but since the resource name is hardwired into the glue-code we need to
# explicitly create a resource called 'libjasper'.  We would actually want
# to have a dependency from jasper.po directly to <prefix>/lib/libjasper.so
# but unfortunately 'genmakefile.pl' cannot generate such a Makefile. So,
# we create the resource in $(PLATFORM) and then copy it.

# [JE, 2000-05-04]: This now goes for Win32 as well; the jasper.dll
# resource is created "in-place" and then moved into $(prefix)/bin
# ($(SP_ROOTLIBDIR)).

# [JE, 2000-05-04]: We allow ourselves to copy the resource in order to
# avoid having it remade every time 'make lib' is called. This was
# previously considered dangerous, since the original problem was that
# several copies of [lib]jasper.$(SHSFX) were lying around. However, this
# ought to be completely safe; the jasper-resource is only looked for in
# *one* place $(SP_ROOTLIBDIR), and thus the copy in library/$(PLATFORM) is
# only used as a "template" from where 'make lib' can copy the resource to
# the correct place (instead of rebuilding it).

ifeq ($(WIN32),no)
SP_ROOTLIBDIR=$(prefix)/lib
jasper.po: $(SP_ROOTLIBDIR)/libjasper.$(SHSFX)
$(SP_ROOTLIBDIR)/libjasper.$(SHSFX): $(PLATFORM)/libjasper.$(SHSFX)
	$(CP) $^ $@

SHARED_RESOURCES += $(PLATFORM)/libjasper.$(SHSFX)
FR_MAKEFILES += Makefile_libjasper

ifeq ($(ENABLE_STATLIB),yes)
STATIC_RESOURCES += $(PLATFORM)/libjasper.$(STSFX)
jasper.po: $(PLATFORM)/libjasper.$(STSFX)
endif

# Special rule for Makefile_libjasper; it uses the same source/object files
# as Makefile_jasper, but the resource itself is called 'libjasper'.
Makefile_libjasper: genmakefile.pl
	$(PERL) genmakefile.pl libjasper jasper.pl $(OBJEXT) $(jasper_src) > $@

else				# WIN32 = yes

SP_ROOTLIBDIR=$(SP_BINDIR)

jasper.po: $(SP_ROOTLIBDIR)/jasper.$(SHSFX)
$(SP_ROOTLIBDIR)/jasper.$(SHSFX): $(PLATFORM)/jasper.$(SHSFX)
	$(CP) $^ $@

SHARED_RESOURCES += $(PLATFORM)/jasper.$(SHSFX)

# [PM] 3.9 should not be needed, already there
# FR_MAKEFILES += Makefile_jasper

ifeq ($(ENABLE_STATLIB),yes)
STATIC_RESOURCES += $(PLATFORM)/jasper.$(STSFX)
jasper.po: $(PLATFORM)/jasper.$(STSFX)
endif

Makefile_jasper: genmakefile.pl
	$(PERL) genmakefile.pl jasper jasper.pl $(OBJEXT) $(jasper_src) > $@
endif				# WIN32 = yes

endif				# JASPER_IS_ONLY_FOREIGN != yes

# [PM] 3.9 Do not include (in particular do not generate) Makefile_* when cleaning
ifeq ($(findstring clean,$(MAKECMDGOALS)),) # clean not found in MAKECMDGOALS
include $(FR_MAKEFILES)
endif


# ##### Special handling of library(db) (obsolete)

# DB_VERSION=3

# $(PLATFORM)/db/config: $(SRCDIR)/db/config.c
# 	$(CC) $(CFLAGS) -DVER="\"$(DB_VERSION)\"" $(OUTPUT_OPT)$@ $<

# db/db_conf.c: $(PLATFORM)/db/config
# 	./$^ > $@

.PHONY: jasper_examples

ifneq ($(JASPER_MODULE),)

ifeq ($(WIN32),no)
SP_ROOTLIBDIR=$(prefix)/lib
else
SP_ROOTLIBDIR=$(SP_BINDIR)
endif

# ------------------------------------------------------------------------------
# Build JNI header files
# ------------------------------------------------------------------------------

#[PD] 3.9 added Jasper (*** FIX? Since no native methods are left in SPTerm,
#                       SPPredicate, SPQuery, SPException and SPCanonicalAtom
#                       (they have been moved to SICStus), should we remove all
#                       but Jasper and SICStus?)
JNI_CLASSES= Jasper SICStus SPTerm SPPredicate SPQuery SPException SPCanonicalAtom
JNI_HDR= $(patsubst %,jasper/se_sics_jasper_%.h,$(JNI_CLASSES))

.PHONY: jni_headers
jni_headers: $(JNI_HDR)

$(JNI_HDR): jasper/se_sics_jasper_%.h: $(SRCDIR)/../se/sics/jasper/%.class
	(cd jasper ; $(JAVAH) -verbose -classpath $(SRCDIR)/../.. -jni se.sics.jasper.$* )
	touch $@

ifeq ($(NEW_MAKE_RULES),yes)

# this is cheating, it is really compiling spnative.c that needs JNI_HDR 
jasper.po: $(JNI_HDR)

endif

# ------------------------------------------------------------------------------
# Build examples in library('jasper/examples')
# ------------------------------------------------------------------------------

XMPLDIR=$(SRCDIR)/jasper/examples

ifeq (yes,no)
JASPER_EXAMPLES=$(warning *** WARNING: Jasper examples are not built, fix this!)
else
#[PD] 3.8.7 no foreign resources in example
#JASPER_EXAMPLES=$(patsubst %, $(XMPLDIR)/%, simple.$(SHSFX) Queens.class Simple.class simple.po jqueens.po)
JASPER_EXAMPLES=$(patsubst %, $(XMPLDIR)/%, Queens.class Simple.class simple.po jqueens.po)
endif

jasper_examples: $(JASPER_EXAMPLES)

#[PD] 3.8.7 no foreign resources in example
#$(XMPLDIR)/simple.$(SHSFX): %.$(SHSFX): %.pl $(SRCDIR)/../$(PCACHE)
#	$(SPLFR) $(SPLFRFLAGS) $< -o $@

$(XMPLDIR)/simple.po $(XMPLDIR)/jqueens.po: %.po: %.pl $(SRCDIR)/../$(PCACHE)
	echo "ensure_loaded('$<'),save_files('$<','$@'),halt." | $(SICSTUS) -m -f

# We don't use jasper.jar here in case it hasn't been built.
XMPL_CLASSPATH='$(SRCDIR)$(PATHSEP)..$(PATHSEP)$(shell $(CYGPATH) "$(XMPLDIR)")'

$(XMPLDIR)/Queens.class $(XMPLDIR)/Simple.class: %.class: %.java
	$(JAVAC) $(JAVACFLAGS) $(JAVACDBGFLAGS) -verbose -deprecation -classpath $(XMPL_CLASSPATH) $^

JAVA_EXAMPLE_FLAGS=$(JAVA_EXECOPTS) \
	-Djava.library.path='$(shell $(CYGPATH) "$(SP_ROOTLIBDIR)")' \
	-Dsicstus.path='$(shell $(CYGPATH) "$(SP_LIBDIR)")' \
	-classpath $(XMPL_CLASSPATH)

#[PD] 3.8.7 no foreign resources in example
#run_simple: $(XMPLDIR)/Simple.class $(XMPLDIR)/simple.po $(XMPLDIR)/simple.$(SHSFX)
run_simple: $(XMPLDIR)/Simple.class $(XMPLDIR)/simple.po
	$(JAVAEXEC) $(JAVA_EXAMPLE_FLAGS) Simple
else
jasper_examples jni_headers:
	@echo "Jasper is not supported on this platform. Target \"$@\" will not be built."
endif

# ------------------------------------------------------------------------------
# Support for require/1
# ------------------------------------------------------------------------------

INDEX.pl: mkindex.po $(MODULES:.po=.pl) 
	@echo
	@echo "Building $@..."
	@echo =========================================
	@echo 
	echo "ensure_loaded(mkindex),make_index:make_library_index('.')." | $(SICSTUS) -f

# ------------------------------------------------------------------------------
# Installation
# ------------------------------------------------------------------------------

INSTALLFILES = Makefile Makefile_* README genmakefile.pl hidemod.pl mklibs.pl

ifeq ($(WIN32),no)
# [PM] 3.9b5 license.pl is only used on non-Windows.
INSTALLFILES += license.pl
endif				# !WIN32

.PHONY: install
install:
	@echo "Creating directory $(SP_LIBDIR)/library..."
	$(MKDIR) "$(SP_LIBDIR)/library"
	$(INSTALL_DATA) $(SRCDIR)/../Common $(SP_LIBDIR)
	: # Install makefile fragments for foreign resources
	: # and some auxilliary Prolog code
	for file in $(INSTALLFILES); do \
	    echo "Installing $$file..." ;\
	    $(MKDIR) "$(SP_LIBDIR)/library/`dirname $$file`" ;\
	    $(INSTALL_DATA) $$file "$(SP_LIBDIR)/library/`dirname $$file`" ;\
	done
	: # Install additional Tcl/Tk stuff
	if [ ".$(TCLTK_MODULE)." != ".." ]; then \
	    for file in tkconsole.pl tkconsole.tcl tkterm.tcl; do \
	    	echo "Installing $$file..." ;\
	    	$(INSTALL_DATA) $$file "$(SP_LIBDIR)/library" ;\
	    done ;\
	fi
	: # Install Prolog source code for all the modules
	for srcfile in $(MODULES:.po=.pl); do \
	    echo "Installing $$srcfile..." ;\
	    $(MKDIR) "$(SP_LIBDIR)/library/`dirname $$srcfile`" ;\
	    $(INSTALL_DATA) $$srcfile "$(SP_LIBDIR)/library/`dirname $$srcfile`" ;\
	done
	: # Install auxilliary Prolog source code in module subdirectories
	for subdir in $(SUBDIR_MODULES:.po=); do \
	    echo "Creating directory $(SP_LIBDIR)/library/$$subdir" ;\
	    $(MKDIR) "$(SP_LIBDIR)/library/$$subdir" ;\
	    for file in $$subdir/*; do \
		if [ -f $$file ]; then \
		    echo "Installing $$file..." ;\
		    $(INSTALL_DATA) $$file "$(SP_LIBDIR)/library/$$subdir" ;\
		fi ;\
	    done ;\
	done
	: # Install example directories
	: # [PM] 3.8.4 Ensure wcx_example is installed
	for exfile in `find */examples wcx_example \( -name CVS -prune \) -o \( -type f -a -print \)`; do \
	    echo "Installing $$exfile..." ;\
	    $(MKDIR) "$(SP_LIBDIR)/library/`dirname $$exfile`" ;\
	    $(INSTALL_DATA) $$exfile "$(SP_LIBDIR)/library/`dirname $$exfile`" ;\
	done
	: # Misc stuff which does not fit in the other categories
	: # categories.
	: # [PM] 3.9 removed chr_decl.pl fromthis list
	for file in INDEX.pl charsio.c context.pl fastrw.* gauge.txt pow.pl random.c sockets.c system.c systos2.c systw32.c timeout.c; do \
	    echo "Installing $$file..." ;\
	    $(MKDIR) "$(SP_LIBDIR)/library/`dirname $$file`" ;\
	    $(INSTALL_DATA) $$file "$(SP_LIBDIR)/library/$$file" ;\
	done
	: # Install object code in $(SP_LIBDIR)/$(PLATFORM)
	@echo "Creating directory $(SP_LIBDIR)/library/$(PLATFORM)..."
	$(MKDIR) "$(SP_LIBDIR)/library/$(PLATFORM)"
	for file in $(PLATFORM)/*.$(SHSFX) $(PLATFORM)/*.$(STSFX) $(PLATFORM)/*.$(OBJEXT) ; do \
	    echo "Installing `basename $$file`..." ;\
	    $(MKDIR) "$(SP_LIBDIR)/library/`dirname $$file`" ;\
	    $(INSTALL) $$file "$(SP_LIBDIR)/library/`dirname $$file`" ;\
	done
	: # Install auxilliary object code in $(SP_LIBDIR)/$(PLATFORM)/<subdir>
	for dir in $(DIRS) clpq clpqr clpr linda objects; do \
	    echo "Creating directory $$dir..." ;\
	    $(MKDIR) "$(SP_LIBDIR)/library/$$dir" ;\
	    for file in $$dir/* ; do \
		if [ -f $$file ]; then \
	            if [ -x $$file ]; then \
	    	        echo "Installing `basename $$file` (utility)..." ;\
	    	        $(INSTALL) $$file "$(SP_LIBDIR)/library/$$dir" ;\
		    else \
	    	        echo "Installing `basename $$file`..." ;\
	    	        $(INSTALL_DATA) $$file "$(SP_LIBDIR)/library/$$dir" ;\
		    fi \
		fi \
	    done \
	done
	: # Install module .po files.
	for file in $(MODULES); do \
	    echo "Installing $$file..." ;\
	    $(MKDIR) "$(SP_LIBDIR)/library/`dirname $$file`" ;\
	    $(INSTALL_DATA) $$file "$(SP_LIBDIR)/library/`dirname $$file`" ;\
	done
ifeq ($(ENABLE_VBSP),yes)
	: # Install Visual Basic support
	for file in vbsp/vbsp.c vbsp/vbsp.bas vbsp/sp_fpwrap.h; do \
	    echo "Installing $$file..." ;\
	    $(MKDIR) "$(SP_LIBDIR)/library/`dirname $$file`" ;\
	    $(INSTALL_DATA) $$file "$(SP_LIBDIR)/library/$$file" ;\
	done
endif



# ------------------------------------------------------------------------------
# Administration
# ------------------------------------------------------------------------------
.PHONY: clean distclean
clean distclean:
	$(RM) -rf $(wildcard $(PLATFORM) */$(PLATFORM) *.pdb *.lib *.exp) $(RMDUMMY)
	$(RM) -f $(wildcard $(SOFTLINKS)) $(RMDUMMY)
	(cd jasper/examples ; $(RM) -f *.class *.$(SHSFX) *.po *.ql *.sav $(RMDUMMY))
	(cd jasper; $(RM) -f se_sics_jasper*.h $(RMDUMMY))
	$(RM) -f $(MODULES) $(RMDUMMY)
	$(RM) -f $(wildcard *_glue.h) $(RMDUMMY)

.PHONY: maintainerclean mostlyclean
maintainerclean mostlyclean: distclean
	$(RM) -f $(FR_MAKEFILES) $(RMDUMMY)
