# Compiler
FC=mpif90
#FFLAGS=-g -warn
FFLAGS=-g -check all -traceback

# Directories
PWD    = $(shell pwd)
OBJDIR = $(PWD)/obj
SRCDIR = $(PWD)/src

## Set ADIOS_DIR here or before doing make
#override ADIOS_DIR:=/home/lei/bin/adios-1.5.0
#override ADIOS_DIR:=/sw/redhat6/adios/1.5.0/rhel6_pgi13.4
#override ADIOS_INC:=` ${ADIOS_DIR}/bin/adios_config -c -f`
#override ADIOS_FLIB:=`${ADIOS_DIR}/bin/adios_config -l -f`

ASDF_LIBDIR=/home/lei/bin/asdf_util/lib
ASDF_INCDIR=/home/lei/bin/asdf_util/include

SACLIBDIR=$(SACHOME)/lib

ADIOS_INC=$(shell adios_config -cf)
ADIOS_FLIB=$(shell adios_config -lf)

# Libraries
LIBS = -lsacio -lsac -lm -lasdf

# Files and folders
TARGET = asdf_converter
_OBJ = ascii_rw.o seismo_variables.o main_subs.o main.o

# Make all
all: $(OBJDIR) $(TARGET)

OBJ = $(patsubst %,$(OBJDIR)/%,$(_OBJ))

$(MKOBJDIR):
	mkdir -p $(MKOBJDIR)

$(OBJDIR)/%.o: $(SRCDIR)/%.f90
	$(FC) -c -o $@ $< $(FFLAGS) -module $(OBJDIR) -I$(ASDF_INCDIR)

$(TARGET): $(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ) -L$(SACLIBDIR) -L$(ASDF_LIBDIR) $(LIBS) $(ADIOS_FLIB)

.PHONY: clean

clean:
	rm -f $(OBJDIR)/*.f90 $(OBJDIR)/*.o $(OBJDIR)/*.mod core.* NUMRECORDS OBSD_FILES SYNT_FILES $(TARGET)
