#     ------------------------------------------------------------------
#
#     makefile pour intel
#
#     Auteur : Christophe Peyret christophe.peyret@onera.fr
#     Copyright (c) 2010 ONERA/DSNA. All rights reserved.
#
#     ------------------------------------------------------------------



TARGET = interop


# Compilers and linker

LD = ifort
FC = ifort
CC = icc  
CX = icpc  

# Paths

SRCDIR = ./src
OBJDIR = ./obj
MODDIR = ./mod
EXEDIR = ./
LIBDIR = /usr/local/lib


# INCL and MODS

INCL   = -I     $(MODDIR) -I ./src
MODS   = -module $(MODDIR)


# Compiler Flags

FCFLAGS    = -c -g  -arch x86_64 $(MODS) $(INCL) -qopenmp
CCFLAGS    = -c -g  -arch x86_64 
CXFLAGS    = -c -g  -arch x86_64 

# Linkage

LDFLAGS =  -qopenmp  -arch x86_64 -static-intel -nofor-main -cxxlib
LIBS    = 


# List of Objects

OBJS =                                    \
                                          \
        $(OBJDIR)/moduleMesh.o            \
        $(OBJDIR)/test.o



# Building Rules

$(TARGET) : $(OBJS)
	$(LD) $(LDFLAGS) $(LIBS) $(OBJS) -o $(EXEDIR)$(TARGET)
	@echo  ----------- ${TARGET} created ----------- 

compil_date :
	touch src/mesh_dg.f90
	\rm -f compil_date.h
	echo "write(*,'(/a )')'------------------------------------------------------------------------'" >  compil_date.h
	echo "write(*,'( a )')'                                                                        '" >> compil_date.h
	echo "write(*,'( a )')'         spaceMesh64 2011 - 64 bits version - Mesh Treatments           '" >> compil_date.h
	echo "write(*,'( a )')'         Copyright (c) ONERA, 1995-2011. All rights reserved            '" >> compil_date.h
	echo "write(*,'( a )')' Developped by Christophe Peyret tel: 0146734778 mail: peyret@onera.fr  '" >> compil_date.h
	echo "write(*,'( a )')'                                                                        '" >> compil_date.h
	echo "write(*,'( a )')' Compiled on" `date` "with" `uname -n` " '"                                >> compil_date.h
	echo "write(*,'( a )')' " `svn info | grep "Changed Date"` "                                   '" >> compil_date.h
	echo "write(*,'( a )')' " `svn info | grep Revision` "                                         '" >> compil_date.h
	echo "write(*,'( a/)')'------------------------------------------------------------------------'" >> compil_date.h

clean :
	\rm -f $(OBJS) $(DIREXE)/$(TARGET) $(MODDIR)/*.mod


# Compilation Rules

$(OBJDIR)/%.o : $(SRCDIR)/%.cpp
	$(CX) $(CXFLAGS) -o $@  $< 

$(OBJDIR)/%.o : $(SRCDIR)/%.c
	$(CC) $(CCFLAGS) -o $@  $< 

$(OBJDIR)/%.o : $(SRCDIR)/%.f90
	$(FC) $(FCFLAGS) -o $@  $< 


$(OBJDIR)/mesh_dg.o : $(SRCDIR)/mesh_dg.f90 compil_date
	$(FC) $(FCFLAGS) -o $(OBJDIR)/mesh_dg.o    $(SRCDIR)/mesh_dg.f90
