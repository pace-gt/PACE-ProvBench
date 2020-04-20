FC     = mpif90
LINKER = mpif90
#FOPTS  = -g -C -traceback -check noarg_temp_created -fpe0 -warn alignments -warn argument_checking -warn errors -warn general -warn uncalled -warn uninitialized -warn usage -common_args -fp-stack-check
#FOPTS  = -O3  -pg #-xT -ipo #-pg 
# Optimization flag for CascadeLake
# FOPTS  = -O3  -ansi-alias -ip -axCORE-AVX512,CORE-AVX2,AVX,SSE4.2 
# Optimization flag for AMD Epyc
FOPTS = -O3 -qopt-report=5 -march=core-avx2 -fma -ftz -fomit-frame-pointer -ipo -no-prec-div -fp-model fast=2
EXE   = les3d.x
ALL: ${EXE}

OBJS = modules.o flowio.o fluxi.o fluxj.o fluxk.o grid.o \
       main.o parallel.o props.o setbc.o setiv.o \
       tmstep.o update.o alloc.o 

.F.o:
	${FC} ${FOPTS} -c $<
.f.o:
	${FC} ${FOPTS} -c $<

${EXE}: ${OBJS}
	${LINKER} ${FOPTS} ${OBJS} -o ${EXE} ${FLIBS}

install: ${OBJTS}
	cp ${EXE} ${INSTALL} 
clean:
	/bin/rm -f ${OBJS} ${EXE} *.mod
