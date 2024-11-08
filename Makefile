# Compiler
FC = ifx  # Intel Fortran Compiler

# Compilation flags for Intel Fortran Compiler in Windows
FCFLAGS = 	/fpp \
			/Qmkl:parallel \
			/O3 \
			/QxHOST \
			/Qipo99 \
			/traceback \
			/Qinit:zero 

# Include directories
INC_DIRS = 	-I"$(IFORT_COMPILER24)include" \
			-I"$(IFORT_COMPILER24)mkl\include" \
			-I"$(IFORT_COMPILER24)compiler\include" \

# Libraries
LIBS =  mkl_intel_lp64.lib \
		mkl_intel_thread.lib \
		mkl_core.lib \
		libiomp5md.lib \

# Individual compilation and execution shortcuts
.PHONY: test01 test02 test03 test04 test05 test06

test01: Lecture_01.exe
	@./Lecture_01.exe

test02: Lecture_02.exe
	@./Lecture_02.exe

test03: Lecture_03.exe
	@./Lecture_03.exe

test04: Lecture_04.exe
	@./Lecture_04.exe

test05: Lecture_05.exe
	@./Lecture_05.exe

test06: Lecture_06.exe
	@./Lecture_06.exe

# Define each Lecture executable target
Lecture_01.exe: Lecture_01.f90
	$(FC) -o $@ Lecture_01.f90 $(FCFLAGS) $(INC_DIRS) $(LIBS)

Lecture_02.exe: Lecture_02.f90
	$(FC) -o $@ Lecture_02.f90 $(FCFLAGS) $(INC_DIRS) $(LIBS)

Lecture_03.exe: Lecture_03.f90
	$(FC) -o $@ Lecture_03.f90 $(FCFLAGS) $(INC_DIRS) $(LIBS)

Lecture_04.exe: Lecture_04.f90
	$(FC) -o $@ Lecture_04.f90 $(FCFLAGS) $(INC_DIRS) $(LIBS)

Lecture_05.exe: Lecture_05.f90
	$(FC) -o $@ Lecture_05.f90 $(FCFLAGS) $(INC_DIRS) $(LIBS)

Lecture_06.exe: Lecture_06.f90
	$(FC) -o $@ Lecture_06.f90 $(FCFLAGS) $(INC_DIRS) $(LIBS)

# Clean target to remove compiled executables
.PHONY: clean
clean:
	@for /r %%i in (*.obj) do del /Q /F "%%i"
	@for /r %%i in (*.mod) do del /Q /F "%%i"
	@for /r %%i in (*.pdb) do del /Q /F "%%i"
	@if exist "Lecture_01*.exe" del /Q /F "Lecture_01*.exe"
	@if exist "Lecture_02*.exe" del /Q /F "Lecture_02*.exe"
	@if exist "Lecture_03*.exe" del /Q /F "Lecture_03*.exe"
	@if exist "Lecture_04*.exe" del /Q /F "Lecture_04*.exe"
	@if exist "Lecture_05*.exe" del /Q /F "Lecture_05*.exe"
	@if exist "Lecture_06*.exe" del /Q /F "Lecture_06*.exe"
