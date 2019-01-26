VERSION = "UNDERFINED"

default:
	$(MAKE) fson
	$(FC) -c hash.f90    $(FLAGS) $(CHECKRANGE) $(OPTHASH)
	$(CC) -c sleep.c     $(OPT)
	$(CC) -c libcurl.c   $(OPT) -DPROXY="\"$(PROXY)\""
	$(FC) -c define.f90  $(FLAGS) $(OPT) -cpp -DBOTKEY="\"$(BOTKEY)\"" -DURLBOTKEY="\"$(URLBOT)\""
	$(FC) -c wsleep.f90  $(FLAGS) $(OPT) -fall-intrinsics
	$(FC) -c strings.f90 $(FLAGS) $(OPT)
	$(FC) -c inline.f90  $(FLAGS) $(OPT)
	$(FC) -c tgAPI.f90   $(FLAGS) $(OPT)
	$(FC) -c wfile.f90   $(FLAGS) $(OPT)
	$(FC) -c actions.f90 $(FLAGS) $(OPT) -DVERSION="\"$(VERSION)\"" -cpp
	$(FC) -c wrank.f90   $(FLAGS) $(OPT)
	$(FC) -c mainmpi.f90 $(FLAGS) $(OPT) -fall-intrinsics
	$(FC) *.o $(FLAGS) -o ../$(EXE) -lcurl

fson:
	$(FC) -c fson_string_m.f90 $(OPT)
	$(FC) -c fson_value_m.f90  $(OPT)
	$(FC) -c fson_path_m.f90   $(OPT)
	$(FC) -c fson.f90          $(OPT) -fbackslash
