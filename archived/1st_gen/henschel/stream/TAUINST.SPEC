# Instrumentation for C
BEGIN_INSTRUMENT_SECTION
  entry code="__notify_intrinsic((void*)\"ENTER @ROUTINE@\", (void *)@BEGIN_LINE@);" lang="c"
  exit  code="__notify_intrinsic((void*)\"LEAVE @ROUTINE@\", (void *)@END_LINE@);" lang="c"
END_INSTRUMENT_SECTION

# Instrumentation for C++
BEGIN_INSTRUMENT_SECTION
  file="*" line=1 code="#ifdef TAUINST_NOSRC\n#  define _TAUINST_FILE NULL\n#else\n#  define _TAUINST_FILE \"@FILE@\"\n#endif\n#include <vt_user.h>" lang="c++"
  decl code="VT_Tracer<sizeof(int)> vt_tracer__(\"@ROUTINE@\", _TAUINST_FILE, @BEGIN_LINE@);" lang="c++"
END_INSTRUMENT_SECTION