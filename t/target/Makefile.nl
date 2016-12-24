# -*- makefile-gmake -*-

$(warning This
should
cause
a
bug)

escaped_nl:
	echo This should not work >$@
