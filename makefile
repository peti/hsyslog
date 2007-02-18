# build haddock documentation, and release archive

PACKAGE     := syslog
RELEASE     := `date --iso-8601`
DISTARCHIVE := $(PACKAGE)-$(RELEASE).tar.gz
DISTFILES   := Syslog.hsc README
GHCURL      := http://haskell.org/ghc/docs/latest/html/libraries
GHCPREFIX   := /usr/local/ghc-current/share/ghc-6.5/html/libraries
GHCFLAGS    := -Wall -O

.PHONY: all clean dist

all::	docs/index.html

dist::	docs/index.html index.html $(DISTFILES)
	@rm -rf $(DISTARCHIVE) $(PACKAGE)-$(RELEASE)
	@mkdir $(PACKAGE)-$(RELEASE)
	@cp -rp $(DISTFILES) docs $(PACKAGE)-$(RELEASE)/
	@echo Created $(DISTARCHIVE).
	@tar cfvz $(DISTARCHIVE) $(PACKAGE)-$(RELEASE)
	@rm -rf $(PACKAGE)-$(RELEASE)

Syslog.hs:	Syslog.hsc
	@rm -f $@
	hsc2hs $<
	@chmod -w $@

#test:		Syslog.hs PollResolver.hs test.hs
#	ghc -threaded $(GHCFLAGS) --make test.hs -o $@ -ladns

docs/index.html: Syslog.hs $(DISTFILES)
	@-mkdir docs
	@haddock -h -t 'FFI Bindings to Syslog' \
	  -i $(GHCURL)/base,$(GHCPREFIX)/base/base.haddock \
	  -i $(GHCURL)/network,$(GHCPREFIX)/network/network.haddock \
	  -s .. -o docs [A-Z]*.hs

index.html:	README
	@lhs2html $<
	@sed -e 's%<p>\(.*Homepage\]</a></p>\)%<p class="title">\1%' <$<.html >$@
	@rm -f README.html

clean::
	rm -rf docs
	rm -f *.o *.hi a.out Syslog.hs Syslog_stub.?
	rm -f README.html $(PACKAGE)-*.tar.gz

redate::
	redate $(DISTFILES)
