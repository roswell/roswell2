PREFIX?=/usr/local
INSTALL_BIN=$(PREFIX)/bin
LIBRARY_PATH=$(PREFIX)/lib
TARGET=bin/lisp

DOCKER_BUILD_OPTION?=
all: $(TARGET)

install: lib/commit
	mkdir -p $(INSTALL_BIN)
	cp -f $(TARGET) $(INSTALL_BIN)
	mkdir -p $(LIBRARY_PATH)/roswell
	cp -r lib/* $(LIBRARY_PATH)/roswell

uninstall:
	rm -rf $(LIBRARY_PATH)/roswell
	rm -f $(PREFIX)/$(TARGET)

####
#dev.
VERSION?=$(shell grep :version lib/roswell2.asd |sed 's/^.*"\(.*\)".*$$/\1/')
# " lem fail
ARCHIVE=roswell-$(VERSION)-$(shell uname -m)-$(shell uname -s)
SBCL?=$(shell which sbcl)
USER_ID?=$(shell id -u)
GROUP_ID?=$(shell id -g)

# invoke linux
# alpine for building environment.
alpine-docker:
	echo "FROM alpine:3.18\\n"\
	     "run /bin/ash -c 'apk add --no-cache make sudo git shadow;" \
	     "adduser -S u;" \
	     "echo \"u ALL=(ALL) NOPASSWD:ALL\" >> /etc/sudoers;" \
	     "mkdir /tmp2;cd /tmp2;" \
	     "git clone https://github.com/roswell/roswell2 base;" \
	     "ln -s base/Makefile Makefile;" \
	     "ln -s base/bin bin;" \
	     "ln -s base/lib lib;" \
	     "make install-alpine alpine-sbcl;'" \
	 | docker build -t roswell2 $(DOCKER_BUILD_OPTION) -
alpine: alpine-docker
	docker run -w /tmp3 -v $$PWD:/tmp3/base --rm -it roswell2 /bin/ash -c \
	  "ln -s base/Makefile Makefile; \
	   ln -s base/bin bin; \
	   ln -s base/lib lib; \
	   ln -s /tmp2/alpine-sbcl alpine-sbcl; \
	   ln -s /tmp2/sbcl.core sbcl.core; \
	   usermod -u $(USER_ID) u; \
	   addgroup -g $(GROUP_ID) u; \
	   usermod -g $(GROUP_ID) u; \
	   chown -R u:u .; \
	   sudo -u u /bin/ash -i"
linux-build: alpine-docker
	docker run -w /tmp3 -v $$PWD:/tmp3/base --rm -it roswell2 /bin/ash -c \
	  "ln -s base/Makefile Makefile; \
	   ln -s base/bin bin; \
	   ln -s base/lib lib; \
	   ln -s /tmp2/alpine-sbcl alpine-sbcl; \
	   ln -s /tmp2/sbcl.core sbcl.core; \
	   usermod -u $(USER_ID) u; \
	   addgroup -g $(GROUP_ID) u; \
	   usermod -g $(GROUP_ID) u; \
	   chown -R u:u .; \
	   sudo -u u make"
# ubuntu for testing environment. try not to copy bin to the environment.
ubuntu:
	docker run -w /tmp2 -v $$PWD:/tmp2/base --rm -it ubuntu:16.04 /bin/bash -c \
	  "apt-get update -y; \
           apt-get install -y sudo make git bzip2; \
	   ln -s base/Makefile Makefile; \
	   ln -s base/bin bin; \
	   ln -s base/lib lib; \
	   /bin/bash -i"

# install packages for environment need to launch with privilege
install-alpine:
	apk add --no-cache alpine-sdk git linux-headers zlib-dev zlib-static \
	    curl-dev curl-static nghttp2-static openssl-libs-static brotli-static zlib-static libidn2-static libunistring-static
	if [ x`which sbcl` == x ]; then \
	  apk add --no-cache sbcl; \
	  git clone --depth 1 --branch=static-executable-v2-2.2.0 https://github.com/daewok/sbcl/ /tmp/sbcl || true; \
	  cd /tmp/sbcl; git apply /tmp2/base/patch-2.2.0; cd -; \
	  cd /tmp/sbcl; echo '"2.2.0-static"' > version.lisp-expr; cd -; \
	  cd /tmp/sbcl; sh make.sh --fancy --with-sb-linkable-runtime --with-sb-prelink-linkage-table; cd -; \
	  apk del --no-cache sbcl; \
	  cd /tmp/sbcl; sh install.sh; \
	fi

quicklisp/setup.lisp:
	git clone --depth 1  https://github.com/quicklisp/quicklisp-client.git quicklisp
	git clone --depth 1  https://github.com/roswell/cl-curl.git quicklisp/local-projects/cl-curl

quicklisp/local-projects/cl-curl/curl.fasl: quicklisp/setup.lisp
	$(SBCL) \
	  --load quicklisp/setup.lisp \
	  --eval "(ql:quickload :cl-curl)" \
	  --eval "(asdf:operate 'asdf:monolithic-concatenate-source-op :cl-curl)" \
	  --load quicklisp/local-projects/cl-curl/curl.lisp \
	  --eval '(compile-file "quicklisp/local-projects/cl-curl/curl.lisp")' \
	  --quit

linkage-info.sexp: quicklisp/local-projects/cl-curl/curl.fasl
	$(SBCL) --non-interactive \
	  --eval "(require :asdf)" \
	  --eval "(require :sb-posix)" \
	  --eval "(require :sb-md5)" \
	  --eval "(require :sb-bsd-sockets)" \
	  --eval "(require :sb-introspect)" \
	  --eval "(require :sb-cltl2)" \
	  --load $< \
	  --eval "(cl-curl:init)" \
	  --eval "#+unix(sb-alien:define-alien-routine (\"execvp\" %execvp) sb-alien:int(program sb-alien:c-string)(argv (* sb-alien:c-string)))" \
	  --load /tmp/sbcl/tools-for-build/dump-linkage-info.lisp \
	  --eval '(sb-dump-linkage-info:dump-to-file "$@")' \
	  --quit

linkage-table-prelink-info-override.c: linkage-info.sexp
	$(SBCL) --script /tmp/sbcl/tools-for-build/create-linkage-table-prelink-info-override.lisp $< $@

%.o: %.c
	while read l; do \
	  eval "$${l%%=*}=\"$${l#*=}\""; \
	done < /tmp/sbcl/src/runtime/sbcl.mk \
	&& $$CC $$CFLAGS -Wno-builtin-declaration-mismatch -o $@ -c $<

sbcl.core: quicklisp/local-projects/cl-curl/curl.fasl
	$(SBCL) --non-interactive \
	  --eval "(require :asdf)" \
	  --eval "(require :sb-posix)" \
	  --eval "(require :sb-md5)" \
	  --eval "(require :sb-bsd-sockets)" \
	  --eval "(require :sb-introspect)" \
	  --eval "(require :sb-cltl2)" \
	  --load $< \
	  --eval "#+unix(sb-alien:define-alien-routine (\"execvp\" %execvp) sb-alien:int(program sb-alien:c-string)(argv (* sb-alien:c-string)))" \
	  --eval '(uiop:dump-image "$@" :compression t)'

alpine-sbcl: linkage-table-prelink-info-override.o sbcl.core
	while read l; do \
	  eval "$${l%%=*}=\"$${l#*=}\""; \
	done < /tmp/sbcl/src/runtime/sbcl.mk \
	&& $$CC -no-pie -static $$LINKFLAGS -o $@ /tmp/sbcl/src/runtime/$$LIBSBCL $< $$LIBS \
	  -static-libgcc -static-libstdc++ -lcurl -lnghttp2 -lssl -lcrypto -lz -lbrotlidec -lbrotlicommon -lidn2 -lunistring

roswell-sbcl:
	$(MAKE) SBCL='ros run -L sbcl-bin +Q' sbcl.core
	ros run -L sbcl-bin --eval '(uiop:copy-file *runtime-pathname* "$@")' --quit
	chmod 755 $@
roswell2-sbcl:
	$(MAKE) SBCL='lisp run -L sbcl --native --' sbcl.core
	lisp run -Q -L sbcl --eval '(uiop:copy-file *runtime-pathname* "$@")' --quit
	chmod 755 $@
normal-sbcl: sbcl.core
	$(SBCL) \
	  --eval "(require :uiop)" \
	  --eval '(uiop:copy-file *runtime-pathname* "$@")' \
	  --quit
	chmod 755 $@

sbcl:
	@cp alpine-sbcl sbcl 2>/dev/null || \
	(rm -f sbcl.core && $(MAKE) roswell-sbcl && cp roswell-sbcl sbcl 2>/dev/null) || \
	(rm -f sbcl.core && $(MAKE) roswell2-sbcl && cp roswell2-sbcl sbcl 2>/dev/null) || \
	(rm -f sbcl.core && $(MAKE) normal-sbcl && cp normal-sbcl sbcl 2>/dev/null)

bin/ros.lisp: sbcl
	./sbcl \
	  --eval '(uiop/configuration::compute-user-cache)' \
	  --eval '(asdf:load-asd (merge-pathnames "bin/roswell-bin.asd" (uiop:getcwd)))' \
	  --eval "(asdf:load-system :roswell-bin)" \
	  --eval '(asdf:make "roswell-bin")' \
	  --eval '(uiop:quit)'

bin/ros.fasl: bin/ros.lisp
	./sbcl \
	  --eval '(uiop/configuration::compute-user-cache)' \
	  --eval '(asdf:load-asd (merge-pathnames "bin/roswell-bin.asd" (uiop:getcwd)))' \
	  --eval "(asdf:load-system :roswell-bin)" \
	  --eval '(compile-file "$<")' \
	  --eval '(uiop:quit)'
	rm -f $<

$(TARGET): bin/ros.fasl lib/commit
	./sbcl \
	  --eval '(uiop/configuration::compute-user-cache)' \
	  --load bin/ros.fasl \
	  --eval "(roswell-bin/main:setup)" \
	  --eval "(setf uiop:*image-entry-point* (uiop:ensure-function \"roswell-bin/main:main\"))" \
	  --eval '(uiop:dump-image "$@" :executable t)'

clean:
	rm -f linkage-info.sexp
	rm -rf quicklisp
	rm -f *.o
	rm -f linkage-table-prelink-info-override.c

archive: lib/commit
	mkdir $(ARCHIVE)
	mkdir $(ARCHIVE)/bin
	mkdir $(ARCHIVE)/lib
	cp $(TARGET) $(ARCHIVE)/bin
	cp -r lib/* $(ARCHIVE)/lib
	sed -n "/####/q;p" Makefile > $(ARCHIVE)/Makefile
	tar jcvf $(ARCHIVE).tbz $(ARCHIVE)

lib/commit:
	echo $(shell cd lib;git show --format='%h %cd' --no-patch|| echo "unknown") > $@

.PHONY: alpine ubuntu clean install uninstall archive lib/commit
