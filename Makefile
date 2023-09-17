PREFIX?=/usr/local
INSTALL_BIN=$(PREFIX)/bin
LIBRARY_PATH=$(PREFIX)/lib
TARGET=bin/ros

all: $(TARGET)

install:
	cp -f $(TARGET) $(INSTALL_BIN)
	mkdir -p $(LIBRARY_PATH)/roswell
	cp -r lib/* $(LIBRARY_PATH)/roswell

uninstall:
	rm -rf $(LIBRARY_PATH)/roswell
	rm -f $(PREFIX)/$(TARGET)

####
# invoke linux
# alpine for building environment.
alpine:
	docker run -w /tmp2 -v $$PWD:/tmp2/base --rm -it alpine:3.18 /bin/ash -c \
	  "apk add --no-cache make sudo; \
	   adduser -S u; \
	   echo 'u ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers; \
	   ln -s base/Makefile Makefile; \
	   ln -s base/bin bin; \
	   ln -s base/lib lib; \
	   make install-alpine alpine-sbcl; \
	   chown -R u:nogroup /tmp2; \
	   chown -R u:nogroup /tmp/sbcl; \
	   sudo -u u /bin/ash -i"
# ubuntu for testing environment. try not to copy bin to the environment.
ubuntu:
	docker run -w /tmp2 -v $$PWD:/tmp2/base --rm -it ubuntu:16.04 /bin/bash -c \
	  "apt-get update -y; \
           apt-get install -y sudo make git; \
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
	sbcl \
	  --load quicklisp/setup.lisp \
	  --eval "(ql:quickload :cl-curl)" \
	  --eval "(asdf:operate 'asdf:monolithic-concatenate-source-op :cl-curl)" \
	  --load quicklisp/local-projects/cl-curl/curl.lisp \
	  --eval '(compile-file "quicklisp/local-projects/cl-curl/curl.lisp")' \
	  --quit

linkage-info.sexp: quicklisp/local-projects/cl-curl/curl.fasl
	sbcl --non-interactive \
	  --eval "(require :asdf)" \
	  --eval "(require :sb-posix)" \
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
	sbcl --script /tmp/sbcl/tools-for-build/create-linkage-table-prelink-info-override.lisp $< $@

%.o: %.c
	while read l; do \
	  eval "$${l%%=*}=\"$${l#*=}\""; \
	done < /tmp/sbcl/src/runtime/sbcl.mk \
	&& $$CC $$CFLAGS -Wno-builtin-declaration-mismatch -o $@ -c $<

sbcl.core: quicklisp/local-projects/cl-curl/curl.fasl
	sbcl --non-interactive \
	  --eval "(require :asdf)" \
	  --eval "(require :sb-posix)" \
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

sbcl: sbcl.core
	cp alpine-sbcl sbcl || \
	sbcl \
	  --eval "(require :uiop)" \
	  --eval '(uiop:copy-file *runtime-pathname* "$@")' \
	  --quit
	chmod 755 $@

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

$(TARGET): bin/ros.fasl
	./sbcl \
	  --eval '(uiop/configuration::compute-user-cache)' \
	  --load bin/ros.fasl \
	  --eval "(sb-posix:unsetenv \"P\")" \
	  --eval "(roswell-bin/uname:uname-s)" \
	  --eval "(roswell-bin/uname:uname-m)" \
	  --eval "(setf uiop:*image-entry-point* (uiop:ensure-function \"roswell-bin/main:main\"))" \
	  --eval '(uiop:dump-image "$@" :executable t)'

clean:
	rm -f sbcl.core linkage-info.sexp
	rm -rf quicklisp
	rm -f *.o
	rm -f linkage-table-prelink-info-override.c

download:
	ros roswell-internal-use download https://github.com/roswell/sbcl_bin/releases/download/2.3.6/sbcl-2.3.6-arm64-linux-musl-binary.tar.bz2 ./sbcl-2.3.6-arm64-linux-musl-binary.tar.bz2
.PHONY: alpine ubuntu clean install uninstall
