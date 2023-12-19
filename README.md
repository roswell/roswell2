# roswell2
it's experiment to modernize roswell and implement all in common lisp.
for portability purpose, roswell is kept maintained.

# how to build.
make; sudo make install

# how to build on linux
make linux-build; sudo make install

# how to uninstall
sudo make uninstall

# rebuild core (you might need it if you update roswell2)
lisp rebuild

# launch sbcl
lisp run -L sbcl

# launch sbcl with quicklisp
lisp run -L sbcl -Q

# launch windows sbcl using wine.(quit immediately because --repl is not specified)
lisp run -L sbcl --version 2.3.11 --os windows --wrap wine --eval "(print *features*)"

# launch sbcl and print message and launch repl after eval.
lisp run -L sbcl --eval '(print "hello world")' --repl
