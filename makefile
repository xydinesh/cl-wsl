all: build
build:
	sbcl --load cl-wsl.asd \
	--eval '(ql:quickload :cl-wsl)' \
       	--eval "(sb-ext:save-lisp-and-die #p\"cl-wsl\" :toplevel #'cl-wsl:main :executable t)"
clean:
	rm -f cl-wsl

