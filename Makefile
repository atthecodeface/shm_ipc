
all:
	jbuilder build @run_test

install:
	jbuilder build @install
	jbuilder install

clean:
	jbuilder clean
