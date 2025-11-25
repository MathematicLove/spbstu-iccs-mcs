CFLAGS  = -std=c99 -Wall -O2

FLEX_PREFIX := $(shell brew --prefix flex 2>/dev/null)
ifeq ($(FLEX_PREFIX),)
    FL_LIB = -lfl
else
    FL_LIB = -L$(FLEX_PREFIX)/lib -lfl
endif

mvm: vm.c lex.yy.c vmparse.tab.c main.c
	$(CC) $(CFLAGS) -o $@ main.c vm.c lex.yy.c vmparse.tab.c $(FL_LIB)

lex.yy.c: vmlex.l
	flex vmlex.l

vmparse.tab.c vmparse.tab.h: vmparse.y
	bison -d vmparse.y

clean:
	rm -f lex.yy.c vmparse.tab.[ch] vmparse.tab.c

distclean: clean
	rm -f mvm
