##
## EPITECH PROJECT, 2022
## ImageCompressor
## File description:
## Makefile
##

CP		=	cp

SRC		=	app/Main.hs 		\
			src/Lib.hs			\

STDLIB_PATH	=	./stdlib
STDLIB_INSTALL_PATH	=	$(shell $(STACK) path --local-bin)/../lib/ek

NAME	=	glados ekc ek

STACK	=	stack

all:
	$(STACK) build
	$(CP) "`$(STACK) path --local-install-root`/bin/"* .

clean:
	$(RM) $(SRC:.hs=.hi)
	$(RM) $(SRC:.hs=.o)

fclean: clean
	$(RM) $(NAME)
	$(RM) -r .stack-work

re: fclean all

tests_run:
	$(STACK) test --coverage
	$(STACK) hpc report --all --destdir test/coverage

functional_test: all
	lit -v ./test/functional

install:
	$(STACK) install
	mkdir -p $(STDLIB_INSTALL_PATH)
	$(CP) $(STDLIB_PATH)/*.ek $(STDLIB_INSTALL_PATH)


.PHONY: all clean fclean re tests_run install
