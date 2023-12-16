##
## EPITECH PROJECT, 2022
## ImageCompressor
## File description:
## Makefile
##

SRC		=	app/Main.hs 		\
			src/Lib.hs			\

NAME	=	glados

STACK	=	stack

all:
	$(STACK) build
	cp "`$(STACK) path --local-install-root`/bin/glados" $(NAME)

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

.PHONY: all clean fclean re tests_run
