##
## EPITECH PROJECT, 2022
## ImageCompressor
## File description:
## Makefile
##

SRC		=	app/Main.hs 		\
			src/Lib.hs			\

NAME	=	glados ekc ek

STACK	=	stack

all:
	$(STACK) build glados:ek glados:ekc glados:glados
	cp "`$(STACK) path --local-install-root`/bin/"* .

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

install:
	$(STACK) install

.PHONY: all clean fclean re tests_run install
