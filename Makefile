##
## EPITECH PROJECT, 2022
## ImageCompressor
## File description:
## Makefile
##

CP		=	cp -f

SRC		=	app/Main.hs 		\
			src/Lib.hs			\

STDLIB	=	stdlib

STDLIB_INSTALL_PATH	=	~/.local/lib/ek

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

install:
	#$(STACK) install
	mkdir -p $(STDLIB_INSTALL_PATH)/$(STDLIB)
	$(CP) ./$(STDLIB)/* $(STDLIB_INSTALL_PATH)/$(STDLIB)

.PHONY: all clean fclean re tests_run install
