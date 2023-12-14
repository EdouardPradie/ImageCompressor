##
## EPITECH PROJECT, 2021
## Makefile
## File description:
## Wolfram
##

OBJ		=	$(shell stack path --local-install-root)

NAME	=	imageCompressor


all:	$(NAME)

$(NAME):
	stack build
	cp $(OBJ)/bin/compressor-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)
	stack purge

re:	fclean all

.PHONY: re fclean clean all
