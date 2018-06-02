# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2018/05/31 15:22:28 by agrumbac          #+#    #+#              #
#    Updated: 2018/06/02 18:01:13 by agrumbac         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

NAME = toc

SOURCES = \
	srcs/Board.ml \
	srcs/UserInterface.ml \
	srcs/main.ml

CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep

all: .depend $(NAME)

$(NAME): opt byt
	ln -fs $(NAME).byt $(NAME)

opt: $(NAME).opt
byt: $(NAME).byt

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)

$(NAME).byt: $(OBJS)
	$(CAMLC) -o $(NAME).byt $(OBJS)

$(NAME).opt: $(OPTOBJS)
	$(CAMLOPT) -o $(NAME).opt $(OPTOBJS)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

clean:
	rm -f srcs/*.cm[iox]
	rm -f srcs/*.o

fclean: clean
	rm -f $(NAME)
	rm -f $(NAME).opt
	rm -f $(NAME).byt

.depend:
	$(CAMLDEP) $(SOURCES) > .depend

re: fclean all

include .depend