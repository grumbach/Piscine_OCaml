# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2018/05/31 15:22:28 by agrumbac          #+#    #+#              #
#    Updated: 2018/05/31 21:07:38 by agrumbac         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

NAME = gardening

SOURCES = gardening.ml

CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep


LIBS= $(WITHGRAPICS)
WITHGRAPICS = graphics.cma -cclib -lGraphics

all: .depend $(NAME)

$(NAME): opt byt
	ln -s $(NAME).byt $(NAME)

opt: $(NAME).opt
byt: $(NAME).byt

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)

$(NAME).byt: $(OBJS)
	$(CAMLC) -o $(NAME).byt $(LIBS) $(OBJS)

$(NAME).opt: $(OPTOBJS)
	$(CAMLOPT) -o $(NAME).opt $(LIBS:.cma=.cmxa) $(OPTOBJS)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

clean:
	rm -f *.cm[iox]
	rm -f $(NAME).o

fclean: clean
	rm -f $(NAME)
	rm -f $(NAME).opt
	rm -f $(NAME).byt

.depend:
	$(CAMLDEP) $(SOURCES) > .depend

re: fclean all

include .depend
