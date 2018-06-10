# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2018/05/31 15:22:28 by agrumbac          #+#    #+#              #
#    Updated: 2018/06/09 23:54:37 by mgrimald         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

NAME = tama

SOURCES = tryHard.ml tama.ml view.ml  main.ml
#view_gui.ml
CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep

#INCLUDES = -I /Users/$(USER)/.opam/system/lib/lablgtk2

LIBS= $(WITHGRAPICS) $(WITHUNIX) 
#$(WITHGTK)
WITHGRAPICS = graphics.cma -cclib -lGraphics
WITHUNIX = unix.cma
WITHGTK = lablgtk.cma

all: .depend $(NAME)

$(NAME): opt byt
	ln -s $(NAME).byt $(NAME)

opt: $(NAME).opt
byt: $(NAME).byt

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)

$(NAME).byt: $(OBJS)
	$(CAMLC) $(INCLUDES) -o $(NAME).byt $(LIBS) $(OBJS)

$(NAME).opt: $(OPTOBJS)
	$(CAMLOPT) $(INCLUDES) -o $(NAME).opt $(LIBS:.cma=.cmxa) $(OPTOBJS)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(INCLUDES) -c $<

.mli.cmi:
	$(CAMLC) $(INCLUDES) -c $<

.ml.cmx:
	$(CAMLOPT) $(INCLUDES) -c $<

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
