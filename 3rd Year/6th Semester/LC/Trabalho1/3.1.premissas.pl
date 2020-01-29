%premissas
morre(X) :- animal(X).
animal(X).
come(X,'260b7cd6-08eb-44bc-a9c2-c374778a8669'(X)):-gato(X),rato('260b7cd6-08eb-44bc-a9c2-c374778a8669'(X)).
gato(tareco).
gato(farrusco).
cao(pluto).
mia(X) :- gato(X).
arranha(X) :- gato(X).
morde(X) :- cao(X), mau(X).
morde(X) :- cao(X), mauHumor(X).