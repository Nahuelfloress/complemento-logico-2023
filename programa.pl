%persona(Apodo, Edad, Peculiaridades).
persona(ale, 15, [claustrofobia, cuentasRapidas, amorPorLosPerros]).
persona(agus, 25, [lecturaVeloz, ojoObservador, minuciosidad]).
persona(fran, 30, [fanDeLosComics]).
persona(rolo, 12, []).
persona(personaDeEjemplo,4,[]).

%esSalaDe(NombreSala, Empresa).
esSalaDe(elPayasoExorcista, salSiPuedes).
esSalaDe(socorro, salSiPuedes).
esSalaDe(linternas, elLaberintoso).
esSalaDe(guerrasEstelares, escapepepe).
esSalaDe(fundacionDelMulo, escapepepe).
esSalaDe(prueba,ejemplo).
esSalaDe(otra,ejemplo).
esSalaDe(a,ejemplo).





%terrorifica(CantidadDeSustos, EdadMinima).
%familiar(Tematica, CantidadDeHabitaciones).
%enigmatica(Candados).

%sala(Nombre, Experiencia).
sala(elPayasoExorcista, terrorifica(100, 18)).
sala(socorro, terrorifica(20, 12)).
sala(linternas, familiar(comics, 5)).
sala(guerrasEstelares, familiar(futurista, 7)).
sala(fundacionDelMulo, enigmatica([combinacionAlfanumerica, deLlave, deBoton])).
sala(prueba,terrorifica(1,2)).
sala(otra,terrorifica(1,2)).
sala(a,familiar(1,1)).




empresa(Empresa):-
    distinct(esSalaDe(_,Empresa)).
    
% 1)

dificultadSegunExperiencia(terrorifica(CantidadDeSustos,EdadMinima),Nivel):-
    sala(_,terrorifica(CantidadDeSustos,EdadMinima)),
    Nivel is abs(CantidadDeSustos - EdadMinima).

dificultadSegunExperiencia(familiar(Tematica,CantidadDeHabitaciones),Nivel):-
    sala(_,familiar(Tematica,CantidadDeHabitaciones)),
    Nivel = CantidadDeHabitaciones,
    Tematica \= futurista.

dificultadSegunExperiencia(familiar(futurista,CantidadDeHabitaciones),15):-
    sala(_,familiar(futurista,CantidadDeHabitaciones)).


dificultadSegunExperiencia(enigmatica(Candados),Nivel):-
    sala(_,enigmatica(Candados)),
    length(Candados, Nivel).

nivelDeDificultadDeLaSala(Sala,Nivel):-
    sala(Sala,Tipo),
    dificultadSegunExperiencia(Tipo,Nivel).


% 4)

%esMacabra(Empresa):-
 %   esSalaDe(_,Empresa),    
  %  forall(esSalaDe(Sala,Empresa),sala(Sala,terrorifica(_,_))).
  

esBuena(Empresa):-
    esSalaDe(Sala,Empresa),
    not((esSalaDe(Sala,Empresa),sala(Sala,terrorifica(_,_)))).


esMacabra(Empresa):-
    esSalaDe(_,Empresa),
    not(esBuena(Empresa)).


    
