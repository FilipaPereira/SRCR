%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Base de Conhecimento com informacao genealogica.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
%:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic utente/4.
:- dynamic servico/4.
:- dynamic medico/3.
:- dynamic consulta/6.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes

solucoes(X,Y,Z) :- findall(X,Y,Z).

comprimento( [],0 ).
comprimento( [H | T],R ) :- comprimento( T,S ), R is S+1.

+utente( ID,NO,I,C ) :: (solucoes( ID,utente(ID,_,_,_),S ),
                        comprimento( S,N ), N == 1).

-utente( ID,NO,I,C ) :: (solucoes( ID,utente(ID,_,_,_),S ),
                         comprimento( S,N ), N == 1).

+servico( ID,D,I,C ) :: (solucoes( ID,servico(ID,_,_,_),S ),
                         comprimento( S,N ), N == 1).
-servico( ID,D,I,C ) :: (solucoes( ID,servico(ID,_,_,_),S ),
                         comprimento( S,N ), N == 1).

+medico( ID,E,I ) :: (solucoes( ID,(medico(ID,_,_)),S ),
                      comprimento( S,N ), N == 1).
-medico( ID,E,I ) :: (solucoes( ID,(medico(ID,_,_)),S ),
                      comprimento( S,N ), N == 1).

+consulta( D,IDU,IDS,C,IDM,E ) ::
                            (utente(IDU,_,_,_),
                            servico(IDS,_,_,_),
                            medico(IDM,_,_)).

+consulta( D,IDU,IDS,C,IDM,E ) ::
                            (solucoes( (IDM,E),(medico(IDM,E,_)),S ),
                             comprimento( S,N ), N >= 1).

%----------------------AUXILIARES----------------------------------
% Extensao do predicado pertence: Elemento,Lista -> {V,F}

pertence(X,[H | T]) :- X = H.
pertence(X,[H | T]) :- pertence(X,T),X \= H.

% Extensao do predicado apagaT: Elemento,ListaInicial,ListaFinal -> {V,F}

apagaT(_,[],[]).
apagaT(X,[H | T], [H | L]) :- X \= H, apagaT(X,T,L).
apagaT(X,[H | T], L) :- apagaT(X,T,L).

% Extensao do predicado removeRepetidos: ListaInicial,ListaFinal -> {V,F}

removeRepetidos([],[]).
removeRepetidos([H | T],R) :- pertence(H,T), apagaT(H,T,S), removeRepetidos(S,B), R = [H|B].
removeRepetidos([H | T],R) :- not(pertence(H,T)),removeRepetidos(T,S), R = [H|S].

% Extensao do predicado somaLista: ListaNum,Resultado -> {V,F}

somaLista( [],0 ).
somaLista( [H | T],R ) :- somaLista( T,S ), R is H+S.

% Extensao do predicado concatenar: ListaX,ListaY,ListaFinal -> {V,F}

concatenar([],L,L).
concatenar(L,[],L).
concatenar([H | T], L, [H | R]) :- concatenar(T,L,R). 

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolucao do conhecimento

evolucao( T ) :- solucoes( I,+T::I,L ),
                    insercao( T ), teste( L ).

involucao( T ) :- solucoes( I,-T::I,L ),
                teste( L ), remocao( T ).

remocao( T ) :- retract( T ).

insercao( T ) :- assert( T ).
insercao( T ) :- retract( T ),!,fail.

teste( [] ).
teste( [R|LR] ) :- R, teste( LR ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utente: IDUt,Nome,Idade,Cidade -> {V,F}
% Extensao do predicado servico: IDServ,Descricao,Instituicao,Cidade -> {V,F}
% Extensao do predicado consulta: Data, IDUt,IDServ,Custo,IDMed,Especialidade -> {V,F}
% Extensao do predicado medico: IDMed,Especialidades,Instituicoes -> {V,F}

%------------------------1--------------------------
registar_utente(X,Y,W,Z) :- evolucao(utente(X,Y,W,Z)).
registar_servico(X,Y,W,Z) :- evolucao(servico(X,Y,W,Z)).
registar_consulta(X,Y,W,Z,H,L) :- evolucao(consulta(X,Y,W,Z,H,L)).
registar_medico(X,Y,W) :- evolucao(medico(X,Y,W)).

%------------------------2--------------------------
remover_utente(X,Y,W,Z) :- involucao(utente(X,Y,W,Z)).
remover_servico(X,Y,W,Z) :- involucao(servico(X,Y,W,Z)).
remover_consulta(X,Y,W,Z,H,L) :- involucao(consulta(X,Y,W,Z,H,L)).
remover_medico(X,Y,W) :- involucao(medico(X,Y,W)).

%------------------------3--------------------------
% Extensao do predicado identificaInstituicoes: Instituicoes -> {V,F}

identificaInstituicoes(Lista) :- solucoes(I,servico(_,_,I,_),L),removeRepetidos(L,Lista).


%------------------------4--------------------------


%------------------------5--------------------------
% Extensao do predicado servicosInstituicao: Instituicao, ListaServicos -> {V,F}

servicosInstituicao(I,Lista) :- solucoes(S,servico(_,S,I,_),R),removeRepetidos(R,Lista).

% Extensao do predicado servicosCidade: Cidade, ListaServicos -> {V,F}

servicosCidade(C,Lista) :- solucoes(S,servico(_,S,_,C),R), removeRepetidos(R,Lista).

% Extensao do predicado servicosData: Data, ListaServicos -> {V,F}

servicosData(D,Lista) :- solucoes(IDServ,consulta(D,_,IDServ,_,_,_),R), descServicos(R,T),removeRepetidos(T,Lista).

% Extensao do predicado servicosCusto: Custo, ListaServicos -> {V,F}

servicosCusto(C,Lista) :- solucoes(IDServ,consulta(_,_,IDServ,C,_,_),L), descServicos(L,T), removeRepetidos(T,Lista). 

%--- Auxiliar da 5 
% Extensao do predicado descServicos: ListaIds, ListaServicos -> {V,F}

descServicos([],[]).
descServicos([H | T],Lista) :- solucoes(S,servico(H,S,_,_),L), descServicos(T,R),concatenar(L,R,Lista).

%------------------------6--------------------------

%------------------------7--------------------------

%------------------------8--------------------------
% Extensao do predicado custoUtente: IDUt,Custo -> {V,F}

custoUtente(ID,R) :- solucoes(C,consulta(_,ID,_,C,_,_),L), somaLista(L,R).

% Extensao do predicado custoServico: IDServ,Custo -> {V,F}

custoServico(ID,R) :- solucoes(C,consulta(_,_,ID,C,_,_),L), somaLista(L,R).

% Extensao do predicado custoData: Data,Custo -> {V,F}

custoData(Data,R) :- solucoes(C,consulta(Data,_,_,C,_,_),L), somaLista(L,R).

% Extensao do predicado custoInstituicao: Instituicao,Custo -> {V,F}

custoInstituicao(I,R) :- solucoes(IDServ,servico(IDServ,_,I,_),Lista), custoListaServ(Lista,R).

%--- Auxiliar da 8
% Extensao do predicado custoListaServ: ListaServicos,Custo -> {V,F}

custoListaServ([],0).
custoListaServ([H | T],Res) :- custoServico(H,S), custoListaServ(T,B), Res is S+B.

