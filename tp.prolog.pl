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



%-------------------------------------------------------------
% Povoamento


utente(1,'Ana',20,'Braga').
utente(2,'Bruno',19,'Braga').
utente(3,'Beatriz',25,'Porto').
utente(4,'Carla',23,'Braga').
utente(5,'João',23,'Braga').

servico(1,'Oftalmologia','Hospital de Braga','Braga').
servico(2,'Ginecologia','Centro de Saúde de Maximinos','Braga').
servico(3,'Cardiologia','Hospital de Braga','Braga').
servico(4,'Ortopedia','Hospital S.João','Porto').
servico(5,'Oftalmologia','Hospital de Braga','Braga').

consulta('10-11-2018',4,2,25,joao,naosei).
consulta('18-03-2018',3,4,43,joao,naosei).
consulta('09-02-2019',5,5,30,joao,naosei).
consulta('28-01-2019',2,3,20,joao,naosei).
consulta('16-09-2018',2,1,35,joao,naosei).
consulta('11-04-2018',1,2,22,joao,naosei).
consulta('13-04-2018',1,2,25,joao,naosei).



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

% Extensao do predicado identificar_utente: #IDUt,Nome,Idade,Cidade -> {V,F}

identificar_utenteID(ID, R) :- solucoes((ID, N, I, C), utente(ID, N, I, C), R).
identificar_utenteNome(NOME, R) :- solucoes((ID, NOME, I, C), utente(ID, NOME, I, C), R).
identificar_utenteIdade(IDADE, R) :- solucoes((ID, N, IDADE, C), utente(ID, N, IDADE, C), R).
identificar_utenteCidade(CIDADE, R) :- solucoes((ID, N, I, CIDADE), utente(ID, N, I, CIDADE), R).

% Extensao do predicado identificar_servico: #IDServ,Descricao,Instituicao,Cidade -> {V,F}

identificar_servicoID(ID, R) :- solucoes((ID, D, I, C), servico(ID, D, I, C), R).
identificar_servicoDescricao(DESC, R) :- solucoes((ID, DESC, I, C), servico(ID, DESC, I, C), R).
identificar_servicoInstituicao(INST, R) :- solucoes((ID, D, INST, C), servico(ID, D, INST, C), R).
identificar_servicoCidade(CITY, R) :- solucoes((ID, D, I, CITY), servico(ID, D, I, CITY), R).

% Extensao do predicado identificar_consulta: Data, #IdUt, #IdServ, Custo
identificar_consultaData(DATE, R) :- solucoes((D, Idutente, Idservico, Custo), consulta(DATE, Idutente, Idservico, Custo), R).
identificar_consultaIDUtente(IDU, R) :- solucoes((D, IDU, Idservico, Custo), consulta(D, IDU, Idservico, Custo), R).
identificar_consultaIDServico(IDS, R) :- solucoes((D, Idutente, IDS, Custo), consulta(D, Idutente, IDS, Custo), R).
identificar_consultaCusto(CUSTO, R) :- solucoes((D, Idutente, Idservico, CUSTO), consulta(D, Idutente, Idservico, CUSTO), R).

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

% Extensao do predicado utentesServico: Servico, Lista_de_Utentes -> {V,F}

utentesServico(I,S):- solucoes(IDU,consulta(_,IDU,I,_,_,_),W),removeRepetidos(W,U),encontrarUtentes(U,S).

% Extensao do predicado utentesInstituicao: Instituicao,Lista_de_Utentes -> {V,F}

utentesInstituicao(I,R) :- solucoes(IDS,servico(IDS,_,I,_),S),servicosUtentes(S,R).

%--- Auxiliar da 6

% Extensao do predicado encontrarUtentes: Lista_de_id_utentes,Lista_de_Utentes -> {V,F}

encontrarUtentes([],[]).
encontrarUtentes([H|T],R) :- identificar_utenteID(H,W),encontrarUtentes(T,S),concatenar(S,W,R).

% Extensao do predicado servicosUtentes: Lista_de_servicos,Lista_de_Utentes -> {V,F}

servicosUtentes([],[]).
servicosUtentes([H|T],R) :- utentesServico(H,W),servicosUtentes(T,S),concatenar(W,S,Z),removeRepetidos(Z,R).


%------------------------7--------------------------

% Extensao do predicado servicosUtente: IDU,Lista_De_Servicos ->{V,F}

servicosUtente([],[]).
servicosUtente(IDU,LR) :- solucoes(IDS,consulta(_,IDU,IDS,_),W),removeRepetidos(W,U),encontrarServicoU(U,LR).

%--- auxiliar da 7
encontrarServicoU([],[]).
encontrarServicoU([H|T],R) :- solucoes((H,N,I,M),servico(H,N,I,M),W),encontrarServicoU(T,S),concatenar(S,W,R).

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

