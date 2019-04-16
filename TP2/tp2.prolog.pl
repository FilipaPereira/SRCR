%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Base de Conhecimento com informacao sobre cuidados de saude.

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
:- dynamic prestador/4.
:- dynamic cuidado/5.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utente: IDUt,Nome,Idade,Morada -> {V,F,D}

utente(1,'Ana Santos',34,'Rua São Gonçalo - S.Vicente - Braga').
utente(2,'Bruno Mendonça',23,'Rua de Santa Catarina - Santo Ildefonso e Bonfim - Porto').
utente(3,'Carla Martins',29,'Rua de Santa Maria - Oliveira do Castelo - Guimarães').
utente(4,'Beatriz Jesus',12,'Rua da Bica de Duarte Belo - São Paulo -Lisboa').
utente(5,'Júlio Carvalho',36,'Rua Santa Margarida - S.Victor - Braga').
utente(6,'Carlos Silva',19,'Rua das Flores - Sé e Vitória - Porto').
utente(7,'Xavier Teixeira',51,'Rua dos Chãos - S.João do Souto - Braga').
utente(8,'Luis Almeida',32,'Rua Padre Feliciano - Fraião - Braga').
utente(9,'Pedro Lima',20,'Rua Prof. Machado Vilela - S.Victor - Braga').
utente(10,'André Campos',42,'Rua da Graça - Graça -Lisboa').
utente(11,'Teresa Cerqueira',54,'Rua Dom João I - Creixomil - Guimarães').
utente(12,'Sérgio Gonçalves',16,'Rua de Quebra Costas - Almedina - Coimbra').
utente(13,'João Pereira',24,'Rua do Souto - Sé - Braga').
utente(14,'Henrique Castro',83,'Rua da Cedofeita - Vitória e Cedofeita - Porto').
utente(15,'Sara Fernandes',27,'Rua Garret - Santa Maria Maior - Lisboa').


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado prestador: IDPrest,Nome,Especialidade,Instituicao -> {V,F,D}

prestador(1,'José Moreira',['Dermatologia','Ortopedia'],['Hospital S.José']).
prestador(2,'Cristina Félix',['Psiquiatria','Otorrinolaringologia'],['Hospital S.José','Hospital da Luz']).
prestador(3,'Helena Pereira',['Oftalmologia','Dermatologia'],['Centro Hospitalar e Universitário de Coimbra']).
prestador(4,'Rodrigo Vieira',['Cardiologia'],['Hospital da Senhora da Oliveira Guimarães']).
prestador(5,'Vitória Pinto',['Ginecologia'],['Hospital da Senhora da Oliveira Guimarães','Centro de Saúde de Maximinos']).
prestador(6,'Mariana Sousa',['Oftalmologia','Ortopedia'],['Centro de Saúde de Maximinos','Hospital de Braga']).
prestador(7,'Susana Costa',['Cardiologia'],['Hospital de Braga']).
prestador(8,'Guilherme Cruz',['Otorrinolaringologia'],['Hospital de Braga']).
prestador(9,'Sofia Lopes',['Dermatologia','Oftalmologia'],['Hospital de Santa Maria']).
prestador(10,'Manuel Marques',['Ortopedia'],['Hospital S.João']).
prestador(11,'Adriana Oliveira',['Cardiologia'],['Hospital S.João']).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cuidado: Data, IDUt,IDPrest,Descricao,Custo -> {V,F,D}

cuidado('10-11-2018',1,5,'Ginecologia',30).
cuidado('18-03-2018',2,10,'Ortopedia',47).
cuidado('09-02-2019',3,5,'Ginecologia',30).
cuidado('28-01-2019',4,2,'Psiquiatria',19).
cuidado('09-02-2019',3,5,'Ginecologia',30).
cuidado('16-09-2018',5,7,'Cardiologia',35).
cuidado('11-04-2018',6,9,'Oftalmologia',22).
cuidado('23-05-2018',7,6,'Oftalmologia',25).
cuidado('29-01-2019',8,7,'Cardiologia',35).
cuidado('11-04-2018',9,8,'Otorrinolaringologia',15).
cuidado('16-09-2018',10,1,'Dermatologia',47).
cuidado('10-12-2018',11,4,'Cardiologia',30).
cuidado('03-11-2018',12,3,'Oftalmologia',25).
cuidado('15-03-2018',13,6,'Oftalmologia',20).
cuidado('25-09-2018',14,11,'Cardiologia',35).
cuidado('12-10-2018',15,1,'Dermatologia',47).
cuidado('18-07-2018',2,9,'Dermatologia',50).
cuidado('02-03-2019',11,4,'Cardiologia',30).
cuidado('28-09-2018',3,4,'Cardiologia',25).
cuidado('12-01-2019',15,2,'Otorrinolaringologia',25).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado servico: IDServ,Descricao,Instituicao,Cidade -> {V,F,D}

servico(1,'Oftalmologia','Hospital de Braga','Braga').
servico(2,'Ginecologia','Centro de Saúde de Maximinos','Braga').
servico(3,'Cardiologia','Hospital da Senhora da Oliveira Guimarães','Guimarães').
servico(4,'Ortopedia','Hospital S.João','Porto').
servico(5,'Oftalmologia','Centro de Saúde de Maximinos','Braga').
servico(6,'Cardiologia','Hospital de Braga','Braga').
servico(7,'Otorrinolaringologia','Hospital de Braga','Braga').
servico(8,'Oftalmologia','Centro Hospitalar e Universitário de Coimbra','Coimbra').
servico(9,'Cardiologia','Hospital S.João','Porto').
servico(10,'Oftalmologia','Hospital de Santa Maria','Porto').
servico(11,'Dermatologia','Hospital S.José' ,'Lisboa').
servico(12,'Otorrinolaringologia','Hospital S.José' ,'Lisboa').
servico(13,'Ginecologia','Hospital da Senhora da Oliveira Guimarães','Guimarães').
servico(14,'Psiquiatria','Hospital da Luz','Lisboa').
servico(15,'Dermatologia','Hospital de Santa Maria','Porto').


%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Extensao do meta-predicado demo: Questao,Resposta -> {V, F, D}

demo( Questao,verdadeiro ) :- Questao.
demo( Questao, falso ) :- -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).

% Extensao do meta-predicado nao: Questao -> {V, F, D}

nao( Q ) :- Q, !, fail.
nao( Q ).

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



% ------------------------------ INVARIANTES -------------------------------%

solucoes(X,Y,Z) :- findall(X,Y,Z).

% Extensao do predicado comprimento: Lista, Resultado -> {V,F}
comprimento( [],0 ).
comprimento( [H | T],R ) :- comprimento( T,S ), R is S+1.


% O ID do utente é único
+utente( ID,NO,I,C ) :: (solucoes( ID,utente(ID,_,_,_),S ),
                        comprimento( S,N ), N == 1).

% Não é possível remover um utente se ele não existir
-utente( ID,NO,I,C ) :: (solucoes( ID,utente(ID,_,_,_),S ),
                         comprimento( S,N ), N == 1).

% O ID do prestador é único
+prestador( ID,N,E,I ) :: (solucoes( ID,(prestador(ID,_,_,_)),S ),
                      comprimento( S,N ), N == 1).

% Não é possível remover um prestador se ele não existir
-prestador( ID,N,E,I ) :: (solucoes( ID,(prestador(ID,_,_,_)),S ),
                      comprimento( S,N ), N == 1).

% Cada cuidado é relativo a um utente, prestador e serviço existentes
+cuidado( ID,IDU,IDP,D,C ) ::
                            (utente(IDU,_,_,_),
                            prestador(IDP,_,_,_)),
                            servico(_,D,_,_),.

% Só pode ser inserido um cuidado se existirem o médico e o serviço a si associados
+cuidado( ID,IDU,IDP,D,C ) ::
                            (solucoes( (IDP,E),(prestador(IDP,_,E,_)),S ),
                             comprimento( S,N ), N >= 1).

% Não é possível remover um cuidado se ele não existir
-cuidado( ID,IDU,IDP,D,C ) ::
                            (solucoes( (ID,IDU,D),(cuidados(ID,IDU,_,D,_)),S ),
                            comprimento( S,N ), N == 1).

% Não é possível remover um utente se houver cuidados em seu nome
-utente( ID,NO,I,C ) :: (solucoes( ID,cuidado(_,ID,_,_,_),S ),
                        comprimento( S,N ), N == 0).

% Não é possível remover um serviço se houver cuidados relativos a si
-servico( ID,D,I,C ) :: (solucoes( ID,consulta(_,_,ID,_,_),S ),
                        comprimento( S,N ), N == 0).

% Não é possível remover um medico se houver cuidados relativos a si
-prestador( ID,N,E,I ) :: (solucoes( ID,cuidado(_,_,ID,_,_),S ),
                        comprimento( S,N ), N == 0).



% ------------------------------ CONHECIMENTO NEGATIVO -------------------------------%

-utente(IDUt, Nome, Idade, Morada) :-
                nao(utente(IDUt, Nome, Idade, Morada)),
                nao(excecao(utente(IDUt, Nome, Idade, Morada))).

-cuidado(Data, IDUt, IDP, Desc, Custo) :-
                nao(cuidado(Data, IDUt, IDP, Desc, Custo)),
                nao(excecao(cuidado(Data, IDUt, IDP, Desc, Custo))).

-prestador(IDP, Nome, Esp, Inst) :-
                nao(prestador(IDP, Nome, Esp, Inst)),
                nao(excecao(prestador(IDP, Nome, Esp, Inst))).


%----------------------------- EVOLUÇÃO DE CONHECIMENTO -------------------------------%

% Extensao do predicao registar_utente: IdUtente, Nome, Idade, Morada -> {V, F}

registar_utente(X,Y,W,Z) :- evolucao(utente(X,Y,W,Z)).


% Extensao do predicao remover_utente: IdUtente, Nome, Idade, Morada -> {V, F}

remover_utente(X,Y,W,Z) :- involucao(utente(X,Y,W,Z)).


% Extensao do predicao registar_cuidado: Data, IDUtente, IDPrestador, Descricao, Custo -> {V, F}

registar_cuidado(V,X,Y,W,Z) :- evolucao(cuidado(V,X,Y,W,Z)).


% Extensao do predicao remover_cuidado: Data, IDUtente, IDPrestador, Descricao, Custo -> {V, F}

remover_cuidado(V,X,Y,W,Z) :- involucao(cuidado(V,X,Y,W,Z)).


% Extensao do predicao registar_prestador: IDPrestador, Nome, Especialidade, Instituicao -> {V, F}

registar_prestador(X,Y,W,Z) :- evolucao(prestador(X,Y,W,Z)).


% Extensao do predicao remover_prestador: IDPrestador, Nome, Especialidade, Instituicao -> {V, F}

remover_prestador(X,Y,W,Z) :- involucao(prestador(X,Y,W,Z)).


   
%-------------------------- AUXILIARES ----------------------------------
% Extensao do predicado pertence: Elemento,Lista -> {V,F}

pertence(X,[H | T]) :- X == H.
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