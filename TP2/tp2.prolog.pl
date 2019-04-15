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
:- dynamic medico/4.
:- dynamic consulta/5.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utente: IDUt,Nome,Idade,Cidade -> {V,F,D}
% Extensao do predicado prestador: IDPrest,Nome,Especialidade,Instituicao -> {V,F,D}
% Extensao do predicado cuidado: Data, IDUt,IDPrest,Descricao,Custo -> {V,F,D}
% Extensao do predicado medico: IDMed,Nome,Especialidades,Instituicoes -> {V,F,D}

% Extensao do predicado consulta: Data, IDUt,IDServ,Custo,IDMed -> {V,F,D}
% Extensao do predicado servico: IDServ,Descricao,Instituicao,Cidade -> {V,F,D}
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


%-----------CONHECIMENTO NEGATIVO

utente(IDUt, Nome, Idade, Cidade) :- nao(utente(IDUt, Nome, Idade, Cidade)), 
                                      nao(excecao(utente(IDUt, Nome, Idade, Cidade))).

servico(IDServ,Descricao,Instituicao,Cidade) :- nao(servico(IDServ,Descricao,Instituicao,Cidade)),
                                                nao(excecao(servico(IDServ,Descricao,Instituicao,Cidade))).

consulta(Data, IDUt,IDServ,Custo,IDMed) :- nao(consulta(Data, IDUt,IDServ,Custo,IDMed)),
                                            nao(excecao(servico(Data, IDUt,IDServ,Custo,IDMed))).

medico(IDMed,Nome,Especialidades,Instituicoes) :- nao(medico(IDMed,Nome,Especialidades,Instituicoes)),
                                                  nao(excecao(medico(IDMed,Nome,Especialidades,Instituicoes))).      