% vim: set ft=prolog:

% Neste arquivo estão especificados os predicados que devem ser implementados.
% Você pode criar predicados auxiliares quando necessário.
%
% No arquivo resame_testes.pl estão os testes para alguns predicados.
%
% Para implementar cada predicado, primeiro você deve ler e entender a
% especificação e o teste.
%
% A especificação dos parâmetros dos predicados segue o formato descrito em
% http://www.swi-prolog.org/pldoc/doc_for?object=section%282,%274.1%27,swi%28%27/doc/Manual/preddesc.html%27%29%29
%
% Um Jogo same é representado por uma lista de colunas, sem os elementos nulos
% (zeros).
% Por exemplo, o jogo
% 2 | 3 0 0 0
% 1 | 2 2 2 0
% 0 | 2 3 3 1
% --+--------
%   | 0 1 2 3
% é representado como [[2, 2, 3], [3, 2], [3, 2], [1]].
% O tamanho deste jogo é 3x4 (linhas x colunas).
%
% Uma posição no jogo é representado por uma estrutura pos com dois argumentos
% (lin, col), onde lin é o número da linha e col é o número da coluna.  No
% exemplo anterior, a posição pos(0, 1) tem cor 3, e a posição pos(1, 2) tem
% cor 2.

% Você pode utilizar os predicados definidos no arquivo resame_utils.pl
:- consult(resame_utils).

%% main(+File) is det
%
%  Carrega um jogo same do arquivo File e imprime uma resolução na saída padrão
%  ou sem-solucao se o jogo não tem solução.

main(File) :-
    read_matrix_file(File, M),
    transpose(M, Same),
    solve(Same, Moves).

%% solve(+Same, -Moves) is nondet
%
%  Verdadeiro se Moves é uma sequência de jogadas (lista de posições) que
%  quando realizadas ("clicadas") resolvem o jogo Same.
%  Este predicado não tem teste de unidade. Ele é testado pelo testador.


solve([], []).
solve(Same, [M|Moves]) :-
    group(Same, Group),
    writeln(Group).
    %remove_group(Same, Group, NewSame),
    %[M|_] = Group,
    %solve(NewSame, Moves).

%% group(+Same, ?Group) is nondet
%
%  Verdadeiro se Group é um grupo de Same. Group é uma lista de posições
%  (estrutura pos(lin,col)). Este predicado é não determinístico e deve ser
%  capaz de gerar todos os grupos de Same. Este predicado não deve gerar grupos
%  repetidos. Este predicado e group/3 para vão utilizar os mesmos precicados
%  auxiliares.

group(Same, Group) :-
    get_same_size(Same, ColSize, LinSize),
    group(Same, pos(1, 2), Group).

%% grupo(+Same, +P, -Group) is semidet
%
%  Verdadeiro se Group é um grupo de Same que contém a posição P.

group(Same, P, Group) :-
    get_color(Same, P, Color),
    Candidates = [P],
    create_group(Candidates, Same, Color, [], Group).

get_color(Same, P, Color) :-
    pos(Lin, Col) = P,
    nth0(Col, Same, X),
    nth0(Lin, X, Color).

create_group([], _, _, TempGroup, Group) :-
    length(TempGroup, X),
    X > 1, 
    Group = TempGroup.

create_group([P | Tail], Same, Color, TempGroup, Group) :-
    is_valid(Same, P),
    same_color(Same, P, Color),
    not(member(P, TempGroup)),
    append(TempGroup, [P], NewGroup),
    get_neighbors(P, Neighbors),
    append(Tail, Neighbors, Candidates),
    create_group(Candidates, Same, Color, NewGroup, Group), !;
    create_group(Tail, Same, Color, TempGroup, Group).

get_neighbors(P, Neighbors) :-
    pos(Lin, Col) = P,
    NewLinPlus is Lin + 1,
    NewColPlus is Col + 1,
    NewLinMinus is Lin - 1,
    NewColMinus is Col - 1,
    Neighbors = [pos(NewLinPlus, Col), pos(Lin, NewColPlus),
        pos(NewLinMinus, Col), pos(Lin, NewColMinus)].

same_color(Same, P, Color) :-
    get_color(Same, P, NewColor),
    NewColor =:= Color.

is_valid(Same, P) :-
    pos(Lin, Col) = P,
    get_same_size(Same, ColSize, LineSize),
    Col >= 0,
    Col < ColSize,
    Lin >= 0,
    Lin < LineSize.

get_same_size(Same, ColSize, LinSize) :-
    [Lines | _] = Same,
    length(Lines, LinSize),
    length(Same, ColSize).

%% remove_group(+Same, +Group, -NewSame) is semidet
%
%  Verdadeiro se NewSame é obtido de Same removendo os elemento especificados
%  em Group. A remoção é feita de acordo com as regras do jogo same.
%  Dica:
%    - crie um predicado auxiliar remove_column_group, que remove os elementos
%    de uma coluna específica
remove_group(Same, Group, NewSame) :-
    writeln([Same, Group, NewSame]), fail.
