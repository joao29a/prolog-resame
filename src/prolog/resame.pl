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
    solve(Same, Moves),
    writeln(Moves).

%% solve(+Same, -Moves) is nondet
%
%  Verdadeiro se Moves é uma sequência de jogadas (lista de posições) que
%  quando realizadas ("clicadas") resolvem o jogo Same.
%  Este predicado não tem teste de unidade. Ele é testado pelo testador.


solve([], []).
solve(Same, [M|Moves]) :-
    group(Same, Group),
    remove_group(Same, Group, NewSame),
    [M|_] = Group,
    solve(NewSame, Moves).

%% group(+Same, ?Group) is nondet
%
%  Verdadeiro se Group é um grupo de Same. Group é uma lista de posições
%  (estrutura pos(lin,col)). Este predicado é não determinístico e deve ser
%  capaz de gerar todos os grupos de Same. Este predicado não deve gerar grupos
%  repetidos. Este predicado e group/3 para vão utilizar os mesmos precicados
%  auxiliares.

group(Same, Group) :-
    length(Same, ColSize), get_same_lin_size(Same, 0, LinSize),
    get_all_pos(Same, 0, 0, ColSize, LinSize, [], ListPos),
    make_groups(Same, ListPos, [], _, Group).

get_all_pos(Same, Col, Lin, ColSize, LinSize, TempList, ListPos) :-
    Lin < LinSize, append(TempList, [pos(Lin, Col)], NewListPos), 
    NewLin is Lin + 1, 
    get_all_pos(Same, Col, NewLin, ColSize, LinSize, NewListPos, ListPos), !.

get_all_pos(Same, Col, Lin, ColSize, Lin, TempList, ListPos) :-
    NewCol is Col + 1, NewCol < ColSize,
    get_same_lin_size(Same, NewCol, LinSize),
    get_all_pos(Same, NewCol, 0, ColSize, LinSize, TempList, ListPos).

get_all_pos(_, _, _, _, _, TempList, TempList).

make_groups(_, _, _, NewGroup, Group) :-
    nonvar(NewGroup),
    Group = NewGroup.

make_groups(Same, [P | Tail], TempGroups, _, Group) :-
    group(Same, P, NewTempGroup),
    not_member_group(TempGroups, NewTempGroup),
    append(TempGroups, [NewTempGroup], NewTempGroups),
    make_groups(Same, Tail, NewTempGroups, NewTempGroup, Group).

make_groups(Same, [_ | Tail], TempGroups, _, Group) :-
    make_groups(Same, Tail, TempGroups, _, Group).

make_groups(_, [], _, _, _) :- fail.

not_member_group([Group | Tail], NewGroup) :-
    [P | _] = NewGroup,
    not(member(P, Group)),
    not_member_group(Tail, NewGroup).

not_member_group([], _).

%% grupo(+Same, +P, -Group) is semidet
%
%  Verdadeiro se Group é um grupo de Same que contém a posição P.

group(Same, P, Group) :-
    get_color(Same, P, Color), Candidates = [P],
    create_group(Candidates, Same, Color, [], Group).

get_color(Same, P, Color) :-
    pos(Lin, Col) = P, nth0(Col, Same, X), nth0(Lin, X, Color).

create_group([], _, _, TempGroup, Group) :-
    length(TempGroup, X), X > 1, Group = TempGroup.

create_group([P | Tail], Same, Color, TempGroup, Group) :-
    is_valid(Same, P), same_color(Same, P, Color), not(member(P, TempGroup)),
    append(TempGroup, [P], NewGroup),
    get_neighbors(P, Neighbors),
    append(Tail, Neighbors, Candidates),
    create_group(Candidates, Same, Color, NewGroup, Group), !;
    create_group(Tail, Same, Color, TempGroup, Group).

get_neighbors(P, Neighbors) :-
    pos(Lin, Col) = P, NewLinPlus is Lin + 1, NewColPlus is Col + 1,
    NewLinMinus is Lin - 1, NewColMinus is Col - 1,
    Neighbors = [pos(NewLinPlus, Col), pos(Lin, NewColPlus),
        pos(NewLinMinus, Col), pos(Lin, NewColMinus)].

same_color(Same, P, Color) :-
    get_color(Same, P, NewColor), NewColor =:= Color.

is_valid(Same, P) :-
    pos(Lin, Col) = P, length(Same, ColSize), Col >= 0, Col < ColSize,
    get_same_lin_size(Same, Col, LineSize), Lin >= 0, Lin < LineSize.

get_same_lin_size(Same, Col, LinSize) :-
    nth0(Col, Same, Line), length(Line, LinSize).

%% remove_group(+Same, +Group, -NewSame) is semidet
%
%  Verdadeiro se NewSame é obtido de Same removendo os elemento especificados
%  em Group. A remoção é feita de acordo com as regras do jogo same.
%  Dica:
%    - crie um predicado auxiliar remove_column_group, que remove os elementos
%    de uma coluna específica
remove_group(Same, Group, NewSame) :-
    make_new_same(Same, Group, 0, NewSame).

make_new_same(Same, Group, Col, [NewCol | RestCol]) :-
    length(Same, LengthSame),
    Col < LengthSame,
    findall(X, member(pos(X, Col), Group), ListLin),
    ListLin \= [],
    nth0(Col, Same, Column),
    remove_column_group(ListLin, Column, Column, NewColumn),
    NewColumn \= [],
    [NewCol | _] = [NewColumn],
    IncCol is Col + 1,
    make_new_same(Same, Group, IncCol, RestCol), !.

make_new_same(Same, Group, Col, [NewCol | RestCol]) :-
    length(Same, LengthSame),
    Col < LengthSame,
    findall(X, member(pos(X, Col), Group), ListLin),
    ListLin = [],
    nth0(Col, Same, Column),
    [NewCol | _] = [Column],
    IncCol is Col + 1,
    make_new_same(Same, Group, IncCol, RestCol), !.

make_new_same(Same, _, Col, []) :-
    length(Same, X),
    X =:= Col, !.

make_new_same(Same, Group, Col, NewSame) :-
    IncCol is Col + 1,
    make_new_same(Same, Group, IncCol, NewSame).

remove_column_group([], _, Column, Column).

remove_column_group([Lin | Tail], SameColumn, Column, FinalColumn) :-
    nth0(Lin, SameColumn, Elem),
    selectchk(Elem, Column, NewColumn),
    remove_column_group(Tail, SameColumn, NewColumn, FinalColumn).
