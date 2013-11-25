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
    get_col_lin_size(Same, ColSize, LinSize), 
    print_solution(Moves, Same, ColSize, LinSize), !.

%% get_col_lin_size(+Same, -ColSize, -LinSize) is det
%
% Verdadeiro se ColSize e LinSize é o tamanho das colunas e linhas do jogo 
% inicial Same
get_col_lin_size(Same, ColSize, LinSize) :-
    length(Same, ColSize),
    [Line | _] = Same,
    length(Line, LinSize).

%% print_solution(+Moves, +Same, +ColSize, +LinSize) is semidet
%
% Verdadeiro se todas as jogadas foram impressas
print_solution([], _, _, _).

print_solution([M|Tail], Same, ColSize, LinSize) :-
    group(Same, M, Group),
    remove_group(Same, Group, NewSame),
    length(NewSame, X),
    check_columns(NewSame, LinSize, RenewSame),
    Total is ColSize - X,
    build_list(Total, LinSize, EmptyColumn),
    append(RenewSame, EmptyColumn, ResultSame),
    print_move(M, ResultSame),
    print_solution(Tail, NewSame, ColSize, LinSize).

%% print_move(+P, +Same) is det
%
% Verdadeiro se um movimento foi impresso
print_move(pos(X, Y), Same) :-
    write(X), put_char(' '),
    write(Y), writeln('\n'),
    transpose(Same, NewSame),
    write_matrix(NewSame),
    write('\n').

%% check_columns(+RenewSame, +LinSize, -NewSame) is semidet
%
% Verdadeiro se um novo same for criado com os zeros adicionados.
check_columns([Column | Rest], LineSize, [NewColumn | RestColumn]) :-
    length(Column, X), Total is LineSize - X,
    add_zeros(Total, ZeroList),
    append(Column, ZeroList, NewColumn),
    check_columns(Rest, LineSize, RestColumn).

check_columns([],_,[]).

%% add_zeros(+Ttoal, -ZeroList) is semidet
%
% Verdadeiro se em uma posição removida for adicionado zero.
add_zeros(Total, [P|Rest]) :-
    Total > 0, P = 0,
    NewTotal is Total - 1,
    add_zeros(NewTotal, Rest).

add_zeros(0, []).

%% build_list(+Total, +LinSize, -NewList) is det
%
% Verdadeiro se um lista com zeros for criada.
build_list(Total, LinSize, [S|T]) :-
    Total > 0, add_zeros(LinSize, List),
    S = List, NewTotal is Total - 1,
    build_list(NewTotal, LinSize, T).

build_list(0, _, []).

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
    get_all_pos(Same, 0, 0, ColSize, LinSize, ListPos),
    make_groups(Same, ListPos, [], _, Group).

%% get_all_pos(+Same, +Col, +Lin, +ColSize, +LinSize, -ListPos) is det
%
% Verdadeiro se todas as posição do jogo same forem criadas.
get_all_pos(Same, Col, Lin, ColSize, LinSize, [P | RestP]) :-
    Lin < LinSize, P = pos(Lin, Col), 
    NewLin is Lin + 1, 
    get_all_pos(Same, Col, NewLin, ColSize, LinSize, RestP), !.

get_all_pos(Same, Col, Lin, ColSize, Lin, ListPos) :-
    NewCol is Col + 1, NewCol < ColSize,
    get_same_lin_size(Same, NewCol, LinSize),
    get_all_pos(Same, NewCol, 0, ColSize, LinSize, ListPos).

get_all_pos(_, _, _, _, _, []).

%% make_groups(+Same, +ListPos, +TempGroups, -NewGroup, - Group) is nondet
%
% Verdadeiro se um Grupo para uma posição for criado. Não há repetição
% de Grupos.
make_groups(_, _, _, NewGroup, Group) :-
    nonvar(NewGroup), Group = NewGroup.

make_groups(Same, [P | Tail], TempGroups, _, Group) :-
    group(Same, P, NewTempGroup),
    not_member_group(TempGroups, NewTempGroup),
    append(TempGroups, [NewTempGroup], NewTempGroups), !,
    make_groups(Same, Tail, NewTempGroups, NewTempGroup, Group).

make_groups(Same, [_ | Tail], TempGroups, _, Group) :-
    make_groups(Same, Tail, TempGroups, _, Group).

make_groups(_, [], _, _, _) :- fail.

%% not_member_group(+TempGroups, +NewGroup) is semidet
%
% Verdadeiro se uma posição não pertencer a um Grupo já visitado.
not_member_group([Group | Tail], NewGroup) :-
    [P | _] = NewGroup, not(member(P, Group)),
    not_member_group(Tail, NewGroup).

not_member_group([], _).

%% grupo(+Same, +P, -Group) is semidet
%
%  Verdadeiro se Group é um grupo de Same que contém a posição P.

group(Same, P, Group) :-
    get_color(Same, P, Color), Candidates = [P],
    create_group(Candidates, Same, Color, [], Group).

%% get_color(+Same, +P, -Color) is det
%
% Verdadeiro se encontrar a cor de um posição P.
get_color(Same, P, Color) :-
    pos(Lin, Col) = P, nth0(Col, Same, X), nth0(Lin, X, Color).

%% create_group(+Candidates, +Same, +Color, -NewGroup) is det
%
% Verdadeiro se um novo grupo for criado para uma posição P.
create_group([], _, _, TempGroup, Group) :-
    length(TempGroup, X), X > 1, Group = TempGroup.

create_group([P | Tail], Same, Color, TempGroup, Group) :-
    is_valid(Same, P), same_color(Same, P, Color), not(member(P, TempGroup)),
    append(TempGroup, [P], NewTempGroup), get_neighbors(P, Neighbors),
    append(Tail, Neighbors, Candidates),
    create_group(Candidates, Same, Color, NewTempGroup, Group), !;
    create_group(Tail, Same, Color, TempGroup, Group).

%% get_neighbors(+P, -Neighbors) is det
%
% Verdadeiro se os vizinhos de uma posição P for encontrada.
get_neighbors(P, Neighbors) :-
    pos(Lin, Col) = P, NewLinPlus is Lin + 1, NewColPlus is Col + 1,
    NewLinMinus is Lin - 1, NewColMinus is Col - 1,
    Neighbors = [pos(NewLinPlus, Col), pos(Lin, NewColPlus),
        pos(NewLinMinus, Col), pos(Lin, NewColMinus)].

%% same_color(+Same, +P, +Color) is semidet
%
% Verdadeiro se a cor de uma posição é igual a posição P "clicada".
same_color(Same, P, Color) :-
    get_color(Same, P, NewColor), NewColor =:= Color.

%% is_valid(+Same, +P) is semidet
%
% Verdadeiro se a posição P está dentro dos limites de tamanho do jogo.
is_valid(Same, P) :-
    pos(Lin, Col) = P, length(Same, ColSize), Col >= 0, Col < ColSize,
    get_same_lin_size(Same, Col, LineSize), Lin >= 0, Lin < LineSize.

% get_same_lin_size(+Same, +Col, -LinSize) is det
%
% Verdadeiro se o tamanho da linha de um coluna especifica for encontrada.
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
    remove_column_group(Same, Group, [], NewSame, pos(0,0)), !.

%% remove_column_group(+Same, +Group, -NewColumn, -NewSame, -P) is semidet
%
% Verdadeiro se um novo same for criado com os elementos removidos.
remove_column_group(Same, Group, NewColumn, NewSame, pos(Lin,Col)) :-
    not(member(pos(Lin,Col), Group)), nth0(Col, Same, Line), 
    nth0(Lin, Line, Elem),
    append(NewColumn, [Elem], NewElemColumn), NewLin is Lin + 1,
    remove_column_group(Same, Group, NewElemColumn, NewSame, 
                                pos(NewLin, Col)).

remove_column_group(Same, Group, NewColumn, NewSame, pos(Lin,Col)) :-
    nth0(Col, Same, Line),
    length(Line, SizeLin),
    Lin < SizeLin,
    NewLin is Lin + 1,
    remove_column_group(Same, Group, NewColumn, NewSame, pos(NewLin,Col)).

remove_column_group(Same, Group, [], NewSame, pos(_,Col)) :-
    length(Same, X), Col < X, NewCol is Col + 1,
    remove_column_group(Same, Group, [], NewSame, pos(0, NewCol)).

remove_column_group(Same, Group, NewColumn, [NewColumn | RestColumn], 
                            pos(_,Col)) :-
    length(Same, X), Col < X, NewCol is Col + 1,
    remove_column_group(Same, Group, [], RestColumn, pos(0, NewCol)).

remove_column_group(_, _, _, [], _).
