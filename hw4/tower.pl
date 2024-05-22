counts(
    [Top1, Top2, Top3, Top4], 
    [Bottom1, Bottom2, Bottom3, Bottom4], 
    [Left1, Left2, Left3, Left4], 
    [Right1, Right2, Right3, Right4]
    ).

ntowers(0,[],[]).
ntowers(N, T, C).

% length of each row in T true
t_row_length([Row|Rows], N) :- 
    length(Row, N),
    t_row_length(Rows, N).
t_row_length([],N).

% length of rows and columns true
t_size(T, N) :-
    length(T, N),
    t_row_length(T, N).

t_uniq_row(Row, N) :-
    length(Row, N),
    fd_domain(Row, 1, N),
    fd_all_different(Row),
    fd_labeling(Row).

t_uniq_col([RowHead|RowTail], [NextRow|RestRows], N, Col, NewCol) :-
    append([RowHead], Col, NewCol),
    t_uniq_col(NextRow, RestRows, N, NewCol, NewNewCol).
t_uniq_col([RowHead|RowTail], [], N, Col, NewCol) :- 
    append([RowHead], Col, NewCol),
    t_uniq_row(NewCol, N).

t_uniq_col_test([Row|Rows], N) :-
    t_uniq_col(Row, Rows, N, [], FinalCol).

t_uniq([Row|Rows], N) :-
    t_uniq_row(Row, N),
    t_uniq(Rows, N).
t_uniq([], N).