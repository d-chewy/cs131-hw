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

uniq_list(Row, N) :-
    length(Row, N),
    fd_domain(Row, 1, N),
    fd_all_different(Row),
    fd_labeling(Row).

/* t_uniq_col([RowHead|RowTail], [NextRow|RestRows], N, Col, NewCol) :-
    append([RowHead], Col, NewCol),
    t_uniq_col(NextRow, RestRows, N, NewCol, NewNewCol).
t_uniq_col([RowHead|RowTail], [], N, Col, NewCol) :- 
    append([RowHead], Col, NewCol),
    uniq_list(NewCol, N). */

/* % EXTRACTS Nth COLUMN AND CHECKS UNIQUENESS
t_uniq_col([Row|RestRows], N, Col, FinalCol) :-
    nth(N, Row, El),
    append(Col, [El], NewCol),
    t_uniq_col(RestRows, N, NewCol, FinalCol).
t_uniq_col([], N, Col, FinalCol) :- uniq_list(Col, Len), FinalCol = Col.

t_uniq_col_helper(T, N, Cols) :-
    N = 1,
    t_uniq_col(T, 1, [], Col),
    Cols = [Col | []], !.
t_uniq_col_helper(T, N, Cols) :-
    t_uniq_col(T, N, [], Col),
    I is N-1,
    t_uniq_col_helper(T, I, RestCols),
    Cols = [Col | RestCols].

% EXTRACTS ALL COLUMNS FROM LEFT TO RIGHT 
t_uniq_cols(T, N, Cols) :-
    t_uniq_col_helper(T, N, RevCols), reverse(Cols, RevCols). */

% EXTRACTS Nth COLUMN AND CHECKS UNIQUENESS
t_uniq_col([Row|RestRows], N, Col) :-
    nth(N, Row, El),
    append(Col, [El], NewCol),
    t_uniq_col(RestRows, N, NewCol).
t_uniq_col([], N, Col) :- uniq_list(Col, Len).

% Checks all columns for uniqueness
t_uniq_cols(T, N) :-
    N = 1,
    t_uniq_col(T, 1, []).
t_uniq_cols(T, N) :-
    t_uniq_col(T, N, []),
    I is N-1,
    t_uniq_cols(T, I), !.

t_uniq_rows([Row|Rows], N) :-
    uniq_list(Row, N),
    t_uniq_rows(Rows, N).
t_uniq_rows([], N).

t_unique(T, N) :- t_uniq_rows(T, N), t_uniq_cols(T, N).

% How does counts factor in to solving this puzzle? For left and right, check corresponding row for heights. For top and bottom, check corresponding columns for heights and match expected rule.