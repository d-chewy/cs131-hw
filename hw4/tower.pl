counts(
    [Top1, Top2, Top3, Top4], 
    [Bottom1, Bottom2, Bottom3, Bottom4], 
    [Left1, Left2, Left3, Left4], 
    [Right1, Right2, Right3, Right4]
    ).

% length of each row in T true
t_row_length([Row|Rows], N) :- 
    length(Row, N),
    t_row_length(Rows, N).
t_row_length([],N).

c_length(T, N) :-
    t_row_length(T, N).

% length of rows and columns true
t_size(T, N) :-
    length(T, N),
    t_row_length(T, N).

all_unique([ ]).
all_unique([H|T]) :-
    member(H, T), !, fail.
all_unique([H|T]) :- all_unique(T).

elements_between(List, Min, Max) :-
    maplist(between(Min, Max), List).

uniq_list(List, N) :-
    length(List, N),
    elements_between(List, 1, N),
    all_unique(List).

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

/* % EXTRACTS Nth COLUMN AND CHECKS UNIQUENESS
t_uniq_col([Row|RestRows], I, Col) :-
    nth(I, Row, El),
    append(Col, [El], NewCol),
    t_uniq_col(RestRows, I, NewCol).
t_uniq_col([], I, Col) :- uniq_list(Col, Len).

% Checks all columns for uniqueness
t_uniq_cols(T, N, I) :-
    (I < N -> 
        t_uniq_col(T, I, []),
        NextI is I + 1,
        t_uniq_cols(T, N, NextI)
    ; t_uniq_col(T, N, [])). */

% =========================
% EXTRACTS Nth COLUMN
t_col([Row|RestRows], N, Col, FinalCol) :-
    nth(N, Row, El),
    append(Col, [El], NewCol),
    t_col(RestRows, N, NewCol, FinalCol).
t_col([], N, Col, FinalCol) :- FinalCol = Col.

t_col_helper(T, N, Cols) :-
    N = 1,
    t_col(T, 1, [], Col),
    Cols = [Col | []], !.
t_col_helper(T, N, Cols) :-
    t_col(T, N, [], Col),
    I is N-1,
    t_col_helper(T, I, RestCols),
    Cols = [Col | RestCols].

% EXTRACTS ALL COLUMNS FROM LEFT TO RIGHT 
t_cols(T, N, Cols) :-
    t_col_helper(T, N, RevCols), reverse(Cols, RevCols).

transpose_T(T, NewT) :- length(T, N), t_cols(T, N, NewT).

t_uniq_cols(T, N) :-
    transpose_T(T, NewT), t_uniq_rows(NewT, N).

t_uniq_rows([Row|Rows], N) :-
    uniq_list(Row, N),
    t_uniq_rows(Rows, N).
t_uniq_rows([], N).

t_unique(T, N) :- t_uniq_rows(T, N), t_uniq_cols(T, N).

% ===========================
% How does counts factor in to solving this puzzle? For left and right, check corresponding row for heights. For top and bottom, check corresponding columns for heights and match expected rule.

count_helper([RowH|RowT], Count, Max, InitCnt) :-
    (RowH > Max ->
        NewMax is RowH, CheckCnt is InitCnt + 1
    ; 
        NewMax is Max, CheckCnt is InitCnt), 
    count_helper(RowT, Count, NewMax, CheckCnt).
count_helper([], Count, Max, CheckCnt) :- Count = CheckCnt.

% used for checking left count and helper for all other checks
counts_check_list([Row|RestRows], [CountH|CountT]) :-
    count_helper(Row, CountH, 0, 0), counts_check_list(RestRows, CountT). 
counts_check_list([], []).

row_reverse_T([], NewT) :- NewT = []. 
row_reverse_T([THead|TTail], NewT) :-
    reverse(THead, RevHead), row_reverse_T(TTail, NewTail), append([RevHead], NewTail, NewT).

counts_right(T, CountList) :- row_reverse_T(T, RevT), counts_check_list(RevT, CountList).

counts_top(T, CountList) :- transpose_T(T, NewT), counts_check_list(NewT, CountList).

counts_bottom(T, CountList) :- transpose_T(T, NewT), row_reverse_T(NewT, FinalT), counts_check_list(FinalT, CountList).

counts_match(T, counts([],[],[],[])) :- !.
counts_match(T, counts(Top, Bottom, Left, Right)) :-
    /* (Iter = 0 ->
        counts_top(T, [Top1, Top2, Top3, Top4]), NewIter is Iter + 1
    ; Iter = 1 ->
        counts_bottom(T, [Bottom1, Bottom2, Bottom3, Bottom4]), NewIter is Iter + 1
    ; Iter = 2 ->
        counts_check_list(T, [Left1, Left2, Left3, Left4]), NewIter is Iter + 1
    ; Iter = 3 ->
        counts_right(T, [Right1, Right2, Right3, Right4]), NewIter is Iter), */
    counts_top(T, Top),
    counts_bottom(T, Bottom),
    counts_check_list(T, Left),
    counts_right(T, Right).
    % counts_match(T, CountRows, NewIter). 

plain_ntower(0,T,C) :- T = [], C = counts([],[],[],[]), !.
plain_ntower(N, T, C) :- t_size(T, N), t_unique(T, N), counts_match(T, C).

ntower(0,T,C) :- T = [], C = counts([],[],[],[]).
ntower(N, T, C):- t_size(T, N), t_unique(T, N), counts_match(T, C).

% ==================================================
% ntower implementation

fd_uniq_list(Row, N) :-
    length(Row, N),
    fd_domain(Row, 1, N),
    fd_all_different(Row),
    fd_labeling(Row).

fd_uniq_row([Row|RestRows], N).

% ==================================================
% part 2 and 3

speedup_helper([T1A|T1B], [T2A|T2B], [T3A|T3B], TimeN, TimePlain) :-
    TimeN is T2A - T1A, TimePlain is T3A - T2A. 


speedup(Ratio) :- 
    statistics(user_time, Time1),
    ntower(5,
          [[2,3,4,5,1],
           [5,4,1,3,2],
           Row3,
           [RC41,5|Row4Tail],
           Row5],
          counts(Top, [4|BottomTail],
                 [Left1,Left2,Left3,Left4,5],
                 Right)),
    statistics(user_time, Time2),
    plain_ntower(5,
          [[2,3,4,5,1],
           [5,4,1,3,2],
           Row3,
           [RC41,5|Row4Tail],
           Row5],
          counts(Top, [4|BottomTail],
                 [Left1,Left2,Left3,Left4,5],
                 Right)),
    statistics(user_time, Time3),
    speedup_helper(Time1, Time2, Time3, TimeN, TimePlain),
    Ratio is TimePlain/TimeN.

ambiguous(N, counts(Top, Bottom, Left, Right), T1, T2) :-
    % Find the first solution T1
    ntower(N, T1, counts(Top, Bottom, Left, Right)),
    % Find a second distinct solution T2
    ntower(N, T2, counts(Top, Bottom, Left, Right)),
    T1 \= T2.  % Ensure T1 and T2 are distinct