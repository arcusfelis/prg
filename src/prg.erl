-module(prg).
-export([generate/5, generate/4, take/1]).

%% @doc Generates the pseudorandom value.
-spec generate(PreviousValue::integer(),
        D::integer(), L::integer(), N::integer()) -> integer().
generate(Prev, D, L, N) ->
    (D+Prev*L) rem N.

-spec generate(PreviousValue::integer(),
        D::integer(), L::integer(), N::integer()) -> [integer()].

%% @doc Generates the list of the pseudorandom values.
generate(Count, Prev, D, L, N) ->
    do_generate(Count, D, L, N, [Prev]).

-spec take([integer()]) -> 
    false | {D::integer(), L::integer(), N::integer()}.

%% @doc Gets the list of elements.
%%      Returns {D, L, N} of false.
take(L) ->
    N = lists:max(L)+1,
    NMax = N+100,
    take(L, N, NMax).



%%
%% Private functions' definition
%%

%% @doc Runs do_generate/4 for C times.
do_generate(C, D, L, N, [Prev|_]=Acc) 
    when C > 0 ->
    do_generate(C-1, D, L, N, 
        [generate(Prev, D, L, N) | Acc]);
do_generate(_C, _D, _L, _N, Acc) ->
    lists:reverse(Acc).


%% @doc Runs take(L, N) for N from N to NMax.
take(L, N, N) ->
    false;
take(L, N, NMax) ->
    case take(L, N) of
    false ->
        take(L, N+1, NMax);
    X -> X
    end.

%% @doc Changes X1, X2, X3 from 0 to I.
%%      Runs do_take/5 for all variants of X1, X2, X3.
take(L, N) ->
    I = 20,
    do_while(0, I, fun(X1) ->
        do_while(0, I, fun(X2) ->
            do_while(0, I, fun(X3) ->
                any([do_take(L, N, X1, X2, X3)
                    ,do_take(L, N, X1, X3, X2)
                    ,do_take(L, N, X2, X1, X3)
                    ,do_take(L, N, X3, X3, X1)
                    ,do_take(L, N, X3, X1, X2)
                    ,do_take(L, N, X3, X2, X1)])
            end)
        end)
    end).

%% @doc Returns non-false value of the list.
-spec any([term()]) -> term().

any([false|T]) ->
    any(T);
any([H|_]) ->
    H;
any([]) ->
    false.

%% @doc Runs the function F(X) from X to I while F(X) is not false.
do_while(I, I, _F) ->
    false;
do_while(X, I, F) ->
    case F(X) of
    false ->
        do_while(X+1, I, F);
    H -> H
    end.

do_take(A, B, C, N, X1, X2, X3) ->
    AA = A+X1*N,
    BB = B+X2*N,
    CC = C+X3*N,
    case (BB-CC) rem (AA-BB) of
    0 ->
        L = (BB-CC) div (AA-BB),
        D = CC - BB*L,
        {D, L};
    _ -> 
        false
    end.

%% @doc First argument is a list.
%%      Elements after 3 are used for validation.
do_take([A,B|[C|_]=T], N, X1, X2, X3) ->
    case do_take(A, B, C, N, X1, X2, X3) of
    {D, L} ->
        check_list(T, D, L, N);
    false ->
        false
    end.
    
%% @doc Validates selected D, L, N for the list of the generated values.
check_list([H1| [H2|_]=T], D, L, N) ->
    case generate(H1, D, L, N) of
    H2 ->
        check_list(T, D, L, N);
    _ -> false
    end;
check_list(_, D, L, N) ->
    {D, L, N}.
