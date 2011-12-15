-module(prg_real).
-export([generate/5]).


%% Example: 
%%  prg_real:generate(10,2,52,34,53).
generate(C, Prev, D, L, N) ->
    [_, D1, L1, Prev1, Prev2] = prg:generate(4, Prev, D, L, N),
    A1 = prg:generate(C, Prev1, D, L, N),
    A2 = prg:generate(C, Prev2, D1*D, L1*L, N),
    lists:zipwith(fun
                      (0,Y) -> 0;
                      (X,Y) when X<Y -> X/Y;
                      (X,Y) -> Y/X
                   end, A1, A2).
    
