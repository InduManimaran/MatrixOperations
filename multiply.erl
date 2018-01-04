-module(multiply).
-export([mul/2,generate_zero_row/2,combine4_quadrants/0,append_column/2,split/1,multiply/4,combine7/3,quadrants/4,write_matrix/0]).



%%sum of two m x n matrices
matrix_sum([],[]) -> [];
matrix_sum(MA, MB) ->
      lists:zipwith(fun(RA,RB) -> lists:zipwith(fun(X,Y)->X+Y end, RA,RB) end, MA, MB).


%%sum of three m x n matrices
matrix_sum(MA, MB,MC) ->
        lists:zipwith3(fun(RA,RB,RC) -> lists:zipwith3(fun(X,Y,Z)->X+Y+Z end, RA,RB,RC) end, MA, MB,MC).

%%difference of two m x n matrices
matrix_diff([],[]) -> [];
matrix_diff(MA, MB) ->
      lists:zipwith(fun(RA,RB) -> lists:zipwith(fun(X,Y)->X-Y end, RA,RB) end, MA, MB).


%% generates ROWS X COLS zero matrix
generate_zero_row(_,0)-> [];
generate_zero_row(COLS,ROWS) when ROWS > 0->
       lists:append([lists:map(fun(_)->0 end, lists:seq(1, COLS))],generate_zero_row(COLS,ROWS-1)); 
generate_zero_row(M,_)-> M.     

%% appends columns filled with 0's
append_column(M,Append) when Append >0 ->
          lists:map(fun(X)->lists:append(X,Append) end,M);
append_column(M,_)->
          M. 


%% Splits a matrix into 4 equal quadrants after appending 0's to rows and columns of the given matrix 
split(M) ->
      N=lists:append(M,generate_zero_row(length(hd(M)),length(M)rem 2)),
      N1=append_column(N,lists:flatten(generate_zero_row(length(hd(N)) rem 2,1))),

      NoR=length(N1) ,
      NoC=length(hd(N1)),
      
      if    NoR<NoC ->
              N2=lists:append(N1,generate_zero_row(NoC,NoC-NoR));
            NoR>NoC->
              N2=append_column(N1,lists:flatten(generate_zero_row(NoR-NoC,1)));
            true->
              N2=N1
      end,

      Split_at=trunc(length(N2)/2),
      MAuh=lists:sublist(N2,Split_at),
      MAlh=lists:nthtail(Split_at,N2),

      Q1=lists:map(fun(X) -> lists:sublist(X,Split_at) end,MAuh),
      Q2=lists:map(fun(X) -> lists:sublist(X,Split_at+1,NoC) end,MAuh),
      Q3=lists:map(fun(X) -> lists:sublist(X,Split_at) end,MAlh),
      Q4=lists:map(fun(X) -> lists:sublist(X,Split_at+1,NoC) end,MAlh),
      
      [Q1,Q2,Q3,Q4].



%% multiplication according to Strassens method
multiply(COMBINER7_ID,A,B,No)when length(A)== 2 andalso length(B)==2 andalso length(hd(A))==2 andalso length(hd(B))==2 ->
      COMBINER4 = spawn(multiply,combine4_quadrants,[]),       
      AF=lists:flatten(A),
      BF=lists:flatten(B),
      C1=[lists:nth(1,AF)*lists:nth(1,BF)+lists:nth(2,AF)*lists:nth(3,BF)],
      C2=[lists:nth(1,AF)*lists:nth(2,BF)+lists:nth(2,AF)*lists:nth(4,BF)],
      C3=[lists:nth(3,AF)*lists:nth(1,BF)+lists:nth(4,AF)*lists:nth(3,BF)],
      C4=[lists:nth(3,AF)*lists:nth(2,BF)+lists:nth(4,AF)*lists:nth(4,BF)],
      COMBINER4!{[C1],[C2],[C3],[C4],No,2,2,COMBINER7_ID};

smultiply(COMBINER7_ID,A,B,No)->
    ResRow=length(A),
    ResCol=length(hd(B)),
    X=split(A),
    Y=split(B),
   
    C4id=spawn(multiply,quadrants,[No,ResRow,ResCol,COMBINER7_ID])  ,       
    COMBINER7 = spawn(multiply,combine7,[7,maps:new(),C4id]),                                                      
   
    T1=matrix_sum(lists:nth(2,X),lists:nth(3,X)),
    T2=matrix_sum(lists:nth(3,Y),lists:nth(2,Y)),
    T3=matrix_sum(lists:nth(4,X),lists:nth(3,X)),
    T4=matrix_diff(lists:nth(4,Y),lists:nth(2,Y)),
    T5=matrix_diff(lists:nth(1,Y),lists:nth(3,Y)),
    T6=matrix_sum(lists:nth(2,X),lists:nth(1,X)),
    T7=matrix_diff(lists:nth(4,X),lists:nth(2,X)),
    T8=matrix_sum(lists:nth(3,Y),lists:nth(4,Y)),
    T9=matrix_diff(lists:nth(1,X),lists:nth(3,X)),
    T10=matrix_sum(lists:nth(1,Y),lists:nth(2,Y)),
                 
    spawn(multiply, multiply, [COMBINER7, T1,T2,1]),
    spawn(multiply, multiply, [COMBINER7,T3,lists:nth(3,Y),2]),
    spawn(multiply, multiply, [COMBINER7,lists:nth(2,X),T4,3]),
    spawn(multiply, multiply, [COMBINER7,lists:nth(3,X),T5,4]),
    spawn(multiply, multiply, [COMBINER7,T6,lists:nth(2,Y),5]),
    spawn(multiply, multiply, [COMBINER7,T7,T8,6]),
    spawn(multiply, multiply, [COMBINER7,T9,T10,7]).


%% generates the resultant quadrants

quadrants(No,ResRow,ResCol,COMBINER7)->
    COMBINER4 = spawn(multiply,combine4_quadrants,[]),
    receive
         {Map}->
                C1=matrix_diff(matrix_sum(maps:get(1,Map),maps:get(4,Map),maps:get(7,Map)),maps:get(5,Map)),
                C2=matrix_sum(maps:get(3,Map),maps:get(5,Map)),
                C3=matrix_sum(maps:get(2,Map),maps:get(4,Map)),
                C4=matrix_diff(matrix_sum(maps:get(1,Map),maps:get(6,Map),maps:get(3,Map)),maps:get(2,Map)),
                COMBINER4!{C1,C2,C3,C4,No,ResRow,ResCol,COMBINER7}  
     end.    


%% After inserting 7 matrices into Maps,
%%sends map to the function 'quadrants'
combine7(0,Map,PID)->
        PID!{Map};
combine7(No,Map,PID)->
        receive
          {Val,Key}->
                Map2=maps:put(Key,Val,Map),
                combine7(No-1,Map2,PID)
        end.


%%  Combines the 4 quadrants of the matrix and removes the additional
%% rows and columns appened with zeros during 'split' stage
%%  UH- upper half of resultant matrix
%%  LH -lower half of resultant matrix
%%  CC- resultant matrix with the correct no of columns
%%  CR- resultant matrix with the correct no of rows

%% sends the result to combine7 function created in the earlier recusive call
combine4_quadrants() ->
      receive
          {C1,C2,C3,C4,No,ResRow,ResCol,COMBINER7_ID} ->
             UH = lists:zipwith(fun(X,Y)->lists:append(X,Y)end,C1,C2),        
              LH = lists:zipwith(fun(X,Y)->lists:append(X,Y) end,C3,C4),
                   
              CR =lists:sublist(lists:append(UH,LH),ResRow),
              if length(hd(CR)) > ResCol ->
                      CC=lists:map(fun(X)->lists:sublist(X,ResCol) end,CR),
                      COMBINER7_ID!{CC,No};
                  true->
                      COMBINER7_ID!{CR,No}
               end  
      end.


%% write the resultant matrix to file
write_matrix()->
      receive
        {Map}->
          {ok,C}=maps:find(1,Map),
          {ok, F}=file:open("Filelocation", [read,write]),
          io:write(F,C)
      end.


%% reads matrix from a file 
read_matrix(Device,List) ->
    case file:read_line(Device) of
    eof ->
         List;
    {ok, Line} ->
        StrNumber = re:split(string:strip(Line, right, 10), "\s+", [notempty]),
        StrNumbers=lists:sublist(StrNumber,length(StrNumber)),
        X=lists:map(fun(Y)->erlang:list_to_integer(Y)end,lists:map(fun(X)-> erlang:binary_to_list(X)end, StrNumbers)),
        read_matrix(Device, lists:append(List,[X]))
    end.


%%Input: Path to the matrix files 
mul(AFile,BFile)->
    {ok, Device1} = file:open(AFile, [read]),
    {ok, Device2} = file:open(BFile, [read]),
    A=read_matrix(Device1,[]),
    B=read_matrix(Device2,[]),
    io:format("A is ~w ~n",[A]),
    io:format("B is ~w ~n",[B]),

    if length(hd(A))==length(B) ->
      Map=maps:new(),
      SPID=spawn(multiply,write_matrix,[]),
      COMBINER7 = spawn(multiply,combine7,[1,Map,SPID]),
      spawn(multiply, multiply, [COMBINER7,A,B,1]);
    true->
       io:format("The matrices can't be multiplied~n")
      end.
    

