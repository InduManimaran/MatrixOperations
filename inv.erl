-module(inv).
-export([main/1,donecounter/1,spawn_workers/4,row_worker/3,write_matrix/1]).
-define(rnd,10000).
-define(rr(X),round(X*1000000)/1000000).


display(SIZE,Col,F)when SIZE>0->
	 io:write(F,ets:lookup_element(matrix_inversion,Col,2)),
	%lookup_element(matrix_inversion,Col,2)],
		display(SIZE-1,Col+1,F);
display(0,_,_)->
io:format("Done writing into file~n").



%%'row_worker' notifies donecounter after it's operation is completed
%% Updates the key 'done' in the ets table after
%% all row_workers have completed theor operations
donecounter(0)->
		ets:update_element(matrix_inversion,done,[{2,1}]);
donecounter(Order)->
		receive
			done->
				donecounter(Order-1)
	  	end.

%% Performs the Row operation as in Gauss-Jordans method
row_worker(DoneID,ColNum,PivotRowNum)->
		Y=ets:lookup_element(matrix_inversion,save,2),
		X=ets:lookup_element(matrix_inversion,ColNum,2),%Current col
		NthEle=lists:nth(PivotRowNum,X),%pivot row's elt
		PivotElt=ets:lookup_element(matrix_inversion,pivotelt,2),
		if PivotRowNum==ColNum ->
				Factor = ?rr(1/PivotElt);
			true->
				Factor=?rr(NthEle/PivotElt)
		end,
		Z=lists:zipwith(fun(A,B) ->?rr((A-(B*Factor))) end, X, Y),
		List1=lists:append(lists:sublist(Z,PivotRowNum-1),[Factor]),
		InsertIntoEts=lists:append(List1,lists:nthtail(PivotRowNum,Z)),
		ets:update_element(matrix_inversion,ColNum,[{2,InsertIntoEts}]),
		DoneID!done.

%% spawns 'row_worker' function for each column
spawn_workers(0, _,_,_) -> [];
spawn_workers(NUM,DoneID,ColNum,PivotRowNum) ->
        spawn(inv, row_worker, [DoneID,ColNum,PivotRowNum]) ,
        spawn_workers(NUM-1,DoneID,ColNum+1,PivotRowNum).



%% First 'PivotColumnNum-1' elements are 0,
%%'PivotColumnNum'th element is PivotElt,
%%followed by 'SIZE-PivotColumnNum' zeros.
genereate_pivot_column(PivotColumnNum,PivotElt,SIZE)->
	lists:append(lists:append(lists:append([lists:map(fun(_)->0 end, lists:seq(1,PivotColumnNum-1))]),[PivotElt]),lists:append([lists:map(fun(_)->0 end, lists:seq(1,SIZE-PivotColumnNum))])).


%% Saves the pivot column ,pivot element in ets table
%% Inserts new pivot column  in ets table
%% Calls spawn workers  
scatter(Order,PivotColumnNum)->
		PivotColumn=ets:lookup_element(matrix_inversion,PivotColumnNum,2),
		PivotElt=lists:nth(PivotColumnNum,PivotColumn),
		if  PivotElt == 0 ->
			Unpivoted=ets:lookup_element(matrix_inversion,unpivoted,2),
			ets:update_element(matrix_inversion,unpivoted,[{2,lists:append(Unpivoted,[PivotColumnNum])}]),
			ets:update_element(matrix_inversion,done,[{2,1}]);
		true->
			ets:update_element(matrix_inversion,save,[{2,PivotColumn}]),
			ets:update_element(matrix_inversion,pivotelt,[{2,PivotElt}]),
			NewPivotColumn=genereate_pivot_column(PivotColumnNum,?rr(1/PivotElt),Order),
			ets:update_element(matrix_inversion,PivotColumnNum,[{2,NewPivotColumn}]),			
        	DoneCounterID=spawn(inv,donecounter,[Order]),
        	spawn(inv,spawn_workers,[Order,DoneCounterID,1,PivotColumnNum])	
        end.

%%Notifies 'sactter_workers' function when concurrent 
%%operation on all columns are completed
%%for a single pivot column.
wait_till_done(PID)->
		IsDone=ets:lookup_element(matrix_inversion,done,2),
		if IsDone ==1->
			PID!done;
		true->
			wait_till_done(PID)
		end.

%% Called by pivot_all function.
%%Calls the function scatter with parameters: the current pivot column and Order
scatter_workers(Order,UnpivotedCols) when length(UnpivotedCols)>0 ->
		wait_till_done(self()),
		receive 
			done->ets:update_element(matrix_inversion,done,[{2,0}])
		end,
		PivotColumnNum = hd(UnpivotedCols),						%hd=head of PIDS
        Unpivoted_Cols = lists:sublist(UnpivotedCols, 2, length(UnpivotedCols)),	
		scatter(Order,PivotColumnNum),
		scatter_workers(Order,Unpivoted_Cols);
scatter_workers(_,[]) -> io:format("Done spawning workers.~n").


%% Pivots all the unpivoted columns
%%Parameters:
%% 			NOUC - No. of unpivoted columns
%% 			SIZE - The no of unpivoted columns in the previous function call.
pivot_all(NOUC,SIZE,PID,WID)->
	Unpivoted=ets:lookup_element(matrix_inversion,unpivoted,2),
	No_Of_Unpivoted_Cols=length(Unpivoted),
		if No_Of_Unpivoted_Cols==NOUC ->
					io:format("Inverse does not exist.~n");
			No_Of_Unpivoted_Cols==0 ->
					io:format("Inversion complete.~n"),
					WID!complete;
			true ->
				ets:update_element(matrix_inversion,unpivoted,[{2,[]}]),
				scatter_workers(SIZE,Unpivoted),
				pivot_all(No_Of_Unpivoted_Cols,SIZE,PID,WID)
		end.



%% This function reads integers from the file 
%%Output:Returns the order of matrix to main

read_integers(Device,0,PID) ->
    case file:read_line(Device) of
    eof ->
        ok;
    {ok, Line} ->
        StrNumber = re:split(string:strip(Line, right, 10), "\s+", [notempty]),
        StrNumbers=lists:sublist(StrNumber,length(StrNumber)),
        A= lists:map(fun(Y)->erlang:list_to_integer(Y)end,lists:map(fun(X)-> erlang:binary_to_list(X)end, StrNumbers)),
        Order=hd(A),
       read_integers(Device,Order,1,PID)
    end.

read_integers(_,Order,COLNO,PID) when COLNO > Order->
    PID!{Order};
read_integers(Device,Order,COLNO,PID) ->
    case file:read_line(Device) of
    eof ->
        ok;
    {ok, Line} ->
        StrNumber = re:split(string:strip(Line, right, 10), "\s+", [notempty]),
        StrNumbers=lists:sublist(StrNumber,length(StrNumber)),
        A= lists:map(fun(Y)->erlang:list_to_integer(Y)end,lists:map(fun(X)-> erlang:binary_to_list(X)end, StrNumbers)),
        ets:insert(matrix_inversion,{COLNO,A}),
        read_integers(Device,Order,COLNO+1,PID)
    end.

write_matrix(Order)->
      receive
        complete->
			 {ok, F}=file:open("/Users/iminsight/Desktop/a.txt", [read,write]),
			  display(Order,1,F)
      end.

%% Input : Complete Path to the file's location
%%File Format: First line has N - the order of the matrix
%%				followed by N rows of N elemenets
%% Output: Inverted file
main(Filename) ->
	ets:new(matrix_inversion, [set, named_table,public]),
    {ok, Device} = file:open(Filename, [read]),
    read_integers(Device,0,self()),
    receive
    	{Order}->
    ets:insert(matrix_inversion,{unpivoted,[]}),
	ets:insert(matrix_inversion,{save,[]}),
	ets:insert(matrix_inversion,{pivotelt,[]}),
	ets:insert(matrix_inversion,{done,1}),
	scatter_workers(Order,lists:seq(1,Order)),
	WID=spawn(inv,write_matrix,[Order]),
	pivot_all(Order,Order,self(),WID)
	end.



