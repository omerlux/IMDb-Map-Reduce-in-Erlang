%%%-------------------------------------------------------------------
%%% @author Ilay
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. ספט׳ 2020 15:51
%%%-------------------------------------------------------------------
-module(master).
-author("Ilay-Omer").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(master_state, {}).
-record(query, {searchVal, searchCategory, resultCategory}).
%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  %%TODO add the distribution of the data to the servers
  {ok, Pid} = gen_server:start_link({global, node()}, ?MODULE, [], []),
  register(masterpid, Pid).
%% note:
%% working...
%% to connect with 'node' master, we need to use net_adm:ping('node'),
%% then we can send gen_server:func({global, 'node'}, _Request)

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #master_state{}} | {ok, State :: #master_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #master_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #master_state{}) ->
  {reply, Reply :: term(), NewState :: #master_state{}} |
  {reply, Reply :: term(), NewState :: #master_state{}, timeout() | hibernate} |
  {noreply, NewState :: #master_state{}} |
  {noreply, NewState :: #master_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #master_state{}} |
  {stop, Reason :: term(), NewState :: #master_state{}}).

%%handle_call({query,generic,SearchVal,SearchCategory,ResultCategory}, _From, State = #master_state{}) ->
%%
%%  %%gen_server:reply(_From, ("Your Query is: Get "++ ResultCategory ++ " of all movies " ++ SearchCategory ++ "'s containing: " ++ SearchVal)),
%%  Reply = ("Your Query is: Get "++ ResultCategory ++ " of all movies " ++ SearchCategory ++ "'s containing: " ++ SearchVal),
%%  {reply, Reply, State};

handle_call(Query = #query{}, _From, State = #master_state{}) ->
  {reply, sendQuery(Query), State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #master_state{}) ->
  {noreply, NewState :: #master_state{}} |
  {noreply, NewState :: #master_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #master_state{}}).
handle_cast(_Request, State = #master_state{}) ->
  io:format("Message received: ~p",[_Request]),
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #master_state{}) ->
  {noreply, NewState :: #master_state{}} |
  {noreply, NewState :: #master_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #master_state{}}).
handle_info(_Info, State = #master_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #master_state{}) -> term()).
terminate(_Reason, _State = #master_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #master_state{},
    Extra :: term()) ->
  {ok, NewState :: #master_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #master_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

sendQuery(Query = #query{}) ->
  Servers = readfile(["serverslist.txt"]),
  NumberOfServers = countList(Servers),
  sendQuery(Query,Servers,NumberOfServers).

sendQuery(_Query = #query{},[],NumberOfServers) ->
  gather(NumberOfServers);

sendQuery(Query = #query{},[Server0|T],_NumberOfServers) ->
  spawn(master, sendServerJob, [self(),Query,Server0]),
  sendQuery(Query,T,_NumberOfServers).

sendServerJob(ParentPID,Query = #query{},Server) ->
  ServerNode = list_to_atom(Server),
  % sending from gen_server the values of the data
  Reply = gen_server:call({serverpid, ServerNode}, Query),
  ParentPID ! Reply.

gather(ExpectedResults) ->
  receive
    Result -> [Result | gather(ExpectedResults-1)]
  end;

gather(0) -> [].

%% readfile - read file as strings separated by lines
readfile(FileName) ->
  {ok, Binary} = file:read_file(FileName),
  string:tokens(erlang:binary_to_list(Binary), "\r\n").

%% countList - returning the number of string elements in the list
countList([_|T]) ->
  count(1, T).
count(X,[_|T]) ->
  count(X+1, T);
count(X,[]) ->
  X.