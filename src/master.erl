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

-record(master_state, {servers = {0,[]}}).
-record(query, {type, searchVal, searchCategory, resultCategory}).
%%%===================================================================
%%%                             API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  {ok, Pid} = gen_server:start_link({global, node()}, ?MODULE, [], []),
  register(masterpid, Pid).


%%%===================================================================
%%%                   gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #master_state{}} | {ok, State :: #master_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  io:format("Master started.~n"),
  net_kernel:monitor_nodes(true),
  % distributing the data to whom that is ready to receive.
  ServersInfo = dataDistributor:distribute(),
  flush(),
  {ok, #master_state{servers = ServersInfo}}.


%%% ----------------------------- Handle Call -----------------------------
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
%% handle_call - query format answer
handle_call(Query = #query{}, {FromPID, _Tag}, State = #master_state{}) ->
  PID = spawn(fun() -> sendQuery(Query, FromPID, element(1,State#master_state.servers),element(2,State#master_state.servers)) end),
  io:format("Received query from ~p, created PID ~p~n", [FromPID, PID]),
  {reply, ok, State};
%% Handle_call - all other queries won't be replied
handle_call(_Request, _From, State = #master_state{}) ->
  {reply, ok, State}.


%%% ----------------------------- Handle Cast -----------------------------
%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #master_state{}) ->
  {noreply, NewState :: #master_state{}} |
  {noreply, NewState :: #master_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #master_state{}}).

%% Handle_cast - nodeup format answer
handle_cast({nodeup, Node}, State = #master_state{}) ->
  case lists:member(atom_to_list(Node), dataDistributor:readfile(["serverslist.txt"])) of
    true -> %% note: no other request can be handled while distributing
      io:format("A new node is up: ~p~n", [Node]),
      ServersInfo = dataDistributor:distribute(),
      flush(),
      UpdatedState = State#master_state{servers = ServersInfo};
    false ->  UpdatedState = State         % server isn't part of the distribution
  end,
  {noreply, UpdatedState};

%% Handle_cast - all other casted messages
handle_cast(_Request, State = #master_state{}) ->
  io:format("Message received: ~p~n", [_Request]),
  {noreply, State}.


%%% ----------------------------- Handle Info -----------------------------
%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #master_state{}) ->
  {noreply, NewState :: #master_state{}} |
  {noreply, NewState :: #master_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #master_state{}}).

%% Handle_info - {nodedown, Node} - distribute the data again because a node is down
handle_info({nodedown, Node}, State = #master_state{}) ->
  case string:str(atom_to_list(Node), "server") > 0 of
    true ->
      io:format("A node is down: ~p~n", [Node]),
      ServersInfo = dataDistributor:distribute(),
      flush();
  false ->
      ServersInfo = State#master_state.servers
  end,
  {noreply, State#master_state{servers = ServersInfo}};

%% Handle_info - all other requests
handle_info(_Info, State = #master_state{}) ->
  io:format("Message received: ~p~n", [_Info]),
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
%%%                       Internal functions
%%%===================================================================
%% sendQuery - sends the query to the servers and returns the client the answer
sendQuery(Query = #query{}, FromPID, NumOfServers, Servers) ->
  Result = sendQuery(Query, Servers, NumOfServers),
  io:format("Received final result at ~p, sending it to ~p~n", [self(), FromPID]),
  NumOfServers2 = lists:flatlength(checkAlive(Servers, [])),
  FromPID ! {NumOfServers2, Result}.

%% sendQuery - sent to all servers, now wait till you gather all the results
sendQuery(_Query = #query{}, [], NumberOfServers) ->
  gen_server:cast({masterpid, node()}, ping), %% TODO: check if it can be deleted
  net_kernel:monitor_nodes(true),
  gather(NumberOfServers);

%% sendQuery - send each server a job by a special pid
sendQuery(Query = #query{}, [Server0 | T], _NumberOfServers) ->
  Self = self(),
  PID = spawn(fun() -> sendServerJob(Self, Query, Server0) end),
  io:format("Entered sendQuery function and ~p spawned process ~p for server ~p~n", [self(), PID, Server0]),
  sendQuery(Query, T, _NumberOfServers).

%% sendServerJob - each pid will return the answer which given by the server, to his parentPID
sendServerJob(ParentPID, Query = #query{}, Server) ->
  % sending from gen_server the values of the data
  gen_server:call({serverpid, Server}, Query),
  receive
    table_error ->
      io:format("Server ~p returned table_error~n", [Server]);
    Reply2 ->
      io:format("sendServerJob function of server ~p entered receive block and sending to ~p~n", [Server, ParentPID]),
      ParentPID ! Reply2
  end,
  io:format("sendServerJob function of server ~p sent a reply to ~p~n", [Server, ParentPID]).

% gather - a pid for each client will wait to get all messages from the servers (# = ExpectedResults)
gather(0) -> [];
gather(ExpectedResults) ->
  io:format("Entered gather function expecting ~p results ~n", [ExpectedResults]),
  receive
    {nodedown, Server} ->
      case string:str(atom_to_list(Server), "server") > 0 of
        true -> % a server is down!! send what you've got
          io:format("A server is down, sending data without it...~n"),
          gather(ExpectedResults-1);
        false -> % this is not a server.. false alarm
          gather(ExpectedResults)
      end;
    {nodeup, _} ->    %% to ignore nodeup
      gather(ExpectedResults);
    Result ->
      io:format("Received result at ~p~n", [self()]),
      Result ++ gather(ExpectedResults - 1)
  end.

%% checkAlive - check each server if alive. Returns a list of server nodes which are alive
checkAlive([S0 | Servers], Nodes) ->
  case net_kernel:connect_node(S0) of
    true -> % erlang:monitor_node(Node, true),    % monitoring the node!
      checkAlive(Servers, [S0 | Nodes]);
    false -> checkAlive(Servers, Nodes)
  end;
checkAlive([], Servers) -> Servers.

% flush - flushing redundant messages
flush() ->
  receive
    _ -> flush()
  after
    0 -> ok
  end.