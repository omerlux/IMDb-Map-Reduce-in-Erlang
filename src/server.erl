%%%-------------------------------------------------------------------
%%% @author Omer
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Oct 2020 15:33
%%%-------------------------------------------------------------------
-module(server).
-author("Omer").

-behaviour(gen_server).

%% API
%TODO: uncomment this
-compile(export_all).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(server_state, {table = none}).

-record(movie_data, {id, title,	original_title,	year,
  date_published, genre, duration, country, language,	director,
  writer, production_company,	actors,	description,	avg_vote,
  votes,	budget,	usa_gross_income,	worlwide_gross_income,
  metascore, reviews_from_users, reviews_from_critics}).

-record(query,
{
  type,
  searchVal,
  searchCategory,
  resultCategory
}).

%% Create a preprocessor
-define(MOVIE_RECORD,record_info(fields, movie_data)).
%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  {ok, Pid} = gen_server:start_link({global, node()}, ?MODULE, [], []),
  register(serverpid, Pid).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #server_state{}} | {ok, State :: #server_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #server_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #server_state{}) ->
  {reply, Reply :: term(), NewState :: #server_state{}} |
  {reply, Reply :: term(), NewState :: #server_state{}, timeout() | hibernate} |
  {noreply, NewState :: #server_state{}} |
  {noreply, NewState :: #server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #server_state{}} |
  {stop, Reason :: term(), NewState :: #server_state{}}).
%% a test query to get info
handle_call(test, _From, State = #server_state{}) ->
  Details = ets:tab2list(State#server_state.table),
  {reply, Details, State};
%% #generic query - get all the (column = filed2) of the map function - contains(field1 = text)
handle_call(Query = #query{}, _From, State = #server_state{}) ->
  % ets table is 'Table'
  #server_state{table = Table} = State,
  Return = mapreduce:get(Table, Query),
  {reply, Return, State};  % Return is the reply
%% all other queries won't be replied
handle_call(_Request, _From, State = #server_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #server_state{}) ->
  {noreply, NewState :: #server_state{}} |
  {noreply, NewState :: #server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #server_state{}}).
handle_cast({store,Data}, State = #server_state{}) ->
  N = atom_to_list(node()),
  Node = string:sub_string(N, 1, string:cspan(N, "@")),
  io:format(Node ++ " received data. Saving...~n"),
  Table = saveData(Data),
  io:format("Done!~n"),
  {noreply, State#server_state{ table = Table}};
handle_cast(_Request, State = #server_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #server_state{}) ->
  {noreply, NewState :: #server_state{}} |
  {noreply, NewState :: #server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #server_state{}}).
handle_info(_Info, State = #server_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #server_state{}) -> term()).
terminate(_Reason, _State = #server_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #server_state{},
    Extra :: term()) ->
  {ok, NewState :: #server_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #server_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% saveData - saving data into ets
saveData(Data) ->
  Table = ets:new(moviesdb, [set, named_table, {read_concurrency, true}]),
  keyVal(Data),
  Table.

%% keyVal - creates list of {key, value} for ets insertion
keyVal([]) ->
  ok;
keyVal([H|T]) ->
  Id = element(1, H),
  Details = #movie_data{
      title = element(2, H),
      original_title = element(3, H),
      year = element(4, H),
      date_published = element(5, H),
      genre = element(6, H),
      duration = element(7, H),
      country = element(8, H),
      language = element(9, H),
      director = element(10, H),
      writer = element(11, H),
      production_company = element(12, H),
      actors = element(13, H),
      description = element(14, H),
      avg_vote = element(15, H),
      votes = element(16, H),
      budget = element(17, H),
      usa_gross_income = element(18, H),
      worlwide_gross_income = element(19, H),
      metascore = element(20, H),
      reviews_from_users = element(21, H),
      reviews_from_critics = element(22, H) },
  ets:insert(moviesdb, {Id, Details}),
  keyVal(T).
