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

-record(server_state, {}).
-record(movie_data, {title,	original_title,	year,
  date_published, genre, duration, country, language,	director,
  writer, production_company,	actors,	description,	avg_vote,
  votes,	budget,	usa_gross_income,	worlwide_gross_income,
  metascore, reviews_from_users,	reviews_from_critics}).

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
  io:format("~p received data, saving it...", [Node]),
  saveData(Data),
  {noreply, State};
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
  ets:new(moviesdb, [set, named_table]),
  MovieList = keyVal(Data).


%% keyVal - creates list of {key, value} for ets insertion
keyVal([]) ->
  ok;
keyVal([H|T]) ->
  Tuple = create_record_from_tuple(?mov)
  keyVal(T).




%% create_record_from_tuple - creates a record from tuple
create_record_from_tuple([Field|Rest],Data,Result) ->
  case lists:keysearch(Field,1,Data) of
    {value,{_,Value}} ->
      create_record_from_tuple(Rest,Data,lists:append(Result,[Value]));
    _ ->
      create_record_from_tuple(Rest,Data,lists:append(Result,[undefined]))
  end.