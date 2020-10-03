%%%-------------------------------------------------------------------
%%% @author Ilay
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. ספט׳ 2020 20:39
%%%-------------------------------------------------------------------
-module(wxclient).
-author("Ilay-Omer").

%% API
-export([start/0, readfile/1]).
-include_lib("wx/include/wx.hrl").
-record(state,
{
  parent,
  config
}).
-record(movie_data, {id, title, original_title, year,
  date_published, genre, duration, country, language, director,
  writer, production_company, actors, description, avg_vote,
  votes, budget, usa_gross_income, worlwide_gross_income,
  metascore, reviews_from_users, reviews_from_critics}).
-record(query, {type, searchVal, searchCategory, resultCategory}).

%% note======================================= TODO: =============================================
%% 1. Mention that the search is case sensitive
%% 2. Add the number of result for every query, not related to the "Nubmer of results" option
%% 3. Showing the data on the Grid?
%% note===========================================================================================


%% Will get the pid of server
%% will send the query on button pressing
start() ->

  %%note: Frame and components build -----------------------------------------------------------------------------------
  WX = wx:new(),
  Frame = wxFrame:new(WX, 1, "Disco Map-Reduce Project"), %%TODO whats the difference from Panel?
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  SubSizer1 = wxBoxSizer:new(?wxVERTICAL),
  SubSizer2 = wxBoxSizer:new(?wxVERTICAL),
  GridSizer = wxGridSizer:new(?wxVERTICAL),

  TopTxt = wxStaticText:new(Frame, ?wxID_ANY, "Query Window"),
  Headline = wxStaticText:new(Frame, ?wxID_ANY, "Insert Value:"),
  TextCtrl = wxTextCtrl:new(Frame, 1, [{value, ""}, {style, ?wxDEFAULT}]),
  wxTextCtrl:setToolTip(TextCtrl, "Enter your search value here"),
  Grid = create_grid(Frame),
  ButtonSend = wxButton:new(Frame, ?wxID_ANY, [{label, "Send Query"}, {style, ?wxBU_EXACTFIT}]),
  wxButton:setToolTip(ButtonSend, "Send your query to the disco"),


  Choices = ["Title", "Year", "Genre", "Duration", "Country", "Language", "Director", "Writer", "Production Company", "Actor", "Description", "Score", "Budget"],
  Choices2 = ["Title", "Year", "Genre", "Duration", "Country", "Language", "Director", "Writer", "Production Company", "Actor", "Description", "Score", "Budget", "Number of results", "All"],

  %% Create a wxListBox that uses multiple selection
  ListBox = wxListBox:new(Frame, 1, [{size, {-1, 100}},
    {choices, Choices}, {style, ?wxLB_SINGLE}]),
  wxListBox:setToolTip(ListBox, "Choose your search value category"),

  ComboBox = wxComboBox:new(Frame, 5, [{choices, Choices2}]),
  wxComboBox:setToolTip(ComboBox, "Choose the category your interested in from the results"),

  wxEvtHandler:connect(ButtonSend, command_button_clicked, [{callback, fun handle_click_event/2},
    {userData, {wx:get_env(), TextCtrl, ListBox, ComboBox}}]),

  %%note: Sizers setup -------------------------------------------------------------------------------------------------
  wxSizer:add(SubSizer1, TopTxt, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(SubSizer1, Headline, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(SubSizer1, TextCtrl, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(SubSizer1, ListBox, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(SubSizer1, ComboBox, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(SubSizer1, ButtonSend, [{flag, ?wxALL}, {border, 8}]),
  wxSizer:add(GridSizer, Grid, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),%%TODO hide it at startup?

  Font = wxFont:new(14, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL, [{underlined, true}]),
  wxTextCtrl:setFont(TopTxt, Font),
  wxSizer:add(MainSizer, SubSizer1, [{flag, ?wxALIGN_LEFT}, {border, 5}]),
  wxSizer:add(MainSizer, SubSizer2, [{flag, ?wxALIGN_RIGHT}, {border, 5}]),
  wxSizer:add(SubSizer2, GridSizer, [{flag, ?wxALIGN_RIGHT}, {border, 5}]),
  wxSizer:hide(SubSizer2, GridSizer, [{recursive, true}]),

  wxWindow:setSizer(Frame, MainSizer),
  wxFrame:show(Frame).


create_grid(Panel) ->
  %% Create the grid with 100 * 5 cells
  Grid = wxGrid:new(Panel, 2, []),
  wxGrid:createGrid(Grid, 100, 5),

  Font = wxFont:new(16, ?wxFONTFAMILY_SWISS,
    ?wxFONTSTYLE_NORMAL,
    ?wxFONTWEIGHT_NORMAL, []),
  %% Fun to set the values and flags of the cells
  Fun =
    fun(Row) ->
      wxGrid:setCellValue(Grid, Row, 0, "Editable"),
      wxGrid:setCellValue(Grid, Row, 1, "Editable"),
      wxGrid:setCellValue(Grid, Row, 2, "Editable"),
      wxGrid:setCellValue(Grid, Row, 3, "Read only"),
      wxGrid:setCellTextColour(Grid, Row, 3, ?wxWHITE),
      wxGrid:setReadOnly(Grid, Row, 3, [{isReadOnly, true}]),
      wxGrid:setCellValue(Grid, Row, 4, "Editable"),
      case Row rem 4 of
        0 -> wxGrid:setCellBackgroundColour(Grid, Row, 3, ?wxRED);
        1 -> wxGrid:setCellBackgroundColour(Grid, Row, 3, ?wxGREEN),
          wxGrid:setCellTextColour(Grid, Row, 2, {255, 215, 0, 255});
        2 -> wxGrid:setCellBackgroundColour(Grid, Row, 3, ?wxBLUE);
        _ -> wxGrid:setCellBackgroundColour(Grid, Row, 1, ?wxCYAN),
          wxGrid:setCellValue(Grid, Row, 1,
            "Centered\nhorizontally"),
          wxGrid:setCellAlignment(Grid, Row, 4,
            0, ?wxALIGN_CENTER),
          wxGrid:setCellValue(Grid, Row, 4,
            "Centered\nvertically"),
          wxGrid:setCellAlignment(Grid, Row, 1,
            ?wxALIGN_CENTER, 0),
          wxGrid:setCellTextColour(Grid, Row, 3, ?wxBLACK),
          wxGrid:setCellAlignment(Grid, Row, 2,
            ?wxALIGN_CENTER,
            ?wxALIGN_CENTER),
          wxGrid:setCellFont(Grid, Row, 0, Font),
          wxGrid:setCellValue(Grid, Row, 2,
            "Centered vertically\nand horizontally"),
          wxGrid:setRowSize(Grid, Row, 80)
      end
    end,
  %% Apply the fun to each row
  wx:foreach(Fun, lists:seq(0, 99)),
  wxGrid:setColSize(Grid, 2, 150),
  wxGrid:connect(Grid, grid_cell_change),
  Grid.

handle_click_event(A = #wx{}, _B) ->
  {Env, TextBox, ListBox, ComboBox} = A#wx.userData,
  wx:set_env(Env),
  Query = #query{type = generic,
    searchVal = wxTextCtrl:getValue(TextBox),
    searchCategory = wxListBox:getString(ListBox, wxListBox:getSelection(ListBox)),
    resultCategory = wxComboBox:getValue(ComboBox)},
  [MasterNode | _T] = readfile(["clientslist.txt"]),
  _Ack = gen_server:call({masterpid, list_to_atom(MasterNode)}, Query),
  receive
    Movies when is_list(Movies) ->
      Window2 = wxWindow:new(),
      Frame2 = wxFrame:new(Window2, ?wxID_ANY, "Popup"),
      wxStaticText:new(Frame2, ?wxID_ANY, data2Text(Movies)),
      wxFrame:show(Frame2),
      wxWindow:setScrollbar(Window2, 1, 0, 16, 50),
      wxWindow:show(Window2);
    _ ->
      Window2 = wxWindow:new(),
      Frame2 = wxFrame:new(Window2, ?wxID_ANY, "Popup"),
      wxStaticText:new(Frame2, ?wxID_ANY, "Reply"),
      wxFrame:show(Frame2),
      wxWindow:show(Window2)
  end.

%% readfile - read file as strings separated by lines
readfile(FileName) ->
  {ok, Binary} = file:read_file(FileName),
  string:tokens(erlang:binary_to_list(Binary), "\r\n").

data2Text([MovieTuple = #movie_data{} | T]) ->
  Movie2Text =
    "Title: " ++ MovieTuple#movie_data.title
    ++ "\nYear: " ++ MovieTuple#movie_data.year
    ++ "\nGenre: " ++ MovieTuple#movie_data.genre
    ++ "\nDuration: " ++ MovieTuple#movie_data.duration
    ++ "\nCountry: " ++ MovieTuple#movie_data.country
    ++ "\nLanguage:" ++ MovieTuple#movie_data.language
    ++ "\nDirector: " ++ MovieTuple#movie_data.director
    ++ "\nWriter: " ++ MovieTuple#movie_data.writer
    ++ "\nProduction: " ++ MovieTuple#movie_data.production_company
    ++ "\nActors: " ++ MovieTuple#movie_data.actors
    ++ "\nDescription: " ++ MovieTuple#movie_data.description
    ++ "\nBudget: " ++ MovieTuple#movie_data.budget,
  Movie2Text ++ "\n" ++ "\n" ++ data2Text(T);

data2Text([]) -> [];
data2Text(_) -> null.



