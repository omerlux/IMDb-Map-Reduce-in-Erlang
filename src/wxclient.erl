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
-export([start/0]).
-include_lib("wx/include/wx.hrl").


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
  TextCtrl  = wxTextCtrl:new(Frame, 1, [{value, ""}, {style, ?wxDEFAULT}]),
  wxTextCtrl:setToolTip(TextCtrl,"Enter your search value here"),
  Grid = create_grid(Frame),
  B31 = wxButton:new(Frame, 31, [{label,"Send Query"}, {style, ?wxBU_EXACTFIT}]),
  wxButton:setToolTip(B31, "Send your query to the disco"),


  Choices = ["Name","Year","Description", "Actor","Budget"],
  Choices2 = ["Name","Year","Description", "Actor","Budget","Number of Results"],

  %% Create a wxListBox that uses multiple selection
  ListBox = wxListBox:new(Frame, 1, [{size, {-1,100}},
    {choices, Choices}, {style, ?wxLB_SINGLE}]),
  wxListBox:setToolTip(ListBox, "Choose your search value category"),

  ComboBox = wxComboBox:new(Frame, 5, [{choices, Choices2}]),
  wxComboBox:setToolTip(ComboBox, "Choose the category your interested in from the results"),

  %%note: Sizers setup -------------------------------------------------------------------------------------------------
  wxSizer:add(SubSizer1, TopTxt, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(SubSizer1, Headline, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(SubSizer1, TextCtrl, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(SubSizer1, ListBox, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(SubSizer1, ComboBox, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(SubSizer1, B31, [{flag, ?wxALL}, {border, 8}]),
  wxSizer:add(GridSizer, Grid, [{flag, ?wxALL bor ?wxEXPAND }, {border, 8}]),%%TODO hide it at startup?

  Font = wxFont:new(14, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL,[{underlined, true}]),
  wxTextCtrl:setFont(TopTxt, Font),
  wxSizer:add(MainSizer, SubSizer1, [{flag, ?wxALIGN_LEFT}, {border, 5}]),
  wxSizer:add(MainSizer, SubSizer2, [{flag, ?wxALIGN_RIGHT}, {border, 5}]),
  wxSizer:add(SubSizer2, GridSizer, [{flag, ?wxALIGN_RIGHT}, {border, 5}]),
  wxSizer:hide(SubSizer2,GridSizer,[{recursive, true}]),

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
      wxGrid:setReadOnly(Grid, Row, 3, [{isReadOnly,true}]),
      wxGrid:setCellValue(Grid, Row, 4, "Editable"),
      case Row rem 4 of
        0 -> wxGrid:setCellBackgroundColour(Grid, Row, 3, ?wxRED);
        1 -> wxGrid:setCellBackgroundColour(Grid, Row, 3, ?wxGREEN),
          wxGrid:setCellTextColour(Grid, Row, 2, {255,215,0,255});
        2 -> wxGrid:setCellBackgroundColour(Grid, Row, 3, ?wxBLUE);
        _ -> wxGrid:setCellBackgroundColour(Grid, Row, 1, ?wxCYAN),
          wxGrid:setCellValue(Grid, Row, 1,
            "Centered\nhorizontally"),
          wxGrid:setCellAlignment(Grid, Row, 4,
            0,?wxALIGN_CENTER),
          wxGrid:setCellValue(Grid, Row, 4,
            "Centered\nvertically"),
          wxGrid:setCellAlignment(Grid, Row, 1,
            ?wxALIGN_CENTER,0),
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
  wx:foreach(Fun, lists:seq(0,99)),
  wxGrid:setColSize(Grid, 2, 150),
  wxGrid:connect(Grid, grid_cell_change),
  Grid.
