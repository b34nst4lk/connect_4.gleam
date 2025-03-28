import gleam/dict.{type Dict}

import lustre_http

import shared.{
  type Game, type GameState, type Turn, Continue, Game, Player, Red, Yellow,
  connect_4_height, connect_4_width, update_game,
}

import bibi/bitboard as b

pub type DebugLog {
  DebugLog(move_count: Int, turn: Turn, state: GameState)
}

pub type PlayerType {
  Human
  AI(bot_name: String)
}

pub type PlayerTypes {
  PlayerTypes(red: PlayerType, yellow: PlayerType)
}

pub type GameModel {
  GameModel(
    game: Game,
    player_types: PlayerTypes,
    move_counter: Int,
    move_history: Dict(Int, DebugLog),
    highlight_column: Int,
  )
}

pub fn get_active_player_type(model: GameModel) -> PlayerType {
  case model.game.active.turn {
    Red -> model.player_types.red
    Yellow -> model.player_types.yellow
  }
}

pub fn new_game(red: PlayerType, yellow: PlayerType) -> GameModel {
  let assert Ok(bitboard) = b.new(connect_4_width, connect_4_height)
  GameModel(
    Game(Player(Red, bitboard), Player(Yellow, bitboard), Continue),
    PlayerTypes(red, yellow),
    0,
    dict.new(),
    -1,
  )
}

pub fn update_model(model: GameModel, column: Int) -> GameModel {
  let #(updated_game, last_cell_updated) = update_game(model.game, column)
  let has_game_changed = updated_game != model.game
  case has_game_changed {
    True ->
      GameModel(
        ..model,
        game: updated_game,
        move_counter: model.move_counter + 1,
        move_history: dict.insert(
          model.move_history,
          last_cell_updated,
          DebugLog(
            model.move_counter,
            model.game.active.turn,
            updated_game.state,
          ),
        ),
        highlight_column: model.highlight_column,
      )
    False -> model
  }
}

pub type Message {
  // Main menu
  UserClickedMainMenu
  UserSelectedBot(t: Turn, bot_name: String)
  UserEnteredNumberOfGames(value: String)

  // Game
  NewGame(player_types: PlayerTypes)
  Move(column: Int)
  HighlightColumn(column: Int)
  UnhighlightColumn

  // AI battle
  UserStartedAIBattle(red: String, yellow: String, number_of_games: String)

  // APIs
  APIReceivedMove(Result(#(Int, Int), lustre_http.HttpError))
}
